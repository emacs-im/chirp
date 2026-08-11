// Copyright (C) 2026 Chirp contributors
// SPDX-License-Identifier: MIT

#[cfg(feature = "test-vector")]
use std::time::Instant;
use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    panic::{catch_unwind, AssertUnwindSafe},
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, TryRecvError},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
#[cfg(feature = "juicebox")]
use chat_xdk_core::keys::juicebox::{JuiceboxApi, JuiceboxClient, JuiceboxConfig};
use chat_xdk_core::{
    keys::juicebox::{RecoverFailureReason, RecoverResult},
    ChatCore,
};
use chat_xdk_core::{
    AttachmentInfo, EncryptMessageParams, Event, MessageContent, ReplyPreviewValidation,
    SendPayload,
};
use emacs::{defun, Env, Result, ResultExt, Transfer, Value};
use serde::{Deserialize, Serialize};
use url::Url;
use zeroize::{Zeroize, Zeroizing};

emacs::plugin_is_GPL_compatible!();

const MODULE_VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "/chat-xdk-0.4.3");
const MAX_JOB_ID: i64 = (1 << 28) - 1;
const MAX_RECOVERY_INPUT_BYTES: usize = 1024 * 1024;
const MAX_REGISTERED_KEYS: usize = 32;
const MAX_DECRYPT_INPUT_BYTES: usize = 20 * 1024 * 1024;
const MAX_ENCRYPT_INPUT_BYTES: usize = 32 * 1024;
const MAX_MESSAGE_TEXT_BYTES: usize = 16 * 1024;
const MAX_SIGNING_KEYS: usize = 512;
#[cfg(feature = "juicebox")]
const RECOVERY_TIMEOUT: Duration = Duration::from_secs(60);

#[derive(Debug)]
enum NativeError {
    Closed,
    Busy,
    Stale,
    InvalidInput(String),
    WorkerFailed,
    Xdk(String),
}

impl fmt::Display for NativeError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closed => formatter.write_str("XChat native session is closed"),
            Self::Busy => formatter.write_str("XChat recovery is already active"),
            Self::Stale => formatter.write_str("XChat recovery job is stale"),
            Self::InvalidInput(message) => formatter.write_str(message),
            Self::WorkerFailed => formatter.write_str("XChat recovery worker failed"),
            Self::Xdk(message) => write!(formatter, "XChat SDK failed: {message}"),
        }
    }
}

impl std::error::Error for NativeError {}

#[derive(Default)]
struct CancelToken {
    requested: AtomicBool,
}

impl CancelToken {
    fn request(&self) -> bool {
        !self.requested.swap(true, Ordering::AcqRel)
    }

    fn requested(&self) -> bool {
        self.requested.load(Ordering::Acquire)
    }

    #[cfg(feature = "test-vector")]
    fn wait(&self, duration: Duration) -> bool {
        let deadline = Instant::now() + duration;
        while !self.requested() {
            let now = Instant::now();
            if now >= deadline {
                break;
            }
            thread::sleep((deadline - now).min(Duration::from_millis(5)));
        }
        self.requested()
    }
}

struct RecoveredKeys {
    bytes: Zeroizing<Vec<u8>>,
    public_key_version: String,
    user_id: String,
}

#[derive(Debug, PartialEq, Eq)]
enum RecoveryFailure {
    NotRegistered,
    InvalidAuth,
    UpgradeRequired,
    RateLimited,
    AssertionFailed,
    KeyReconstructionFailed,
    NoTokens,
    RegisteredKeyMismatch,
    AmbiguousRegisteredKey,
}

enum WorkerTerminal {
    Recovered(RecoveredKeys),
    IncorrectPin { guesses_remaining: Option<u16> },
    Failure(RecoveryFailure),
    Uncertain,
    Cancelled,
    WorkerFailed,
}

struct RecoveryJob {
    id: i64,
    epoch: i64,
    cancel: Arc<CancelToken>,
    receiver: Receiver<WorkerTerminal>,
    thread: Option<JoinHandle<()>>,
}

impl RecoveryJob {
    fn join(&mut self) -> bool {
        self.thread
            .take()
            .is_none_or(|thread| thread.join().is_ok())
    }

    fn cancel_and_join(mut self) {
        self.cancel.request();
        let _ = self.join();
    }

    fn cancel_and_detach(mut self) {
        self.cancel.request();
        drop(self.thread.take());
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryInput {
    user_id: String,
    sdk_config: String,
    tokens: BTreeMap<String, String>,
    max_guess_count: u16,
    registered_keys: Vec<RegisteredKey>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RegisteredKey {
    version: String,
    identity_public_key: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SdkConfiguration {
    realms: Vec<RealmConfiguration>,
    register_threshold: u32,
    recover_threshold: u32,
    pin_hashing_mode: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RealmConfiguration {
    id: String,
    address: String,
    #[serde(default)]
    public_key: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DecryptInput {
    events: Vec<String>,
    signing_keys: Vec<SigningKeyInput>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EncryptTextInput {
    conversation_id: String,
    text: String,
}

#[derive(Serialize)]
struct PreparedTextMessage {
    message_id: String,
    encoded_message_create_event: String,
    encoded_message_event_signature: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SigningKeyInput {
    user_id: String,
    public_key_version: String,
    public_key: String,
    identity_public_key: String,
    identity_public_key_signature: String,
}

#[cfg(feature = "juicebox")]
struct RecoveryRequest {
    pin: Zeroizing<Vec<u8>>,
    user_id: String,
    config: JuiceboxConfig,
    registered_keys: Vec<RegisteredKey>,
}

#[cfg(feature = "juicebox")]
impl Drop for RecoveryRequest {
    fn drop(&mut self) {
        self.config.config_json.zeroize();
        for token in self.config.tokens.values_mut() {
            token.zeroize();
        }
    }
}

struct NativeState {
    core: Option<ChatCore>,
    user_id: Option<String>,
    next_job_id: i64,
    recovery: Option<RecoveryJob>,
}

struct ClosedState {
    core: Option<ChatCore>,
    user_id: Option<String>,
    recovery: Option<RecoveryJob>,
}

struct NativeSession {
    state: Mutex<NativeState>,
}

impl NativeSession {
    fn new() -> Self {
        let core = ChatCore::new();
        core.set_cache_keys(true);
        Self {
            state: Mutex::new(NativeState {
                core: Some(core),
                user_id: None,
                next_job_id: 1,
                recovery: None,
            }),
        }
    }

    fn lock_state(&self) -> std::sync::MutexGuard<'_, NativeState> {
        self.state.lock().unwrap_or_else(|error| error.into_inner())
    }

    fn with_core<T>(
        &self,
        operation: impl FnOnce(&ChatCore) -> std::result::Result<T, NativeError>,
    ) -> std::result::Result<T, NativeError> {
        let state = self.lock_state();
        let core = state.core.as_ref().ok_or(NativeError::Closed)?;
        operation(core)
    }

    fn close(&self) -> ClosedState {
        let mut state = self.lock_state();
        ClosedState {
            core: state.core.take(),
            user_id: state.user_id.take(),
            recovery: state.recovery.take(),
        }
    }

    fn destroy(&self) -> bool {
        let mut closed = self.close();
        let existed = closed.core.is_some();
        if let Some(recovery) = closed.recovery.take() {
            recovery.cancel_and_join();
        }
        drop(closed.user_id.take());
        drop(closed.core.take());
        existed
    }

    fn live(&self) -> bool {
        self.lock_state().core.is_some()
    }

    fn unlocked(&self) -> std::result::Result<bool, NativeError> {
        let state = self.lock_state();
        state
            .core
            .as_ref()
            .map(|core| core.is_unlocked() && state.user_id.is_some())
            .ok_or(NativeError::Closed)
    }

    fn decrypt(&self, input_json: String) -> std::result::Result<String, NativeError> {
        let (events, signing_keys) = parse_decrypt_input(input_json)?;
        self.with_core(|core| {
            if !core.is_unlocked() {
                return Err(NativeError::InvalidInput(
                    "XChat native session is locked".into(),
                ));
            }
            let output = decrypt_events(core, &events, &signing_keys)?;
            serde_json::to_string(&output).map_err(|error| NativeError::Xdk(error.to_string()))
        })
    }

    fn prepare_text_payload(
        &self,
        input_json: &str,
    ) -> std::result::Result<SendPayload, NativeError> {
        let input = parse_encrypt_text_input(input_json)?;
        let state = self.lock_state();
        let core = state.core.as_ref().ok_or(NativeError::Closed)?;
        let sender_id = state.user_id.as_ref().ok_or_else(|| {
            NativeError::InvalidInput("XChat native sender identity is unavailable".into())
        })?;
        if !core.is_unlocked() {
            return Err(NativeError::InvalidInput(
                "XChat native session is locked".into(),
            ));
        }
        let mut params = EncryptMessageParams::new(input.conversation_id, input.text);
        params.sender_id = Some(sender_id.clone());
        core.encrypt_message(params)
            .map_err(|_| NativeError::Xdk("message encryption failed for this conversation".into()))
    }

    fn encrypt_text(&self, input_json: String) -> std::result::Result<String, NativeError> {
        let payload = self.prepare_text_payload(&input_json)?;
        serde_json::to_string(&PreparedTextMessage {
            message_id: payload.message_id,
            encoded_message_create_event: payload.encrypted_content,
            encoded_message_event_signature: payload.encoded_event_signature,
        })
        .map_err(|error| NativeError::Xdk(error.to_string()))
    }

    fn spawn_recovery(
        &self,
        epoch: i64,
        operation: impl FnOnce(Arc<CancelToken>) -> WorkerTerminal + Send + 'static,
    ) -> std::result::Result<i64, NativeError> {
        if epoch <= 0 {
            return Err(NativeError::InvalidInput(
                "XChat recovery epoch must be positive".into(),
            ));
        }
        let mut state = self.lock_state();
        let core = state.core.as_ref().ok_or(NativeError::Closed)?;
        if core.is_unlocked() {
            return Err(NativeError::InvalidInput(
                "XChat native session is already unlocked".into(),
            ));
        }
        if state.recovery.is_some() {
            return Err(NativeError::Busy);
        }
        let id = state.next_job_id;
        let next_id = id
            .checked_add(1)
            .filter(|next| *next <= MAX_JOB_ID)
            .ok_or_else(|| NativeError::InvalidInput("XChat recovery job IDs exhausted".into()))?;
        let cancel = Arc::new(CancelToken::default());
        let worker_cancel = Arc::clone(&cancel);
        let (sender, receiver) = mpsc::channel();
        let worker = thread::Builder::new()
            .name("chirp-xchat-recovery".into())
            .spawn(move || {
                let mut terminal =
                    catch_unwind(AssertUnwindSafe(|| operation(Arc::clone(&worker_cancel))))
                        .unwrap_or(WorkerTerminal::WorkerFailed);
                if worker_cancel.requested() {
                    terminal = WorkerTerminal::Cancelled;
                }
                let _ = sender.send(terminal);
            })
            .map_err(|_| NativeError::WorkerFailed)?;
        state.next_job_id = next_id;
        state.recovery = Some(RecoveryJob {
            id,
            epoch,
            cancel,
            receiver,
            thread: Some(worker),
        });
        Ok(id)
    }

    #[cfg(feature = "juicebox")]
    fn start_recovery(
        &self,
        epoch: i64,
        pin: String,
        input_json: String,
    ) -> std::result::Result<i64, NativeError> {
        let request = parse_recovery_request(pin, input_json)?;
        self.spawn_recovery(epoch, move |cancel| network_recover(request, cancel))
    }

    #[cfg(feature = "test-vector")]
    fn start_mock_recovery(
        &self,
        epoch: i64,
        pin: String,
        delay_msec: i64,
    ) -> std::result::Result<i64, NativeError> {
        if !(0..=5_000).contains(&delay_msec) {
            return Err(NativeError::InvalidInput(
                "XChat mock recovery delay is out of range".into(),
            ));
        }
        let pin = validate_pin(pin)?;
        let delay = Duration::from_millis(delay_msec as u64);
        self.spawn_recovery(epoch, move |cancel| mock_recover(pin, delay, &cancel))
    }

    fn poll_recovery(&self, id: i64, epoch: i64) -> std::result::Result<RecoveryPoll, NativeError> {
        let (mut job, terminal) = {
            let mut state = self.lock_state();
            if state.core.is_none() {
                return Err(NativeError::Closed);
            }
            let job = state.recovery.as_ref().ok_or(NativeError::Stale)?;
            if job.id != id || job.epoch != epoch {
                return Err(NativeError::Stale);
            }
            match job.receiver.try_recv() {
                Err(TryRecvError::Empty) => return Ok(RecoveryPoll::Pending),
                result => {
                    let job = state.recovery.take().ok_or(NativeError::Stale)?;
                    let terminal = result.ok();
                    (job, terminal)
                }
            }
        };
        if !job.join() {
            return Err(NativeError::WorkerFailed);
        }
        if job.cancel.requested() {
            return Ok(RecoveryPoll::Cancelled);
        }
        match terminal.ok_or(NativeError::WorkerFailed)? {
            WorkerTerminal::Recovered(keys) => {
                let mut state = self.lock_state();
                let core = state.core.as_ref().ok_or(NativeError::Closed)?;
                core.import_keys_with_version(&keys.bytes, &keys.public_key_version)
                    .map_err(|error| NativeError::Xdk(error.to_string()))?;
                state.user_id = Some(keys.user_id);
                Ok(RecoveryPoll::Unlocked {
                    public_key_version: keys.public_key_version,
                })
            }
            WorkerTerminal::IncorrectPin { guesses_remaining } => {
                Ok(RecoveryPoll::IncorrectPin { guesses_remaining })
            }
            WorkerTerminal::Failure(reason) => Ok(RecoveryPoll::Failure(reason)),
            WorkerTerminal::Uncertain => Ok(RecoveryPoll::Uncertain),
            WorkerTerminal::Cancelled => Ok(RecoveryPoll::Cancelled),
            WorkerTerminal::WorkerFailed => Err(NativeError::WorkerFailed),
        }
    }

    fn cancel_recovery(&self, id: i64, epoch: i64) -> std::result::Result<bool, NativeError> {
        let state = self.lock_state();
        if state.core.is_none() {
            return Err(NativeError::Closed);
        }
        let job = state.recovery.as_ref().ok_or(NativeError::Stale)?;
        if job.id != id || job.epoch != epoch {
            return Err(NativeError::Stale);
        }
        Ok(job.cancel.request())
    }
}

impl Drop for NativeSession {
    fn drop(&mut self) {
        let mut closed = self.close();
        if let Some(recovery) = closed.recovery.take() {
            recovery.cancel_and_detach();
        }
        drop(closed.user_id.take());
        drop(closed.core.take());
    }
}

impl Transfer for NativeSession {
    fn type_name() -> &'static str {
        "Chirp XChat native session"
    }
}

#[derive(Debug, PartialEq, Eq)]
enum RecoveryPoll {
    Pending,
    Unlocked { public_key_version: String },
    IncorrectPin { guesses_remaining: Option<u16> },
    Failure(RecoveryFailure),
    Uncertain,
    Cancelled,
}

fn validate_pin(pin: String) -> std::result::Result<Zeroizing<Vec<u8>>, NativeError> {
    let pin = Zeroizing::new(pin.into_bytes());
    if pin.len() != 4 || !pin.iter().all(u8::is_ascii_digit) {
        return Err(NativeError::InvalidInput(
            "XChat recovery PIN must contain exactly four ASCII digits".into(),
        ));
    }
    Ok(pin)
}

#[cfg(feature = "juicebox")]
fn parse_recovery_request(
    pin: String,
    input_json: String,
) -> std::result::Result<RecoveryRequest, NativeError> {
    let encoded = Zeroizing::new(input_json);
    let pin = validate_pin(pin)?;
    if encoded.len() > MAX_RECOVERY_INPUT_BYTES {
        return Err(NativeError::InvalidInput(
            "XChat recovery configuration exceeds 1 MiB".into(),
        ));
    }
    let mut input: RecoveryInput = serde_json::from_str(&encoded)
        .map_err(|_| NativeError::InvalidInput("XChat recovery configuration is invalid".into()))?;
    if let Err(error) = validate_recovery_input(&input) {
        input.zeroize_secrets();
        return Err(error);
    }
    let config = JuiceboxConfig::new(
        std::mem::take(&mut input.sdk_config),
        std::mem::take(&mut input.tokens).into_iter().collect(),
        input.max_guess_count,
    );
    Ok(RecoveryRequest {
        pin,
        user_id: std::mem::take(&mut input.user_id),
        config,
        registered_keys: std::mem::take(&mut input.registered_keys),
    })
}

impl RecoveryInput {
    fn zeroize_secrets(&mut self) {
        self.sdk_config.zeroize();
        for token in self.tokens.values_mut() {
            token.zeroize();
        }
    }
}

fn validate_recovery_input(input: &RecoveryInput) -> std::result::Result<(), NativeError> {
    if input.user_id.is_empty()
        || input.user_id.len() > 32
        || !input.user_id.bytes().all(|byte| byte.is_ascii_digit())
    {
        return Err(NativeError::InvalidInput(
            "XChat recovery user identity is invalid".into(),
        ));
    }
    if input.sdk_config.len() > MAX_RECOVERY_INPUT_BYTES / 2 {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox SDK configuration is too large".into(),
        ));
    }
    if !(1..=20).contains(&input.max_guess_count) {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox guess limit is invalid".into(),
        ));
    }
    if input.registered_keys.is_empty() || input.registered_keys.len() > MAX_REGISTERED_KEYS {
        return Err(NativeError::InvalidInput(
            "XChat registered public-key count is invalid".into(),
        ));
    }

    let sdk: SdkConfiguration = serde_json::from_str(&input.sdk_config).map_err(|_| {
        NativeError::InvalidInput("XChat Juicebox SDK configuration is invalid".into())
    })?;
    if sdk.realms.is_empty() || sdk.realms.len() > 16 {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox realm count is invalid".into(),
        ));
    }
    let realm_count = sdk.realms.len() as u32;
    if sdk.recover_threshold == 0
        || sdk.recover_threshold <= realm_count / 2
        || sdk.recover_threshold > sdk.register_threshold
        || sdk.register_threshold > realm_count
    {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox thresholds are invalid".into(),
        ));
    }
    if sdk.pin_hashing_mode != "Standard2019" {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox PIN hashing mode is unsupported".into(),
        ));
    }

    let mut realm_ids = HashSet::with_capacity(sdk.realms.len());
    for realm in sdk.realms {
        if !valid_hex(&realm.id, 32) || !realm_ids.insert(realm.id) {
            return Err(NativeError::InvalidInput(
                "XChat Juicebox realm identity is invalid".into(),
            ));
        }
        let address = Url::parse(&realm.address).map_err(|_| {
            NativeError::InvalidInput("XChat Juicebox realm address is invalid".into())
        })?;
        if !realm.address.is_ascii()
            || realm.address.len() > 2048
            || address.scheme() != "https"
            || address.host_str().is_none()
            || address.port().is_some_and(|port| port != 443)
            || !address.username().is_empty()
            || address.password().is_some()
            || address.query().is_some()
            || address.fragment().is_some()
        {
            return Err(NativeError::InvalidInput(
                "XChat Juicebox realm address is not trusted HTTPS".into(),
            ));
        }
        if realm
            .public_key
            .as_deref()
            .is_some_and(|key| !valid_hex(key, 64))
        {
            return Err(NativeError::InvalidInput(
                "XChat Juicebox realm public key is invalid".into(),
            ));
        }
    }

    let token_ids = input.tokens.keys().cloned().collect::<HashSet<_>>();
    if token_ids != realm_ids
        || input.tokens.values().any(|token| {
            token.is_empty()
                || token.len() > 16 * 1024
                || !token.bytes().all(|byte| byte.is_ascii_graphic())
        })
    {
        return Err(NativeError::InvalidInput(
            "XChat Juicebox realm tokens are invalid".into(),
        ));
    }

    let mut versions = HashSet::with_capacity(input.registered_keys.len());
    for key in &input.registered_keys {
        if key.version.is_empty()
            || key.version.len() > 128
            || !key.version.bytes().all(|byte| byte.is_ascii_digit())
            || (key.version.len() > 1 && key.version.starts_with('0'))
            || !versions.insert(key.version.as_str())
        {
            return Err(NativeError::InvalidInput(
                "XChat registered public-key version is invalid".into(),
            ));
        }
        let identity = BASE64.decode(&key.identity_public_key).map_err(|_| {
            NativeError::InvalidInput("XChat registered identity key is invalid".into())
        })?;
        if !matches!(identity.len(), 33 | 65 | 91) {
            return Err(NativeError::InvalidInput(
                "XChat registered identity key has an invalid length".into(),
            ));
        }
    }
    Ok(())
}

fn valid_hex(value: &str, length: usize) -> bool {
    value.len() == length && value.bytes().all(|byte| byte.is_ascii_hexdigit())
}

fn matching_public_key_version(
    recovered: &[u8],
    registered_keys: &[RegisteredKey],
) -> std::result::Result<String, RecoveryFailure> {
    let core = ChatCore::new();
    core.import_keys(recovered)
        .map_err(|_| RecoveryFailure::KeyReconstructionFailed)?;
    let mut matches = registered_keys.iter().filter(|key| {
        core.matches_registered_key(&key.identity_public_key)
            .unwrap_or(false)
    });
    let version = matches.next().map(|key| key.version.clone());
    let ambiguous = version.is_some() && matches.next().is_some();
    core.lock();
    if ambiguous {
        Err(RecoveryFailure::AmbiguousRegisteredKey)
    } else {
        version.ok_or(RecoveryFailure::RegisteredKeyMismatch)
    }
}

fn recovery_terminal(
    result: RecoverResult,
    recovered: impl FnOnce(Zeroizing<Vec<u8>>) -> WorkerTerminal,
) -> WorkerTerminal {
    match result {
        RecoverResult::Success(bytes) => recovered(bytes),
        RecoverResult::Failure {
            reason: RecoverFailureReason::InvalidPin,
            guesses_remaining,
        } => WorkerTerminal::IncorrectPin { guesses_remaining },
        RecoverResult::Failure {
            reason: RecoverFailureReason::NotRegistered,
            ..
        } => WorkerTerminal::Failure(RecoveryFailure::NotRegistered),
        RecoverResult::Failure {
            reason: RecoverFailureReason::InvalidAuth,
            ..
        } => WorkerTerminal::Failure(RecoveryFailure::InvalidAuth),
        RecoverResult::Failure {
            reason: RecoverFailureReason::UpgradeRequired,
            ..
        } => WorkerTerminal::Failure(RecoveryFailure::UpgradeRequired),
        RecoverResult::Failure {
            reason: RecoverFailureReason::RateLimitExceeded,
            ..
        } => WorkerTerminal::Failure(RecoveryFailure::RateLimited),
        RecoverResult::Failure {
            reason: RecoverFailureReason::Transient,
            ..
        } => WorkerTerminal::Uncertain,
        RecoverResult::Failure {
            reason: RecoverFailureReason::Assertion,
            ..
        } => WorkerTerminal::Failure(RecoveryFailure::AssertionFailed),
        RecoverResult::KeyReconstructionFailed => {
            WorkerTerminal::Failure(RecoveryFailure::KeyReconstructionFailed)
        }
        RecoverResult::NoTokens => WorkerTerminal::Failure(RecoveryFailure::NoTokens),
    }
}

#[cfg(feature = "juicebox")]
enum NetworkOutcome {
    Recovered(RecoverResult),
    Cancelled,
    TimedOut,
}

#[cfg(feature = "juicebox")]
async fn wait_for_cancellation(cancel: &CancelToken) {
    while !cancel.requested() {
        tokio::time::sleep(Duration::from_millis(5)).await;
    }
}

#[cfg(feature = "juicebox")]
fn recover_with_api(
    request: RecoveryRequest,
    cancel: Arc<CancelToken>,
    client: Arc<dyn JuiceboxApi>,
    timeout: Duration,
) -> WorkerTerminal {
    if cancel.requested() {
        return WorkerTerminal::Cancelled;
    }
    let runtime = match tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
    {
        Ok(runtime) => runtime,
        Err(_) => return WorkerTerminal::WorkerFailed,
    };
    let outcome = runtime.block_on(async {
        tokio::select! {
            biased;
            _ = wait_for_cancellation(&cancel) => NetworkOutcome::Cancelled,
            _ = tokio::time::sleep(timeout) => NetworkOutcome::TimedOut,
            result = client.recover_private_key(&request.pin, &request.config) => {
                NetworkOutcome::Recovered(result)
            }
        }
    });
    match outcome {
        NetworkOutcome::Recovered(result) => recovery_terminal(result, |bytes| {
            match matching_public_key_version(&bytes, &request.registered_keys) {
                Ok(public_key_version) => WorkerTerminal::Recovered(RecoveredKeys {
                    bytes,
                    public_key_version,
                    user_id: request.user_id.clone(),
                }),
                Err(reason) => WorkerTerminal::Failure(reason),
            }
        }),
        NetworkOutcome::Cancelled => WorkerTerminal::Cancelled,
        NetworkOutcome::TimedOut => WorkerTerminal::Uncertain,
    }
}

#[cfg(feature = "juicebox")]
fn network_recover(request: RecoveryRequest, cancel: Arc<CancelToken>) -> WorkerTerminal {
    recover_with_api(
        request,
        cancel,
        Arc::new(JuiceboxClient::new()),
        RECOVERY_TIMEOUT,
    )
}

#[cfg(feature = "test-vector")]
fn mock_recover(pin: Zeroizing<Vec<u8>>, delay: Duration, cancel: &CancelToken) -> WorkerTerminal {
    if cancel.wait(delay) {
        return WorkerTerminal::Cancelled;
    }
    let correct = pin.as_slice() == b"2580";
    let uncertain = pin.as_slice() == b"0000";
    let panic_requested = pin.as_slice() == b"9999";
    drop(pin);
    if panic_requested {
        panic!("synthetic recovery worker failure");
    }
    let (result, public_key_version, user_id) = if correct {
        match official_recovered_keys() {
            Ok(keys) => (
                RecoverResult::Success(keys.bytes),
                keys.public_key_version,
                keys.user_id,
            ),
            Err(_) => return WorkerTerminal::WorkerFailed,
        }
    } else if uncertain {
        (
            RecoverResult::Failure {
                reason: RecoverFailureReason::Transient,
                guesses_remaining: None,
            },
            String::new(),
            String::new(),
        )
    } else {
        (
            RecoverResult::Failure {
                reason: RecoverFailureReason::InvalidPin,
                guesses_remaining: Some(19),
            },
            String::new(),
            String::new(),
        )
    };
    let terminal = recovery_terminal(result, |bytes| {
        WorkerTerminal::Recovered(RecoveredKeys {
            bytes,
            public_key_version,
            user_id,
        })
    });
    if cancel.requested() {
        drop(terminal);
        WorkerTerminal::Cancelled
    } else {
        terminal
    }
}

fn parse_decrypt_input(
    input_json: String,
) -> std::result::Result<(Vec<String>, Vec<chat_xdk_core::SigningKeyEntry>), NativeError> {
    if input_json.len() > MAX_DECRYPT_INPUT_BYTES {
        return Err(NativeError::InvalidInput(
            "XChat native decrypt input exceeds 20 MiB".into(),
        ));
    }
    let input: DecryptInput = serde_json::from_str(&input_json)
        .map_err(|_| NativeError::InvalidInput("XChat native decrypt input is invalid".into()))?;
    if input.events.is_empty() || input.events.len() > 200 {
        return Err(NativeError::InvalidInput(
            "XChat native decrypt event count is invalid".into(),
        ));
    }
    if input.signing_keys.is_empty() || input.signing_keys.len() > MAX_SIGNING_KEYS {
        return Err(NativeError::InvalidInput(
            "XChat native signing-key count is invalid".into(),
        ));
    }
    let mut identities = HashSet::with_capacity(input.signing_keys.len());
    let mut total_key_bytes = 0usize;
    let mut signing_keys = Vec::with_capacity(input.signing_keys.len());
    for key in input.signing_keys {
        let identity = (key.user_id.clone(), key.public_key_version.clone());
        total_key_bytes = total_key_bytes
            .checked_add(key.public_key.len())
            .and_then(|total| total.checked_add(key.identity_public_key.len()))
            .and_then(|total| total.checked_add(key.identity_public_key_signature.len()))
            .ok_or_else(|| {
                NativeError::InvalidInput("XChat native signing-key size overflowed".into())
            })?;
        if key.user_id.is_empty()
            || key.user_id.len() > 32
            || !key.user_id.bytes().all(|byte| byte.is_ascii_digit())
            || key.public_key_version.is_empty()
            || key.public_key_version.len() > 128
            || !key
                .public_key_version
                .bytes()
                .all(|byte| byte.is_ascii_digit())
            || (key.public_key_version.len() > 1 && key.public_key_version.starts_with('0'))
            || !identities.insert(identity)
            || !valid_base64_length(&key.public_key, &[33, 65, 91])
            || !valid_base64_length(&key.identity_public_key, &[33, 65, 91])
            || !valid_base64_length(&key.identity_public_key_signature, &[64])
        {
            return Err(NativeError::InvalidInput(
                "XChat native signing key is invalid".into(),
            ));
        }
        signing_keys.push(chat_xdk_core::SigningKeyEntry {
            user_id: key.user_id,
            public_key_version: key.public_key_version,
            public_key: key.public_key,
            identity_public_key: key.identity_public_key,
            identity_public_key_signature: key.identity_public_key_signature,
        });
    }
    if total_key_bytes > 4 * 1024 * 1024 {
        return Err(NativeError::InvalidInput(
            "XChat native signing keys exceed 4 MiB".into(),
        ));
    }
    Ok((input.events, signing_keys))
}

fn parse_encrypt_text_input(
    input_json: &str,
) -> std::result::Result<EncryptTextInput, NativeError> {
    if input_json.len() > MAX_ENCRYPT_INPUT_BYTES {
        return Err(NativeError::InvalidInput(
            "XChat native message input exceeds 32 KiB".into(),
        ));
    }
    let input: EncryptTextInput = serde_json::from_str(input_json)
        .map_err(|_| NativeError::InvalidInput("XChat native message input is invalid".into()))?;
    if input.conversation_id.is_empty()
        || input.conversation_id.len() > 256
        || input.conversation_id.contains(',')
        || !input
            .conversation_id
            .bytes()
            .all(|byte| byte.is_ascii_graphic())
    {
        return Err(NativeError::InvalidInput(
            "XChat native conversation ID is invalid".into(),
        ));
    }
    if input.text.trim().is_empty() || input.text.len() > MAX_MESSAGE_TEXT_BYTES {
        return Err(NativeError::InvalidInput(
            "XChat message must contain between 1 and 16384 UTF-8 bytes".into(),
        ));
    }
    Ok(input)
}

fn valid_base64_length(value: &str, lengths: &[usize]) -> bool {
    value.len() <= 2048
        && BASE64
            .decode(value)
            .is_ok_and(|decoded| lengths.contains(&decoded.len()))
}

#[derive(Serialize)]
struct VerifiedMessage {
    sequence_id: Option<String>,
    id: Option<String>,
    sender_id: Option<String>,
    conversation_id: Option<String>,
    created_at_msec: Option<i64>,
    content_kind: String,
    text: Option<String>,
    attachments: Vec<VerifiedAttachment>,
    reply: bool,
    reply_text: Option<String>,
    reply_attachment_count: usize,
    key_version: Option<String>,
    verified: bool,
}

#[derive(Serialize)]
struct VerifiedAttachment {
    kind: String,
    url: Option<String>,
    preview_url: Option<String>,
    name: Option<String>,
}

fn media_kind(value: Option<&str>) -> &str {
    match value {
        Some(kind @ ("image" | "gif" | "video" | "audio" | "file" | "svg")) => kind,
        _ => "media",
    }
}

fn verified_attachment(attachment: &AttachmentInfo) -> VerifiedAttachment {
    match attachment {
        AttachmentInfo::Media(media) => {
            let kind = media_kind(media.media_type.as_deref());
            VerifiedAttachment {
                kind: kind.into(),
                url: media.legacy_media_url_https.clone(),
                preview_url: media.legacy_media_preview_url.clone(),
                name: media.filename.clone(),
            }
        }
        AttachmentInfo::Url(url) => VerifiedAttachment {
            kind: "url".into(),
            url: url.url.clone(),
            preview_url: None,
            name: url.display_title.clone(),
        },
        AttachmentInfo::Post(post) => VerifiedAttachment {
            kind: "post".into(),
            url: post.post_url.clone(),
            preview_url: None,
            name: None,
        },
        AttachmentInfo::UnifiedCard(card) => VerifiedAttachment {
            kind: "unified-card".into(),
            url: card.url.clone(),
            preview_url: None,
            name: None,
        },
        AttachmentInfo::Money(money) => VerifiedAttachment {
            kind: "money".into(),
            url: None,
            preview_url: None,
            name: money.fallback_text.clone(),
        },
    }
}

fn verified_message(message: chat_xdk_core::Message) -> Option<VerifiedMessage> {
    if !message.verified {
        return None;
    }
    let (content_kind, text) = match &message.content {
        MessageContent::Text { text, .. } => ("text", Some(text.clone())),
        MessageContent::Reaction { emoji, .. } => ("reaction", Some(emoji.clone())),
        MessageContent::ReactionRemoved { emoji, .. } => ("reaction-removed", Some(emoji.clone())),
        MessageContent::Edit { new_text, .. } => ("edit", Some(new_text.clone())),
        MessageContent::MarkRead => ("mark-read", None),
        MessageContent::MarkUnread => ("mark-unread", None),
        MessageContent::Unknown { .. } => ("unknown", None),
    };
    let reply = matches!(
        message.reply_preview_validation,
        Some(ReplyPreviewValidation::Valid)
    );
    let preview = if reply {
        match &message.content {
            MessageContent::Text {
                replying_to_preview,
                ..
            } => replying_to_preview.as_ref(),
            _ => None,
        }
    } else {
        None
    };
    Some(VerifiedMessage {
        sequence_id: message.meta.sequence_id,
        id: message.meta.id,
        sender_id: message.meta.sender_id,
        conversation_id: message.meta.conversation_id,
        created_at_msec: message.meta.created_at_msec,
        content_kind: content_kind.into(),
        text,
        attachments: message
            .attachments
            .iter()
            .map(verified_attachment)
            .collect(),
        reply,
        reply_text: preview.and_then(|value| value.message_text.clone()),
        reply_attachment_count: preview
            .and_then(|value| value.attachments.as_ref())
            .map_or(0, Vec::len),
        key_version: message.key_version,
        verified: true,
    })
}

#[derive(Serialize)]
struct DecryptOutput {
    messages: Vec<VerifiedMessage>,
    errors: BTreeMap<usize, String>,
}

fn decrypt_events(
    core: &ChatCore,
    events: &[String],
    signing_keys: &[chat_xdk_core::SigningKeyEntry],
) -> std::result::Result<DecryptOutput, NativeError> {
    if events.len() > 200 {
        return Err(NativeError::InvalidInput(
            "XChat native decrypt accepts at most 200 events".into(),
        ));
    }
    let event_bytes = events.iter().try_fold(0usize, |total, event| {
        total.checked_add(event.len()).ok_or_else(|| {
            NativeError::InvalidInput("XChat native decrypt input size overflowed".into())
        })
    })?;
    if event_bytes > 16 * 1024 * 1024 {
        return Err(NativeError::InvalidInput(
            "XChat native decrypt input exceeds 16 MiB".into(),
        ));
    }
    for event in events {
        if event.len() > 2 * 1024 * 1024
            || BASE64
                .decode(event)
                .map_or(true, |decoded| decoded.len() > 1024 * 1024)
        {
            return Err(NativeError::InvalidInput(
                "XChat native decrypt event is invalid".into(),
            ));
        }
    }

    let event_refs = events.iter().map(String::as_str).collect::<Vec<_>>();
    let result = core.decrypt_events(&event_refs, signing_keys);
    let messages = result
        .messages
        .into_iter()
        .filter_map(|decrypted| match decrypted.event {
            Event::Message(message) => verified_message(*message),
            _ => None,
        })
        .collect();
    let errors = result.errors.into_iter().collect();
    Ok(DecryptOutput { messages, errors })
}

#[emacs::module(
    name = "chirp-xchat-native-module",
    defun_prefix = "chirp-xchat-native"
)]
fn init(env: &Env) -> Result<()> {
    env.define_error(
        "chirp-xchat-native-error",
        "Chirp XChat native module error",
        (env.intern("error")?,),
    )?;
    Ok(())
}

/// Return the native module and pinned chat-xdk versions.
#[defun]
fn version() -> Result<&'static str> {
    Ok(MODULE_VERSION)
}

/// Create and return an opaque XChat crypto session.
#[defun(user_ptr)]
fn session_create() -> Result<NativeSession> {
    Ok(NativeSession::new())
}

/// Return whether SESSION still owns native crypto state.
#[defun]
fn session_live_p(session: &NativeSession) -> Result<bool> {
    Ok(session.live())
}

/// Return whether SESSION has imported its native identity keys.
#[defun]
fn session_unlocked_p(env: &Env, session: &NativeSession) -> Result<bool> {
    session
        .unlocked()
        .or_signal(env, "chirp-xchat-native-error")
}

/// Return verified plaintext events decrypted inside SESSION.
#[defun]
fn decrypt(env: &Env, session: &NativeSession, input_json: String) -> Result<String> {
    session
        .decrypt(input_json)
        .or_signal(env, "chirp-xchat-native-error")
}

/// Prepare one encrypted and signed text message inside SESSION.
#[defun]
fn encrypt_text(env: &Env, session: &NativeSession, input_json: String) -> Result<String> {
    session
        .encrypt_text(input_json)
        .or_signal(env, "chirp-xchat-native-error")
}

/// Destroy SESSION's native crypto state and return whether it was live.
#[defun]
fn session_destroy(session: &NativeSession) -> Result<bool> {
    Ok(session.destroy())
}

/// Start one official Juicebox recovery attempt in SESSION.
#[cfg(feature = "juicebox")]
#[defun]
fn recovery_start(
    env: &Env,
    session: &NativeSession,
    epoch: i64,
    pin: String,
    input_json: String,
) -> Result<i64> {
    session
        .start_recovery(epoch, pin, input_json)
        .or_signal(env, "chirp-xchat-native-error")
}

/// Poll recovery JOB-ID in SESSION for EPOCH without blocking.
#[defun]
fn recovery_poll<'e>(
    env: &'e Env,
    session: &NativeSession,
    job_id: i64,
    epoch: i64,
) -> Result<Value<'e>> {
    let poll = session
        .poll_recovery(job_id, epoch)
        .or_signal(env, "chirp-xchat-native-error")?;
    recovery_poll_value(env, poll, job_id)
}

/// Request cancellation of recovery JOB-ID in SESSION for EPOCH.
#[defun]
fn recovery_cancel(env: &Env, session: &NativeSession, job_id: i64, epoch: i64) -> Result<bool> {
    session
        .cancel_recovery(job_id, epoch)
        .or_signal(env, "chirp-xchat-native-error")
}

/// Verify the bundled official synthetic vector inside SESSION.
#[cfg(feature = "test-vector")]
#[defun]
fn test_decrypt_official_vector(env: &Env, session: &NativeSession) -> Result<String> {
    run_official_vector(session).or_signal(env, "chirp-xchat-native-error")
}

/// Start one delayed, network-free synthetic recovery in SESSION.
#[cfg(feature = "test-vector")]
#[defun]
fn test_recovery_start(
    env: &Env,
    session: &NativeSession,
    epoch: i64,
    pin: String,
    delay_msec: i64,
) -> Result<i64> {
    session
        .start_mock_recovery(epoch, pin, delay_msec)
        .or_signal(env, "chirp-xchat-native-error")
}

/// Poll synthetic recovery JOB-ID in SESSION for EPOCH.
#[cfg(feature = "test-vector")]
#[defun]
fn test_recovery_poll<'e>(
    env: &'e Env,
    session: &NativeSession,
    job_id: i64,
    epoch: i64,
) -> Result<Value<'e>> {
    let poll = session
        .poll_recovery(job_id, epoch)
        .or_signal(env, "chirp-xchat-native-error")?;
    recovery_poll_value(env, poll, job_id)
}

/// Cancel synthetic recovery JOB-ID in SESSION for EPOCH.
#[cfg(feature = "test-vector")]
#[defun]
fn test_recovery_cancel(
    env: &Env,
    session: &NativeSession,
    job_id: i64,
    epoch: i64,
) -> Result<bool> {
    session
        .cancel_recovery(job_id, epoch)
        .or_signal(env, "chirp-xchat-native-error")
}

fn recovery_poll_value<'e>(env: &'e Env, poll: RecoveryPoll, job_id: i64) -> Result<Value<'e>> {
    match poll {
        RecoveryPoll::Pending => recovery_status(env, "pending", job_id),
        RecoveryPoll::Unlocked { public_key_version } => env.list((
            env.intern(":status")?,
            env.intern("unlocked")?,
            env.intern(":job-id")?,
            job_id,
            env.intern(":public-key-version")?,
            public_key_version,
        )),
        RecoveryPoll::IncorrectPin {
            guesses_remaining: Some(guesses),
        } => env.list((
            env.intern(":status")?,
            env.intern("incorrect-pin")?,
            env.intern(":job-id")?,
            job_id,
            env.intern(":guesses-remaining")?,
            i64::from(guesses),
        )),
        RecoveryPoll::IncorrectPin {
            guesses_remaining: None,
        } => recovery_status(env, "incorrect-pin", job_id),
        RecoveryPoll::Failure(reason) => recovery_status(
            env,
            match reason {
                RecoveryFailure::NotRegistered => "not-registered",
                RecoveryFailure::InvalidAuth => "invalid-auth",
                RecoveryFailure::UpgradeRequired => "upgrade-required",
                RecoveryFailure::RateLimited => "rate-limited",
                RecoveryFailure::AssertionFailed => "assertion-failed",
                RecoveryFailure::KeyReconstructionFailed => "key-reconstruction-failed",
                RecoveryFailure::NoTokens => "no-tokens",
                RecoveryFailure::RegisteredKeyMismatch => "registered-key-mismatch",
                RecoveryFailure::AmbiguousRegisteredKey => "ambiguous-registered-key",
            },
            job_id,
        ),
        RecoveryPoll::Uncertain => recovery_status(env, "uncertain", job_id),
        RecoveryPoll::Cancelled => recovery_status(env, "cancelled", job_id),
    }
}

fn recovery_status<'e>(env: &'e Env, status: &str, job_id: i64) -> Result<Value<'e>> {
    env.list((
        env.intern(":status")?,
        env.intern(status)?,
        env.intern(":job-id")?,
        job_id,
    ))
}

#[cfg(feature = "test-vector")]
#[derive(serde::Deserialize)]
struct OfficialVector {
    private_keys_concat_b64: String,
    event_key_change_b64: String,
    event_message_b64: String,
    event_conversation_id: String,
    event_sender_id: String,
    event_recipient_key_version: String,
    event_signing_key_version: String,
    event_message_text: String,
    identity_public_b64: String,
    signing_public_b64: String,
    identity_public_key_signature_b64: String,
}

#[cfg(feature = "test-vector")]
fn parse_official_vector() -> std::result::Result<OfficialVector, NativeError> {
    serde_json::from_str(include_str!("../tests/fixtures/sdk_vectors.json"))
        .map_err(|_| NativeError::InvalidInput("official XChat vector is invalid".into()))
}

#[cfg(feature = "test-vector")]
fn take_private_keys(
    vector: &mut OfficialVector,
) -> std::result::Result<Zeroizing<Vec<u8>>, NativeError> {
    let encoded = Zeroizing::new(std::mem::take(&mut vector.private_keys_concat_b64));
    BASE64
        .decode(encoded.as_bytes())
        .map(Zeroizing::new)
        .map_err(|_| {
            NativeError::InvalidInput("official XChat private-key vector is invalid".into())
        })
}

#[cfg(feature = "test-vector")]
fn official_recipient_user_id(vector: &OfficialVector) -> std::result::Result<String, NativeError> {
    vector
        .event_conversation_id
        .split([':', '-'])
        .find(|participant| {
            *participant != vector.event_sender_id
                && !participant.is_empty()
                && participant.bytes().all(|byte| byte.is_ascii_digit())
        })
        .map(str::to_string)
        .ok_or_else(|| NativeError::InvalidInput("official XChat recipient is invalid".into()))
}

#[cfg(feature = "test-vector")]
fn official_recovered_keys() -> std::result::Result<RecoveredKeys, NativeError> {
    let mut vector = parse_official_vector()?;
    let user_id = official_recipient_user_id(&vector)?;
    let bytes = take_private_keys(&mut vector)?;
    Ok(RecoveredKeys {
        bytes,
        public_key_version: vector.event_recipient_key_version,
        user_id,
    })
}

#[cfg(feature = "test-vector")]
fn run_official_vector(session: &NativeSession) -> std::result::Result<String, NativeError> {
    let mut vector = parse_official_vector()?;
    let user_id = official_recipient_user_id(&vector)?;
    let private_keys = take_private_keys(&mut vector)?;
    let output = session.with_core(|core| {
        core.import_keys_with_version(&private_keys, &vector.event_recipient_key_version)
            .map_err(|error| NativeError::Xdk(error.to_string()))?;
        let signing_keys = [chat_xdk_core::SigningKeyEntry {
            user_id: vector.event_sender_id,
            public_key_version: vector.event_signing_key_version,
            public_key: vector.signing_public_b64,
            identity_public_key: vector.identity_public_b64,
            identity_public_key_signature: vector.identity_public_key_signature_b64,
        }];
        let events = vec![vector.event_key_change_b64, vector.event_message_b64];
        let output = decrypt_events(core, &events, &signing_keys)?;
        if output.messages.len() != 1
            || output.messages[0].text.as_deref() != Some(&vector.event_message_text)
        {
            return Err(NativeError::Xdk(
                "official vector did not produce its verified message".into(),
            ));
        }
        serde_json::to_string(&output).map_err(|error| NativeError::Xdk(error.to_string()))
    })?;
    session.lock_state().user_id = Some(user_id);
    Ok(output)
}

#[cfg(all(test, feature = "test-vector"))]
mod tests {
    use super::*;
    #[cfg(feature = "juicebox")]
    use chat_xdk_core::keys::juicebox::{DeleteResult, RegisterResult};
    #[cfg(feature = "juicebox")]
    use std::sync::atomic::AtomicUsize;

    #[cfg(feature = "juicebox")]
    enum MockRecovery {
        Return(Mutex<Option<RecoverResult>>),
        Pending(Arc<AtomicBool>),
    }

    #[cfg(feature = "juicebox")]
    struct MockJuicebox {
        calls: Arc<AtomicUsize>,
        recovery: MockRecovery,
    }

    #[cfg(feature = "juicebox")]
    struct DropFlag(Arc<AtomicBool>);

    #[cfg(feature = "juicebox")]
    impl Drop for DropFlag {
        fn drop(&mut self) {
            self.0.store(true, Ordering::Release);
        }
    }

    #[cfg(feature = "juicebox")]
    #[async_trait::async_trait]
    impl JuiceboxApi for MockJuicebox {
        async fn register_private_key(
            &self,
            _pin: &[u8],
            _config: &JuiceboxConfig,
            _secret: &[u8],
        ) -> RegisterResult {
            unreachable!("recovery tests never register keys")
        }

        async fn recover_private_key(
            &self,
            _pin: &[u8],
            _config: &JuiceboxConfig,
        ) -> RecoverResult {
            self.calls.fetch_add(1, Ordering::AcqRel);
            match &self.recovery {
                MockRecovery::Return(result) => result
                    .lock()
                    .unwrap_or_else(|error| error.into_inner())
                    .take()
                    .expect("mock recovery is called exactly once"),
                MockRecovery::Pending(dropped) => {
                    let _drop = DropFlag(Arc::clone(dropped));
                    std::future::pending::<RecoverResult>().await
                }
            }
        }

        async fn delete_keys(&self, _config: &JuiceboxConfig) -> DeleteResult {
            unreachable!("recovery tests never delete keys")
        }
    }

    fn wait_for_terminal(
        session: &NativeSession,
        job_id: i64,
        epoch: i64,
    ) -> std::result::Result<RecoveryPoll, NativeError> {
        for _ in 0..200 {
            let poll = session.poll_recovery(job_id, epoch)?;
            if poll != RecoveryPoll::Pending {
                return Ok(poll);
            }
            thread::sleep(Duration::from_millis(5));
        }
        panic!("synthetic recovery did not settle");
    }

    fn wait_for_worker_exit(session: &NativeSession) {
        for _ in 0..200 {
            let finished = session
                .lock_state()
                .recovery
                .as_ref()
                .and_then(|job| job.thread.as_ref())
                .is_some_and(JoinHandle::is_finished);
            if finished {
                return;
            }
            thread::sleep(Duration::from_millis(5));
        }
        panic!("synthetic recovery worker did not exit");
    }

    fn valid_recovery_input() -> RecoveryInput {
        let vector = parse_official_vector().expect("official vector parses");
        let realm_id = "01".repeat(16);
        RecoveryInput {
            user_id: "2222".into(),
            sdk_config: serde_json::json!({
                "realms": [{
                    "id": realm_id.clone(),
                    "address": "https://realm.example/",
                    "public_key": "11".repeat(32)
                }],
                "register_threshold": 1,
                "recover_threshold": 1,
                "pin_hashing_mode": "Standard2019"
            })
            .to_string(),
            tokens: [(realm_id, "opaque-token".to_string())]
                .into_iter()
                .collect(),
            max_guess_count: 20,
            registered_keys: vec![RegisteredKey {
                version: vector.event_recipient_key_version,
                identity_public_key: vector.identity_public_b64,
            }],
        }
    }

    #[cfg(feature = "juicebox")]
    fn valid_recovery_json() -> String {
        let input = valid_recovery_input();
        let registered_keys = input
            .registered_keys
            .iter()
            .map(|key| {
                serde_json::json!({
                    "version": key.version,
                    "identity_public_key": key.identity_public_key
                })
            })
            .collect::<Vec<_>>();
        serde_json::json!({
            "user_id": input.user_id,
            "sdk_config": input.sdk_config,
            "tokens": input.tokens,
            "max_guess_count": input.max_guess_count,
            "registered_keys": registered_keys
        })
        .to_string()
    }

    #[cfg(feature = "juicebox")]
    fn valid_recovery_request() -> RecoveryRequest {
        let input = valid_recovery_input();
        RecoveryRequest {
            pin: validate_pin("2580".into()).expect("PIN is valid"),
            user_id: input.user_id,
            config: JuiceboxConfig::new(
                input.sdk_config,
                input.tokens.into_iter().collect(),
                input.max_guess_count,
            ),
            registered_keys: input.registered_keys,
        }
    }

    #[test]
    fn official_vector_returns_only_verified_plaintext() {
        let session = NativeSession::new();
        let output = run_official_vector(&session).expect("official vector decrypts");
        let parsed: serde_json::Value = serde_json::from_str(&output).expect("valid JSON");
        assert_eq!(parsed["messages"].as_array().map(Vec::len), Some(1));
        assert_eq!(parsed["messages"][0]["verified"], true);
        assert_eq!(
            parsed["errors"].as_object().map(serde_json::Map::len),
            Some(0)
        );
        assert!(!output.contains("private_key"));
        assert!(!output.contains("conversation_key"));
        assert!(!output.contains("original_b64"));

        let message = &parsed["messages"][0];
        let conversation_id = message["conversation_id"]
            .as_str()
            .expect("fixture has a conversation ID");
        let plaintext = "outbound fixture message";
        let prepared = session
            .encrypt_text(
                serde_json::json!({
                    "conversation_id": conversation_id,
                    "text": plaintext
                })
                .to_string(),
            )
            .expect("verified cached key prepares a message");
        let payload: serde_json::Value =
            serde_json::from_str(&prepared).expect("prepared payload is JSON");
        assert!(payload["message_id"].as_str().is_some());
        assert!(payload["encoded_message_create_event"].as_str().is_some());
        assert!(payload["encoded_message_event_signature"]
            .as_str()
            .is_some());
        assert!(!prepared.contains(plaintext));
        assert!(!prepared.contains("conversation_key"));
    }

    #[test]
    fn verified_message_preserves_media_and_valid_reply_facts() {
        let preview = chat_xdk_core::ReplyingToPreview {
            sender_id: None,
            message_text: Some("quoted".into()),
            entities: None,
            attachments: None,
            sender_display_name: None,
            replying_to_message_sequence_id: None,
            replying_to_message_id: None,
        };
        let content = MessageContent::Text {
            text: String::new(),
            entities: None,
            attachments: None,
            replying_to_preview: Some(preview),
            forwarded_message: None,
            sent_from: None,
            quick_reply: None,
            ctas: None,
        };
        let attachment = AttachmentInfo::Media(chat_xdk_core::MediaAttachmentInfo {
            media_hash_key: Some("media-hash".into()),
            dimensions: None,
            media_type: Some("image".into()),
            duration_millis: None,
            filesize_bytes: None,
            filename: Some("photo.jpg".into()),
            attachment_id: None,
            legacy_media_url_https: Some("https://pbs.twimg.com/photo.jpg".into()),
            legacy_media_preview_url: None,
        });
        let message = chat_xdk_core::Message {
            meta: chat_xdk_core::EventMeta::default(),
            content,
            key_version: Some("1".into()),
            verified: true,
            should_notify: None,
            ttl_msec: None,
            attachments: vec![attachment],
            media_hashes: Vec::new(),
            reply_preview_validation: Some(ReplyPreviewValidation::Valid),
        };
        let mut invalid_reply = message.clone();
        invalid_reply.reply_preview_validation = Some(ReplyPreviewValidation::Invalid);
        let output = verified_message(message).expect("verified message is exported");
        assert_eq!(output.content_kind, "text");
        assert_eq!(output.text.as_deref(), Some(""));
        assert!(output.reply);
        assert_eq!(output.reply_text.as_deref(), Some("quoted"));
        assert_eq!(output.attachments.len(), 1);
        assert_eq!(output.attachments[0].kind, "image");
        assert_eq!(
            output.attachments[0].url.as_deref(),
            Some("https://pbs.twimg.com/photo.jpg")
        );
        let invalid = verified_message(invalid_reply).expect("message remains verified");
        assert!(!invalid.reply);
        assert_eq!(invalid.reply_text, None);
        assert_eq!(invalid.reply_attachment_count, 0);
    }

    #[test]
    fn destroy_is_idempotent_and_closes_the_session() {
        let session = NativeSession::new();
        assert!(session.destroy());
        assert!(!session.destroy());
        assert!(matches!(
            run_official_vector(&session),
            Err(NativeError::Closed)
        ));
    }

    #[test]
    fn decrypt_bounds_are_enforced_before_sdk_work() {
        let core = ChatCore::new();
        let too_many = vec![String::new(); 201];
        assert!(matches!(
            decrypt_events(&core, &too_many, &[]),
            Err(NativeError::InvalidInput(_))
        ));
        let too_large = vec!["x".repeat(16 * 1024 * 1024 + 1)];
        assert!(matches!(
            decrypt_events(&core, &too_large, &[]),
            Err(NativeError::InvalidInput(_))
        ));
    }

    #[test]
    fn encrypt_text_input_is_strictly_bounded() {
        let valid = serde_json::json!({
            "conversation_id": "1-2",
            "text": "hello"
        })
        .to_string();
        assert!(parse_encrypt_text_input(&valid).is_ok());

        let sender_override = serde_json::json!({
            "conversation_id": "1-2",
            "sender_id": "9999",
            "text": "hello"
        })
        .to_string();
        assert!(matches!(
            parse_encrypt_text_input(&sender_override),
            Err(NativeError::InvalidInput(_))
        ));

        let empty = valid.replace("hello", "   ");
        assert!(matches!(
            parse_encrypt_text_input(&empty),
            Err(NativeError::InvalidInput(_))
        ));
        let oversized = serde_json::json!({
            "conversation_id": "1-2",
            "text": "x".repeat(MAX_MESSAGE_TEXT_BYTES + 1)
        })
        .to_string();
        assert!(matches!(
            parse_encrypt_text_input(&oversized),
            Err(NativeError::InvalidInput(_))
        ));
    }

    #[cfg(feature = "juicebox")]
    #[test]
    fn production_recovery_json_is_strictly_parsed_before_work() {
        let request = parse_recovery_request("2580".into(), valid_recovery_json())
            .expect("valid recovery input parses");
        assert_eq!(request.user_id, "2222");
        assert_eq!(request.config.max_guess_count, 20);
        assert_eq!(request.registered_keys.len(), 1);

        let invalid =
            valid_recovery_json().replace("\"max_guess_count\":20", "\"max_guess_count\":0");
        assert!(matches!(
            parse_recovery_request("2580".into(), invalid),
            Err(NativeError::InvalidInput(_))
        ));
    }

    #[test]
    fn recovery_input_requires_bounded_https_realms_and_exact_tokens() {
        let mut input = valid_recovery_input();
        assert!(validate_recovery_input(&input).is_ok());

        input.user_id = "other-user".into();
        assert!(matches!(
            validate_recovery_input(&input),
            Err(NativeError::InvalidInput(_))
        ));

        input = valid_recovery_input();
        input.sdk_config = input.sdk_config.replace("https://", "http://");
        assert!(matches!(
            validate_recovery_input(&input),
            Err(NativeError::InvalidInput(_))
        ));

        input = valid_recovery_input();
        input.sdk_config = input
            .sdk_config
            .replace("https://realm.example/", "https://realm.example:443/");
        assert!(validate_recovery_input(&input).is_ok());

        input = valid_recovery_input();
        input.sdk_config = input
            .sdk_config
            .replace("https://realm.example/", "https://realm.example:8443/");
        assert!(matches!(
            validate_recovery_input(&input),
            Err(NativeError::InvalidInput(_))
        ));

        input = valid_recovery_input();
        input.tokens.clear();
        assert!(matches!(
            validate_recovery_input(&input),
            Err(NativeError::InvalidInput(_))
        ));
    }

    #[test]
    fn recovered_identity_selects_exact_registered_version() {
        let mut vector = parse_official_vector().expect("official vector parses");
        let private_keys = take_private_keys(&mut vector).expect("private keys decode");
        let registered = vec![RegisteredKey {
            version: vector.event_recipient_key_version.clone(),
            identity_public_key: vector.identity_public_b64.clone(),
        }];
        assert_eq!(
            matching_public_key_version(&private_keys, &registered),
            Ok(vector.event_recipient_key_version)
        );

        let ambiguous = vec![
            RegisteredKey {
                version: "1".into(),
                identity_public_key: vector.identity_public_b64.clone(),
            },
            RegisteredKey {
                version: "2".into(),
                identity_public_key: vector.identity_public_b64,
            },
        ];
        assert_eq!(
            matching_public_key_version(&private_keys, &ambiguous),
            Err(RecoveryFailure::AmbiguousRegisteredKey)
        );
    }

    #[cfg(feature = "juicebox")]
    #[test]
    fn production_worker_calls_recovery_once_and_preserves_transient_state() {
        let calls = Arc::new(AtomicUsize::new(0));
        let client = Arc::new(MockJuicebox {
            calls: Arc::clone(&calls),
            recovery: MockRecovery::Return(Mutex::new(Some(RecoverResult::Failure {
                reason: RecoverFailureReason::Transient,
                guesses_remaining: None,
            }))),
        });
        let terminal = recover_with_api(
            valid_recovery_request(),
            Arc::new(CancelToken::default()),
            client,
            Duration::from_secs(1),
        );
        assert!(matches!(terminal, WorkerTerminal::Uncertain));
        assert_eq!(calls.load(Ordering::Acquire), 1);
    }

    #[cfg(feature = "juicebox")]
    #[test]
    fn production_cancellation_drops_the_inflight_recovery_future() {
        let calls = Arc::new(AtomicUsize::new(0));
        let dropped = Arc::new(AtomicBool::new(false));
        let client = Arc::new(MockJuicebox {
            calls: Arc::clone(&calls),
            recovery: MockRecovery::Pending(Arc::clone(&dropped)),
        });
        let session = NativeSession::new();
        let request = valid_recovery_request();
        let job_id = session
            .spawn_recovery(12, move |cancel| {
                recover_with_api(request, cancel, client, Duration::from_secs(10))
            })
            .expect("recovery starts");
        for _ in 0..200 {
            if calls.load(Ordering::Acquire) == 1 {
                break;
            }
            thread::sleep(Duration::from_millis(5));
        }
        assert_eq!(calls.load(Ordering::Acquire), 1);
        assert!(session.cancel_recovery(job_id, 12).unwrap());
        assert_eq!(
            wait_for_terminal(&session, job_id, 12).unwrap(),
            RecoveryPoll::Cancelled
        );
        assert!(dropped.load(Ordering::Acquire));
        assert!(!session.unlocked().unwrap());
    }

    #[test]
    fn official_recovery_failures_remain_distinct() {
        let cases = [
            (
                RecoverFailureReason::NotRegistered,
                RecoveryFailure::NotRegistered,
            ),
            (
                RecoverFailureReason::InvalidAuth,
                RecoveryFailure::InvalidAuth,
            ),
            (
                RecoverFailureReason::UpgradeRequired,
                RecoveryFailure::UpgradeRequired,
            ),
            (
                RecoverFailureReason::RateLimitExceeded,
                RecoveryFailure::RateLimited,
            ),
            (
                RecoverFailureReason::Assertion,
                RecoveryFailure::AssertionFailed,
            ),
        ];
        for (reason, expected) in cases {
            let terminal = recovery_terminal(
                RecoverResult::Failure {
                    reason,
                    guesses_remaining: None,
                },
                |_| unreachable!("failure cannot recover keys"),
            );
            assert!(matches!(terminal, WorkerTerminal::Failure(actual) if actual == expected));
        }
        assert!(matches!(
            recovery_terminal(RecoverResult::KeyReconstructionFailed, |_| unreachable!()),
            WorkerTerminal::Failure(RecoveryFailure::KeyReconstructionFailed)
        ));
        assert!(matches!(
            recovery_terminal(RecoverResult::NoTokens, |_| unreachable!()),
            WorkerTerminal::Failure(RecoveryFailure::NoTokens)
        ));
        assert!(matches!(
            recovery_terminal(
                RecoverResult::Failure {
                    reason: RecoverFailureReason::InvalidPin,
                    guesses_remaining: Some(0)
                },
                |_| unreachable!()
            ),
            WorkerTerminal::IncorrectPin {
                guesses_remaining: Some(0)
            }
        ));
    }

    #[test]
    fn async_mock_recovery_imports_keys_once() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(7, "2580".into(), 20)
            .expect("recovery starts");
        assert_eq!(
            session.poll_recovery(job_id, 7).unwrap(),
            RecoveryPoll::Pending
        );
        assert!(matches!(
            session.start_mock_recovery(7, "2580".into(), 0),
            Err(NativeError::Busy)
        ));
        assert_eq!(
            wait_for_terminal(&session, job_id, 7).unwrap(),
            RecoveryPoll::Unlocked {
                public_key_version: "1".into()
            }
        );
        assert!(session.unlocked().unwrap());
        assert_eq!(session.lock_state().user_id.as_deref(), Some("2222"));
        assert!(matches!(
            session.poll_recovery(job_id, 7),
            Err(NativeError::Stale)
        ));
    }

    #[cfg(feature = "juicebox")]
    #[test]
    fn recovered_registered_identity_signs_the_outbound_event() {
        let recovered = official_recovered_keys().expect("official keys recover");
        let client = Arc::new(MockJuicebox {
            calls: Arc::new(AtomicUsize::new(0)),
            recovery: MockRecovery::Return(Mutex::new(Some(RecoverResult::Success(
                recovered.bytes,
            )))),
        });
        let session = NativeSession::new();
        let request = valid_recovery_request();
        let job_id = session
            .spawn_recovery(21, move |cancel| {
                recover_with_api(request, cancel, client, Duration::from_secs(1))
            })
            .expect("production recovery starts");
        assert!(matches!(
            wait_for_terminal(&session, job_id, 21).unwrap(),
            RecoveryPoll::Unlocked { .. }
        ));

        let vector = parse_official_vector().expect("official vector parses");
        let sender_id = official_recipient_user_id(&vector).expect("recipient is valid");
        let remote_key = chat_xdk_core::SigningKeyEntry {
            user_id: vector.event_sender_id.clone(),
            public_key_version: vector.event_signing_key_version.clone(),
            public_key: vector.signing_public_b64.clone(),
            identity_public_key: vector.identity_public_b64.clone(),
            identity_public_key_signature: vector.identity_public_key_signature_b64.clone(),
        };
        session
            .with_core(|core| {
                let events = [
                    vector.event_key_change_b64.clone(),
                    vector.event_message_b64.clone(),
                ];
                let output = decrypt_events(core, &events, &[remote_key])?;
                if output.messages.len() != 1 {
                    return Err(NativeError::Xdk(
                        "official key event did not seed the conversation".into(),
                    ));
                }
                Ok(())
            })
            .expect("conversation key is cached");

        let plaintext = "identity-bound outbound message";
        let input = serde_json::json!({
            "conversation_id": vector.event_conversation_id.clone(),
            "text": plaintext
        })
        .to_string();
        let payload = session
            .prepare_text_payload(&input)
            .expect("bound identity prepares a message");
        let framed = chat_xdk_core::internals::frame_send_payload(
            &payload,
            &payload.message_id,
            &sender_id,
            &vector.event_conversation_id,
        )
        .expect("prepared payload frames as an inbound event");
        let sender_key = chat_xdk_core::SigningKeyEntry {
            user_id: sender_id.clone(),
            public_key_version: payload.signature_info.public_key_version.clone(),
            public_key: vector.signing_public_b64.clone(),
            identity_public_key: vector.identity_public_b64.clone(),
            identity_public_key_signature: vector.identity_public_key_signature_b64.clone(),
        };
        let verified = session
            .with_core(|core| decrypt_events(core, &[framed], &[sender_key]))
            .expect("prepared event decrypts");
        assert_eq!(verified.messages.len(), 1);
        assert_eq!(
            verified.messages[0].sender_id.as_deref(),
            Some(sender_id.as_str())
        );
        assert_eq!(verified.messages[0].text.as_deref(), Some(plaintext));
        assert!(verified.messages[0].verified);

        let forged = chat_xdk_core::internals::frame_send_payload(
            &payload,
            &payload.message_id,
            "9999",
            &vector.event_conversation_id,
        )
        .expect("forged sender event frames");
        let forged_key = chat_xdk_core::SigningKeyEntry {
            user_id: "9999".into(),
            public_key_version: payload.signature_info.public_key_version,
            public_key: vector.signing_public_b64,
            identity_public_key: vector.identity_public_b64,
            identity_public_key_signature: vector.identity_public_key_signature_b64,
        };
        let rejected = session
            .with_core(|core| decrypt_events(core, &[forged], &[forged_key]))
            .expect("forged event is processed safely");
        assert!(rejected.messages.is_empty());
        assert_eq!(rejected.errors.len(), 1);
    }

    #[test]
    fn invalid_pin_preserves_remaining_guesses() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(8, "1111".into(), 0)
            .expect("recovery starts");
        assert_eq!(
            wait_for_terminal(&session, job_id, 8).unwrap(),
            RecoveryPoll::IncorrectPin {
                guesses_remaining: Some(19)
            }
        );
        assert!(!session.unlocked().unwrap());

        let next_job_id = session
            .start_mock_recovery(8, "2580".into(), 0)
            .expect("a later explicit submission starts");
        assert!(next_job_id > job_id);
        assert_eq!(
            wait_for_terminal(&session, next_job_id, 8).unwrap(),
            RecoveryPoll::Unlocked {
                public_key_version: "1".into()
            }
        );
    }

    #[test]
    fn stale_identifiers_do_not_consume_or_cancel_current_recovery() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(9, "2580".into(), 20)
            .expect("recovery starts");
        assert!(matches!(
            session.poll_recovery(job_id + 1, 9),
            Err(NativeError::Stale)
        ));
        assert!(matches!(
            session.cancel_recovery(job_id, 10),
            Err(NativeError::Stale)
        ));
        assert_eq!(
            wait_for_terminal(&session, job_id, 9).unwrap(),
            RecoveryPoll::Unlocked {
                public_key_version: "1".into()
            }
        );
    }

    #[test]
    fn cancellation_and_destroy_settle_pending_workers() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(10, "2580".into(), 1_000)
            .expect("recovery starts");
        assert!(session.cancel_recovery(job_id, 10).unwrap());
        assert!(!session.cancel_recovery(job_id, 10).unwrap());
        assert_eq!(
            wait_for_terminal(&session, job_id, 10).unwrap(),
            RecoveryPoll::Cancelled
        );

        let second = session
            .start_mock_recovery(10, "2580".into(), 1_000)
            .expect("another explicit recovery starts");
        assert!(second > job_id);
        assert!(session.destroy());
        assert!(!session.live());
    }

    #[test]
    fn cancellation_after_delivery_prevents_key_import() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(11, "2580".into(), 0)
            .expect("recovery starts");
        wait_for_worker_exit(&session);
        assert!(session.cancel_recovery(job_id, 11).unwrap());
        assert_eq!(
            wait_for_terminal(&session, job_id, 11).unwrap(),
            RecoveryPoll::Cancelled
        );
        assert!(!session.unlocked().unwrap());
    }

    #[test]
    fn worker_panics_are_sanitized() {
        let session = NativeSession::new();
        let job_id = session
            .start_mock_recovery(11, "9999".into(), 0)
            .expect("recovery starts");
        assert!(matches!(
            wait_for_terminal(&session, job_id, 11),
            Err(NativeError::WorkerFailed)
        ));
        assert!(!session.unlocked().unwrap());
    }

    #[test]
    fn worker_boundary_types_are_send() {
        fn assert_send<T: Send>() {}
        assert_send::<WorkerTerminal>();
        assert_send::<RecoveryJob>();
        #[cfg(feature = "juicebox")]
        assert_send::<RecoveryRequest>();
    }
}
