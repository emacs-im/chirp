# Optional XChat native module

`chirp-xchat-module` is an optional GNU Emacs dynamic module built around X's official Rust `chat-xdk`. It owns XChat private keys, versioned conversation keys, signature verification, message and downloaded-media decryption, local-media stream encryption, and outgoing message encryption/signing without exposing key material to Emacs Lisp. Direct-message entry requires it, while other Chirp features remain usable without it.

## Security and lifecycle boundary

### Unlock and identity

Each explicit `M-x chirp-direct-messages` unlock submission creates one native job and one official `recover_private_key` call, with no Chirp-level retry; the inbox opens only after recovery succeeds. The requested X user becomes the native session's sender identity only after the recovered identity key matches the registered-key response, and per-message input cannot override it. The official native SDK contacts only validated HTTPS Juicebox realms. PINs, recovered private keys, and conversation keys never cross back into Lisp; Lisp receives only status, signature-verified domain data, decrypted attachment bytes, encrypted staging metadata, and opaque outgoing envelopes.

### Media and outgoing messages

The native module owns the cryptographic side of outgoing media. It validates the selected file and detected content type, enforces the plaintext size bound, stream-encrypts into a session-owned temporary file, and returns only file metadata plus the opaque verified conversation-key version. Final message encryption is pinned to that version, so key rotation cannot separate the attachment ciphertext from its message envelope. Release, failure, cancellation, and session destruction all remove the staged ciphertext.

Lisp owns the network side. `chirp-x.el` creates a transport-only upload UUID, initializes the upload through authenticated X GraphQL, sends bounded ciphertext parts to TON with session cookies and CSRF, and finalizes through GraphQL. Native code never receives X credentials or calls X APIs.

Outgoing text, typed media descriptors, reply targets, bounded key-event envelopes, and reactions enter the native module only for synchronous bounded SDK operations. Lisp receives the signed opaque event envelope and submits it once through the fixed X write operation.

### Cancellation and destruction

Recovery workers own only Rust values and never retain an Emacs environment or Lisp value. Epoch and job identities reject stale polling and cancellation, a 60-second outer timeout reports an uncertain result, and cancellation drops the in-flight Tokio future. Explicit session destruction cancels and joins recovery before dropping official SDK state and native media stages. The user-pointer finalizer only requests cancellation and detaches from recovery, so GC cannot block on a worker.

## Build and test

The build uses exact upstream commits while keeping their large prebuilt archives out of this repository:

```bash
native/bootstrap-deps.sh
(
  cd native/chirp-xchat-module
  cargo test --locked --features test-vector
  cargo build --locked --features test-vector
  cargo build --release --locked
)
native/check-release.sh
```

Then set `chirp-xchat-native-module-file` to the absolute release-library path with `M-x customize-option`. Chirp deliberately does not search source trees, build directories, or `load-path` for this security-sensitive module. The usual Linux build output is `native/chirp-xchat-module/target/release/libchirp_xchat_native.so`; use the corresponding `.dylib` or `.dll` name on other platforms.

Developers with an existing exact checkout can avoid downloading it again:

```bash
CHIRP_CHAT_XDK_SOURCE=/path/to/chat-xdk \
CHIRP_JUICEBOX_SDK_SOURCE=/path/to/juicebox-sdk \
native/bootstrap-deps.sh
```

The required revisions are verified by `bootstrap-deps.sh`:

- `chat-xdk` 0.4.3: `b9d8d44cf0abdbf8c76a1422aa1bd60cc0042d6e`
- Juicebox SDK 0.3.4: `3ac7202fb24a25dcbd1cf54156edbf41370394de`
- Apache Thrift: `deb36fa409849de45973b04ffc3ce49d277ca90a`
- Rust: 1.91.1

The `test-vector` feature embeds only X's published synthetic fixture and exposes synthetic recovery test functions; neither is present in distributed builds. To run the ERT module smoke after the feature-enabled debug build:

```bash
MODULE_FILE="$(find native/chirp-xchat-module/target/debug -maxdepth 1 \
  -type f \( -name 'libchirp_xchat_native.so' \
  -o -name 'libchirp_xchat_native.dylib' \
  -o -name 'chirp_xchat_native.dll' \) -print -quit)"
CHIRP_XCHAT_MODULE_FILE="$MODULE_FILE" \
  emacs -Q --batch --module-assertions -L . -L lisp -l ert \
  -l test/chirp-xchat-native-test.el -f ert-run-tests-batch-and-exit
```

Use the platform's actual Cargo library suffix in `CHIRP_XCHAT_MODULE_FILE`. Build dependencies live under ignored `native/.deps/`; build products live under the crate's ignored `target/`.

`chat-xdk` and Juicebox SDK are MIT-licensed. Apache Thrift is Apache-2.0-licensed. Their source remains in the ignored build cache and is not copied into Chirp releases. Chirp supports local source builds only; do not redistribute a compiled module until it carries the complete generated attribution bundle described in `THIRD_PARTY_NOTICES.md`.
