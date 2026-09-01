# Optional XChat native module

`chirp-xchat-module` is an optional GNU Emacs dynamic module built around X's official Rust `chat-xdk`. It owns XChat private keys, versioned conversation keys, signature verification, message and downloaded-media decryption, and outgoing encryption/signing without exposing key material to Emacs Lisp. Direct-message entry requires it, while other Chirp features remain usable without it.

The release module enables the official XDK Juicebox integration. Each explicit `M-x chirp-direct-messages` unlock submission creates one native job and one `recover_private_key` call, with no Chirp-level retry; the inbox opens only after recovery succeeds. The requested X user is checked against the registered-key response and becomes the native session's sender identity only after the recovered identity key matches that record; outgoing message input cannot override it. X GraphQL and authenticated Chat media downloads remain in Lisp, while the official native SDK contacts the validated HTTPS Juicebox realms and decrypts downloaded ciphertext with an exact verified conversation-key version retained from the corresponding event batch. PINs, recovered private keys, and conversation keys never cross back into Lisp; only status, signature-verified domain data, decrypted attachment bytes, and opaque outgoing envelopes do.

Recovery workers own only Rust values and never retain an Emacs environment or Lisp value. Epoch and job identities reject stale polling and cancellation, a 60-second outer timeout reports an uncertain result, and cancellation drops the in-flight Tokio future. Outgoing plaintext enters the module only for a synchronous bounded SDK call; Lisp receives an opaque message event and signature envelope, which it submits once through the fixed X write operation. Explicit session destruction cancels and joins before dropping the official SDK state. The user-pointer finalizer only requests cancellation and detaches, so GC cannot block on a worker.

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
