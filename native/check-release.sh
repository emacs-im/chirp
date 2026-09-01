#!/usr/bin/env bash
set -euo pipefail

NATIVE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly NATIVE_DIR
readonly CRATE_DIR="$NATIVE_DIR/chirp-xchat-module"

fail() {
  printf 'check-release: %s\n' "$*" >&2
  exit 1
}

module_file=${CHIRP_XCHAT_RELEASE_MODULE_FILE:-}
if test -z "$module_file"; then
  candidates=()
  for candidate in \
    "$CRATE_DIR/target/release/libchirp_xchat_native.so" \
    "$CRATE_DIR/target/release/libchirp_xchat_native.dylib" \
    "$CRATE_DIR/target/release/chirp_xchat_native.dll"
  do
    test ! -f "$candidate" || candidates+=("$candidate")
  done
  test "${#candidates[@]}" -eq 1 ||
    fail "expected exactly one release module; set CHIRP_XCHAT_RELEASE_MODULE_FILE"
  module_file=${candidates[0]}
fi

test -f "$module_file" || fail "release module is missing: $module_file"
for forbidden in \
  'fixture event message' \
  'private_keys_concat_b64' \
  'chirp-xchat-native-test-decrypt-official-vector' \
  'chirp-xchat-native-test-recovery-start' \
  'chirp-xchat-native-test-recovery-poll' \
  'chirp-xchat-native-test-recovery-cancel' \
  'synthetic recovery worker failure'
do
  if LC_ALL=C grep -aFq -- "$forbidden" "$module_file"; then
    fail "release module contains test-vector data: $forbidden"
  fi
done

emacs_bin=${EMACS:-emacs}
command -v "$emacs_bin" >/dev/null 2>&1 || fail "Emacs is unavailable: $emacs_bin"
CHIRP_XCHAT_RELEASE_MODULE_FILE="$module_file" "$emacs_bin" \
  -Q --batch --module-assertions --eval '
(progn
  (module-load (getenv "CHIRP_XCHAT_RELEASE_MODULE_FILE"))
  (unless (featurep (quote chirp-xchat-native-module))
    (error "Native module did not provide its feature"))
  (unless (equal (chirp-xchat-native-version) "0.2.5/chat-xdk-0.4.3")
    (error "Release module version is incompatible"))
  (dolist (function
           (quote (chirp-xchat-native-decrypt
                   chirp-xchat-native-decrypt-media
                   chirp-xchat-native-encrypt-text
                   chirp-xchat-native-encrypt-reply
                   chirp-xchat-native-encrypt-reaction
                   chirp-xchat-native-prepare-media
                   chirp-xchat-native-release-media
                   chirp-xchat-native-recovery-start
                   chirp-xchat-native-recovery-poll
                   chirp-xchat-native-recovery-cancel)))
    (unless (fboundp function)
      (error "Release module lacks production function: %s" function)))
  (dolist (function
           (quote (chirp-xchat-native-test-decrypt-official-vector
                   chirp-xchat-native-test-recovery-start
                   chirp-xchat-native-test-recovery-poll
                   chirp-xchat-native-test-recovery-cancel)))
    (when (fboundp function)
      (error "Release module exposed test function: %s" function)))
  (let ((session (chirp-xchat-native-session-create)))
    (unless (and (chirp-xchat-native-session-live-p session)
                 (not (chirp-xchat-native-session-unlocked-p session))
                 (chirp-xchat-native-session-destroy session)
                 (not (chirp-xchat-native-session-live-p session)))
      (error "Release module session lifecycle failed"))))'

printf 'Release module passed: %s\n' "$module_file"
