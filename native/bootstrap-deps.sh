#!/usr/bin/env bash
set -euo pipefail

NATIVE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
readonly NATIVE_DIR
readonly DEPS_DIR="$NATIVE_DIR/.deps"
readonly CHAT_XDK_COMMIT="b9d8d44cf0abdbf8c76a1422aa1bd60cc0042d6e"
readonly CHAT_XDK_REF="refs/tags/go/chatxdk/v0.4.3"
readonly JUICEBOX_SDK_COMMIT="3ac7202fb24a25dcbd1cf54156edbf41370394de"
readonly JUICEBOX_SDK_REF="refs/tags/0.3.4"
readonly THRIFT_COMMIT="deb36fa409849de45973b04ffc3ce49d277ca90a"

fail() {
  printf 'bootstrap-deps: %s\n' "$*" >&2
  exit 1
}

verify_checkout() {
  local name=$1 checkout=$2 commit=$3 status
  test "$(git -C "$checkout" rev-parse HEAD)" = "$commit" ||
    fail "$checkout is not at required commit $commit"
  status="$(git -C "$checkout" status --porcelain=v1 --untracked-files=all)"
  test -z "$status" ||
    fail "$name checkout has local changes; refusing a non-reproducible build"
  printf 'Using %s at %s\n' "$name" "$commit"
}

link_source() {
  local name=$1 source=$2 commit=$3 destination="$DEPS_DIR/$1"
  source="$(realpath -- "$source")"
  git -C "$source" rev-parse --git-dir >/dev/null 2>&1 ||
    fail "$source is not a Git checkout"
  verify_checkout "$name" "$source" "$commit"
  if test -e "$destination" || test -L "$destination"; then
    test "$(realpath -- "$destination")" = "$source" ||
      fail "$destination already exists and does not point to $source"
  else
    ln -s -- "$source" "$destination"
  fi
}

fetch_sparse() {
  local name=$1 url=$2 ref=$3 commit=$4 destination="$DEPS_DIR/$1"
  shift 4
  if ! git -C "$destination" rev-parse --git-dir >/dev/null 2>&1; then
    test ! -e "$destination" || fail "$destination exists but is not a Git checkout"
    git init --quiet "$destination"
    git -C "$destination" remote add origin "$url"
    git -C "$destination" sparse-checkout init --cone
    git -C "$destination" sparse-checkout set "$@"
    GIT_TERMINAL_PROMPT=0 git -C "$destination" \
      fetch --quiet --depth=1 --filter=blob:none origin "$ref"
    git -C "$destination" checkout --quiet --detach FETCH_HEAD
  fi
  verify_checkout "$name" "$destination" "$commit"
}

mkdir -p "$DEPS_DIR"

if test -n "${CHIRP_CHAT_XDK_SOURCE:-}"; then
  link_source chat-xdk "$CHIRP_CHAT_XDK_SOURCE" "$CHAT_XDK_COMMIT"
else
  fetch_sparse chat-xdk https://github.com/xdevplatform/chat-xdk.git \
    "$CHAT_XDK_REF" "$CHAT_XDK_COMMIT" crates/core crates/macros
fi

if test -n "${CHIRP_JUICEBOX_SDK_SOURCE:-}"; then
  link_source juicebox-sdk "$CHIRP_JUICEBOX_SDK_SOURCE" \
    "$JUICEBOX_SDK_COMMIT"
else
  fetch_sparse juicebox-sdk \
    https://github.com/juicebox-systems/juicebox-sdk.git \
    "$JUICEBOX_SDK_REF" "$JUICEBOX_SDK_COMMIT" rust
fi

if test -n "${CHIRP_THRIFT_SOURCE:-}"; then
  link_source thrift "$CHIRP_THRIFT_SOURCE" "$THRIFT_COMMIT"
else
  fetch_sparse thrift https://github.com/apache/thrift.git \
    "$THRIFT_COMMIT" "$THRIFT_COMMIT" lib/rs
fi
