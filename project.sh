#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  build)
    hpack
    cabal new-build \
      $CABAL_FLAGS "$@"
    ;;

  test)
    hpack
    stack test \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    hpack
    stack bench \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    hpack
    stack repl \
      $CABAL_FLAGS "$@"
    ;;
esac
