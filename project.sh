#!/usr/bin/env bash

CABAL_FLAGS="-j8"

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
    cabal new-test \
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

  exec)
    hpack
    stack exec \
      $CABAL_FLAGS "$@"
    ;;
esac

