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
esac
