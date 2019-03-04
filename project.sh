#!/usr/bin/env bash

CABAL_FLAGS="-j8"

cmd="$1"

shift

case "$cmd" in
  build)
    cabal new-build --enabled-tests \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal new-test --enable-tests \
      $CABAL_FLAGS "$@"
    ;;
esac
