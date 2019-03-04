#!/usr/bin/env bash

CABAL_FLAGS="-j8"

cmd="$1"

shift

case "$cmd" in
  build)
    cabal v2-build --enable-tests \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal v2-test --enable-tests \
      $CABAL_FLAGS "$@"
    ;;
esac
