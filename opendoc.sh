#!/bin/bash

LIB="Ships"

if [[ "$OSTYPE" == "darwin"* ]]; then
  open _build/default/_doc/_html/$LIB/$LIB/index.html
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/$LIB/$LIB/)
    explorer.exe ${DOCPATH} || true
  else
    nautilus _build/default/_doc/_html/$LIB/$LIB/
  fi
fi