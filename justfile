#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]
set shell := ["bash", "-cu"]

_default:
  @just --list -u


lint:
  cargo clippy --all-targets --all-features -- -D warnings
  ruff check
  oxlint

fix:
  cargo fix --allow-dirty
  ruff fix
  oxlint --fix
