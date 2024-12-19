#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]
set shell := ["bash", "-cu"]

_default:
  @just --list -u


lint:
  cargo clippy --all-targets --all-features -- -D warnings
  ruff check
  pnpm run lint

fix:
  cargo fix --allow-dirty
  ruff check --fix
  pnpm run fix

fmt:
  cargo fmt --all
  ruff format
  pnpm run format

check:
  cargo check
  mypy .
  tsc