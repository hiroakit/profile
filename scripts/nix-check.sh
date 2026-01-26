#!/usr/bin/env bash
set -euo pipefail

echo "Running nix flake checks..."
nix flake check
