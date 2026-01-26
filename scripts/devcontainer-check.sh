#!/usr/bin/env bash
set -euo pipefail

echo "Running nix flake check..."
nix flake check

echo "Applying home-manager config for devcontainer..."
nix run nixpkgs#home-manager -- switch --flake .#devcontainer@devcontainer
