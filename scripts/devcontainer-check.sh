#!/usr/bin/env bash
set -euo pipefail

echo "Running nix flake check..."
nix --accept-flake-config flake check

echo "Applying home-manager config for devcontainer..."
nix --accept-flake-config run nixpkgs#home-manager -- switch --flake .#devcontainer@devcontainer
