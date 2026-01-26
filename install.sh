#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

is_linux() { [[ "${OSTYPE:-}" == linux* ]]; }
is_darwin() { [[ "${OSTYPE:-}" == darwin* ]]; }

echo "Initializing submodules (if any)..."
if command -v git >/dev/null 2>&1 && [ -f "$ROOT_DIR/.gitmodules" ]; then
  git submodule update --init --recursive
fi

if is_linux; then
  echo "Linux detected: applying Nix/Home Manager config (Codespaces-friendly)."

  if ! command -v nix >/dev/null 2>&1; then
    echo "Nix not found; installing Nix..."
    # Prefer Determinate Nix installer (works well in containers).
    if command -v curl >/dev/null 2>&1; then
      curl -fsSL https://install.determinate.systems/nix | sh -s -- install --no-confirm || true
    fi

    # Load Nix profile if installed.
    if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
      # shellcheck disable=SC1091
      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    elif [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
      # shellcheck disable=SC1091
      . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
    fi
  fi

  # Ensure flakes are enabled even on older Nix installs.
  mkdir -p "${HOME}/.config/nix"
  if [ ! -f "${HOME}/.config/nix/nix.conf" ] || ! grep -q "experimental-features" "${HOME}/.config/nix/nix.conf"; then
    printf "\nexperimental-features = nix-command flakes\n" >> "${HOME}/.config/nix/nix.conf"
  fi

  if ! command -v nix >/dev/null 2>&1; then
    echo "ERROR: nix command not available after install attempt." >&2
    exit 1
  fi

  echo "Applying Home Manager configuration..."
  nix --accept-flake-config run nixpkgs#home-manager -- switch --flake "$ROOT_DIR#devcontainer@devcontainer"

  echo "Done."
  exit 0
fi

if is_darwin; then
  echo "macOS detected: running legacy (dotnet-script + Homebrew) installer."

  echo "Checking dotnet"
  if ! type dotnet >/dev/null 2>&1; then
    if [ ! -f /usr/local/share/dotnet/dotnet ]; then
      echo "dotnet doesn't exist."
      ./scripts/install-dotnet.sh
    fi

    eval "$(/usr/libexec/path_helper -s)"
  fi

  echo "Checking dotnet script"
  if ! (dotnet tool list -g | grep dotnet-script > /dev/null 2>&1); then
    echo "Install dotnet-script"
    dotnet tool install -g dotnet-script
  fi

  echo "Checking Homebrew"
  if ! type brew >/dev/null 2>&1; then
    if [ ! -d /opt/homebrew/bin ]; then
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi

  echo "Run installation scripts"
  dotnet script scripts/Main.csx
  exit 0
fi

echo "Unsupported OS: OSTYPE='${OSTYPE:-unknown}'" >&2
exit 1
