{ config, lib, pkgs, hostConfig, ... }:
let
  username = hostConfig.username;
  homeDir =
    if pkgs.stdenv.isDarwin
    then "/Users/${username}"
    else "/home/${username}";
in
{
  imports = [
    ./modules/common.nix
    ./modules/emacs.nix
    ./modules/macos.nix
    ./modules/vscode.nix
    ./modules/tmux.nix
    ./modules/wsl.nix
    ./modules/claude.nix
  ];

  home.username = username;
  home.homeDirectory = homeDir;
  home.stateVersion = "24.05";

  nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;
}
