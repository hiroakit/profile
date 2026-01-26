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
    ./modules/macos.nix
    ./modules/wsl.nix
  ];

  home.username = username;
  home.homeDirectory = homeDir;
  home.stateVersion = "24.05";

  nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;
}
