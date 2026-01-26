{ config, lib, pkgs, ... }:
{
  config = lib.mkIf pkgs.stdenv.isLinux {
    home.sessionPath = [
      "${config.home.homeDirectory}/.local/bin"
    ];
  };
}
