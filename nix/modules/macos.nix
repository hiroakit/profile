{ lib, pkgs, ... }:
{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.sessionPath = [
      "/opt/homebrew/bin"
      "/opt/homebrew/sbin"
    ];

    xdg.configFile."ghostty/config.ghostty".source = ../../resources/ghostty/config.ghostty;
  };
}
