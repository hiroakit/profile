{ config, pkgs, ... }:
{
  system.stateVersion = 4;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  programs.zsh.enable = true;
}
