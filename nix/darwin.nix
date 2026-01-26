{ config, pkgs, hostConfig, ... }:
{
  system.stateVersion = 4;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  programs.zsh.enable = true;
  users.users.${hostConfig.username}.home = "/Users/${hostConfig.username}";
}
