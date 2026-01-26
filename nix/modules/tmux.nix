{ config, lib, pkgs, ... }:
{
  home.file = {
    ".tmux.conf".source = ../../resources/tmux/tmux.conf;
    ".tmux-powerlinerc".source = ../../resources/tmux/tmux-powerlinerc;
    ".tmux-powerline".source = ../../resources/tmux/tmux-powerline;
  };
}
