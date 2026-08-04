{ config, lib, pkgs, ... }:
{
  home.file = {
    ".gitconfig".source = ../../resources/git/.gitconfig;
    ".vimrc".source = ../../resources/vim/.vimrc;
    ".zshenv".source = ../../resources/zsh/.zshenv;
    ".zsh".source = ../../resources/zsh/.zsh;
  };
}
