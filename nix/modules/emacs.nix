{ config, lib, pkgs, hostConfig, ... }:
let
  # Emacsは自分で /Applications/Emacs.app に入れる。
  # nixやHomebrewではインストールしない。
  emacsApp = "/Applications/Emacs.app";
  emacsBin = "${emacsApp}/Contents/MacOS/Emacs";

  # Emacsは ~/.emacs.d 配下に elpa, custom.el, eln-cache などを書き込む。
  # nix storeは読み取り専用でそれができないため、macOSでは
  # リポジトリの実体へシンボリックリンクを張って可変にしておく。
  dotfilesDir = hostConfig.dotfilesDir or null;
  useOutOfStoreSymlink = pkgs.stdenv.isDarwin && dotfilesDir != null;

  # コマンドラインからEmacs.appのEmacsを起動するラッパー。
  # ~/.local/bin は .zshrc でPATHに追加している。
  #
  # emacsclientが -a '' でdaemonを起動する時もPATH上のemacsを探すため、
  # このラッパーがあるとEmacs.appのEmacsがサーバーになる。
  emacsWrapper = ''
    #!/bin/sh
    #
    # emacs -- コマンドラインからEmacs.appのEmacsを起動する
    #
    # GUIで開きたくない時は -nw (--no-window-system) を付ける。
    #
    set -eu

    if [ ! -x "${emacsBin}" ]; then
        echo "emacs: ${emacsBin} が見つからない。${emacsApp} を用意する。" >&2
        exit 127
    fi

    exec "${emacsBin}" "$@"
  '';
in
{
  home.file = {
    ".emacs.d".source =
      if useOutOfStoreSymlink
      then config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/resources/emacs/.emacs.d"
      else ../../resources/emacs/.emacs.d;
  } // lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".local/bin/emacs" = {
      executable = true;
      text = emacsWrapper;
    };
  };
}
