{ config, lib, pkgs, ... }:
let
  settingsSource = ../../resources/vscode/Code/User/settings.json;
  snippetSource = ../../resources/vscode/Code/User/snippets/markdown.json;
in
{
  home.file = lib.mkMerge [
    (lib.mkIf pkgs.stdenv.isDarwin {
      "Library/Application Support/Code/User/settings.json".source = settingsSource;
      "Library/Application Support/Code/User/snippets/markdown.json".source = snippetSource;
    })
    (lib.mkIf pkgs.stdenv.isLinux {
      ".config/Code/User/settings.json".source = settingsSource;
      ".config/Code/User/snippets/markdown.json".source = snippetSource;
      ".vscode-server/data/Machine/settings.json".source = settingsSource;
      ".vscode-server/data/Machine/snippets/markdown.json".source = snippetSource;
    })
  ];
}
