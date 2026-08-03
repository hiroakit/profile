{ config, lib, pkgs, ... }:
{
  home.file = {
    ".claude/settings.json".source = ../../resources/claude/settings.json;
    ".claude/hooks/record-command-history.py" = {
      source = ../../resources/claude/hooks/record-command-history.py;
      executable = true;
    };
  };
}
