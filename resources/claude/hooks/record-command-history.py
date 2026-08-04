#!/usr/bin/env python3
"""Record the Bash commands Claude Code runs into a searchable history file.

Installed as a PostToolUse hook (matcher "Bash") by resources/claude/settings.json.
Claude Code feeds the hook payload as JSON on stdin; see
https://code.claude.com/docs/en/hooks

The history is kept in its own file instead of ${HISTFILE} so that zsh's own
history stays untouched.  resources/zsh/.zsh/.zclaude reads it back.

Record format is one tab separated line per command, so grep, fzf and zsh can
read it without a JSON parser:

    <epoch>\t<session-id>\t<cwd>\t<command>

Backslash, tab, carriage return and newline inside the fields are escaped
(\\\\, \\t, \\r, \\n) to keep one record on one line.

Environment:
    CLAUDE_HISTFILE   history file path (default ~/.claude/command-history.tsv)
    CLAUDE_HISTSIZE   records to keep when the file is trimmed (default 20000)
"""

import json
import os
import sys
import time

DEFAULT_HISTFILE = "~/.claude/command-history.tsv"
DEFAULT_HISTSIZE = 20000

# The file is only rewritten once it grows past this, so the common path stays
# a single append.
TRIM_THRESHOLD_BYTES = 8 * 1024 * 1024


def history_file():
    path = os.environ.get("CLAUDE_HISTFILE") or DEFAULT_HISTFILE
    return os.path.abspath(os.path.expanduser(path))


def history_size():
    try:
        size = int(os.environ.get("CLAUDE_HISTSIZE", ""))
    except ValueError:
        return DEFAULT_HISTSIZE
    return size if size > 0 else DEFAULT_HISTSIZE


def escape(value):
    # The backslash has to be escaped first, otherwise it would escape the
    # backslashes introduced below.
    return (
        value.replace("\\", "\\\\")
        .replace("\t", "\\t")
        .replace("\r", "\\r")
        .replace("\n", "\\n")
    )


def append(path, line):
    """Append one record, keeping the file private to the user."""
    descriptor = os.open(path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o600)
    try:
        try:
            import fcntl

            fcntl.flock(descriptor, fcntl.LOCK_EX)
        except (ImportError, OSError):
            # Without flock the O_APPEND write is still atomic enough for the
            # short records written here.
            pass
        os.write(descriptor, line.encode("utf-8"))
    finally:
        os.close(descriptor)


def trim(path, keep):
    """Drop the oldest records once the file gets large."""
    if os.path.getsize(path) <= TRIM_THRESHOLD_BYTES:
        return

    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        records = handle.readlines()
    if len(records) <= keep:
        return

    temporary = path + ".tmp"
    with open(temporary, "w", encoding="utf-8") as handle:
        handle.writelines(records[-keep:])
    os.chmod(temporary, 0o600)
    os.replace(temporary, path)


def main():
    try:
        payload = json.load(sys.stdin)
    except (ValueError, UnicodeDecodeError):
        return 0
    if not isinstance(payload, dict) or payload.get("tool_name") != "Bash":
        return 0

    tool_input = payload.get("tool_input")
    command = tool_input.get("command") if isinstance(tool_input, dict) else None
    if not isinstance(command, str) or not command.strip():
        return 0

    path = history_file()
    directory = os.path.dirname(path)
    if directory:
        os.makedirs(directory, exist_ok=True)

    record = "\t".join(
        [
            str(int(time.time())),
            escape(str(payload.get("session_id") or "-")),
            escape(str(payload.get("cwd") or "-")),
            escape(command),
        ]
    )
    append(path, record + "\n")
    trim(path, history_size())
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:  # noqa: BLE001 - logging must never disturb the session
        sys.exit(0)
