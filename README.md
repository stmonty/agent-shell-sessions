# agent-shell-sessions

A small Emacs dashboard for [agent-shell](https://github.com/xenodium/agent-shell).
See your live sessions, notice finished responses you haven't read, and restore
saved conversations after restarting Emacs.

- Sortable session list with status, project, last activity, and task.
- **Needs input** for permission requests; **Done · unread** for unseen completions.
- A Transient command menu, opened with `?`.
- Automatic workspace saving and explicit session restoration.
- One-second refresh while visible; no table refresh while hidden.

## Setup

Requires Emacs 30.1+, agent-shell 0.83.3+, and Transient 0.7.2+.
Install agent-shell and configure an agent first, then add this checkout:

```elisp
(add-to-list 'load-path "/path/to/agent-shell-sessions")
(require 'agent-shell-sessions)
(global-set-key (kbd "C-c A") #'agent-shell-sessions)
```

Run `M-x agent-shell-sessions` or use your chosen binding. Loading the library
enables session tracking and automatic saving. It does not start agents or
install global key bindings by itself.

## Commands

Press `?` in the dashboard to open the menu. These keys also work directly:

| Key | Action |
| --- | --- |
| `RET` | Visit a live session, or restore and visit a saved one |
| `o` | Open the selected session in another window |
| `N` | Start a new session in a chosen directory |
| `s` | Save the workspace immediately |
| `r` | Restore the selected saved session in the background |
| `R` | Restore all saved sessions that aren't already open |
| `d` | Forget the selected saved entry |
| `g` | Refresh the list |
| `i` | Change the refresh interval immediately; `0` means manual |
| `q` | Close the dashboard; in the menu, close the menu |

Click a column heading to sort. Selecting a session or its viewport clears its
unread marker; simply displaying it in an unselected pane does not. **Last
activity** means time since a prompt or agent notification, not task duration.

## Saved sessions

Metadata is saved automatically after session changes and when Emacs exits, to
`agent-shell-sessions.json` in `user-emacs-directory`. The file contains session
IDs, agent identifiers, directories, names, titles, activity times, and unread
flags. The agent stores the actual conversation.

After restarting, open the dashboard and restore a **Saved** or **Saved · unread**
row. Restoration starts an agent process and requires the provider to support
resuming that conversation. Running requests do not continue across Emacs exit.
Failed restores remain visible.

Closed sessions stay saved until forgotten. To remove a live session from the
workspace, kill its shell buffer, refresh the dashboard, then press `d` on its
saved row. This does not delete the provider's conversation or Markdown transcript.

```elisp
;; Set these before requiring the package.
(setq agent-shell-sessions-refresh-interval 5) ; nil for manual refresh
(setq agent-shell-sessions-workspace-file
      (expand-file-name "agent-shell-sessions.json" user-emacs-directory))
```

Writes replace the JSON file atomically. Multiple Emacs instances sharing the
same workspace file are not coordinated; give each instance a separate file.

## Development

With the dependencies installed in Emacs's normal package directory:

```sh
make check                   # ERT tests and byte compilation
make check EMACS=/path/to/emacs
make clean                   # remove compiled output
```

Tests use temporary workspace files and mock agent startup. They do not contact
providers. Tested with Emacs 30.2 and agent-shell 0.83.3. Some detailed status and
background restoration code uses agent-shell internals, so upstream API changes
may require updates.

## License

Copyright (C) 2026 Monty Bichouna.

GNU General Public License, version 3 or later. See [COPYING](COPYING).
