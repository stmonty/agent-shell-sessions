# agent-shell-sessions

A simple (vibe-coded) Emacs dashboard for [agent-shell](https://github.com/xenodium/agent-shell).

- See all your sessions, their status, and what they're working on.
- Spot sessions waiting for input and finished responses you haven't read.
- Automatically save your workspace and restore sessions after restarting Emacs.
- Manage sessions through a Magit-style Transient menu.

## Setup

Requires Emacs 30.1+, agent-shell 0.83.3+, and Transient 0.7.2+.

```elisp
(add-to-list 'load-path "/path/to/agent-shell-sessions")
(require 'agent-shell-sessions)
(global-set-key (kbd "C-c A") #'agent-shell-sessions)
```

Open with `C-c A` or `M-x agent-shell-sessions`. Press `?` for the command menu,
`RET` to visit or restore a session, and `N` to start a new one.

The list refreshes while visible. Sessions are saved automatically; restoring
a conversation requires support from the agent.

## License

Copyright (C) 2026 Monty Bichouna.

[GPL v3 or later](LICENSE).
