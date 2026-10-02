;;; agent-shell-sessions.el --- Dashboard for agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Monty Bichouna

;; Author: Monty Bichouna <stmontydev@gmail.com>
;; Maintainer: Monty Bichouna <stmontydev@gmail.com>
;; URL: https://github.com/stmonty/agent-shell-sessions
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (agent-shell "0.83.3") (transient "0.7.2"))
;; Keywords: tools, processes, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A sortable dashboard for live and saved agent-shell sessions, with
;; unread completion tracking, a Transient menu, and workspace restoration.
;; Require this library to enable tracking and automatic workspace saving;
;; run M-x agent-shell-sessions to open the dashboard and press ? for actions.
;; No global keys are installed and saved agents are only restored on request.
;;
;; Status, enumeration, events, and activity times use agent-shell's public
;; APIs.  Detailed state, config resolution, and background startup also use
;; internal APIs; see the README for the tested dependency versions.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'tabulated-list)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'transient)

(defgroup agent-shell-sessions nil
  "Overview of live agent shell buffers."
  :group 'agent-shell
  :prefix "agent-shell-sessions-"
  :link '(url-link "https://github.com/stmonty/agent-shell-sessions"))

(defcustom agent-shell-sessions-refresh-interval 1
  "Seconds between refreshes while the session list is visible.
Nil disables automatic refresh; use `g' to refresh manually.
Use `agent-shell-sessions-set-refresh-interval' to change this immediately
in existing dashboard buffers."
  :type '(choice (const :tag "Manual refresh" nil)
                 (natnum :tag "Refresh interval in seconds (minimum 1)"))
  :group 'agent-shell-sessions)

(defcustom agent-shell-sessions-workspace-file
  (expand-file-name "agent-shell-sessions.json" user-emacs-directory)
  "File remembering sessions for explicit restoration after Emacs exits.
Contains session IDs, agent identifiers, directories, names, titles,
activity times, and unread flags.  Conversations remain with the agent.
Closed sessions remain saved until forgotten from the dashboard."
  :type 'file
  :group 'agent-shell-sessions)

(defvar agent-shell-sessions--records nil
  "Workspace records currently held in memory.")
(defvar agent-shell-sessions--loaded-file nil
  "Workspace file most recently read.")
(defvar agent-shell-sessions--last-written nil
  "Snapshot of records last read from or written to disk.")
(defvar agent-shell-sessions--load-error nil
  "Error preventing the workspace file from being overwritten, or nil.")
(defvar agent-shell-sessions--save-timer nil
  "Pending timer that coalesces workspace writes.")
(defvar agent-shell-sessions--restore-errors nil
  "Alist of session keys and restoration error messages.")
(defvar agent-shell-sessions--restoring-record nil
  "Dynamically bound while starting a saved session.")
(defvar-local agent-shell-sessions--restore-record nil
  "Workspace record being restored in this shell, or nil.")

(defconst agent-shell-sessions--buffer-name "*Agent Sessions*"
  "Name of the dashboard buffer.")
(defconst agent-shell-sessions--statuses
  '((blocked "Needs input" warning)
    (unread "Done · unread" font-lock-warning-face)
    (saved-unread "Saved · unread" font-lock-warning-face)
    (restore-error "Restore failed" error)
    (unknown "Unknown" error)
    (stopped "Stopped" shadow)
    (ready "Ready" success)
    (busy "Working" font-lock-keyword-face)
    (starting "Starting" shadow)
    (saved "Saved" shadow))
  "Status symbols, labels, and faces, in attention priority order.")

(defvar-local agent-shell-sessions--timer nil
  "Dashboard refresh timer, or nil when refresh is manual.")
(defvar-local agent-shell-sessions--summary ""
  "Summary of session counts shown in the mode line.")
(defvar-local agent-shell-sessions--unread nil
  "Non-nil in a shell with a completed turn that has not been visited.
Tracked from completion events, independently of dashboard refreshes.")
(defvar-local agent-shell-sessions--subscription nil
  "Event subscription token for this shell.")

;;; Workspace persistence

(defun agent-shell-sessions--record-key (record)
  "Return RECORD's identity across processes and Emacs restarts."
  (list (plist-get record :agent) (plist-get record :directory)
        (plist-get record :id)))

(defun agent-shell-sessions--valid-record-p (record)
  "Whether RECORD is a supported workspace JSON record."
  (and (listp record)
       (seq-every-p (lambda (key)
                      (let ((value (plist-get record key)))
                        (and (stringp value) (not (string-empty-p value)))))
                    '(:id :agent :directory :name))
       (file-name-absolute-p (plist-get record :directory))
       (stringp (plist-get record :title))
       (or (null (plist-get record :updated))
           (and (numberp (plist-get record :updated))
                (>= (plist-get record :updated) 0)))
       (memq (plist-get record :unread) '(nil t))))

(defun agent-shell-sessions--ensure-loaded ()
  "Read workspace data once, without evaluating code or starting agents.
Refuse to overwrite invalid data so it can be recovered by the user."
  (unless (equal agent-shell-sessions--loaded-file
                 agent-shell-sessions-workspace-file)
    (setq agent-shell-sessions--loaded-file agent-shell-sessions-workspace-file
          agent-shell-sessions--records nil
          agent-shell-sessions--last-written nil
          agent-shell-sessions--load-error nil
          agent-shell-sessions--restore-errors nil)
    (when (file-exists-p agent-shell-sessions-workspace-file)
      (condition-case err
          (let ((data (with-temp-buffer
                        (insert-file-contents agent-shell-sessions-workspace-file)
                        (json-parse-buffer :object-type 'plist :array-type 'list
                                           :null-object nil :false-object nil))))
            (unless (and (equal (plist-get data :version) 1)
                         (plist-member data :sessions)
                         (listp (plist-get data :sessions))
                         (seq-every-p #'agent-shell-sessions--valid-record-p
                                      (plist-get data :sessions)))
              (error "Unsupported workspace format"))
            (setq agent-shell-sessions--records (plist-get data :sessions)
                  agent-shell-sessions--last-written
                  (copy-tree agent-shell-sessions--records)))
        (error
         (setq agent-shell-sessions--load-error (error-message-string err))
         (display-warning 'agent-shell-sessions
                          (format "Cannot read %s: %s. File left unchanged."
                                  agent-shell-sessions-workspace-file
                                  agent-shell-sessions--load-error)))))))

(defun agent-shell-sessions--record (buffer)
  "Return serializable metadata for BUFFER, or nil before it has an ID."
  (with-current-buffer buffer
    (when-let* ((id (agent-shell-session-id))
                (agent (map-elt (agent-shell-get-config buffer) :identifier)))
      (let ((updated (agent-shell-last-activity-time)))
        (list :id id :agent (format "%s" agent)
              :directory (file-name-as-directory (expand-file-name default-directory))
              :name (buffer-name buffer)
              :title (or (map-nested-elt agent-shell--state '(:session :title)) "")
              :updated (and updated (float-time updated))
              :unread (and agent-shell-sessions--unread t))))))

(defun agent-shell-sessions--remember ()
  "Remember the current shell, scheduling a write only if metadata changed."
  (agent-shell-sessions--ensure-loaded)
  (unless agent-shell-sessions--restore-record
    (when-let* ((record (agent-shell-sessions--record (current-buffer))))
      (let* ((key (agent-shell-sessions--record-key record))
             (old (seq-find (lambda (entry)
                              (equal key (agent-shell-sessions--record-key entry)))
                            agent-shell-sessions--records)))
        (unless (equal old record)
          (setq agent-shell-sessions--records
                (cons record (remove old agent-shell-sessions--records)))
          (unless (timerp agent-shell-sessions--save-timer)
            (setq agent-shell-sessions--save-timer
                  (run-at-time 2 nil #'agent-shell-sessions--save-safely))))))))

(defun agent-shell-sessions--write-workspace ()
  "Atomically write changed metadata with owner-only file permissions."
  (agent-shell-sessions--ensure-loaded)
  (when agent-shell-sessions--load-error
    (user-error "Workspace unreadable; fix %s and reload the library: %s"
                agent-shell-sessions-workspace-file agent-shell-sessions--load-error))
  (unless (equal agent-shell-sessions--records agent-shell-sessions--last-written)
    (let* ((file (expand-file-name agent-shell-sessions-workspace-file))
           (directory (file-name-directory file))
           temporary)
      (make-directory directory t)
      (unwind-protect
          (progn
            (setq temporary (make-temp-file (expand-file-name ".agent-sessions-" directory)))
            (with-temp-file temporary
              (insert (json-serialize
                       (list :version 1 :sessions (vconcat agent-shell-sessions--records))
                       :null-object nil :false-object :false))
              (insert "\n"))
            (set-file-modes temporary #o600)
            (rename-file temporary file t)
            (setq agent-shell-sessions--last-written
                  (copy-tree agent-shell-sessions--records)))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun agent-shell-sessions-save ()
  "Save all live and previously remembered sessions, without agent requests."
  (interactive)
  (agent-shell-sessions--ensure-loaded)
  (dolist (buffer (agent-shell-buffers))
    (with-current-buffer buffer (agent-shell-sessions--remember)))
  (when (timerp agent-shell-sessions--save-timer)
    (cancel-timer agent-shell-sessions--save-timer))
  (setq agent-shell-sessions--save-timer nil)
  (agent-shell-sessions--write-workspace)
  (when (called-interactively-p 'interactive)
    (message "Saved %d sessions" (length agent-shell-sessions--records))))

(defun agent-shell-sessions--save-safely ()
  "Save automatically, reporting failures without blocking Emacs shutdown."
  (condition-case err
      (agent-shell-sessions-save)
    (error (message "Agent workspace save failed: %s" (error-message-string err)))))

(defun agent-shell-sessions--finish-restore ()
  "Check a restored shell's ID and recover its saved unread state."
  (when-let* ((record agent-shell-sessions--restore-record)
              (id (agent-shell-session-id)))
    (let ((key (agent-shell-sessions--record-key record)))
      (if (equal id (plist-get record :id))
          (setq agent-shell-sessions--unread
                (and (plist-get record :unread)
                     (not (eq (current-buffer) (agent-shell-sessions--selected-shell)))))
        (setf (alist-get key agent-shell-sessions--restore-errors nil nil #'equal)
              "Agent started a new conversation; original saved session retained")
        (message "Could not restore %s: agent started a different session"
                 (plist-get record :name)))
      (setq agent-shell-sessions--restore-record nil))))

;;; Session events and unread completions

(defun agent-shell-sessions--selected-shell ()
  "Return the shell selected in the focused frame, including viewports.
An unselected window or an unfocused Emacs frame does not count as read."
  (unless (or (active-minibuffer-window)
              (eq (frame-focus-state (selected-frame)) nil))
    (with-current-buffer (window-buffer (selected-window))
      (cond
       ((derived-mode-p 'agent-shell-mode) (current-buffer))
       ((derived-mode-p 'agent-shell-viewport-view-mode
                        'agent-shell-viewport-edit-mode)
        (agent-shell-shell-buffer :viewport-buffer (current-buffer)
                                  :no-create t :no-error t))))))

(defun agent-shell-sessions--mark-selected-read ()
  "Mark the selected session's completion as read."
  (when-let* ((buffer (agent-shell-sessions--selected-shell))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when agent-shell-sessions--unread
        (setq agent-shell-sessions--unread nil)
        (agent-shell-sessions--remember)))))

(defun agent-shell-sessions--on-event (event)
  "Track EVENT in the emitting shell without redrawing the dashboard."
  (pcase (map-elt event :event)
    ((or 'input-submitted 'init-started)
     (setq agent-shell-sessions--unread nil))
    ((or 'init-finished 'session-restored)
     (agent-shell-sessions--finish-restore))
    ('error
     (when agent-shell-sessions--restore-record
       (setf (alist-get (agent-shell-sessions--record-key
                         agent-shell-sessions--restore-record)
                        agent-shell-sessions--restore-errors nil nil #'equal)
             (format "%s" (or (map-nested-elt event '(:data :message))
                              "Agent reported an error while restoring")))
       (setq agent-shell-sessions--restore-record nil)))
    ('turn-complete
     ;; Cancellations and interrupted turns must not look like successful work.
     (setq agent-shell-sessions--unread
           (and (equal (map-nested-elt event '(:data :stop-reason)) "end_turn")
                (not (eq (current-buffer)
                         (agent-shell-sessions--selected-shell)))))))
  ;; Never clear unread on cleanup: it must survive closing the buffer/Emacs.
  (when (memq (map-elt event :event)
              '(init-finished session-restored session-title-changed
                              input-submitted turn-complete clean-up))
    (agent-shell-sessions--remember)))

(defun agent-shell-sessions--track ()
  "Subscribe once to the current shell's events, including after a restart.
Existing idle sessions start as read; past completions cannot be inferred."
  (unless (seq-some
           (lambda (subscription)
             (equal (map-elt subscription :token)
                    agent-shell-sessions--subscription))
           (map-elt agent-shell--state :event-subscriptions))
    (setq agent-shell-sessions--restore-record agent-shell-sessions--restoring-record
          agent-shell-sessions--unread nil
          agent-shell-sessions--subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :on-event #'agent-shell-sessions--on-event))
    (when agent-shell-sessions--restore-record
      ;; Show a useful response when resuming an unread session.
      (setq-local agent-shell-session-restore-verbosity 'last)
      (let ((directory (plist-get agent-shell-sessions--restore-record :directory)))
        (setq-local agent-shell-cwd-function (lambda () directory))))
    (agent-shell-sessions--remember)))

;;; Status and table entries

(defun agent-shell-sessions--text (value)
  "Make VALUE a plain, single-line table cell."
  (replace-regexp-in-string
   "[[:space:][:cntrl:]]+" " "
   (string-trim (substring-no-properties (format "%s" (or value ""))))))

(defun agent-shell-sessions--tool-label (tool)
  "Summarize TOOL without displaying command bodies or multiline payloads."
  (let ((title (map-elt tool :title))
        (kind (map-elt tool :kind)))
    (cond
     ((or (equal kind "execute") (map-elt tool :command)) "Running command")
     ((and (stringp title) (not (string-empty-p (string-trim title)))
           (<= (length title) 80) (not (string-match-p "[\n\r]" title)))
      (agent-shell-sessions--text title))
     (t (or (cdr (assoc kind '(("read" . "Reading files")
                               ("edit" . "Editing files")
                               ("search" . "Searching")
                               ("fetch" . "Fetching data")
                               ("delete" . "Deleting files")
                               ("move" . "Moving files")
                               ("think" . "Thinking"))))
            "Running tool")))))

(defun agent-shell-sessions--activity-text (value)
  "Bound VALUE's activity text and tooltip to a single short line.
Limit the input before normalization, since a task title can be a full prompt."
  (let* ((text (format "%s" (or value "")))
         (prefix (substring text 0 (min (length text) 256))))
    (truncate-string-to-width
     (concat (agent-shell-sessions--text prefix)
             (if (> (length text) 256) "…" ""))
     80 nil nil "…")))

(defun agent-shell-sessions--snapshot (buffer)
  "Read BUFFER's status and activity without changing its state."
  (with-current-buffer buffer
    (let* ((state agent-shell--state)
           (client (map-elt state :client))
           (process (map-elt client :process))
           (status (cond
                    ((and process (not (process-live-p process))) 'stopped)
                    ((and (map-elt state :initialized) (not process)) 'stopped)
                    ((not (agent-shell-session-id)) 'starting)
                    (t (let ((status (agent-shell-status)))
                         (if (and (eq status 'ready) agent-shell-sessions--unread)
                             'unread
                           status)))))
           (tools (map-elt state :tool-calls))
           (pending (seq-find
                     (lambda (entry)
                       (map-elt (cdr entry) :permission-request-id))
                     tools))
           (active (seq-filter
                    (lambda (entry)
                      (member (map-elt (cdr entry) :status)
                              '("pending" "in_progress")))
                    tools))
           (title (map-nested-elt state '(:session :title)))
           (activity
            (pcase status
              ('blocked (format "Permission: %s"
                                (if pending
                                    (agent-shell-sessions--tool-label (cdr pending))
                                  "open session to respond")))
              ('busy
               (if active
                   (concat (agent-shell-sessions--tool-label (cdar active))
                           (if (cdr active)
                               (format " (+%d more)" (1- (length active)))
                             ""))
                 (pcase (map-elt state :last-entry-type)
                   ("agent_message_chunk" "Writing response")
                   ("agent_thought_chunk" "Thinking")
                   (_ "Processing request"))))
              ('starting "Connecting / initializing session")
              ('stopped "Agent process is not running")
              ((or 'ready 'unread)
               (if (and (stringp title) (not (string-empty-p title)))
                   title
                 (if (eq status 'unread)
                     "Response finished — visit session to read"
                   "Ready for your next prompt"))))))
      (list :status status :activity activity
            :directory default-directory
            :updated (agent-shell-last-activity-time)))))

(defun agent-shell-sessions--age (timestamp)
  "Format time elapsed since TIMESTAMP."
  (if (not timestamp)
      "-"
    (let ((seconds (max 0 (floor (float-time (time-subtract nil timestamp))))))
      (cond ((< seconds 60) (format "%ds" seconds))
            ((< seconds 3600) (format "%dm" (/ seconds 60)))
            ((< seconds 86400) (format "%dh" (/ seconds 3600)))
            (t (format "%dd" (/ seconds 86400)))))))

(defun agent-shell-sessions--entry (buffer)
  "Build a table row for BUFFER, retaining unreadable sessions as Unknown."
  (let ((snapshot
         (condition-case err
             (agent-shell-sessions--snapshot buffer)
           (error (list :status 'unknown
                        :directory (buffer-local-value 'default-directory buffer)
                        :activity (error-message-string err))))))
    (agent-shell-sessions--row buffer (buffer-name buffer) snapshot)))

(defun agent-shell-sessions--row (id name snapshot)
  "Format a live or saved session ID and NAME using SNAPSHOT."
  (let* ((status (assq (plist-get snapshot :status) agent-shell-sessions--statuses))
         (directory (plist-get snapshot :directory))
         (updated (plist-get snapshot :updated))
         (activity (agent-shell-sessions--activity-text (plist-get snapshot :activity))))
    (list id
          (vector
           (propertize (nth 1 status) 'face (nth 2 status))
           (propertize (agent-shell-sessions--text name) 'help-echo name)
           (propertize (agent-shell-sessions--text
                        (file-name-nondirectory (directory-file-name directory)))
                       'help-echo directory)
           (propertize (agent-shell-sessions--age updated)
                       'agent-shell-sessions-time (if updated (float-time updated) 0)
                       'help-echo (if updated
                                      (concat "Last prompt or agent notification: "
                                              (format-time-string "%F %T" updated))
                                    "No activity yet"))
           (propertize activity 'help-echo activity)))))

(defun agent-shell-sessions--live-for-record (record)
  "Find a live or restoring buffer for RECORD without starting an agent."
  (let ((key (agent-shell-sessions--record-key record)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (when-let* ((live (or agent-shell-sessions--restore-record
                               (agent-shell-sessions--record buffer))))
           (equal key (agent-shell-sessions--record-key live)))))
     (agent-shell-buffers))))

(defun agent-shell-sessions--saved-entry (record)
  "Build a dashboard row for dormant RECORD."
  (let* ((key (agent-shell-sessions--record-key record))
         (error (alist-get key agent-shell-sessions--restore-errors nil nil #'equal)))
    (agent-shell-sessions--row
     key (plist-get record :name)
     (list :status (cond (error 'restore-error)
                         ((plist-get record :unread) 'saved-unread)
                         (t 'saved))
           :directory (plist-get record :directory)
           :updated (plist-get record :updated)
           :activity (or error (concat "RET: restore — " (plist-get record :title)))))))

;;; Workspace commands

(defun agent-shell-sessions--restore (record)
  "Start RECORD's saved session in the background or return its live buffer.
The installed internal start API supplies no-focus, unlike the public API.
Use the current agent config; never deserialize executables or credentials."
  (or (agent-shell-sessions--live-for-record record)
      (let* ((default-directory (plist-get record :directory))
             (config (seq-find
                      (lambda (entry)
                        (equal (format "%s" (map-elt entry :identifier))
                               (plist-get record :agent)))
                      (agent-shell--resolved-agent-configs))))
        (unless (file-directory-p default-directory)
          (user-error "Project directory no longer exists: %s" default-directory))
        (unless config
          (user-error "Agent configuration unavailable: %s" (plist-get record :agent)))
        (setf (alist-get (agent-shell-sessions--record-key record)
                         agent-shell-sessions--restore-errors nil t #'equal) nil)
        (let* ((agent-shell-sessions--restoring-record record)
               (agent-shell-cwd-function (lambda () default-directory))
               (buffer (agent-shell--start :config config
                                           :session-id (plist-get record :id)
                                           :new-session t :no-focus t)))
          (with-current-buffer buffer
            (rename-buffer (plist-get record :name) t)
            ;; Also handles a backend that completes initialization synchronously.
            (agent-shell-sessions--finish-restore)
            (agent-shell-sessions--remember))
          buffer))))

(defun agent-shell-sessions--saved-at-point ()
  "Return the saved record at point, or signal a user error."
  (agent-shell-sessions--ensure-loaded)
  (or (seq-find (lambda (record)
                  (equal (tabulated-list-get-id)
                         (agent-shell-sessions--record-key record)))
                agent-shell-sessions--records)
      (user-error "Select a saved session")))

(defun agent-shell-sessions-restore ()
  "Restore the saved session at point in the background."
  (interactive)
  (let* ((record (agent-shell-sessions--saved-at-point))
         (key (agent-shell-sessions--record-key record)))
    (condition-case err
        (prog1 (agent-shell-sessions--restore record)
          (agent-shell-sessions-refresh))
      (error
       (setf (alist-get key agent-shell-sessions--restore-errors nil nil #'equal)
             (error-message-string err))
       (agent-shell-sessions-refresh)
       (user-error "%s" (error-message-string err))))))

(defun agent-shell-sessions-restore-all ()
  "Start all saved sessions not already open, leaving focus in the dashboard."
  (interactive)
  (agent-shell-sessions--ensure-loaded)
  (let ((started 0) (open 0) (failed 0))
    (dolist (record (copy-sequence agent-shell-sessions--records))
      (if (agent-shell-sessions--live-for-record record)
          (cl-incf open)
        (condition-case err
            (progn (agent-shell-sessions--restore record) (cl-incf started))
          (error
           (cl-incf failed)
           (setf (alist-get (agent-shell-sessions--record-key record)
                            agent-shell-sessions--restore-errors nil nil #'equal)
                 (error-message-string err))))))
    (agent-shell-sessions-refresh)
    (message "Starting %d sessions; %d already open; %d could not start"
             started open failed)))

(defun agent-shell-sessions-forget ()
  "Forget the saved row at point, leaving the agent's conversation untouched."
  (interactive)
  (let ((record (agent-shell-sessions--saved-at-point)))
    (let ((previous agent-shell-sessions--records))
      (setq agent-shell-sessions--records (remove record previous))
      (condition-case err
          (agent-shell-sessions--write-workspace)
        (error
         (setq agent-shell-sessions--records previous)
         (signal (car err) (cdr err)))))
    (agent-shell-sessions-refresh)
    (message "Forgot workspace entry; the agent's stored conversation is unchanged")))

;;; Dashboard refresh and navigation

(defun agent-shell-sessions--status-less-p (a b)
  "Sort entries A and B by attention priority, then session name."
  (let* ((labels (mapcar #'cadr agent-shell-sessions--statuses))
         (a-rank (seq-position labels (aref (cadr a) 0) #'equal))
         (b-rank (seq-position labels (aref (cadr b) 0) #'equal)))
    (if (= a-rank b-rank)
        (string-lessp (aref (cadr a) 1) (aref (cadr b) 1))
      (< a-rank b-rank))))

(defun agent-shell-sessions--updated-less-p (a b)
  "Sort entries A and B with latest activity first."
  (> (get-text-property 0 'agent-shell-sessions-time (aref (cadr a) 3))
     (get-text-property 0 'agent-shell-sessions-time (aref (cadr b) 3))))

(defun agent-shell-sessions-refresh (&rest _)
  "Refresh the session list, preserving the selected session and sorting."
  (interactive)
  (agent-shell-sessions--ensure-loaded)
  (agent-shell-sessions--fit-columns)
  (setq tabulated-list-entries
        (append
         (mapcar #'agent-shell-sessions--entry
                 (seq-filter #'buffer-live-p (agent-shell-buffers)))
         (mapcar #'agent-shell-sessions--saved-entry
                 (seq-remove #'agent-shell-sessions--live-for-record
                             agent-shell-sessions--records))))
  (setq agent-shell-sessions--summary
        (if (null tabulated-list-entries)
            "No sessions — N: new session"
          (mapconcat
           (lambda (status)
             (format "%d %s"
                     (seq-count (lambda (entry)
                                  (equal (aref (cadr entry) 0) (cadr status)))
                                tabulated-list-entries)
                     (downcase (cadr status))))
           (seq-filter
            (lambda (status)
              (seq-some (lambda (entry)
                          (equal (aref (cadr entry) 0) (cadr status)))
                        tabulated-list-entries))
            agent-shell-sessions--statuses)
           " · ")))
  (tabulated-list-print t)
  (force-mode-line-update))

(defun agent-shell-sessions--tick (buffer)
  "Refresh BUFFER only when visible, without selecting its window."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-sessions-mode)
        (agent-shell-sessions-refresh)))))

(defun agent-shell-sessions--stop-timer ()
  "Cancel this dashboard's refresh timer."
  (when (timerp agent-shell-sessions--timer)
    (cancel-timer agent-shell-sessions--timer))
  (setq agent-shell-sessions--timer nil))

(defun agent-shell-sessions--restart-timer ()
  "Apply the current refresh interval without leaving an old timer running."
  (agent-shell-sessions--stop-timer)
  (when agent-shell-sessions-refresh-interval
    (setq agent-shell-sessions--timer
          (run-at-time (max 1 agent-shell-sessions-refresh-interval)
                       (max 1 agent-shell-sessions-refresh-interval)
                       #'agent-shell-sessions--tick (current-buffer))))
  (force-mode-line-update))

(defun agent-shell-sessions--refresh-description ()
  "Describe the dashboard's running timer, including older loaded settings."
  (if (timerp agent-shell-sessions--timer)
      (format "Auto-refresh: %ss" (timer--repeat-delay agent-shell-sessions--timer))
    "Manual refresh"))

(defun agent-shell-sessions-set-refresh-interval (seconds)
  "Refresh dashboards every SECONDS, or only manually when zero.
Applies immediately in all dashboard buffers for this Emacs session."
  (interactive
   (list (read-number "Refresh interval in seconds (0 = manual): "
                      (or agent-shell-sessions-refresh-interval 0))))
  (unless (and (integerp seconds) (>= seconds 0))
    (user-error "Use a whole number of seconds, zero or greater"))
  (setq agent-shell-sessions-refresh-interval (unless (zerop seconds) seconds))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-sessions-mode)
        (agent-shell-sessions--restart-timer)))))

(defun agent-shell-sessions--selected-buffer ()
  "Return the session at point, restoring it if it is a saved row."
  (let ((buffer (tabulated-list-get-id)))
    (cond ((and (bufferp buffer) (buffer-live-p buffer)) buffer)
          ((consp buffer) (agent-shell-sessions-restore))
          (t (user-error "No session on this row; press g to refresh")))))

(defun agent-shell-sessions-visit ()
  "Visit the session at point in this window."
  (interactive)
  (switch-to-buffer (agent-shell-sessions--selected-buffer)))

(defun agent-shell-sessions-visit-other-window ()
  "Visit the session at point in another window."
  (interactive)
  (pop-to-buffer (agent-shell-sessions--selected-buffer)
                 '(display-buffer-pop-up-window (inhibit-same-window . t))))

(defun agent-shell-sessions-new (directory)
  "Start a new agent shell in DIRECTORY with no dashboard text in its input."
  (interactive
   (list (read-directory-name
          "New agent session in: "
          (if-let* ((buffer (tabulated-list-get-id))
                    ((bufferp buffer))
                    ((buffer-live-p buffer)))
              (buffer-local-value 'default-directory buffer)
            default-directory))))
  (let ((default-directory (file-name-as-directory directory))
        ;; The upstream command otherwise copies the current row or region.
        (agent-shell-context-sources nil))
    (call-interactively #'agent-shell-new-shell)))

(defun agent-shell-sessions--row-selected-p ()
  "Whether a live or saved session is selected in the dashboard."
  (let ((id (tabulated-list-get-id)))
    (or (and (bufferp id) (buffer-live-p id)) (consp id))))

(defun agent-shell-sessions--saved-row-selected-p ()
  "Whether the selected dashboard row represents a saved session."
  (consp (tabulated-list-get-id)))

;;; Dashboard interface

(transient-define-prefix agent-shell-sessions-menu ()
  "Show session and workspace actions for the selected dashboard row."
  [["Session"
    ("RET" "Visit / restore" agent-shell-sessions-visit
     :inapt-if-not agent-shell-sessions--row-selected-p)
    ("o" "Open other window" agent-shell-sessions-visit-other-window
     :inapt-if-not agent-shell-sessions--row-selected-p)
    ("N" "New session" agent-shell-sessions-new)]
   ["Workspace"
    ("s" "Save now" agent-shell-sessions-save :transient t)
    ("r" "Restore selected" agent-shell-sessions-restore
     :inapt-if-not agent-shell-sessions--saved-row-selected-p)
    ("R" "Restore all" agent-shell-sessions-restore-all)
    ("d" "Forget saved entry" agent-shell-sessions-forget
     :inapt-if-not agent-shell-sessions--saved-row-selected-p)]
   ["View"
    ("g" "Refresh now" agent-shell-sessions-refresh :transient t)
    ("i" "Refresh interval" agent-shell-sessions-set-refresh-interval
     :description agent-shell-sessions--refresh-description :transient t)
    ("h" "Mode help" describe-mode)
    ("q" "Close menu" transient-quit-one)]]
  (interactive)
  (unless (derived-mode-p 'agent-shell-sessions-mode)
    (agent-shell-sessions))
  (transient-setup 'agent-shell-sessions-menu))

(defvar agent-shell-sessions-mode-map (make-sparse-keymap)
  "Keymap for `agent-shell-sessions-mode'.")
;; Update the existing map too, so loading a newer version adds its commands.
(let ((map agent-shell-sessions-mode-map))
  (set-keymap-parent map tabulated-list-mode-map)
  (define-key map (kbd "RET") #'agent-shell-sessions-visit)
  (define-key map (kbd "o") #'agent-shell-sessions-visit-other-window)
  (define-key map (kbd "N") #'agent-shell-sessions-new)
  (define-key map (kbd "r") #'agent-shell-sessions-restore)
  (define-key map (kbd "R") #'agent-shell-sessions-restore-all)
  (define-key map (kbd "s") #'agent-shell-sessions-save)
  (define-key map (kbd "d") #'agent-shell-sessions-forget)
  (define-key map (kbd "g") #'agent-shell-sessions-refresh)
  (define-key map (kbd "i") #'agent-shell-sessions-set-refresh-interval)
  (define-key map (kbd "n") #'next-line)
  (define-key map (kbd "p") #'previous-line)
  (define-key map (kbd "?") #'agent-shell-sessions-menu)
  map)

(defun agent-shell-sessions--fit-columns ()
  "Leave room for activity even in a normal-width window."
  (let* ((width (window-body-width (or (get-buffer-window (current-buffer) t)
                                       (selected-window))))
         (format (vector
                  '("Status" 14 agent-shell-sessions--status-less-p)
                  (list "Session" (max 18 (min 32 (/ width 4))) t)
                  (list "Project" (max 10 (min 20 (/ width 6))) t)
                  '("Last activity" 13 agent-shell-sessions--updated-less-p)
                  '("Activity / Task" 0 t))))
    (unless (equal tabulated-list-format format)
      (setq tabulated-list-format format)
      (tabulated-list-init-header))))

(defun agent-shell-sessions--no-wrap ()
  "Keep each session on one screen line, including with global visual lines."
  (when visual-line-mode
    (visual-line-mode -1))
  ;; Disabling visual-line-mode can restore a saved nil value, so set this last.
  (setq-local truncate-lines t))

(define-derived-mode agent-shell-sessions-mode tabulated-list-mode "Agent Sessions"
  "List live and saved agent sessions, including other projects.

Needs input means a permission response is pending.  Ready means the
agent can accept another prompt, including a reply to a question.
Done · unread means a turn finished successfully while you were elsewhere.
Select the session (or its viewport) to clear that mark.  Tracking starts
when this library is loaded and continues while the dashboard is closed.
Saved sessions survive Emacs exit.  Restore them explicitly with RET or r,
or restore all with R.  Saved · unread preserves an unread completion.
Session metadata is saved automatically; d forgets a saved entry without
deleting the provider's conversation.  Running requests cannot survive exit.
Activity shows active tools or the current session title when ready.
Project is the session's working directory; hover for its full path.
Last activity is time since the last prompt or incoming agent notification,
not time since dashboard refresh or completion.  Ages update on refresh.

\\<agent-shell-sessions-mode-map>
\\[agent-shell-sessions-menu]: command menu (Transient)
\\[agent-shell-sessions-visit]: visit session
\\[agent-shell-sessions-visit-other-window]: visit in another window
\\[agent-shell-sessions-new]: new session (prompts for directory)
\\[agent-shell-sessions-restore]: restore saved session in background
\\[agent-shell-sessions-restore-all]: restore all saved sessions
\\[agent-shell-sessions-save]: save workspace now
\\[agent-shell-sessions-forget]: forget saved entry
\\[agent-shell-sessions-refresh]: refresh
\\[agent-shell-sessions-set-refresh-interval]: change refresh interval immediately
\\[tabulated-list-sort]: sort by column at point (or click a heading)
\\[quit-window]: close dashboard

Refreshes automatically every second while visible by default.
Use `g' for an immediate refresh.  Configure the interval with
`agent-shell-sessions-refresh-interval'.
Refreshing sends no agent requests; restoring starts agent processes."
  (setq tabulated-list-padding 1
        tabulated-list-sort-key '("Status" . nil))
  (setq-local display-line-numbers nil)
  ;; Also cover M-x agent-shell/new-shell invoked directly from this buffer.
  (setq-local agent-shell-context-sources nil)
  (setq-local mode-line-format
              '(" " mode-line-buffer-identification "   "
                (:eval (agent-shell-sessions--refresh-description))
                "   "
                agent-shell-sessions--summary
                "   RET: visit/restore  R: restore all  g: refresh  ?: menu"))
  (add-hook 'visual-line-mode-hook #'agent-shell-sessions--no-wrap nil t)
  (agent-shell-sessions--no-wrap)
  (add-hook 'tabulated-list-revert-hook #'agent-shell-sessions-refresh nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-sessions--stop-timer nil t)
  (add-hook 'change-major-mode-hook #'agent-shell-sessions--stop-timer nil t)
  (agent-shell-sessions--fit-columns)
  (agent-shell-sessions--restart-timer))

;;;###autoload
(defun agent-shell-sessions ()
  "Show all live agent shell sessions in a sortable dashboard."
  (interactive)
  (let ((directory default-directory)
        (buffer (get-buffer-create agent-shell-sessions--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-sessions-mode)
        (agent-shell-sessions-mode))
      (setq default-directory directory)
      (agent-shell-sessions-refresh))
    (pop-to-buffer buffer)))

;;; Initialization

;; Capture even short turns and turns completed while the dashboard is closed.
;; The command hook only checks the selected buffer; it never scans transcripts.
(add-hook 'agent-shell-mode-hook #'agent-shell-sessions--track)
(add-hook 'post-command-hook #'agent-shell-sessions--mark-selected-read)
(add-hook 'kill-emacs-hook #'agent-shell-sessions--save-safely)
;; Allow recovery after the workspace file has been repaired and this is reloaded.
(when agent-shell-sessions--load-error
  (setq agent-shell-sessions--loaded-file nil))
(agent-shell-sessions--ensure-loaded)
(dolist (buffer (agent-shell-buffers))
  (with-current-buffer buffer
    (agent-shell-sessions--track)))

(provide 'agent-shell-sessions)
;;; agent-shell-sessions.el ends here
