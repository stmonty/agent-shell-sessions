;;; agent-shell-sessions-tests.el --- Dashboard tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Monty Bichouna
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run `make test' from the repository root.  Agent startup is mocked and
;; all workspace files live in temporary directories.

;;; Code:

(require 'ert)

;; Keep both the interactive fixtures and the exit hook away from real data.
(defvar agent-shell-sessions-test--directory (make-temp-file "agent-sessions-tests-" t))
(defvar agent-shell-sessions-workspace-file)
(setq agent-shell-sessions-workspace-file
      (expand-file-name "workspace.json" agent-shell-sessions-test--directory))
(require 'agent-shell-sessions)
(add-hook 'kill-emacs-hook
          (lambda () (delete-directory agent-shell-sessions-test--directory t)) t)

(defmacro agent-shell-sessions-test--workspace (&rest body)
  "Run BODY with isolated workspace storage and bookkeeping."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "agent-workspace-" t))
          ;; Mocking file primitives must not invoke the local native toolchain.
          (native-comp-enable-subr-trampolines nil)
          (agent-shell-sessions-workspace-file (expand-file-name "workspace.json" directory))
          (agent-shell-sessions--records nil)
          (agent-shell-sessions--loaded-file nil)
          (agent-shell-sessions--last-written nil)
          (agent-shell-sessions--load-error nil)
          (agent-shell-sessions--restore-errors nil)
          (agent-shell-sessions--save-timer nil))
     (unwind-protect (progn (agent-shell-sessions--ensure-loaded) ,@body)
       (when (timerp agent-shell-sessions--save-timer)
         (cancel-timer agent-shell-sessions--save-timer))
       (delete-directory directory t))))

(defun agent-shell-sessions-test--record (&optional id)
  "Return example workspace metadata with session ID."
  (list :id (or id "saved-session") :agent "fixture" :directory temporary-file-directory
        :name "Saved agent" :title "A saved task" :updated 1234.0 :unread t))

(defmacro agent-shell-sessions-test--with-shell (&rest body)
  "Run BODY with a local shell fixture and an inert, live pipe process."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (setq-local shell-maker--config (make-shell-maker-config :name "agent"))
     (let ((process (make-pipe-process :name "dashboard-test" :noquery t)))
       (unwind-protect
           (progn
             (setq-local agent-shell--state
                         (agent-shell--make-state :buffer (current-buffer)))
             (map-put! agent-shell--state :initialized t)
             (map-put! agent-shell--state :client (list (cons :process process)))
             (map-put! agent-shell--state :session '((:id . "test-session")))
             ,@body)
         (delete-process process)))))

(ert-deftest agent-shell-sessions-status-transitions ()
             (agent-shell-sessions-test--with-shell
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'ready))
              (setq-local shell-maker--busy t)
              (map-put! agent-shell--state :tool-calls
                        '(("tool" . ((:title . "Read init.el") (:status . "in_progress")))))
              (let ((snapshot (agent-shell-sessions--snapshot (current-buffer))))
                (should (eq (plist-get snapshot :status) 'busy))
                (should (equal (plist-get snapshot :activity) "Read init.el")))
              (map-put! agent-shell--state :tool-calls
                        '(("tool" . ((:title . "Read init.el") (:status . "in_progress")
                                     (:permission-request-id . 42)))))
              (let ((snapshot (agent-shell-sessions--snapshot (current-buffer))))
                (should (eq (plist-get snapshot :status) 'blocked))
                (should (equal (plist-get snapshot :activity) "Permission: Read init.el")))
              (setq shell-maker--busy nil)
              ;; Old tool entries must not make an idle session look busy or blocked.
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'ready))
              (delete-process process)
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'stopped))))

(ert-deftest agent-shell-sessions-initialization-and-missing-process ()
             (agent-shell-sessions-test--with-shell
              (map-put! agent-shell--state :session nil)
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'starting))
              (map-put! agent-shell--state :client nil)
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'stopped))
              (map-put! agent-shell--state :initialized nil)
              (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                     :status) 'starting))))

(ert-deftest agent-shell-sessions-streaming-and-task-title ()
             (agent-shell-sessions-test--with-shell
              (setq-local shell-maker--busy t)
              (map-put! agent-shell--state :last-entry-type "agent_message_chunk")
              (should (equal (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                        :activity) "Writing response"))
              (setq shell-maker--busy nil)
              (map-put! agent-shell--state :session
                        '((:id . "test-session")
                          (:title . "Fix line numbers\nand build dashboard")))
              (let ((row (agent-shell-sessions--entry (current-buffer))))
                (should (equal (aref (cadr row) 4)
                               "Fix line numbers and build dashboard")))))

(ert-deftest agent-shell-sessions-unreadable-state-stays-visible ()
             (agent-shell-sessions-test--with-shell
              (setq shell-maker--config nil)
              (let ((row (agent-shell-sessions--entry (current-buffer))))
                (should (eq (car row) (current-buffer)))
                (should (equal (aref (cadr row) 0) "Unknown"))
                (should (string-match-p "No shell-maker config" (aref (cadr row) 4))))))

(ert-deftest agent-shell-sessions-sort-and-refresh-preserve-selection ()
             (let ((one (generate-new-buffer "dashboard-ready"))
                   (two (generate-new-buffer "dashboard-blocked")))
               (unwind-protect
                   (with-temp-buffer
                     (agent-shell-sessions-mode)
                     (cl-letf (((symbol-function 'agent-shell-buffers)
                                (lambda () (list one two)))
                               ((symbol-function 'agent-shell-sessions--snapshot)
                                (lambda (buffer)
                                  (list :status (if (eq buffer one) 'ready 'blocked)
                                        :directory "/tmp/project/" :activity "Test"))))
                       (agent-shell-sessions-refresh)
                       (goto-char (point-min))
                       (should (eq (tabulated-list-get-id) two))
                       (forward-line 1)
                       (should (eq (tabulated-list-get-id) one))
                       (agent-shell-sessions-refresh)
                       (should (eq (tabulated-list-get-id) one))
                       (should (string-match-p "1 needs input" agent-shell-sessions--summary))
                       ;; A killed row disappears without breaking refresh or selection.
                       (kill-buffer two)
                       (agent-shell-sessions-refresh)
                       (should (= (length tabulated-list-entries) 1))
                       (should (eq (tabulated-list-get-id) one))))
                 (when (buffer-live-p one) (kill-buffer one))
                 (when (buffer-live-p two) (kill-buffer two)))))

(ert-deftest agent-shell-sessions-hidden-dashboard-does-not-refresh ()
             (with-temp-buffer
               (agent-shell-sessions-mode)
               (cl-letf (((symbol-function 'agent-shell-buffers)
                          (lambda () (ert-fail "Hidden dashboard polled sessions"))))
                 (agent-shell-sessions--tick (current-buffer)))))

(ert-deftest agent-shell-sessions-empty-list-and-timer-cleanup ()
             (let ((buffer (generate-new-buffer "dashboard-timer"))
                   (agent-shell-sessions-refresh-interval 1)
                   timer)
               (unwind-protect
                   (with-current-buffer buffer
                     (agent-shell-sessions-mode)
                     (setq timer agent-shell-sessions--timer)
                     (should (memq timer timer-list))
                     (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
                       (agent-shell-sessions-refresh)
                       (should (string-match-p "No sessions" agent-shell-sessions--summary))
                       (should-error (agent-shell-sessions--selected-buffer) :type 'user-error))
                     (fundamental-mode)
                     (should-not (memq timer timer-list))
                     (agent-shell-sessions-mode)
                     (setq timer agent-shell-sessions--timer))
                 (kill-buffer buffer))
               (should-not (memq timer timer-list))))

(ert-deftest agent-shell-sessions-updated-sort-is-numeric ()
             (let ((old (list nil (vector "" "" ""
                                          (propertize "2h" 'agent-shell-sessions-time 10))))
                   (new (list nil (vector "" "" ""
                                          (propertize "10m" 'agent-shell-sessions-time 20)))))
               (should (agent-shell-sessions--updated-less-p new old))
               (should-not (agent-shell-sessions--updated-less-p old new))))

(ert-deftest agent-shell-sessions-fast-turn-recorded-without-dashboard ()
             (agent-shell-sessions-test--with-shell
              (let ((agent-shell-inhibit-system-sleep nil))
                (agent-shell-sessions--track)
                (agent-shell-sessions--track)
                (should (= (length (map-elt agent-shell--state :event-subscriptions)) 1))
                ;; Neither a dashboard nor a refresh occurs between these events.
                (agent-shell--emit-event :event 'input-submitted)
                (agent-shell--emit-event :event 'turn-complete
                                         :data '((:stop-reason . "end_turn")))
                (should (eq (plist-get (agent-shell-sessions--snapshot (current-buffer))
                                       :status) 'unread))
                ;; Refreshing or reloading subscriptions must not acknowledge the result.
                (agent-shell-sessions--entry (current-buffer))
                (agent-shell-sessions--track)
                (should agent-shell-sessions--unread)
                (agent-shell--emit-event :event 'input-submitted)
                (should-not agent-shell-sessions--unread)
                (agent-shell--emit-event :event 'turn-complete
                                         :data '((:stop-reason . "cancelled")))
                (should-not agent-shell-sessions--unread))))

(ert-deftest agent-shell-sessions-reading-and-background-window ()
             (save-window-excursion
               (agent-shell-sessions-test--with-shell
                (let ((shell (current-buffer))
                      (agent-shell-inhibit-system-sleep nil))
                  (agent-shell-sessions--track)
                  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t)))
                    ;; Watching the selected session at completion counts as read.
                    (switch-to-buffer shell)
                    (agent-shell--emit-event :event 'turn-complete
                                             :data '((:stop-reason . "end_turn")))
                    (should-not agent-shell-sessions--unread)
                    ;; Visible in a different pane is not the same as selected.
                    (let ((other (split-window-right)))
                      (set-window-buffer other (get-buffer-create "*scratch*"))
                      (select-window other)
                      (with-current-buffer shell
                        (agent-shell--emit-event :event 'turn-complete
                                                 :data '((:stop-reason . "end_turn")))
                        (should agent-shell-sessions--unread))
                      (agent-shell-sessions--mark-selected-read)
                      (should (buffer-local-value 'agent-shell-sessions--unread shell))
                      (switch-to-buffer shell)
                      (run-hooks 'post-command-hook)
                      (should-not agent-shell-sessions--unread)))))))

(ert-deftest agent-shell-sessions-unfocused-frame-does-not-acknowledge ()
             (save-window-excursion
               (agent-shell-sessions-test--with-shell
                (switch-to-buffer (current-buffer))
                (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) nil)))
                  (agent-shell-sessions--on-event
                   '((:event . turn-complete) (:data . ((:stop-reason . "end_turn")))))
                  (agent-shell-sessions--mark-selected-read)
                  (should agent-shell-sessions--unread)))))

(ert-deftest agent-shell-sessions-viewport-acknowledges-shell ()
             (save-window-excursion
               (agent-shell-sessions-test--with-shell
                (let ((shell (current-buffer)))
                  (setq agent-shell-sessions--unread t)
                  (with-temp-buffer
                    (setq major-mode 'agent-shell-viewport-view-mode)
                    (switch-to-buffer (current-buffer))
                    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                              ((symbol-function 'agent-shell-shell-buffer)
                               (lambda (&rest _) shell)))
                      (agent-shell-sessions--mark-selected-read)))
                  (should-not agent-shell-sessions--unread)))))

(ert-deftest agent-shell-sessions-three-completions-out-of-ten ()
             (let (shells)
               (unwind-protect
                   (progn
                     (dotimes (i 10)
                       (let ((shell (generate-new-buffer (format "session-%d" i))))
                         (push shell shells)
                         (with-current-buffer shell
                           (when (< i 3)
                             (agent-shell-sessions--on-event '((:event . input-submitted)))
                             (agent-shell-sessions--on-event
                              '((:event . turn-complete)
                                (:data . ((:stop-reason . "end_turn")))))))))
                     (with-temp-buffer
                       (agent-shell-sessions-mode)
                       (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () shells))
                                 ((symbol-function 'agent-shell-sessions--snapshot)
                                  (lambda (buffer)
                                    (list :status (if (buffer-local-value
                                                       'agent-shell-sessions--unread buffer)
                                                      'unread 'ready)
                                          :directory "/tmp/project/" :activity "Task"))))
                         (agent-shell-sessions-refresh)
                         (goto-char (point-min))
                         (dotimes (_ 3)
                           (should (equal (aref (tabulated-list-get-entry) 0) "Done · unread"))
                           (forward-line 1))
                         (dotimes (_ 7)
                           (should (equal (aref (tabulated-list-get-entry) 0) "Ready"))
                           (forward-line 1))
                         (should (string-match-p "3 done · unread" agent-shell-sessions--summary))
                         (should (string-match-p "7 ready" agent-shell-sessions--summary)))))
                 (mapc #'kill-buffer shells))))

(ert-deftest agent-shell-sessions-no-wrap-with-global-visual-lines ()
             (let ((was-enabled global-visual-line-mode))
               (unwind-protect
                   (save-window-excursion
                     (global-visual-line-mode 1)
                     (with-temp-buffer
                       (switch-to-buffer (current-buffer))
                       (visual-line-mode 1)
                       (agent-shell-sessions-mode)
                       ;; Also cover later global or manual attempts to enable wrapping.
                       (visual-line-mode 1)
                       (should-not visual-line-mode)
                       (should truncate-lines)
                       (let ((inhibit-read-only t))
                         (insert (make-string 300 ?x) "\nnext session\n"))
                       (goto-char (point-min))
                       (vertical-motion 1)
                       (should (= (line-number-at-pos) 2))))
                 (global-visual-line-mode (if was-enabled 1 -1)))))

(ert-deftest agent-shell-sessions-workspace-roundtrip-and-cleanup ()
             (agent-shell-sessions-test--workspace
              (agent-shell-sessions-test--with-shell
               (map-put! agent-shell--state :agent-config '((:identifier . fixture)))
               (map-put! agent-shell--state :last-activity-time (seconds-to-time 1234))
               (setq agent-shell-sessions--unread t)
               (agent-shell-sessions--on-event '((:event . clean-up)))
               (agent-shell-sessions-save)
               (should (= (logand (file-modes agent-shell-sessions-workspace-file) #o777) #o600))
               (let ((records (copy-tree agent-shell-sessions--records)))
                 ;; Simulate fresh Emacs: discard all in-memory workspace state.
                 (setq agent-shell-sessions--records nil agent-shell-sessions--loaded-file nil)
                 (agent-shell-sessions--ensure-loaded)
                 (should (equal records agent-shell-sessions--records))
                 (should (plist-get (car agent-shell-sessions--records) :unread))
                 (should (equal (plist-get (car agent-shell-sessions--records) :id) "test-session"))
                 ;; Unchanged metadata must not cause another disk write.
                 (cl-letf (((symbol-function 'rename-file)
                            (lambda (&rest _) (ert-fail "Unchanged workspace was rewritten"))))
                   (agent-shell-sessions-save))))))

(ert-deftest agent-shell-sessions-invalid-workspace-is-preserved ()
             (agent-shell-sessions-test--workspace
              (with-temp-file agent-shell-sessions-workspace-file (insert "broken json"))
              (setq agent-shell-sessions--loaded-file nil)
              (cl-letf (((symbol-function 'display-warning) #'ignore))
                (agent-shell-sessions--ensure-loaded))
              (should agent-shell-sessions--load-error)
              (setq agent-shell-sessions--records (list (agent-shell-sessions-test--record)))
              (should-error (agent-shell-sessions--write-workspace) :type 'user-error)
              (should (equal (with-temp-buffer
                               (insert-file-contents agent-shell-sessions-workspace-file)
                               (buffer-string)) "broken json"))))

(ert-deftest agent-shell-sessions-atomic-write-failure-preserves-workspace ()
             (agent-shell-sessions-test--workspace
              (setq agent-shell-sessions--records (list (agent-shell-sessions-test--record)))
              (agent-shell-sessions--write-workspace)
              (let ((original (with-temp-buffer
                                (insert-file-contents agent-shell-sessions-workspace-file)
                                (buffer-string))))
                (push (agent-shell-sessions-test--record "second") agent-shell-sessions--records)
                (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (error "Disk failure"))))
                  (should-error (agent-shell-sessions--write-workspace)))
                (should (equal original (with-temp-buffer
                                          (insert-file-contents agent-shell-sessions-workspace-file)
                                          (buffer-string))))
                (should-not (directory-files directory nil "^\\.agent-sessions-")))))

(ert-deftest agent-shell-sessions-saved-rows-dont-start-agents ()
             (agent-shell-sessions-test--workspace
              (setq agent-shell-sessions--records (list (agent-shell-sessions-test--record)))
              (agent-shell-sessions--write-workspace)
              (with-temp-buffer
                (agent-shell-sessions-mode)
                (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
                          ((symbol-function 'agent-shell--start)
                           (lambda (&rest _) (ert-fail "Refresh started an agent"))))
                  (agent-shell-sessions-refresh)
                  (goto-char (point-min))
                  (should (equal (aref (tabulated-list-get-entry) 0) "Saved · unread"))
                  (agent-shell-sessions-forget)
                  (should-not agent-shell-sessions--records)
                  (setq agent-shell-sessions--loaded-file nil)
                  (agent-shell-sessions--ensure-loaded)
                  (should-not agent-shell-sessions--records)))))

(ert-deftest agent-shell-sessions-restore-preserves-identity-and-unread ()
             (agent-shell-sessions-test--workspace
              (let ((record (agent-shell-sessions-test--record "test-session"))
                    (config '((:identifier . fixture)))
                    calls)
                (setq agent-shell-sessions--records (list record))
                (agent-shell-sessions-test--with-shell
                 (let ((shell (current-buffer)))
                   (map-put! agent-shell--state :session nil)
                   (map-put! agent-shell--state :agent-config config)
                   (setq default-directory temporary-file-directory)
                   (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list shell)))
                             ((symbol-function 'agent-shell--resolved-agent-configs)
                              (lambda () (list config)))
                             ((symbol-function 'agent-shell--start)
                              (lambda (&rest args)
                                (setq calls args)
                                (should (equal default-directory (plist-get record :directory)))
                                (should (equal (agent-shell-cwd) (plist-get record :directory)))
                                (with-current-buffer shell (agent-shell-sessions--track))
                                shell)))
                     (should (eq (agent-shell-sessions--restore record) shell))
                     (should (equal (plist-get calls :session-id) "test-session"))
                     (should (eq (plist-get calls :config) config))
                     (should (plist-get calls :new-session))
                     (should (plist-get calls :no-focus))
                     ;; Repeated restore while initializing reuses the pending buffer.
                     (setq calls nil)
                     (should (eq (agent-shell-sessions--restore record) shell))
                     (should-not calls)
                     (map-put! agent-shell--state :session '((:id . "test-session")))
                     (agent-shell-sessions--on-event '((:event . session-restored)))
                     (agent-shell-sessions--on-event '((:event . init-finished)))
                     (should agent-shell-sessions--unread)
                     (should-not agent-shell-sessions--restore-record)
                     ;; A later restore-all also reuses the established buffer.
                     (should (eq (agent-shell-sessions--restore record) shell))
                     (should-not calls)))))))

(ert-deftest agent-shell-sessions-restore-fallback-keeps-original ()
             (agent-shell-sessions-test--workspace
              (let ((record (agent-shell-sessions-test--record "original")))
                (setq agent-shell-sessions--records (list record))
                (agent-shell-sessions-test--with-shell
                 (map-put! agent-shell--state :agent-config '((:identifier . fixture)))
                 (setq agent-shell-sessions--restore-record record)
                 (agent-shell-sessions--on-event '((:event . init-finished)))
                 (should-not agent-shell-sessions--unread)
                 (should (member record agent-shell-sessions--records))
                 (should (equal (aref (cadr (agent-shell-sessions--saved-entry record)) 0)
                                "Restore failed"))
                 (should (= (length agent-shell-sessions--records) 2))))))

(ert-deftest agent-shell-sessions-missing-config-or-directory-does-not-launch ()
             (agent-shell-sessions-test--workspace
              (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
                        ((symbol-function 'agent-shell--resolved-agent-configs) (lambda () nil))
                        ((symbol-function 'agent-shell--start)
                         (lambda (&rest _) (ert-fail "Invalid restore launched an agent"))))
                (should-error (agent-shell-sessions--restore (agent-shell-sessions-test--record))
                              :type 'user-error)
                (let ((record (agent-shell-sessions-test--record)))
                  (setq record (plist-put record :directory (expand-file-name "missing/" directory)))
                  (should-error (agent-shell-sessions--restore record) :type 'user-error)))))

(ert-deftest agent-shell-sessions-restore-all-skips-open-and-continues-on-error ()
             (agent-shell-sessions-test--workspace
              (setq agent-shell-sessions--records
                    (mapcar #'agent-shell-sessions-test--record '("open" "bad" "good")))
              (let (attempted)
                (with-temp-buffer
                  (agent-shell-sessions-mode)
                  (cl-letf (((symbol-function 'agent-shell-sessions--live-for-record)
                             (lambda (record) (equal (plist-get record :id) "open")))
                            ((symbol-function 'agent-shell-sessions--restore)
                             (lambda (record)
                               (push (plist-get record :id) attempted)
                               (when (equal (plist-get record :id) "bad")
                                 (error "Provider unavailable"))))
                            ((symbol-function 'agent-shell-sessions-refresh) #'ignore))
                    (agent-shell-sessions-restore-all)))
                (should (equal (nreverse attempted) '("bad" "good")))
                (should (equal (cdar agent-shell-sessions--restore-errors) "Provider unavailable")))))

(ert-deftest agent-shell-sessions-command-payloads-stay-out-of-table ()
             (agent-shell-sessions-test--with-shell
              (setq-local shell-maker--busy t)
              (map-put! agent-shell--state :tool-calls
                        (list (cons "command"
                                    (list (cons :kind "execute")
                                          (cons :status "in_progress")
                                          (cons :title (concat "python3 <<'PY'\n"
                                                               (make-string 100000 ?x)))))))
              (let ((cell (aref (cadr (agent-shell-sessions--entry (current-buffer))) 4)))
                (should (equal cell "Running command"))
                (should (equal (get-text-property 0 'help-echo cell) "Running command")))
              (map-put! agent-shell--state :tool-calls nil)
              (map-put! agent-shell--state :last-entry-type "agent_thought_chunk")
              (should (equal (aref (cadr (agent-shell-sessions--entry (current-buffer))) 4)
                             "Thinking"))))

(ert-deftest agent-shell-sessions-long-titles-and-permissions-are-bounded ()
             (agent-shell-sessions-test--with-shell
              (setq-local shell-maker--busy t)
              (map-put! agent-shell--state :tool-calls
                        '(("tool" . ((:title . "A multiline\ncommand body")
                                     (:status . "in_progress")
                                     (:permission-request-id . 123)))))
              (let ((cell (aref (cadr (agent-shell-sessions--entry (current-buffer))) 4)))
                (should (equal cell "Permission: Running tool")))
              (setq shell-maker--busy nil)
              (map-put! agent-shell--state :session
                        (list (cons :id "test-session")
                              (cons :title (concat (make-string 100000 ?界) "\nRest of prompt"))))
              (let ((cell (aref (cadr (agent-shell-sessions--entry (current-buffer))) 4)))
                (should (<= (string-width cell) 80))
                (should-not (string-match-p "[\n\r]" cell))
                (should (string-suffix-p "…" cell))
                (should (<= (string-width (get-text-property 0 'help-echo cell)) 80)))))

(ert-deftest agent-shell-sessions-refresh-menu-replaces-existing-timers ()
             (let ((agent-shell-sessions-refresh-interval 1))
               (with-temp-buffer
                 (agent-shell-sessions-mode)
                 (let ((old agent-shell-sessions--timer))
                   (should (equal (agent-shell-sessions--refresh-description) "Auto-refresh: 1s"))
                   (agent-shell-sessions-set-refresh-interval 5)
                   (should-not (memq old timer-list))
                   (should (= (timer--repeat-delay agent-shell-sessions--timer) 5))
                   (should (equal (agent-shell-sessions--refresh-description) "Auto-refresh: 5s"))
                   (setq old agent-shell-sessions--timer)
                   (agent-shell-sessions-set-refresh-interval 0)
                   (should-not (memq old timer-list))
                   (should-not agent-shell-sessions--timer)
                   (should (equal (agent-shell-sessions--refresh-description) "Manual refresh"))
                   (should-error (agent-shell-sessions-set-refresh-interval -1) :type 'user-error)
                   (should-error (agent-shell-sessions-set-refresh-interval 0.5) :type 'user-error)
                   (agent-shell-sessions-set-refresh-interval 5)
                   (should (timerp agent-shell-sessions--timer))))))

(ert-deftest agent-shell-sessions-transient-disables-inapplicable-actions ()
             (agent-shell-sessions-test--workspace
              (save-window-excursion
                (with-temp-buffer
                  (switch-to-buffer (current-buffer))
                  (agent-shell-sessions-mode)
                  (agent-shell-sessions-refresh)
                  (unwind-protect
                      (progn
                        (call-interactively (key-binding (kbd "?")))
                        (let ((restore (seq-find (lambda (suffix)
                                                   (eq (oref suffix command)
                                                       'agent-shell-sessions-restore))
                                                 transient--suffixes)))
                          (should restore)
                          (should (oref restore inapt)))
                        (should (with-current-buffer " *transient*"
                                  (and (string-match-p "Workspace" (buffer-string))
                                       (string-match-p "Auto-refresh: 1s" (buffer-string))))))
                    (transient--emergency-exit))))))

(ert-deftest agent-shell-sessions-transient-acts-on-selected-saved-session ()
             (agent-shell-sessions-test--workspace
              (setq agent-shell-sessions--records (list (agent-shell-sessions-test--record)))
              (save-window-excursion
                (with-temp-buffer
                  (switch-to-buffer (current-buffer))
                  (agent-shell-sessions-mode)
                  (agent-shell-sessions-refresh)
                  (goto-char (point-min))
                  (unwind-protect
                      (progn
                        (agent-shell-sessions-menu)
                        (let ((restore (seq-find (lambda (suffix)
                                                   (eq (oref suffix command)
                                                       'agent-shell-sessions-restore))
                                                 transient--suffixes))
                              (save (seq-find (lambda (suffix)
                                                (eq (oref suffix command) 'agent-shell-sessions-save))
                                              transient--suffixes)))
                          (should-not (oref restore inapt))
                          (should (equal (plist-get (agent-shell-sessions--saved-at-point) :id)
                                         "saved-session"))
                          (call-interactively (oref save command))
                          (should (file-exists-p agent-shell-sessions-workspace-file))))
                    (transient--emergency-exit))))))

(ert-deftest agent-shell-sessions-new-does-not-copy-dashboard-context ()
             (let ((agent-shell-context-sources '(region line)))
               (dolist (viewport '(nil t))
                 (let ((agent-shell-prefer-viewport-interaction viewport)
                       (shell (generate-new-buffer "new-shell-fixture"))
                       (inserted 'not-called)
                       start-args start-directory)
                   (unwind-protect
                       (with-temp-buffer
                         (agent-shell-sessions-mode)
                         ;; Simulate an older dashboard buffer after reloading the library.
                         (setq-local agent-shell-context-sources '(region line))
                         (let ((inhibit-read-only t)) (insert "Ready  Dashboard row\n"))
                         (goto-char (point-min))
                         (set-mark (point-max))
                         (activate-mark)
                         (cl-letf (((symbol-function 'agent-shell--auto-preferred-config)
                                    (lambda () '((:identifier . fixture))))
                                   ((symbol-function 'agent-shell--start)
                                    (lambda (&rest args)
                                      (setq start-args args start-directory default-directory)
                                      shell))
                                   ((symbol-function 'agent-shell--display-and-insert-context)
                                    (lambda (_buffer text) (setq inserted text)))
                                   ((symbol-function 'agent-shell--display-viewport-when-ready)
                                    (lambda (&rest args) (setq inserted (plist-get args :append)))))
                           ;; Exercise upstream DWIM/context code, only mocking process startup.
                           (agent-shell-sessions-new temporary-file-directory)
                           (should-not inserted)
                           (should (plist-get start-args :new-session))
                           (should (equal start-directory temporary-file-directory))
                           (should (equal agent-shell-context-sources '(region line)))))
                     (kill-buffer shell))))))

(ert-deftest agent-shell-sessions-context-disabled-only-in-dashboard ()
             (let ((agent-shell-context-sources '(line)))
               (with-temp-buffer
                 (agent-shell-sessions-mode)
                 (let ((inhibit-read-only t)) (insert "Dashboard row"))
                 (goto-char (point-min))
                 (should-not (agent-shell--context)))
               (with-temp-buffer
                 (insert "Useful source code")
                 (goto-char (point-min))
                 (should (string-match-p "Useful source code" (agent-shell--context))))))

;;; agent-shell-sessions-tests.el ends here
