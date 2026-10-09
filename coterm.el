;;; coterm.el --- Terminal emulation for comint -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Filename: coterm.el
;; Author: Miha Rihtaršič <miha@kamnitnik.top>
;; Version: 1.6
;; Keywords: processes
;; Package-Requires: ((emacs "26.1") (compat "31.1"))
;; URL: https://repo.or.cz/emacs-coterm.git

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If the global `coterm-mode' is enabled, proper terminal emulation will be
;; supported for all newly spawned comint processes.  This allows you to use
;; more complex console programs such as "less" and "mpv" and full-screen TUI
;; programs such as "vi", "top", "htop" or even "emacs -nw".
;;
;; If you prefer to enable coterm-mode only on specific commands,
;; then, instead of activating `coterm-mode', use `coterm-apply' as an
;; advice.  For instance:
;;
;;   (advice-add 'shell :around 'coterm-apply)
;;
;; In addition to that, the following two local minor modes may be used:
;;
;; `coterm-char-mode': if enabled, most characters you type are sent directly
;; to the subprocess, which is useful for interacting with full-screen TUI
;; programs.
;;
;; `coterm-auto-char-mode': if enabled, coterm will enter and leave
;; `coterm-char-mode' automatically as appropriate.  For example, if you
;; execute "less" in a shell buffer, coterm will detect that "less" is running
;; and automatically enable char mode so that you can interact with "less"
;; directly.  Once you leave the "less" program, coterm will disable char mode
;; so that you can interact with your shell in the normal comint way.  This
;; mode is enabled by default in all coterm comint buffers.
;;
;; Automatic entrance into char mode is indicated by "AChar" in the modeline.
;; Non-automatic entrance into char mode is indicated by "Char".
;; Automatic exit of char mode is indicated by no text in the modeline.
;; Non-automatic exit of char mode is indicated by "Line".
;;
;; The command `coterm-char-mode-cycle' is a handy command to cycle between
;; automatic char-mode, char-mode enabled and char-mode disabled.
;;
;;;; Shell integration:
;;
;; Optionally, you can configure your shell to demarcate prompts and
;; command output through OSC 133 escape sequences.  To do so, add the
;; following to your shell init, say ~/.bashrc:
;;
;;   case "$TERM" in eterm-*)
;;     PS0='\e]133;C\a'"$PS0"
;;     PS1='\[\e]133;D;$?\a\]'"$PS1"
;;   esac
;;
;; If coterm sees any of those sequences, it disables the heuristic
;; `coterm-auto-char-mode' and uses only the OSC 133 information to
;; decide when to switch between char and line mode.
;;
;; The above settings will also cause coterm to display the exit
;; status of each command in the mode line.  If you wish to disable
;; that feature, leave out the ";$?" part in PS1.
;;
;;;; Installation:
;;
;; To install coterm, type M-x package-install RET coterm RET
;;
;; It is best to add the following elisp snippet to your Emacs init file, to
;; enable `coterm-mode' automatically on startup:
;;
;;   (coterm-mode)
;;
;;   ;; Alternatively, to enhance M-x shell but keep other
;;   ;; comint-based modes unmodified:
;;   (advice-add 'shell :around 'coterm-apply)
;;
;;   ;; Optional: bind `coterm-char-mode-cycle' to C-; in comint
;;   (with-eval-after-load 'comint
;;     (define-key comint-mode-map (kbd "C-;") #'coterm-char-mode-cycle))
;;
;;   ;; If your process repeats what you have already typed, try customizing
;;   ;; `comint-process-echoes':
;;   ;;   (setq-default comint-process-echoes t)
;;
;;
;;;; Differences from M-x term:
;;
;; coterm is written as an upgrade to comint.  For existing comint users, the
;; behaviour of comint doesn't change with coterm enabled except for the added
;; functionality that we can now use TUI programs.  It is therefore good for
;; users who generally prefer comint to term.el but sometimes miss the superior
;; terminal emulation that term.el provides.
;;
;; Coterm also provides `coterm-auto-char-mode' which aims to eliminate the
;; need to manually enable and disable char mode.
;;
;;
;;;; Some common problems:
;;
;; If some TUI programs misbehave, try checking your TERM environment variable
;; with 'echo $TERM' in your coterm enabled M-x shell.  It should normally be
;; set to "eterm-color".  If if isn't, it might be that one of your shell
;; initialization files (~/.bashrc) changes it, so check for that and remove
;; the change.
;;
;; The default "less" prompt, when invoked as 'less ~/some/file', is too
;; generic and isn't recognized by `coterm-auto-char-mode', so char mode isn't
;; entered automatically.  It is recommended to make your "less" prompt more
;; complete and recognizable by adding the character "m" or "M" to your LESS
;; environment variable.  For example, in your ~/.bashrc, add this line:
;;
;;   export LESS="FRXim"
;;
;; The "FRX" options make "less" more compatible with "git", and the "i" option
;; enables case insensitive search in less.  See man page less(1) for more
;; information.  Automatic char mode detection also usually fails if
;; "--incsearch" is enabled in "less".  It is advised to either turn this
;; option off or to use manual char mode.
;;
;;
;;;; Bugs, suggestions and patches can be sent to
;;
;;    bugs-doseganje (at) groups.io
;;
;; and can be viewed at https://groups.io/g/bugs-doseganje/topics.  As this
;; package is stored in GNU ELPA, non-trivial patches require copyright
;; assignment to the FSF, see info node "(emacs) Copyright Assignment".
;;
;; Some useful information you can send in your bug reports:
;;
;; After enabling `coterm-mode', open up an M-x shell and copy the output of
;; the following shell command:
;;
;;   export | cat -v | grep 'LESS\|TERM'; stty;
;;
;; You can also set the variable `coterm--t-log-buffer' to "coterm-log",
;; reproduce the issue and attach the contents of the buffer named
;; "coterm-log", which now contains all process output that was sent to coterm.

;;; Code:

(require 'comint)
(require 'term)
(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;; Mode functions and configuration

(defcustom coterm-term-name term-term-name
  "Name to use for TERM.
coterm will use this option to set the TERM environment variable
for the subprocess.  TUI programs usually consult this
environment variable to decide which escape sequences it should
send to the terminal.  It is recommended to leave this set to
\"eterm-color\", the terminal type coterm emulates."
  :group 'comint
  :type 'string)

(defvar coterm-termcap-format term-termcap-format
  "Termcap capabilities supported by coterm.")

(defvar coterm-term-environment-function #'comint-term-environment
  "Function to calculate environment for comint processes.
If non-nil, it is called with zero arguments and should return a
list of environment variable settings to apply to comint
subprocesses.")
(make-obsolete-variable 'coterm-term-environment-function nil "1.7")

(defvar coterm-start-process-function #'start-file-process
  "Function called to start a comint process.
It is called with the same arguments as `start-process' and
should return a process.")
(make-obsolete-variable 'coterm-start-process-function nil "1.7")

(defun comint-exec-1@coterm-config (f &rest args)
  "Make spawning processes for comint more configurable.
With this advice installed on `coterm-exec-1', you use the
settings `coterm-extra-environment-function' and
`coterm-start-process-function' to control how comint spawns a
process."
  (cl-letf*
      ((start-file-process (symbol-function #'start-file-process))
       (comint-term-environment (symbol-function #'comint-term-environment))
       ((symbol-function #'start-file-process)
        (lambda (&rest args)
          (fset #'start-file-process start-file-process)
          (apply coterm-start-process-function args)))
       ((symbol-function #'comint-term-environment)
        (lambda (&rest args)
          (fset #'comint-term-environment comint-term-environment)
          (apply coterm-term-environment-function args))))
    (apply f args)))
(make-obsolete 'comint-exec-1@coterm-config nil "1.7")
(advice-remove #'comint-exec-1 'comint-exec-1@coterm-config)

(defvar coterm--terminfo nil
  "Memoization place for function `coterm--terminfo'.")

(defun coterm--terminfo (name)
  "Terminfo information for a terminal named NAME."
  (let ((file (expand-file-name (format "%c/%s" (string-to-char name) name)
                                data-directory)))
    (cond ((not (file-readable-p file)) nil)
          ((file-remote-p default-directory)
           (with-memoization (alist-get name coterm--terminfo nil nil #'equal)
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents file)
               (base64-encode-region (point-min) (point-max) t)
               (insert "TERMINFO=b64:")
               (buffer-string))))
          (t (format "TERMINFO=%s" (directory-file-name data-directory))))))

(defun coterm-term-environment ()
  "Return an environment variable list for terminal configuration."
  (when coterm-term-name
    (let (ret)
      (when coterm-termcap-format
        (push (format coterm-termcap-format "TERMCAP="
                      coterm-term-name
                      (floor (window-screen-lines))
                      (window-max-chars-per-line))
              ret))
      (when-let* ((terminfo (coterm--terminfo coterm-term-name)))
        (push terminfo ret))
      (push (format "TERM=%s" coterm-term-name) ret)
      ret)))

;;;###autoload
(defun coterm-apply (&rest args)
  "Advice used to enable `coterm-mode' for a specific command.
For example, to make M-x shell use coterm, add the following to your
configuration:

  (advice-add #\\='shell :around #\\='coterm-apply)

If you use this approach, then you should not enable the global
`coterm-mode'."
  (cl-letf (((default-value 'coterm-mode) t))
    (apply args)))

;;;###autoload
(define-minor-mode coterm-mode
  "Improved terminal emulation in comint processes.
When this mode is enabled, terminal emulation is enabled for all
newly spawned comint processes, allowing you to use more complex
console programs such as \"less\" and \"mpv\" and full-screen
programs such as \"vi\", \"top\", \"htop\" or even \"emacs -nw\".

Environment variables for comint processes are set according to
variables `coterm-term-name' and `coterm-termcap-format'.

If you prefer enabling coterm only for a specific comint instead of
globally, use `coterm-apply.'"
  :global t
  :group 'comint)

(defun coterm--comint-exec (fun buffer name command startfile switches)
  "Advice for `comint-exec', kicks in only when `coterm-mode' is non-nil."
  (if (not coterm-mode)
      (funcall fun buffer name command startfile switches)
    (cl-letf (((symbol-function #'comint-term-environment)
               #'coterm-term-environment))
      (funcall fun buffer name "sh" startfile ; Adapted from `term-exec-1'
               `("-c" "stty -nl sane -echo && exec \"$@\""
                 "coterm" ,command ,@switches))
      (add-function :around (process-filter (get-buffer-process buffer))
                    #'coterm--t-emulate-terminal)
      buffer)))

;; We don't control the comint initialization order: first process
;; then major mode, or vice-versa.  So we need both below.
(advice-add #'comint-exec :around #'coterm--comint-exec)
(add-hook 'comint-mode-hook #'coterm--init)

;;; Char mode

(defvar coterm-char-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map term-raw-map)
    (define-key map [remap term-char-mode] #'coterm-char-mode-cycle)
    (define-key map [remap term-line-mode] #'coterm-char-mode-cycle)
    map))

(define-minor-mode coterm-char-mode
  "Send characters you type directly to the inferior process.
When this mode is enabled, the keymap `coterm-char-mode-map' is
active, which inherits from `term-raw-map'.  In this map, each
character is sent to the process, except for the escape
character (usually C-c).  You can set `term-escape-char' to
customize it."
  :lighter "")

(defvar-local coterm--char-old-scroll-margin nil)

(define-minor-mode coterm-scroll-snap-mode
  "Keep scroll synchronized.
Useful for full-screen terminal programs to keep them on screen."
  :keymap nil
  (if coterm-scroll-snap-mode
      (progn
        (unless coterm--char-old-scroll-margin
          (setq coterm--char-old-scroll-margin
                (cons scroll-margin
                      (local-variable-p 'scroll-margin)))
          (setq-local scroll-margin 0))
        (add-hook 'coterm-t-after-insert-hook #'coterm--scroll-snap 'append t)
        (coterm--scroll-snap))
    (when-let* ((margin coterm--char-old-scroll-margin))
      (setq coterm--char-old-scroll-margin nil)
      (if (cdr margin)
          (setq scroll-margin (car margin))
        (kill-local-variable 'scroll-margin)))
    (remove-hook 'coterm-t-after-insert-hook #'coterm--scroll-snap t)))

(defvar coterm--t-home)
(defvar coterm--t-home-off)

(defun coterm--scroll-snap ()
  ;; We need to check for `coterm-scroll-snap-mode' because a function in
  ;; `coterm-t-after-insert-hook' might have changed it
  (when coterm-scroll-snap-mode
    (let* ((buf (current-buffer))
           (pmark (process-mark (get-buffer-process buf)))
           (sel-win (selected-window))
           (w sel-win))
      ;; Avoid infinite loop in strange case where minibuffer window
      ;; is selected but not active.
      (while (window-minibuffer-p w)
        (setq w (next-window w nil t)))
      (while
          (progn
            (when (and (eq buf (window-buffer w))
                       ;; Only snap if point is on pmark
                       (= (window-point w) pmark))
              (if (eq sel-win w)
                  (progn
                    (goto-char coterm--t-home)
                    (forward-line coterm--t-home-off)
                    (forward-line 0)
                    (recenter 0)
                    (goto-char pmark))
                (with-selected-window w
                  (goto-char coterm--t-home)
                  (forward-line coterm--t-home-off)
                  (forward-line 0)
                  (recenter 0)
                  (goto-char pmark))))
            (setq w (next-window w nil t))
            (not (eq w sel-win)))))))

(defvar coterm-auto-char-mode)

(defun coterm-char-mode-cycle ()
  "Cycle between char mode on, off and auto.

If `coterm-auto-char-mode' is enabled, disable it and enable
both `coterm-char-mode' and `coterm-scroll-snap-mode'.

If `coterm-char-mode' is enabled, disable it along with
`coterm-scroll-snap-mode'.

If it is disabled, enable `coterm-auto-char-mode'."
  (interactive)
  (cond
   (coterm-auto-char-mode
    (coterm-auto-char-mode -1)
    (coterm-char-mode 1)
    (coterm-scroll-snap-mode 1))
   (coterm-char-mode
    (coterm-char-mode -1)
    (coterm-scroll-snap-mode -1))
   (t (coterm-auto-char-mode 1))))

;;;; Automatic entry to char mode

(define-minor-mode coterm-auto-char-mode
  "Whether we should enter or leave char mode automatically.
If enabled, `coterm-auto-char-functions' are consulted to set
`coterm-char-mode' and `coterm-scroll-snap-mode' automatically.

By default, functions in `coterm-auto-char-functions' try to
guess which mode is appropriate based on various heuristics.  See
their doc strings for more information."
  :lighter ""
  (if coterm-auto-char-mode
      (progn
        (add-hook 'coterm-t-after-insert-hook #'coterm--auto-char nil t)
        (add-hook 'post-command-hook #'coterm--auto-char nil t)
        (coterm--auto-char))
    (remove-hook 'coterm-t-after-insert-hook #'coterm--auto-char t)
    (remove-hook 'post-command-hook #'coterm--auto-char t)))

(defvar coterm-auto-char-lighter-mode-format
  '(coterm-char-mode (coterm-auto-char-mode " AChar" " Char")
                     (coterm-auto-char-mode "" " Line")))

(define-minor-mode coterm-auto-char-lighter-mode
  "Show current char mode status in modeline."
  :lighter coterm-auto-char-lighter-mode-format)

(defvar coterm-auto-char-functions
  (list #'coterm--auto-char-alternative-sub-buffer
        #'coterm--auto-char-less-prompt
        #'coterm--auto-char-mpv-prompt
        #'coterm--auto-char-not-eob
        #'coterm--auto-char-leave-both)
  "Abnormal hook to enter or leave `coterm-char-mode'.
This hook is run after every command and process output, if
`coterm-auto-char-mode' enabled.  It is only called if point is
on process's mark.

Each function is called with zero argumets and with `point-max'
on the end of process output until one returns non-nil.")

(defun coterm--auto-char ()
  "Automatically enter or leave `coterm-char-mode'.
If point is not on process mark, leave `coterm-char-mode' and
`coterm-scroll-snap-mode'.  Otherwise, call functions from
`coterm-auto-char-functions' until one returns non-nil."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (if (and pmark (= (point) pmark))
        (save-restriction
          (coterm--narrow-to-process-output pmark)
          (run-hook-with-args-until-success 'coterm-auto-char-functions))
      (when coterm-char-mode (coterm-char-mode -1))
      (when coterm-scroll-snap-mode (coterm-scroll-snap-mode -1)))))

(defvar coterm--t-alternative-sub-buffer)

(defun coterm--auto-char-alternative-sub-buffer ()
  "Enter `coterm-char-mode' if using an alternative sub-buffer."
  (when coterm--t-alternative-sub-buffer
    (unless coterm-char-mode (coterm-char-mode 1))
    (unless coterm-scroll-snap-mode (coterm-scroll-snap-mode 1))
    t))

(defun coterm--auto-char-less-prompt ()
  "Enter `coterm-char-mode' if a \"less\" prompt is detected.
In addition, temporarily modify `coterm-auto-char-functions' such
that char mode is maintained even if the user presses \"/\",
\":\", \"ESC\", \"-\" or a digit."
  (when (and (eobp) (coterm--auto-char-less-prompt-1))
    (unless coterm-char-mode (coterm-char-mode 1))
    (unless coterm-scroll-snap-mode (coterm-scroll-snap-mode 1))
    (cl-labels
        ((hook ()
           (if (not (eobp))
               (rem-hook)
             (or
              (bolp)                    ; Empty last line if "less" is slow
              (coterm--auto-char-less-prompt-1)
              (progn
                (forward-line 0)
                ;; Various secondary prompts that "less" outputs
                (prog1 (looking-at (concat
                                    "\\(?: ESC\\| :\\)\\'\\|"
                                    "Examine: \\|"
                                    "[Ll]og file: \\|"
                                    "Target line: \\|"
                                    "Backwards scroll limit: \\|"
                                    "\\(?:set \\|goto \\||\\)mark: \\|"
                                    "[:_+!-]\\|"
                                    "\\(?:.* \\)?[/?]"))
                  (goto-char (point-max))))
              (rem-hook))))
         (rem-hook ()
           (remove-hook 'coterm-auto-char-functions #'hook t)
           (remove-hook 'coterm-auto-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-scroll-snap-mode-hook #'rem-hook t)
           nil))
      (add-hook 'coterm-auto-char-functions #'hook nil t)
      (add-hook 'coterm-auto-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-scroll-snap-mode-hook #'rem-hook nil t))
    t))

(defun coterm--auto-char-less-prompt-1 ()
  "Return t if point is after a less prompt."
  (let ((opoint (point)))
    (forward-line 0)
    (prog1 (looking-at
            (concat
             "\\(?:"
             ":\\|"
             "\\(?:.* \\)?" "(END)\\|"
             "byte [0-9]+\\|"
             "lines [0-9]+-[0-9]+\\|"
             "100%\\|"
             "\\(?:.* \\)?" "\\(:?[0-9]?[0-9]\\|100\\)" "%\\|"
             ".*(press h for help or q to quit)\\|"
             ".*(press RETURN)"
             "\\)\\'"))
      (goto-char opoint))))

(defun coterm--auto-char-mpv-prompt ()
  "Enter `coterm-char-mode' if a mpv prompt is detected.
However, simply entering it isn't satisfactory, because mpv often
erases its status prompt for brief periods of time before
redrawing it again.  Because we don't want to leave char mode for
these brief periods, we temporarily modify
`coterm-auto-char-functions' such that `coterm-char-mode' is kept
active if these status prompt erasures are detected."
  (when (coterm--auto-char-mpv-prompt-1)
    (coterm-char-mode 1)
    (cl-labels
        ((hook ()
           (or (coterm--auto-char-mpv-prompt-1)
               ;; If we are on the last lane and this line is empty, it is
               ;; likely because mpv has erased its status buffer for a brief
               ;; period before redrawing it.
               (and (eobp) (bolp))
               (ignore (rem-hook))))
         (rem-hook ()
           (remove-hook 'coterm-auto-char-functions #'hook t)
           (remove-hook 'coterm-auto-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-scroll-snap-mode-hook #'rem-hook t)))
      (add-hook 'coterm-auto-char-functions #'hook nil t)
      (add-hook 'coterm-auto-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-scroll-snap-mode-hook #'rem-hook nil t))
    t))

(defun coterm--auto-char-mpv-prompt-1 ()
  "Return t if mpv is likely running."
  (when (or (bolp) (eolp))
    (let ((opoint (point)))
      (forward-line -1)
      (prog1 (looking-at
              (concat "\\(?:.*\n\\)?"
                      (regexp-opt '("[statusline] " "")) ; mpv -v
                      (regexp-opt '("(Paused) " "(Buffering) " "(...) " ""))
                      ;; There can be nothing before colon if you play an audio
                      ;; file with mpv and deselect the only audio track.
                      "\\(?:[AV]\\|AV\\)?: "
                      "-?[0-9][0-9]:[0-9][0-9]:[0-9][0-9] / "
                      "-?[0-9][0-9]:[0-9][0-9]:[0-9][0-9] "
                      "([0-9]?[0-9]?[0-9]%).*"
                      "\\(?:"
                      "\n\\[-*\\+-*\\]"
                      "\\)?"
                      "\\'"))
        (goto-char opoint)))))

(defun coterm--auto-char-not-eob ()
  "Enter `coterm-char-mode' if a full-screen TUI program is detected.
We assume that if the cursor moves more than 9 lines above the
bottom row, a full-screen program is likely being drawn.  In this
case, enter `coterm-char-mode' and `coterm-scroll-snap-mode' and
temporarily modify `coterm-auto-char-functions' such that we will
only leave these modes once cursor moves to the bottom line."
  (when (looking-at "\\(?:.*\n\\)\\{9,\\}")
    (coterm-char-mode 1)
    (coterm-scroll-snap-mode 1)
    (cl-labels
        ((hook ()
           (or (looking-at ".*\n.")
               (ignore (rem-hook))))
         (rem-hook ()
           (remove-hook 'coterm-auto-char-functions #'hook t)
           (remove-hook 'coterm-auto-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-char-mode-hook #'rem-hook t)
           (remove-hook 'coterm-scroll-snap-mode-hook #'rem-hook t)))
      (add-hook 'coterm-auto-char-functions #'hook nil t)
      (add-hook 'coterm-auto-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-char-mode-hook #'rem-hook nil t)
      (add-hook 'coterm-scroll-snap-mode-hook #'rem-hook nil t))
    t))

(defun coterm--auto-char-leave-both ()
  (when coterm-char-mode (coterm-char-mode -1))
  (when coterm-scroll-snap-mode (coterm-scroll-snap-mode -1))
  t)

(defun coterm--narrow-to-process-output (pmark)
  "Widen and narrow to process output.
If there is no user input at end of buffer, simply widen.  PMARK
is the process mark."
  (widen)
  (unless comint-use-prompt-regexp
    (unless (eq (get-char-property (max 1 (1- (point-max))) 'field)
                'output)
      (narrow-to-region
       (point-min)
       (previous-single-property-change (point-max) 'field nil pmark)))))

;;; Terminal emulation

;; This is essentially a re-implementation of term.el's terminal emulation.  I
;; could have simply reused functions from term.el but that would have been
;; unsatisfactory in my opinion.  That is mostly due to the fact that term.el's
;; terminal emulation inserts a lot of redundant trailing whitespace and empty
;; lines, which I believe is very distracting for ordinary comint usage.
;;
;; Terminal emulation is coordinate based, for example, "move cursor to row 11
;; and column 21".  A coordinate position may not be reachable in an Emacs
;; buffer because the specified line is currently too short or there aren't
;; enough lines in the buffer.  term.el automatically inserts empty lines and
;; spaces in order to move point to a specified coordinate position, which
;; often results in trailing whitespace.
;;
;; coterm takes a different approach.  Rather than insert whitespace, we move
;; point close to the target terminal cursor coordinates and save the offset in
;; the variables `coterm--t-row-off' and `coterm--t-col-off'.  Only when
;; terminal emulation requires insertion of actual text do we have to be able
;; to reach the current cursor coordinates.  We may have to insert newlines and
;; spaces to make this position reachable, but inserting text after this
;; whitespace means that it isn't trailing or redundant (except if the inserted
;; text consists of only whitespace).
;;
;;
;; Line wrapping:
;;
;; term.el wraps lines correctly and accurately.  When text is to be inserted
;; at the right edge, term.el will first move the cursor to the beginning of
;; the next line.
;;
;; The beauty of comint, on the other hand, is that it inserts long lines
;; unchanged and leaves line wrapping up to Emacs.  One can easily use
;; `toggle-truncate-lines' or even `word-wrap' to change display of long lines
;; from compiler output for example.  That is why it was decided that coterm
;; will follow suit and insert long lines unchanged.  However, this means that
;; terminal emulation isn't fully accurate for long lines.  Up to now, "less"
;; was the only program I've encountered that relies on accurate line wrapping,
;; so a workaround aimed at "less" specifically was implemented (search for the
;; term "less" in the function `coterm--t-emulate-terminal').

(defconst coterm--t-control-seq-regexp
  ;; Differences from `term-control-seq-regexp':
  ;;
  ;; For optimization, we try matching "\r\n" as whole, if possible, instead of
  ;; \r and \n separately
  ;;
  ;; Removed: \032 (\C-z)
  ;; Added: OSC sequence \e] ... ; ... \e\\ (or \a)
  ;; Added: sequences \e= and \e>
  ;; Added: Invalid sequence \e\e, used by package `bash-completion'
  (concat
   ;; A control character,
   "\\(?:[\n\000\007\t\b\016\017]\\|\r\n?\\|"
   ;; a C1 escape coded character (see [ECMA-48] section 5.3 "Elements
   ;; of the C1 set"),
   "\e\\(?:[DM78c=>\e]\\|"
   ;; Emacs specific control sequence from term.el.  In coterm, we simply
   ;; ignore them.
   "AnSiT[^\n]+\n\\|"
   ;; OSC seqence.  We print them normally to let
   ;; `comint-output-filter-functions' handle them
   "][0-9A-Za-z]*;.*?\\(?:\a\\|\e\\\\\\)\\|"
   ;; or an escape sequence (section 5.4 "Control Sequences"),
   "\\[\\([\x30-\x3F]*\\)[\x20-\x2F]*[\x40-\x7E]\\)\\)")
  "Regexp matching control sequences handled by coterm.")

(defconst coterm--t-control-seq-prefix-regexp "\e")

(defvar coterm--t-log-buffer nil
  "If non-nil, log process output to this buffer.
Set it to a name of a buffer if you want to record process output
for debugging purposes.")

(defvar-local coterm--t-height nil
  "Number of lines in window.")
(defvar-local coterm--t-width nil
  "Number of columns in window.")
(defvar-local coterm--t-scroll-beg nil
  "First row of the scrolling area.")
(defvar-local coterm--t-scroll-end nil
  "First row after the end of the scrolling area.")

(defvar-local coterm--t-home nil
  "Marks the \"home\" position for cursor addressing.
`coterm--t-home-off' should be taken into account as well.")
(defvar-local coterm--t-home-off 0
  "How many rows lower the home position actually is.
This usually is needed if `coterm--t-home' is on the last line of
the buffer.")
(defvar-local coterm--t-row-off 0
  "How many rows lower the current position actually is.
May be non-zero if point on the last line of accessible portion
of the buffer.  More precisely, this variable can only be
non-zero if there are no \\n characters after point.")
(defvar-local coterm--t-col-off 0
  "How many cols to the right the current position actually is.
Non-zero only if point is on the end of line or on a character
that spans more than one columen.  In the latter case, this
variable's value can be negative.")

(defvar-local coterm--t-row nil
  "Cache of current terminal row if non-nil.")
(defvar-local coterm--t-col nil
  "Cache of current terminal column if non-nil.")

(defun coterm--t-row ()
  "Return terminal's current row.
Use the variable `coterm--t-row' as the cache if non-nil.  Set it
to nil to invalidate the cache."
  (or coterm--t-row
      (setq coterm--t-row
            (- (+ (save-restriction
                    (save-excursion
                      (narrow-to-region coterm--t-home (point-max))
                      (+ (forward-line -9999) 9999)))
                  coterm--t-row-off)
               coterm--t-home-off))))

(defun coterm--t-col ()
  "`current-column' with cache.
The variable `coterm--t-col' holds the cache if
non-nil. Set it to nil to invalidate the cache."
  (or coterm--t-col
      (setq coterm--t-col (+ (current-column) coterm--t-col-off))))

(defvar-local coterm--t-alternative-sub-buffer nil
  "Non-nil if using an alternative sub-buffer (termcap smcup).")

(defvar-local coterm--t-saved-cursor nil)
(defvar-local coterm--t-insert-mode nil)
(defvar-local coterm--t-unhandled-fragment nil)

(defvar coterm-t-after-insert-hook nil
  "Hook run after inserting process output.")

(defun coterm--init ()
  "Initialize current buffer for coterm."
  (when-let* ((_ coterm-mode)
              (process (get-buffer-process (current-buffer))))
    ;; Remember that this buffer uses coterm-mode, even if the global
    ;; minor mode is later disabled.
    (setq-local coterm-mode t)
    (setq coterm--t-height (floor (window-screen-lines)))
    (setq coterm--t-width (window-max-chars-per-line))
    (setq coterm--t-home (point-min-marker))
    (setq coterm--t-scroll-beg 0)
    (setq coterm--t-scroll-end coterm--t-height)

    (setq-local comint-inhibit-carriage-motion t)
    (coterm-auto-char-mode)
    (coterm-auto-char-lighter-mode)

    (when (featurep 'ansi-osc)          ;New in Emacs 29
      (add-hook 'comint-output-filter-functions
                #'comint-osc-process-output nil t)
      (cl-pushnew '("133" . coterm--osc-133-handler)
                  ansi-osc-handlers :test #'equal))
    (setq mode-line-process
          `("" ,mode-line-process (:eval (coterm--formatted-exit-status))))

    (add-function :filter-return
                  (local 'window-adjust-process-window-size-function)
                  (lambda (size)
                    (when size
                      (coterm--t-reset-size (cdr size) (car size)))
                    size)
                  '((name . coterm-maybe-reset-size)))))

(defun coterm--t-reset-size (height width)
  (let ((shrunk (< height coterm--t-height)))
    (setq coterm--t-height height)
    (setq coterm--t-width width)
    (setq coterm--t-scroll-beg 0)
    (setq coterm--t-scroll-end height)
    (when-let* ((shrunk)
                (proc (get-buffer-process (current-buffer)))
                (pmark (process-mark proc)))
      (save-excursion
        (save-restriction
          (coterm--narrow-to-process-output pmark)
          (goto-char pmark)
          (setq coterm--t-row nil)
          (when (>= (coterm--t-row) height)
            (cond
             (coterm--t-alternative-sub-buffer
              (goto-char coterm--t-home)
              (forward-line (- coterm--t-row height -1))
              (delete-region coterm--t-home (point)))
             (t
              (cl-incf coterm--t-home-off (- coterm--t-row coterm--t-height -1))
              (setq coterm--t-row (1- coterm--t-height))))))))))

(defun coterm--t-goto (row col)
  "Move point to a position that approximates ROW and COL.
Set `coterm--t-row-off' and `coterm--t-col-off' accordingly."
  (goto-char coterm--t-home)
  (setq coterm--t-row-off
        (+ (forward-line (+ coterm--t-home-off row))
           (if (bolp) 0 1)))
  (setq coterm--t-row row)
  (setq coterm--t-col-off
        (- col (move-to-column (setq coterm--t-col col)))))

(defun coterm--t-apply-proc-filt (proc-filt process str)
  "Insert STR at point using PROC-FILT and PROCESS.
Basically, call PROC-FILT with the arguments PROCESS and STR, but
adjusting `ansi-color-context-region' and setting PROCESS' mark
to point beforehand.

If STR contains newlines, the caller must take care that
`coterm--t-row' is adjusted accordingly."
  (when-let* ((context ansi-color-context-region)
              (marker (cadr context)))
    (set-marker marker (point)))
  (let ((pmark (process-mark process)))
    (set-marker pmark (point))
    (funcall proc-filt process str)
    ;; Needed for emacs version < 27 with buggy functions in
    ;; `comint-output-filter-functions' which upredictably move point
    (goto-char pmark))
  (unless (string-empty-p str)
    (setq coterm--t-col nil)))

(defun coterm--t-switch-to-alternate-sub-buffer (proc-filt process set)
  (cond
   ((and set (null coterm--t-alternative-sub-buffer))
    (setq coterm--t-alternative-sub-buffer
          (list coterm--t-home
                coterm--t-home-off
                (coterm--t-row)
                (coterm--t-col)))
    (setq coterm--t-home-off 0)
    (setq coterm--t-row-off 0)
    (setq coterm--t-col-off 0)
    (setq coterm--t-row 0)
    (setq coterm--t-col 0)
    (goto-char (point-max))
    (unless (bolp)
      (coterm--t-apply-proc-filt proc-filt process "\n"))
    (setq coterm--t-home (point-marker)))

   ((and (not set) coterm--t-alternative-sub-buffer)
    (delete-region coterm--t-home (point-max))
    (set-marker coterm--t-home (car coterm--t-alternative-sub-buffer))
    (set-marker (car coterm--t-alternative-sub-buffer) nil)
    (setq coterm--t-home-off (nth 1 coterm--t-alternative-sub-buffer))
    (coterm--t-goto (nth 2 coterm--t-alternative-sub-buffer)
                    (nth 3 coterm--t-alternative-sub-buffer))
    (setq coterm--t-alternative-sub-buffer nil)

    (when (>= (coterm--t-row) coterm--t-height)
      (let ((opoint (point)))

        (setq coterm--t-home-off
              (forward-line (+ 1 (- coterm--t-height) coterm--t-row-off)))
        (unless (eolp)
          (cl-incf coterm--t-home-off)
          (forward-line 0))
        (set-marker coterm--t-home (point))
        (setq coterm--t-row (1- coterm--t-height))
        (goto-char opoint)))

    (cl-labels
        ((hook ()
           (remove-hook 'coterm-t-after-insert-hook #'hook t)
           (unless coterm--t-alternative-sub-buffer
             (let ((coterm-scroll-snap-mode t))
               (coterm--scroll-snap)))))
      (add-hook 'coterm-t-after-insert-hook #'hook nil t)))))

(defun coterm--t-down-line (proc-filt process)
  "Go down one line or scroll if at bottom.
This takes into account the scroll region as specified by
`coterm--t-scroll-beg' and `coterm--t-scroll-end'.  If required,
PROC-FILT and PROCESS are used to scroll with deletion and
insertion of empty lines."
  (let ((orow (coterm--t-row))
        (ocol (coterm--t-col)))
    (cond
     ((= orow (1- coterm--t-scroll-end))
      (let ((moved (and (zerop (forward-line)) (bolp))))
        ;; Remove top line or move home marker
        (save-excursion
          (goto-char coterm--t-home)
          (cond ((or coterm--t-alternative-sub-buffer
                     (not (zerop coterm--t-scroll-beg)))
                 ;; Remove top line
                 (and (zerop (forward-line
                              (+ coterm--t-home-off coterm--t-scroll-beg)))
                      (bolp)
                      (delete-region (point) (progn (forward-line) (point)))))
                (t
                 ;; Move home marker
                 (if (and (zerop (forward-line)) (bolp))
                     (set-marker coterm--t-home (point))
                   (cl-incf coterm--t-home-off)))))
        ;; Insert an empty line at the bottom
        (cond (moved
               (unless (eobp)
                 (let ((opoint (point)))
                   (coterm--t-apply-proc-filt proc-filt process "\n")
                   (goto-char opoint)))
               (setq coterm--t-col-off ocol))
              (t
               (cl-incf coterm--t-row-off)
               (setq coterm--t-col-off (- ocol (move-to-column ocol)))))))
     ((= orow (1- coterm--t-height))
      ;; Do nothing, behaviour of xterm
      (ignore))
     (t
      ;; Move point vertically down
      (unless (and (zerop (forward-line)) (bolp))
        (cl-incf coterm--t-row-off))
      (cl-incf coterm--t-row)
      (setq coterm--t-col-off (- ocol (move-to-column ocol)))))))

(defun coterm--t-up-line (proc-filt process)
  "Go up one line or scroll if at top.
This takes into account the scroll region as specified by
`coterm--t-scroll-beg' and `coterm--t-scroll-end'.  If required,
PROC-FILT and PROCESS are used to scroll with deletion and
insertion of empty lines."
  (let ((orow (coterm--t-row))
        (ocol (coterm--t-col)))
    (cond
     ((= orow coterm--t-scroll-beg)
      ;; Remove bottom line
      (save-excursion
        (goto-char coterm--t-home)
        (and (zerop (forward-line
                     (+ coterm--t-home-off coterm--t-scroll-end -1)))
             (bolp)
             (delete-region (point) (progn (forward-line) (point)))))
      ;; Insert an empty line at the top or move home marker
      (cond ((and (not coterm--t-alternative-sub-buffer)
                  (zerop coterm--t-scroll-beg))
             ;; Move home marker
             (forward-line -1)
             (set-marker coterm--t-home (point))
             (setq coterm--t-home-off 0)
             (setq coterm--t-row 0)
             (setq coterm--t-col-off (- ocol (move-to-column ocol))))
            ((not (zerop coterm--t-row-off))
             (ignore))
            (t
             ;; Insert an empty line at the top
             (forward-line 0)
             (let ((opoint (point)))
               (coterm--t-apply-proc-filt proc-filt process "\n")
               (goto-char opoint))
             (setq coterm--t-col-off ocol))))

     ((= orow 0)
      ;; Behaviour of xterm
      (ignore))

     ((zerop coterm--t-row-off)
      ;; Move point vetically up
      (forward-line -1)
      (cl-decf coterm--t-row)
      (setq coterm--t-col-off (- ocol (move-to-column ocol))))
     (t
      (cl-decf coterm--t-row)
      (cl-decf coterm--t-row-off)))))

(defun coterm--t-clear-screen ()
  "Clear terminal screen.
If not using alternative sub-buffer, simply move home marker to
point-max"
  (setq coterm--t-row-off (coterm--t-row))
  (setq coterm--t-col-off (coterm--t-col))
  (if coterm--t-alternative-sub-buffer
      (delete-region coterm--t-home (point-max))
    (setq coterm--t-home-off 0)
    (goto-char (point-max))
    (unless (bolp)
      (cl-incf coterm--t-row-off)
      (setq coterm--t-home-off 1)
      (setq coterm--t-col-off
            (- coterm--t-col-off (move-to-column coterm--t-col-off))))
    (set-marker coterm--t-home (point))))

(defun coterm--t-insert (proc-filt process str newlines)
  "Insert STR at point using PROC-FILT and PROCESS.
Synchronise PROCESS's mark beforehand and insert at its position.
NEWLINES is the number of newlines STR contains.  Unless it is
zero, insertion must happen at the end of accessible portion of
buffer and the scrolling region must begin at the top of the
terminal screen.

This function also converts all occuences of \"\\r\\n\" into
\"\\n\" in STR before inserting it."
  (setq str (string-replace "\r" "" str))
  (unless (zerop coterm--t-row-off)
    (setq coterm--t-col-off (coterm--t-col))
    (goto-char (point-max)))
  (unless (and (zerop coterm--t-col-off) (zerop coterm--t-row-off))
    (coterm--t-apply-proc-filt proc-filt process
                               (concat (make-string coterm--t-row-off ?\n)
                                       (make-string (max 0 coterm--t-col-off) ?\s)))
    (setq coterm--t-col-off 0 coterm--t-row-off 0))
  (cond
   ((not (zerop newlines))
    (coterm--t-apply-proc-filt proc-filt process str)
    (when coterm--t-row
      (cl-incf coterm--t-row newlines))

    ;; Scroll if necessary
    (when (>= (coterm--t-row) coterm--t-height)
      (let ((opoint (point)))
        (forward-line (- 1 coterm--t-height))
        (set-marker coterm--t-home (point))
        (setq coterm--t-home-off 0)
        (setq coterm--t-row (1- coterm--t-height))
        (goto-char opoint))))

   ((not (eobp))
    (if coterm--t-insert-mode
        (coterm--t-apply-proc-filt proc-filt process str)
      ;; If not in insert mode, replace text
      (let ((old-col (coterm--t-col)))
        (coterm--t-apply-proc-filt proc-filt process str)
        (when (< old-col (coterm--t-col))
          (delete-region
           (point)
           (progn (move-to-column (- (* 2 coterm--t-col) old-col))
                  (point)))))))

   (t (coterm--t-apply-proc-filt proc-filt process str))))

(defun coterm--t-emulate-terminal (proc-filt process string)
  (let* ((pmark (process-mark process))
         (match 0)
         (will-insert-newlines 0)
         (inhibit-read-only t)
         restore-point
         last-match-end
         old-pmark
         buf
         ctl-params ctl-end)

    (cl-macrolet
        ;; Macros for looping through control sequences
        ((ins ()
           `(progn
              (let ((str (substring string last-match-end match)))
                (unless (equal "" str)
                  (coterm--t-insert proc-filt process str
                                    will-insert-newlines)
                  (setq will-insert-newlines 0)))
              (setq last-match-end ctl-end)))
         (pass-through ()
           `(ignore))
         (ctl-params* ()
           `(mapcar #'string-to-number (split-string ctl-params ";")))
         (car-or-1 ()
           `(max 1 (car (ctl-params*))))
         (cadr-or-0 ()
           `(or (cadr (ctl-params*)) 0)))

      (if (not (and string
                    (setq buf (process-buffer process))
                    (buffer-live-p buf)))
          (funcall proc-filt process string)

        (with-current-buffer buf
          (when coterm--t-log-buffer
            (with-current-buffer (get-buffer-create coterm--t-log-buffer)
              (save-excursion
                (goto-char (point-max))
                (insert (make-string 70 ?=) ?\n)
                (insert string ?\n))))

          (when-let* ((fragment coterm--t-unhandled-fragment))
            (setq string (concat fragment string))
            (setq coterm--t-unhandled-fragment nil))

          (setq restore-point (if (= (point) pmark) pmark (point-marker)))
          (setq old-pmark (copy-marker pmark window-point-insertion-type))

          (save-restriction
            (coterm--narrow-to-process-output pmark)
            (goto-char pmark)
            ;; (setq coterm--t-row nil)
            (setq coterm--t-col nil)
            (setq coterm--t-row-off 0)
            (setq coterm--t-col-off 0)

            ;; scroll cursor pmark into view by moving home marker if necessary
            (let ((opoint (point)))
              (cond
               ((<= opoint coterm--t-home)
                (forward-line 0)
                (set-marker coterm--t-home (point))
                (setq coterm--t-home-off 0)
                (setq coterm--t-row 0))
               (t
                (unless (zerop coterm--t-home-off)
                  (goto-char coterm--t-home)
                  (forward-line coterm--t-home-off)
                  (set-marker coterm--t-home (point))
                  (setq coterm--t-home-off 0)
                  (goto-char opoint))
                (forward-line (- 1 coterm--t-height))
                (if (<= (point) coterm--t-home)
                    (setq coterm--t-row nil)
                  (set-marker coterm--t-home (point))
                  (setq coterm--t-row (1- coterm--t-height)))))
              (goto-char opoint))

            (while (setq match (string-match coterm--t-control-seq-regexp
                                             string ctl-end))
              (setq ctl-params (match-string 1 string))
              (setq ctl-end (match-end 0))

              (pcase (aref string match)
                ((and ?\r (guard (= ctl-end (+ 2 match))))
                 ;; A match string of length two and beginning with \r means
                 ;; that we have matched "\r\n".  In this case, and if we are
                 ;; at eob, we pass-through to avoid an unnecessary call to
                 ;; `substring' which is expensive.  In the most common case
                 ;; when the process just outputs text at eob without any
                 ;; control sequences, we will end up inserting the whole
                 ;; string without a single call to `substring'.
                 (if (and (eobp)
                          (not coterm--t-alternative-sub-buffer)
                          (= 0 coterm--t-scroll-beg))
                     (progn (pass-through)
                            (cl-incf will-insert-newlines))
                   (ins)
                   (setq coterm--t-col 0
                         coterm--t-col-off 0)
                   (move-to-column 0)
                   (coterm--t-down-line proc-filt process)))
                (?\n (ins) ;; (terminfo: cud1, ind)
                     (coterm--t-down-line proc-filt process))
                (?\r (ins) ;; (terminfo: cr)
                     (setq coterm--t-col 0
                           coterm--t-col-off 0)
                     (move-to-column 0))
                ;; TAB (terminfo: ht)
                ((and ?\t (guard (eobp)))
                 ;; Insert a TAB as is, if at eob
                 (pass-through))
                (?\t
                 ;; Otherwise, move cursor to the next tab stop
                 (ins)
                 (setq coterm--t-col
                       (min (1- coterm--t-width)
                            (+ (coterm--t-col) 8 (- (mod coterm--t-col 8)))))
                 (setq coterm--t-col-off (- coterm--t-col (move-to-column coterm--t-col))))
                (?\b ;; (terminfo: cub1)
                 (ins)
                 (if (and (= (1- (coterm--t-col)) coterm--t-width)
                          (eq (char-before) ?\s))
                     ;; Awkward hack to make line-wrapping work in "less".
                     ;; Very specific to the way "less" performs wrapping: When
                     ;; reaching the end of line, instead of sending "\r\n" to
                     ;; go to the start of the next line, it sends " \b": a
                     ;; space which wraps to the next line in most terminals
                     ;; and a backspace to move to the start of the line.  Here
                     ;; we detect this and handle it like an ordinary "\r\n".
                     ;;
                     ;; For all other cases, coterm does not perform any
                     ;; wrapping at all.
                     (progn
                       (delete-char -1)
                       (setq coterm--t-col 0
                             coterm--t-col-off 0)
                       (move-to-column 0)
                       (coterm--t-down-line proc-filt process))
                   ;; (debug)
                   (setq coterm--t-col (max 0 (1- coterm--t-col)))
                   (setq coterm--t-col-off (- coterm--t-col (move-to-column coterm--t-col)))))
                (?\C-g (ins) ;; (terminfo: bel)
                       (beep t))
                ;; Ignore NUL, Shift Out, Shift In.
                ((or ?\0 14 15 '()) (ins))
                (?\e
                 (pcase (aref string (1+ match))
                   (?D (ins)
                       (coterm--t-down-line proc-filt process))
                   (?M (ins) ;; (terminfo: ri)
                       (coterm--t-up-line proc-filt process))
                   (?7 (ins) ;; Save cursor (terminfo: sc)
                       (setq coterm--t-saved-cursor
                             (list (coterm--t-row)
                                   (coterm--t-col)
                                   ansi-color-context-region
                                   ansi-color-context)))
                   (?8 (ins) ;; Restore cursor (terminfo: rc)
                       (when-let* ((cursor coterm--t-saved-cursor))
                         (setq coterm--t-saved-cursor nil)
                         (coterm--t-goto
                          (min (car cursor) (1- coterm--t-height))
                          (progn (setq cursor (cdr cursor))
                                 (min (car cursor) (1- coterm--t-width))))
                         (setq cursor (cdr cursor))
                         (setq ansi-color-context-region (car cursor))
                         (setq ansi-color-context (cadr cursor))))
                   (?c (ins) ;; \Ec - Reset (terminfo: rs1)
                       (erase-buffer)
                       (setq ansi-color-context-region nil)
                       (setq ansi-color-context nil)
                       (setq coterm--t-home-off 0)
                       (setq coterm--t-row 0)
                       (setq coterm--t-row-off 0)
                       (setq coterm--t-col 0)
                       (setq coterm--t-col-off 0)
                       (setq coterm--t-scroll-beg 0)
                       (setq coterm--t-scroll-end coterm--t-height)
                       (setq coterm--t-insert-mode nil))
                   (?\] (pass-through)) ;; OSC sequence, handled by comint
                   (?A (ins)) ;; Ignore term.el specific \eAnSiT sequences
                   ;; mpv outputs sequences \E= and \E>.  Ignore them
                   ((or ?= ?>) (ins))
                   (?\[
                    (pcase (aref string (1- ctl-end))
                      (?m ;; Let `comint-output-filter-functions' handle this
                       (pass-through))
                      ((or ?H ?f) ;; cursor motion (terminfo: cup,home)
                       (ins)
                       (coterm--t-goto
                        (1- (max 1 (min (car-or-1) coterm--t-height)))
                        (1- (max 1 (min (cadr-or-0) coterm--t-width)))))
                      (?A ;; cursor up (terminfo: cuu, cuu1)
                       (ins)
                       (coterm--t-goto (max (- (coterm--t-row) (car-or-1))
                                            coterm--t-scroll-beg)
                                       (coterm--t-col)))
                      (?B ;; cursor down (terminfo: cud)
                       (ins)
                       (coterm--t-goto (min (+ (coterm--t-row) (car-or-1))
                                            (1- coterm--t-scroll-end))
                                       (coterm--t-col)))
                      (?C ;; \E[C - cursor right (terminfo: cuf, cuf1)
                       (ins)
                       (setq coterm--t-col (min (+ (coterm--t-col) (car-or-1))
                                                (1- coterm--t-width)))
                       (setq coterm--t-col-off (- coterm--t-col (move-to-column coterm--t-col))))
                      (?D ;; \E[D - cursor left (terminfo: cub)
                       (ins)
                       (setq coterm--t-col (max (- (coterm--t-col) (car-or-1))
                                                0))
                       (setq coterm--t-col-off (- coterm--t-col (move-to-column coterm--t-col))))
                      (?E ;; \E[E - cursor down and column 0
                       (ins)
                       (coterm--t-goto (min (+ (coterm--t-row) (car-or-1))
                                            (1- coterm--t-scroll-end))
                                       0))
                      (?F ;; \E[F - cursor up and column 0
                       (ins)
                       (coterm--t-goto (max (- (coterm--t-row) (car-or-1))
                                            coterm--t-scroll-beg)
                                       0))
                      (?G ;; \E[G - horizontal cursor position
                       (ins)
                       (setq coterm--t-col (min (1- (car-or-1))
                                                (1- coterm--t-width)))
                       (setq coterm--t-col-off (- coterm--t-col (move-to-column coterm--t-col))))
                      ;; \E[J - clear to end of screen (terminfo: ed, clear)
                      ((and ?J (guard (eq 0 (car (ctl-params*)))))
                       (ins)
                       (when (zerop coterm--t-row-off)
                         (if (= (point) coterm--t-home)
                             (coterm--t-clear-screen)
                           (delete-region (point) (point-max)))))
                      ((and ?J (guard (eq 1 (car (ctl-params*)))))
                       (ins)
                       (if (zerop coterm--t-row-off)
                           (let ((opoint (point))
                                 (orow (coterm--t-row))
                                 (ocol (coterm--t-col)))
                             (goto-char coterm--t-home)
                             (forward-line coterm--t-home-off)
                             (delete-region (point) opoint)
                             (unless (eobp)
                               (coterm--t-apply-proc-filt
                                proc-filt process
                                (concat (make-string orow ?\n)
                                        (unless (eolp)
                                          (make-string ocol ?\s)))))
                             (coterm--t-goto orow ocol))
                         (coterm--t-clear-screen)))
                      (?J (ins) (coterm--t-clear-screen))
                      ;; \E[K - clear to end of line (terminfo: el, el1)
                      ((and ?K (guard (eq 1 (car (ctl-params*)))))
                       (ins)
                       (and
                        (not (bolp))
                        (zerop coterm--t-row-off)
                        (let ((ocol (coterm--t-col)))
                          (delete-region (point) (progn (forward-line 0) (point)))
                          (if (eolp)
                              (setq coterm--t-col-off ocol)
                            (coterm--t-apply-proc-filt
                             proc-filt process (make-string ocol ?\s))
                            (setq coterm--t-col-off 0)))))
                      ((and ?K (guard (eobp)))
                       (pass-through))
                      (?K
                       (ins)
                       (when (< (coterm--t-col) coterm--t-width)
                         (let ((opoint (point)))
                           (when (zerop (forward-line))
                             (when (bolp) (backward-char))
                             (delete-region opoint (point)))
                           (goto-char opoint))))
                      (?L ;; \E[L - insert lines (terminfo: il, il1)
                       (ins)
                       (when (<= coterm--t-scroll-beg (coterm--t-row)
                                 (1- coterm--t-scroll-end))
                         (let ((coterm--t-scroll-beg coterm--t-row))
                           (dotimes (_ (min (- coterm--t-scroll-end coterm--t-row)
                                            (car-or-1)))
                             (coterm--t-up-line proc-filt process)))))
                      (?M ;; \E[M - delete lines (terminfo: dl, dl1)
                       (ins)
                       (when (<= coterm--t-scroll-beg (coterm--t-row)
                                 (1- coterm--t-scroll-end))
                         (let ((coterm--t-scroll-beg coterm--t-row)
                               (orow coterm--t-row)
                               (ocol (coterm--t-col)))
                           (coterm--t-goto (1- coterm--t-scroll-end) ocol)
                           (dotimes (_ (min (- coterm--t-scroll-end orow)
                                            (car-or-1)))
                             (coterm--t-down-line proc-filt process))
                           (coterm--t-goto orow ocol))))
                      (?P ;; \E[P - delete chars (terminfo: dch, dch1)
                       (ins)
                       (when (zerop coterm--t-row-off)
                         (let ((opoint (point)))
                           (move-to-column (+ (coterm--t-col) (car-or-1)))
                           (delete-region opoint (point)))))
                      (?@ ;; \E[@ - insert spaces (terminfo: ich)
                       (ins)
                       (let ((width (min (car-or-1) (max 0 (- coterm--t-width
                                                              (coterm--t-col)))))
                             (opoint (point)))
                         (unless (eolp)
                           (coterm--t-apply-proc-filt proc-filt process
                                                      (make-string width ?\s))
                           (goto-char opoint))))
                      (?h ;; \E[?h - DEC Private Mode Set
                       (ins)
                       (pcase (car (ctl-params*))
                         (47 ;; (terminfo: smcup)
                          (coterm--t-switch-to-alternate-sub-buffer
                           proc-filt process t))
                         (4 ;; (terminfo: smir)
                          (setq coterm--t-insert-mode t))))
                      (?l ;; \E[?l - DEC Private Mode Reset
                       (ins)
                       (pcase (car (ctl-params*))
                         (47 ;; (terminfo: rmcup)
                          (coterm--t-switch-to-alternate-sub-buffer
                           proc-filt process nil))
                         (4 ;; (terminfo: rmir)
                          (setq coterm--t-insert-mode nil))))
                      (?n ;; \E[6n - Report cursor position (terminfo: u7)
                       (ins)
                       (process-send-string
                        process
                        ;; (terminfo: u6)
                        (format "\e[%s;%sR"
                                (1+ (coterm--t-row))
                                (1+ (coterm--t-col)))))
                      (?r ;; \E[r - Set scrolling region (terminfo: csr)
                       (ins)
                       (let ((beg (1- (car-or-1)))
                             (end (max 1 (cadr-or-0))))
                         (setq coterm--t-scroll-beg
                               (if (< beg coterm--t-height) beg 0))
                         (setq coterm--t-scroll-end
                               (if (<= 1 end coterm--t-height)
                                   end coterm--t-height))))))))))

            (cond
             ((setq match (string-match coterm--t-control-seq-prefix-regexp
                                        string ctl-end))
              (ins)
              (setq coterm--t-unhandled-fragment (substring string match)))
             ((null last-match-end)
              ;; Optimization: no substring means no string copying
              (coterm--t-insert proc-filt process string will-insert-newlines))
             (t
              (ins)))

            ;; Synchronize pmark and remove all trailing whitespace after it.
            (unless (and (zerop coterm--t-col-off) (zerop coterm--t-row-off))
              (coterm--t-insert proc-filt process "" 0))
            (set-marker pmark (point))
            (skip-chars-forward " \n")
            (when (eobp)
              (delete-region pmark (point))))

          ;; Restore point (this restores it only for the selected window)
          (goto-char restore-point)
          (unless (eq restore-point pmark)
            (set-marker restore-point nil))

          ;; Restore points of non-selected windows, if their `window-point'
          ;; was on pmark
          (let* ((sel-win (selected-window))
                 (w (next-window sel-win nil t)))
            ;; Avoid infinite loop in strange case where minibuffer window
            ;; is selected but not active.
            (while (window-minibuffer-p w)
              (setq w (next-window w nil t)))
            (while (not (eq w sel-win))
              (and (eq buf (window-buffer w))
                   (= (window-point w) old-pmark)
                   (set-window-point w pmark))
              (setq w (next-window w nil t)))
            (set-marker old-pmark nil))

          (run-hooks 'coterm-t-after-insert-hook))))))

;;; Shell integration
;;
;; We follow VTE's implementation of OSC 133, see e.g.
;; https://gitlab.gnome.org/GNOME/vte/-/blob/0.84.1/src/vte.sh.in#L109-132.
;;
;; We recommend ending the OSC 133 sequence with BEL instead of ESC \
;; because the trailing backslash can interfere with the subsequent
;; content of PS0 and PS1.

(defvar-local coterm--last-exit-status nil
  "The exit status last reported by an OSC 133 D escape sequence.")

(defun coterm--osc-133-handler (_ text)
  "Function reacting to OSC 133 escape sequences.
This is intended to be used as a member of `ansi-osc-handlers'."
  (pcase (string-to-char text)
    ;; Nothing interesting to do with cases A and B?
    (?C (coterm-auto-char-mode -1)
        (coterm-char-mode)
        (setq coterm--last-exit-status nil))
    (?D (coterm-auto-char-mode -1)
        (coterm-char-mode -1)
        (setq coterm--last-exit-status
              (when (string-match "[0-9]+" text)
                (string-to-number (match-string 0 text)))))
    (?L (unless (bolp)
          (insert ?\n)
          (backward-char)
          ;; After inserting a new line, adjust process mark and
          ;; `coterm--t-row' accordingly.
          (when-let* ((proc (get-buffer-process (current-buffer)))
                      (pmark (process-mark proc)))
            (when (= pmark (point))
              (move-marker pmark (1+ (point)))))
          (when coterm--t-row
            (cl-incf coterm--t-row))))))

(defvar coterm--formatted-exit-status
  (let ((fmt #("/SIG%s" 1 6 ( help-echo "Last command received a signal"
                              face error)))
        (map (make-hash-table))
        (i 128))
    (puthash 0 #("/0" 1 2 (help-echo "Last command succeeded" face success)) map)
    (dolist (sig (cdr (reverse (signal-names))))
      (puthash (cl-incf i) (format fmt sig) map))
    map)
  "Hash table mapping exit statuses to formatted strings.")

(defun coterm--formatted-exit-status ()
  "The value of `coterm--last-exit-status' formatted for the mode line."
  (when coterm--last-exit-status
    (or (gethash coterm--last-exit-status coterm--formatted-exit-status)
        (format #("/%s" 1 3 (face warning help-echo "Last command failed"))
                coterm--last-exit-status))))

(provide 'coterm)
;;; coterm.el ends here
