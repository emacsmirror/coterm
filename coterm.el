;; -*- lexical-binding: t; -*-

;;; Terminal emulation

(defconst coterm--t-control-seq-regexp
  ;; Differences from `term-control-seq-regexp':
  ;; Removed: \t, \032 (\C-z)
  ;; Added: OSC sequence \e] ... ; ... \e\\ (or \a)
  (concat
   ;; A control character,
   "\\(?:[\n\000\007\b\016\017]\\|\r\n?\\|"
   ;; a C1 escape coded character (see [ECMA-48] section 5.3 "Elements
   ;; of the C1 set"),
   "\e\\(?:[DM78c]\\|"
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

(defvar-local coterm--t-height nil
  "Number of lines in window.")
(defvar-local coterm--t-width nil
  "Number of columns in window.")
(defvar-local coterm--t-scroll-beg nil
  "First row of the scrolling area.")
(defvar-local coterm--t-scroll-end nil
  "First row after the end of the scrolling area.")

(defvar-local coterm--t-home-marker nil
  "Marks the \"home\" position for cursor addressing.
`coterm--t-home-offset' should be taken into account as well.")
(defvar-local coterm--t-home-offset 0
  "How many rows lower the home position actually is.
This usually is needed if `coterm--t-home-marker' is on the last
line of the buffer.")
(defvar-local coterm--t-row nil
  "Current position from home marker.
If nil, the current position is at process mark.")
(defvar-local coterm--t-col nil
  "Current position from home marker.
If nil, the current position is at process mark.")

(defvar-local coterm--t-pmark-in-sync nil
  "If t, pmark is guaranteed to be in sync.
In sync with variables `coterm--t-home-marker',
`coterm--t-home-offset', `coterm--t-row' and `coterm--t-col'")

(defvar-local coterm--t-saved-cursor nil)
(defvar-local coterm--t-insert-mode nil)
(defvar-local coterm--t-unhandled-fragment nil)

(defun coterm--t-reset-size (height width)
  (setq coterm--t-height height)
  (setq coterm--t-width width)
  (setq coterm--t-scroll-beg 0)
  (setq coterm--t-scroll-end height)
  (setq coterm--t-pmark-in-sync nil)

  (when coterm--t-row
    (setq coterm--t-col (max coterm--t-col (1- coterm--t-width)))
    (when (>= coterm--t-row coterm--t-height)
      (cl-incf coterm--t-home-offset (- coterm--t-row coterm--t-height -1))
      (setq coterm--t-row (1- coterm--t-height))
      (coterm--t-normalize-home-offset))))

(defun coterm--t-goto (row col)
  "Move point to a position that approximates ROW and COL.
Return non-nil if the position was actually reached."
  (goto-char coterm--t-home-marker)
  (and
   (zerop (forward-line
           (+ coterm--t-home-offset row)))
   (not (eobp))
   (<= col (move-to-column col))))

(defun coterm--t-apply-proc-filt (proc-filt process str)
  "Insert STR using PROC-FILT and PROCESS.
Basically, call PROC-FILT with the arguments PROCESS and STR, but
adjusting `ansi-color-context-region' beforehand."
  (when-let ((context ansi-color-context-region))
    (set-marker (cadr context) (process-mark process)))
  (funcall proc-filt process str))

(defun coterm--t-delete-region (row1 col1 &optional row2 col2)
  "Delete text between two positions.
Deletes resulting trailing whitespace as well.  ROW1, COL1, ROW2
and COL2 specify the two positions.  ROW2 and COL2 can be nil, in
which case delete to `point-max'.

This function moves point to the beginning of the deleted
region."
  (coterm--t-goto row1 col1)
  (delete-region (point)
                 (if row2
                     (progn (coterm--t-goto row2 col2) (point))
                   (point-max)))
  (when (eolp)
    (let ((opoint (point)))
      (skip-chars-backward " ") (delete-region (point) opoint)))
  (when (eobp)
    (let ((opoint (point)))
      (skip-chars-backward "\n") (delete-region (point) opoint)))
  (setq coterm--t-pmark-in-sync nil))

(defun coterm--t-open-space (proc-filt process newlines spaces)
  "Insert NEWLINES newlines and SPACES spaces at point.
Insert them using PROC-FILT and PROCESS.  Afterwards, remove
characters that were moved after the column specified by
`coterm--t-width'."
  (unless (eobp)
    (set-marker (process-mark process) (point))
    (coterm--t-apply-proc-filt
     proc-filt process
     (concat (make-string newlines ?\n)
             (unless (eolp)
               (make-string spaces ?\s))))
    ;; Delete chars that are after the width of the terminal
    (goto-char (process-mark process))
    (move-to-column coterm--t-width)
    (delete-region (point) (progn (forward-line 1) (1- (point))))
    (setq coterm--t-pmark-in-sync nil)))

(defun coterm--t-normalize-home-offset ()
  (save-excursion
    (goto-char coterm--t-home-marker)
    (let ((left-to-move (forward-line coterm--t-home-offset)))
      (unless (bolp)
        (cl-incf left-to-move)
        (forward-line 0))
      (set-marker coterm--t-home-marker (point))
      (setq coterm--t-home-offset (max 0 left-to-move)))))

(defun coterm--t-scroll-by-deletion-p ()
  (or (/= coterm--t-scroll-beg 0)
      (/= coterm--t-scroll-end coterm--t-height)))

(defun coterm--t-down-line (proc-filt process)
  "Go down one line or scroll if at bottom.
This takes into account the scroll region as specified by
`coterm--t-scroll-beg' and `coterm--t-scroll-end'.  If required
PROC-FILT and PROCESS are used to scroll with deletion and
insertion of empty lines."
  (cond
   ((and (= coterm--t-row (1- coterm--t-scroll-end))
         (coterm--t-scroll-by-deletion-p))
    (coterm--t-delete-region coterm--t-scroll-beg 0
                             (1+ coterm--t-scroll-beg) 0)
    (coterm--t-goto coterm--t-row 0)
    (coterm--t-open-space proc-filt process 1 0))
   ((and (= coterm--t-row (1- coterm--t-height))
         (coterm--t-scroll-by-deletion-p))
    ;; Behaviour of xterm
    (ignore))
   ((< coterm--t-row (1- coterm--t-height))
    (cl-incf coterm--t-row))
   (t
    (cl-incf coterm--t-home-offset)
    (coterm--t-normalize-home-offset)))
  (setq coterm--t-pmark-in-sync nil))

(defun coterm--t-up-line (proc-filt process)
  "Go up one line or scroll if at top.
This takes into account the scroll region as specified by
`coterm--t-scroll-beg' and `coterm--t-scroll-end'.  If required
PROC-FILT and PROCESS are used to scroll with deletion and
insertion of empty lines."
  (cond
   ((and (= coterm--t-row coterm--t-scroll-beg)
         (coterm--t-scroll-by-deletion-p))
    (coterm--t-delete-region (1- coterm--t-scroll-end) 0
                             coterm--t-scroll-end 0)
    (coterm--t-goto coterm--t-row 0)
    (coterm--t-open-space proc-filt process 1 0))
   ((and (= coterm--t-row 0)
         (coterm--t-scroll-by-deletion-p))
    ;; Behaviour of xterm
    (ignore))
   ((< 0 coterm--t-row)
    (cl-decf coterm--t-row))
   (t
    (coterm--t-delete-region (1- coterm--t-scroll-end) 0)
    (cl-decf coterm--t-home-offset)
    (coterm--t-normalize-home-offset)))
  (setq coterm--t-pmark-in-sync nil))

(defun coterm--t-adjust-pmark (proc-filt process)
  "Set PROCESS's mark to `coterm--t-row' and `coterm--t-col'.
If necessary, this function inserts newlines and spaces using
PROC-FILT, so use it sparingly, usually only before inserting
non-whitespace text."
  (unless coterm--t-pmark-in-sync
    (let ((pmark (process-mark process)))
      (save-excursion
        (goto-char coterm--t-home-marker)

        (let ((newlines (forward-line
                         (+ coterm--t-row coterm--t-home-offset))))
          (unless (bolp)
            (cl-incf newlines))
          (unless (zerop newlines)
            (set-marker pmark (point))
            (coterm--t-apply-proc-filt
             proc-filt process (make-string newlines ?\n))))

        (let ((col (move-to-column coterm--t-col)))
          (set-marker pmark (point))
          (when (< col coterm--t-col)
            (coterm--t-apply-proc-filt
             proc-filt process (make-string (- coterm--t-col col) ?\s))))))
    (setq coterm--t-pmark-in-sync t)
    (coterm--t-normalize-home-offset)))

(defun coterm--t-approximate-pmark (pmark)
  "Sets PMARK to point close to `coterm--t-row' and col.
Don't modify buffer.  If `coterm--t-row' and `coterm--t-col'
point to an unreachable location, locate PMARK as close to it as
possible and return nil.  Otherwise, locate PMARK exactly and
return t."
  (or coterm--t-pmark-in-sync
      (setq coterm--t-pmark-in-sync
            (save-excursion
              (prog1 (coterm--t-goto coterm--t-row coterm--t-col)
                (set-marker pmark (point)))))))

(defun coterm--t-insert (proc-filt process str newlines)
  "Insert STR using PROC-FILT and PROCESS.
Synchronise PROCESS's mark beforehand and insert at its position.
NEWLINES is the number of newlines STR contains. Unless it is
zero, insertion must happen at the end of accessible portion of
buffer and the scrolling region must cover the whole screen."
  (coterm--t-adjust-pmark proc-filt process)
  (coterm--t-apply-proc-filt proc-filt process str)
  (save-excursion
    (goto-char (process-mark process))
    (let ((column (current-column)))
      (if (zerop newlines)
          (if coterm--t-insert-mode
              ;; In insert mode, delete text outside the width of the terminal
              (progn
                (move-to-column coterm--t-width)
                (delete-region
                 (point) (progn (forward-line 1) (1- (point)))))
            ;; If not in insert mode, replace text
            (when (> column coterm--t-col)
              (delete-region
               (point)
               (progn (move-to-column (- (* 2 column) coterm--t-col))
                      (point)))))
        (cl-incf coterm--t-row newlines)
        ;; We've inserted newlines, so we must scroll if necessary
        (when (>= coterm--t-row coterm--t-height)
          (save-excursion
            (goto-char coterm--t-home-marker)
            (forward-line (+ coterm--t-home-offset
                             (- coterm--t-row coterm--t-height -1)))
            (set-marker coterm--t-home-marker (point))
            (setq coterm--t-home-offset 0)
            (setq coterm--t-row (1- coterm--t-height)))))
      (setq coterm--t-col (min column (1- coterm--t-width))))))

(defun coterm--t-maybe-adjust-from-pmark (pos)
  "Point `coterm--t-row' and `coterm--t-col' POS.
If `coterm--t-home-marker' is nil, initialize it sensibly."
  (unless coterm--t-home-marker
    (setq coterm--t-home-marker (point-min-marker))
    (setq coterm--t-home-offset 0))
  (save-excursion
    (goto-char pos)
    (setq coterm--t-col (current-column))
    (coterm--t-normalize-home-offset)
    (forward-line 0)
    (if (> (point) coterm--t-home-marker)
        ;; Here, `coterm--t-home-offset' is guaranteed to be 0
        (save-restriction
          (narrow-to-region coterm--t-home-marker (point))
          (let ((lines-left (forward-line (- 1 coterm--t-height))))
            (when (= 0 lines-left)
              (set-marker coterm--t-home-marker (point)))
            (setq coterm--t-row (+ -1 coterm--t-height lines-left))))
      (progn
        (set-marker coterm--t-home-marker (point))
        (setq coterm--t-home-offset 0)
        (setq coterm--t-row 0)))))

(defun coterm--t-emulate-terminal (proc-filt process string)
  (when-let ((fragment coterm--t-unhandled-fragment))
    (setq string (concat fragment string))
    (setq coterm--t-unhandled-fragment nil))

  (let* ((pmark (process-mark process))
         (match 0)
         (will-insert-newlines 0)
         restore-point
         last-match-end
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
         (dirty ()
           `(setq coterm--t-pmark-in-sync nil))
         (pass-through ()
           `(ignore))
         (car-or-1 ()
           `(max 1 (car ctl-params)))
         (cadr-or-0 ()
           `(or (cadr ctl-params) 0)))

      (if (not (and string
                    (setq buf (process-buffer process))
                    (buffer-live-p buf)))
          (funcall proc-filt process string)

        (with-current-buffer buf
          (setq restore-point (if (= (point) pmark) pmark (point-marker)))
          (coterm--t-maybe-adjust-from-pmark pmark)
          (save-restriction
            (widen)
            (unless (text-property-any
                     pmark (point-max) 'field 'output)
              ;; If pmark is at the end of buffer, not counting user input,
              ;; prevent changing this user input by narrowing the buffer
              (narrow-to-region (point-min) pmark))

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
                 (if (and coterm--t-pmark-in-sync
                          (= pmark (point-max))
                          (not (coterm--t-scroll-by-deletion-p)))
                     (progn (pass-through)
                            (cl-incf will-insert-newlines))
                   (ins)
                   (setq coterm--t-col 0)
                   (coterm--t-down-line proc-filt process)))
                (?\n (ins) ;; (terminfo: cud1, ind)
                     (coterm--t-down-line proc-filt process))
                (?\r (ins) ;; (terminfo: cr)
                     (setq coterm--t-col 0)
                     (dirty))
                (?\b (ins) ;; (terminfo: cub1)
                     (setq coterm--t-col (max (1- coterm--t-col) 0))
                     (dirty))
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
                             (list coterm--t-row
                                   coterm--t-col
                                   ansi-color-context-region
                                   ansi-color-context)))
                   (?8 (ins) ;; Restore cursor (terminfo: rc)
                       (when-let ((cursor coterm--t-saved-cursor))
                         (setq coterm--t-row (max (car cursor) (1- coterm--t-height)))
                         (setq cursor (cdr cursor))
                         (setq coterm--t-col (max (car cursor) (1- coterm--t-width)))
                         (setq cursor (cdr cursor))
                         (setq ansi-color-context-region (car cursor))
                         (setq ansi-color-context (cadr cursor))
                         (dirty)))
                   (?c (ins) ;; \Ec - Reset (terminfo: rs1)
                       (erase-buffer)
                       (setq ansi-color-context-region nil)
                       (setq ansi-color-context nil)
                       (setq coterm--t-home-offset 0)
                       (setq coterm--t-row 0)
                       (setq coterm--t-col 0)
                       (setq coterm--t-scroll-beg 0)
                       (setq coterm--t-scroll-end coterm--t-height)
                       (setq coterm--t-insert-mode nil))
                   (?\] (pass-through)) ;; OSC sequence, handled by comint
                   (?A (ins)) ;; Ignore term.el specific \eAnSiT sequences
                   (?\[
                    (pcase (aref string (1- ctl-end))
                      (?m ; Let `comint-output-filter-functions' handle this
                       (pass-through))
                      (char
                       (setq ctl-params (mapcar #'string-to-number
                                                (split-string ctl-params ";")))
                       (ins)
                       (pcase char
                         (?H ;; cursor motion (terminfo: cup,home)
                          (setq coterm--t-row
                                (1- (max 1 (min (car-or-1) coterm--t-height))))
                          (setq coterm--t-col
                                (1- (max 1 (min (cadr-or-0) coterm--t-width))))
                          (dirty))
                         (?A ;; cursor up (terminfo: cuu, cuu1)
                          (setq coterm--t-row (max (- coterm--t-row (car-or-1))
                                                   coterm--t-scroll-beg))
                          (dirty))
                         (?B ;; cursor down (terminfo: cud)
                          (setq coterm--t-row (min (+ coterm--t-row (car-or-1))
                                                   (1- coterm--t-scroll-end)))
                          (dirty))
                         (?C ;; \E[C - cursor right (terminfo: cuf, cuf1)
                          (setq coterm--t-col (min (+ coterm--t-col (car-or-1))
                                                   (1- coterm--t-width)))
                          (dirty))
                         (?D ;; \E[D - cursor left (terminfo: cub)
                          (setq coterm--t-col (max (- coterm--t-col (car-or-1))
                                                   0))
                          (dirty))
                         ;; \E[J - clear to end of screen (terminfo: ed, clear)
                         ((and ?J (guard (eq 0 (car ctl-params))))
                          (coterm--t-delete-region coterm--t-row coterm--t-col)
                          (dirty))
                         ((and ?J (guard (eq 1 (car ctl-params))))
                          (coterm--t-delete-region 0 0 coterm--t-row
                                                   coterm--t-col)
                          (coterm--t-open-space
                           proc-filt process coterm--t-row coterm--t-col))
                         (?J (coterm--t-delete-region 0 0))
                         ;; \E[K - clear to end of line (terminfo: el, el1)
                         ((and ?K (guard (eq 1 (car ctl-params))))
                          (coterm--t-delete-region coterm--t-row 0
                                                   coterm--t-row coterm--t-col)
                          (coterm--t-open-space proc-filt process 0 coterm--t-col))
                         (?K (coterm--t-delete-region
                              coterm--t-row coterm--t-col
                              coterm--t-row coterm--t-width))
                         (?L ;; \E[L - insert lines (terminfo: il, il1)
                          (when (<= coterm--t-scroll-beg coterm--t-row
                                    (1- coterm--t-scroll-end))
                            (let ((lines
                                   (min (- coterm--t-scroll-end coterm--t-row)
                                        (car-or-1))))
                              ;; Remove from bottom
                              (coterm--t-delete-region
                               (- coterm--t-scroll-end lines) 0
                               coterm--t-scroll-end 0)
                              ;; Insert at position
                              (coterm--t-goto coterm--t-row 0)
                              (coterm--t-open-space proc-filt process lines 0))))
                         (?M ;; \E[M - delete lines (terminfo: dl, dl1)
                          (when (<= coterm--t-scroll-beg coterm--t-row
                                    (1- coterm--t-scroll-end))
                            (let ((lines
                                   (min (- coterm--t-scroll-end coterm--t-row)
                                        (car-or-1))))
                              ;; Insert at bottom
                              (coterm--t-goto coterm--t-scroll-end 0)
                              (coterm--t-open-space proc-filt process lines 0)
                              ;; Remove at position
                              (coterm--t-delete-region
                               coterm--t-row 0
                               (+ coterm--t-row lines) 0))))
                         (?P ;; \E[P - delete chars (terminfo: dch, dch1)
                          (coterm--t-delete-region
                           coterm--t-row coterm--t-col
                           coterm--t-row (+ coterm--t-col (car-or-1))))
                         (?@ ;; \E[@ - insert spaces (terminfo: ich)
                          (let ((width (min (car-or-1) (- coterm--t-width
                                                          coterm--t-col -1))))
                            (coterm--t-goto coterm--t-row coterm--t-col)
                            (coterm--t-open-space proc-filt process 0 width)
                            (cl-incf coterm--t-col width)
                            (setq coterm--t-col (min coterm--t-col
                                                     (1- coterm--t-width)))
                            (dirty)))
                         (?h ;; \E[?h - DEC Private Mode Set
                          (pcase (car ctl-params)
                            ;; (49 ;; (terminfo: smcup)
                            ;;  (coterm--t-switch-to-alternate-sub-buffer t))
                            (4 ;; (terminfo: smir)
                             (setq coterm--t-insert-mode t))))
                         (?l ;; \E[?l - DEC Private Mode Reset
                          (pcase (car ctl-params)
                            ;; (49 ;; (terminfo: rmcup)
                            ;;  (coterm--t-switch-to-alternate-sub-buffer nil))
                            (4 ;; (terminfo: rmir)
                             (setq coterm--t-insert-mode nil))))
                         (?n ;; \E[6n - Report cursor position (terminfo: u7)
                          (process-send-string
                           process
                           ;; (terminfo: u6)
                           (format "\e[%s;%sR"
                                   (1+ coterm--t-row)
                                   (1+ coterm--t-col))))
                         (?r ;; \E[r - Set scrolling region (terminfo: csr)
                          (let ((beg (1- (car-or-1)))
                                (end (max 1 (cadr-or-0))))
                            (setq coterm--t-scroll-beg
                                  (if (< beg coterm--t-height) beg 0))
                            (setq coterm--t-scroll-end
                                  (if (<= 1 end coterm--t-height)
                                      end coterm--t-height))))))))))))

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
            (coterm--t-adjust-pmark proc-filt process)
            (widen)
            (goto-char pmark)
            (skip-chars-forward " \n")
            (when (eobp)
              (delete-region pmark (point))))

          (goto-char restore-point)
          (unless (eq restore-point pmark)
            (set-marker restore-point nil)))))))

;;; Mode functions and configuration

(defcustom coterm-term-name "eterm-color"
  "Name to use for TERM."
  :group 'comint
  :type 'string)

(defun coterm--comint-strip-CR (_)
  "Remove all \\r characters from last output."
  (save-excursion
    (goto-char comint-last-output-start)
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (while (progn (skip-chars-forward "^\r")
                    (< (point) pmark))
        (delete-char 1)))))

(defun coterm--init ()
  "Initialize current buffer for coterm."
  (when-let ((process (get-buffer-process (current-buffer))))
    (setq coterm--t-height (floor (window-screen-lines)))
    (setq coterm--t-width (window-max-chars-per-line))
    (setq coterm--t-scroll-beg 0)
    (setq coterm--t-scroll-end coterm--t-height)

    (setq-local comint-inhibit-carriage-motion t)
    (add-hook 'comint-output-filter-functions #'coterm--comint-strip-CR)

    (add-function :filter-return
                  (local 'window-adjust-process-window-size-function)
                  (lambda (size)
                    (when size
                      (coterm--t-reset-size (cdr size) (car size)))
                    size)
                  '((name . coterm-maybe-reset-size)))

    (add-function :around (process-filter process)
                  #'coterm--t-emulate-terminal)))

(defvar coterm-term-environment-function #'comint-term-environment
  "Function to calculate environment for comint processes.
If non-nil, it is called with zero arguments and should return a
list of environment variable settings to apply to comint
subprocesses.")

(defvar coterm-start-process-function #'start-file-process
  "Function called to start a comint process.
It is called with the same arguments as `start-process' and
should return a process.")

(defvar coterm-termcap-format
  "%s%s:li#%d:co#%d:cl=\\E[H\\E[J:cd=\\E[J:bs:am:xn:cm=\\E[%%i%%d;%%dH\
:nd=\\E[C:up=\\E[A:ce=\\E[K:ho=\\E[H:pt\
:al=\\E[L:dl=\\E[M:DL=\\E[%%dM:AL=\\E[%%dL:cs=\\E[%%i%%d;%%dr:sf=^J\
:dc=\\E[P:DC=\\E[%%dP:IC=\\E[%%d@:im=\\E[4h:ei=\\E[4l:mi:\
:mb=\\E[5m:mh=\\E[2m:ZR=\\E[23m:ZH=\\E[3m\
:so=\\E[7m:se=\\E[m:us=\\E[4m:ue=\\E[m:md=\\E[1m:mr=\\E[7m:me=\\E[m\
:UP=\\E[%%dA:DO=\\E[%%dB:LE=\\E[%%dD:RI=\\E[%%dC\
:kl=\\EOD:kd=\\EOB:kr=\\EOC:ku=\\EOA:kN=\\E[6~:kP=\\E[5~:@7=\\E[4~:kh=\\E[1~\
:mk=\\E[8m:cb=\\E[1K:op=\\E[39;49m:Co#256:pa#32767\
:AB=\\E[48;5;%%dm:AF=\\E[38;5;%%dm:cr=^M\
:bl=^G:do=^J:le=^H:ta=^I:se=\\E[27m:ue=\\E[24m\
:kb=^?:kD=^[[3~:sc=\\E7:rc=\\E8:r1=\\Ec:"
  "Termcap capabilities supported by coterm.")

(define-advice comint-exec-1 (:around (f &rest args) coterm-config)
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

(define-minor-mode coterm-mode
  "Better terminal emulation in comint processes."
  :global t
  :group 'comint
  (if coterm-mode

      (progn
        (add-hook 'comint-mode-hook #'coterm--init)
        (setq coterm-term-environment-function
              (lambda ()
                (let (ret)
                  (push (format "TERMINFO=%s" data-directory)
                        ret)
                  (when coterm-term-name
                    (push (format "TERM=%s" coterm-term-name) ret))
                  (when coterm-termcap-format
                    (push (format coterm-termcap-format "TERMCAP="
                                  coterm-term-name
                                  (floor (window-screen-lines))
                                  (window-max-chars-per-line))
                          ret))
                  ret)))
        (setq coterm-start-process-function
              (lambda (name buffer command &rest switches)
                (apply #'start-file-process name buffer
                       ;; Adapted from `term-exec-1'
                       "sh" "-c"
                       (format "stty -nl sane -echo 2>%s;\
if [ $1 = .. ]; then shift; fi; exec \"$@\"" null-device)
                       ".." command switches))))

    (remove-hook 'comint-exec-hook #'coterm--init)
    (setq coterm-term-environment-function #'comint-term-environment)
    (setq coterm-start-process-function #'start-file-process)))
