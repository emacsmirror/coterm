;; -*- lexical-binding: t; -*-

;;; Terminal emulation

;; Removed \t, \032 (\C-z), \eAnSiT
(defconst coterm-t-control-seq-regexp
  (concat
   ;; A control character,
   "\\(?:[\r\n\000\007\b\016\017]\\|"
   ;; a C1 escape coded character (see [ECMA-48] section 5.3 "Elements
   ;; of the C1 set"),
   "\e\\(?:[DM78c]\\|"
   ;; or an escape sequence (section 5.4 "Control Sequences"),
   "\\[\\([\x30-\x3F]*\\)[\x20-\x2F]*[\x40-\x7E]\\)\\)")
  "Regexp matching control sequences handled by term.el.")

(defconst coterm-t-control-seq-prefix-regexp "\e")

(defvar-local coterm-t-height t
  "Number of lines in window.")
(defvar-local coterm-t-width nil
  "Number of columns in window.")

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
(defvar-local coterm-t-insert-mode nil)

(defvar-local coterm--t-unhandled-fragment nil)

(defun coterm--t-reset-size (height width)
  (setq coterm-t-height height)
  (setq coterm-t-width width))

(defun coterm--t-point (row col)
  "Return position that approximates ROW and COL."
  (save-excursion
    (goto-char coterm--t-home-marker)
    (and
     (zerop (forward-line
             (+ coterm--t-home-offset row)))
     (not (eobp))
     (move-to-column col))
    (point)))

(defun coterm--t-delete-region (row1 col1 row2 col2)
  (delete-region (coterm--t-point row1 col1)
                 (coterm--t-point row2 col2))
  (setq coterm--t-pmark-in-sync nil))

(defun coterm--t-clear-region (proc-filt process row1 col1 row2 col2)
  (save-excursion
    (let ((p1 (coterm--t-point row1 col1))
          (p2 (coterm--t-point row2 col2))
          row col h)
      (if (> p2 p1)
          (setq row row1 col col1
                h (- row2 row1))
        (setq row row2 col col2
              h (- row1 row2)))
      (delete-region p1 p2)
      (coterm--t-open-space proc-filt process row col h (abs (- col2 col1)))
      (setq coterm--t-pmark-in-sync nil))))

(defun coterm--t-open-space (proc-filt process row col height width)
  (save-excursion
    (goto-char (coterm--t-point row col))
    (unless (eobp)
      (set-marker (process-mark process) (point))
      (funcall
       proc-filt process
       (concat (make-string height ?\n)
               (unless (eolp)
                 (make-string (+ width (if (= row 0) 0 col)) ?\s))))
      (setq coterm--t-pmark-in-sync nil))))

(defun coterm--t-normalize-home-offset ()
  (save-excursion
    (goto-char coterm--t-home-marker)
    (let ((left-to-move (forward-line coterm--t-home-offset)))
      (unless (bolp)
        (cl-incf left-to-move)
        (forward-line 0))
      (set-marker coterm--t-home-marker (point))
      (setq coterm--t-home-offset left-to-move))))

(defun coterm-t-scroll-into-view ()
  (let ((height coterm-t-height)
        (row coterm--t-row)
        (home coterm--t-home-marker))
    (cond
     ((>= row height)
      (save-excursion
        (goto-char home)
        (let ((left-to-move (forward-line (+ coterm--t-home-offset
                                             (- row height -1)))))
          (unless (bolp)
            (cl-incf left-to-move)
            (forward-line 0))
          (set-marker home (point))
          (setq coterm--t-home-offset left-to-move)
          (setq coterm--t-row (1- height)))))
     ((< row 0)
      (save-excursion
        (goto-char home)
        (forward-line row)
        (set-marker home (point))
        (cl-incf coterm--t-home-offset 0)
        (setq coterm--t-row 0))))))

(defun coterm-t-down (n)
  (cl-incf coterm--t-row n)
  (setq coterm--t-pmark-in-sync nil)
  (coterm-t-scroll-into-view))

;; Moves pmark, inserts
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
            (funcall proc-filt process (make-string newlines ?\n))))

        (let ((col (move-to-column coterm--t-col)))
          (set-marker pmark (point))
          (when (< col coterm--t-col)
            (funcall proc-filt process
                     (make-string (- coterm--t-col col) ?\s))))))
    (setq coterm--t-pmark-in-sync t)
    (coterm--t-normalize-home-offset)))

;; Moves pmark
(defun coterm--t-approximate-pmark (pmark)
  "Sets PMARK to point close to `coterm--t-row' and col.
Don't modify buffer.  If `coterm--t-row' and `coterm--t-col'
point to an unreachable location, locate PMARK as close to it as
possible and return nil.  Otherwise, locate PMARK exactly and
return t."
  (or coterm--t-pmark-in-sync
      (save-excursion
        (goto-char coterm--t-home-marker)
        (setq coterm--t-pmark-in-sync
              (prog1
                  (and
                   (zerop (forward-line
                           (+ coterm--t-home-offset coterm--t-row)))
                   (bolp)
                   (<= coterm--t-col (move-to-column coterm--t-col)))
                (set-marker pmark (point)))))))

;; Moves pmark and inserts
(defun coterm--t-insert (proc-filt process str newlines)
  "Insert STR using PROC-FILT and PROCESS.
Synchronise PROCESS's mark beforehand and insert at its position.
NEWLINES is the number of newlines STR contains. Unless it is
zero, insertion should happen at the end of accessible portion of
buffer."
  (coterm--t-adjust-pmark proc-filt process)
  (funcall proc-filt process str)
  (save-excursion
    (goto-char (process-mark process))
    (let ((column (current-column)))
      (if (zerop newlines)
          (if coterm-t-insert-mode
              (progn
                (move-to-column coterm-t-width)
                (delete-region
                 (point) (progn (forward-line 1) (1- (point)))))
            (delete-region
             (point)
             (progn (move-to-column (- (* 2 column) coterm--t-col)) (point))))
        (cl-incf coterm--t-row newlines)
        (coterm-t-scroll-into-view))
      (setq coterm--t-col column))))

;; Depends on pmark
(defun coterm--t-maybe-adjust-from-pmark (pos)
  "If `coterm--t-row' and col are nil, point them to POS.
`coterm--t-home-marker' may also be nil, in which case,
initialize it sensibly."
  (unless coterm--t-home-marker
    (setq coterm--t-home-marker (point-min-marker))
    (setq coterm--t-home-offset 0))
  (unless coterm--t-row
    (save-excursion
      (goto-char pos)
      (setq coterm--t-col (current-column))
      (coterm--t-normalize-home-offset)
      (forward-line 0)
      (if (> (point) coterm--t-home-marker)
          (save-restriction
            (narrow-to-region coterm--t-home-marker (point))
            (let ((lines-left (forward-line (- 1 coterm-t-height))))
              (when (= 0 lines-left)
                (set-marker coterm--t-home-marker (point)))
              (setq coterm--t-row (+ -1 coterm-t-height lines-left))))
        (progn
          (set-marker coterm--t-home-marker (point))
          (setq coterm--t-home-offset 0)
          (setq coterm--t-row 0))))
    (setq coterm--t-pmark-in-sync t)))

(defun coterm-t-emulate-terminal (proc-filt process string)
  (when-let ((fragment coterm--t-unhandled-fragment))
    (setq string (concat fragment string))
    (setq coterm--t-unhandled-fragment nil))

  (let* ((pmark (process-mark process))
         (match 0)
         (will-insert-newlines 0)
         restore-point
         last-match-end
         buf fragment
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
           `(ignore)))

      (if (not (and string
                    (setq buf (process-buffer process))
                    (buffer-live-p buf)))
          (funcall proc-filt process string)

        (with-current-buffer buf
          (setq restore-point (if (= (point) pmark) pmark (point-marker)))
          (coterm--t-maybe-adjust-from-pmark pmark)
          (save-restriction
            (unless (text-property-any
                     pmark (point-max) 'field 'output)
              ;; If pmark is at the end of buffer, not counting user input,
              ;; prevent changing this user input by narrowing the buffer
              (narrow-to-region (point-min) pmark))

            (while (setq match (string-match coterm-t-control-seq-regexp
                                             string ctl-end))
              (setq ctl-params (match-string 1 string))
              (setq ctl-end (match-end 0))

              (pcase (aref string match)
                ((and ?\n
                      (guard coterm--t-pmark-in-sync)
                      (guard (= pmark (point-max))))
                 (pass-through)
                 (cl-incf will-insert-newlines))
                (?\n (ins)
                     (coterm-t-down 1)
                     (setq coterm--t-col 0))
                (?\r (ins) ;; (terminfo: cr)
                     (setq coterm--t-col 0)
                     (dirty))
                (?\b (ins) ;; (terminfo: cub1)
                     (cl-decf coterm--t-col 1)
                     (dirty))
                (?\C-g (ins) ;; (terminfo: bel)
                       (beep t))
                ;; Ignore NUL, Shift Out, Shift In.
                ((or ?\0 14 15 '()) (ins))
                (?\e
                 (pcase (aref string (1+ match))
                   (?D (ins)
                       (coterm-t-down 1))
                   (?M (ins) ;; (terminfo: ri)
                       (coterm-t-down -1))
                   (?7 (ins) ;; Save cursor (terminfo: sc)
                       (coterm-t-scroll-into-view)
                       (setq coterm--t-saved-cursor
                             (list coterm--t-row
                                   coterm--t-col
                                   (when (boundp 'ansi-color-context-region)
                                     (list ansi-color-context-region)))))
                   (?8 (ins) ;; Restore cursor (terminfo: rc)
                       (when-let ((cursor coterm--t-saved-cursor))
                         (setq coterm--t-row (car cursor))
                         (setq cursor (cdr cursor))
                         (setq coterm--t-col (car cursor))
                         (setq cursor (cdr cursor))
                         (when (car cursor)
                           (setq coterm--t-row (caar cursor)))))
                   (?c (ins) ;; \Ec - Reset (terminfo: rs1)
                       (erase-buffer)
                       (when (boundp 'ansi-color-context-region)
                         (setq ansi-color-context-region nil))
                       (setq coterm--t-row 0)
                       (setq coterm--t-col 0)
                       (setq coterm-t-insert-mode nil))
                   (?\[
                    (pcase (aref string (1- ctl-end))
                      (?m    ; Let `comint-output-filter-functions' handle this
                       (pass-through))
                      (char
                       (setq ctl-params (mapcar #'string-to-number
                                                (split-string ctl-params ";")))
                       (pcase char
                         (?H (ins) ;; cursor motion (terminfo: cup,home)
                             (setq coterm--t-row
                                   (1- (max 1 (min (or (nth 0 ctl-params) 0) coterm-t-height))))
                             (setq coterm--t-col
                                   (1- (max 1 (min (or (nth 1 ctl-params) 0) coterm-t-width))))
                             (dirty))
                         (?A (ins) ;; cursor up (terminfo: cuu, cuu1)
                             (cl-decf coterm--t-row (car ctl-params))
                             (setq coterm--t-row (max coterm--t-row 0))
                             (dirty))
                         (?B (ins) ;; cursor down (terminfo: cud)
                             (cl-incf coterm--t-row (car ctl-params))
                             (setq coterm--t-row (min coterm--t-row (1- coterm-t-height)))
                             (dirty))
                         (?C (ins) ;; \E[C - cursor right (terminfo: cuf, cuf1)
                             (cl-incf coterm--t-col (car ctl-params))
                             (setq coterm--t-col (min coterm--t-col (1- coterm-t-width)))
                             (dirty))
                         (?D (ins) ;; \E[D - cursor left (terminfo: cub)
                             (cl-decf coterm--t-col (car ctl-params))
                             (setq coterm--t-col (max coterm--t-col 0))
                             (dirty))
                         ;; \E[J - clear to end of screen (terminfo: ed, clear)
                         ((and ?J (guard (eq 0 (car ctl-params))))
                          (ins)
                          (delete-region (coterm--t-point coterm--t-row coterm--t-col)
                                         (point-max))
                          (dirty))
                         ((and ?J (guard (eq 1 (car ctl-params))))
                          (ins)
                          (coterm--t-clear-region
                           proc-filt process 0 0 coterm--t-row coterm--t-col))
                         (?J
                          (ins)
                          (delete-region (coterm--t-point 0 0) (point-max))
                          (dirty))
                         (?K ;; \E[K - clear to end of line (terminfo: el, el1)
                          (ins)
                          (coterm--t-clear-region
                           proc-filt process
                           coterm--t-row coterm--t-col
                           coterm--t-row (if (eq 1 (car ctl-params)) 0
                                           coterm-t-width)))
                         (?L ;; \E[L - insert lines (terminfo: il, il1)
                          ;; Remove from bottom
                          (coterm--t-delete-region
                           (- coterm-t-height (car ctl-params)) 0
                           coterm-t-height 0)
                          ;; Insert at position
                          (coterm--t-open-space
                           proc-filt process coterm--t-row 0
                           (car ctl-params) 0))
                         (?M ;; \E[M - delete lines (terminfo: dl, dl1)
                          ;; Insert at bottom
                          (coterm--t-open-space
                           proc-filt process coterm-t-height 0
                           (car ctl-params) 0)
                          ;; Remove at position
                          (coterm--t-delete-region
                           coterm--t-row 0
                           (+ coterm--t-row (car ctl-params)) 0))))))))))

            (cond
             ((setq match (string-match coterm-t-control-seq-prefix-regexp
                                        string ctl-end))
              (while (setq match (string-match coterm-t-control-seq-prefix-regexp
                                               string (1+ match)))
                (setq ctl-end (1+ match)))
              (ins)
              (setq coterm--t-unhandled-fragment (substring string last-match-end)))
             ((and (null last-match-end)
                   ;; TODO Smo na koncu, razen vhoda
                   )
              ;; Optimization, no substring means no string copying
              (coterm--t-insert proc-filt process string will-insert-newlines))
             (t
              (ins)))

            ;; Here, we are at the end of process filtering. `coterm--t-row'
            ;; and `coterm--t-col' may point to a not yet existent location
            ;; after (point-max).  First, we move `pmark' as close to this
            ;; position as possible.
            (if (coterm--t-approximate-pmark pmark)
                ;; If we succeed, clear `coterm--t-row' and col.  On next
                ;; output, we will initialize them to point to `pmark'.
                (setq coterm--t-row nil
                      coterm--t-col nil
                      coterm--t-pmark-in-sync nil)
              ;; If we don't succeed, leave the variables unchanged.  They will
              ;; be used to handle next output.
              (ignore)))

          (goto-char restore-point)
          (unless (eq restore-point pmark)
            (set-marker restore-point nil)))))))

;;; Mode functions and configuration

(defcustom coterm-term-name ;; term-term-name
  "dumb"
  "Name to use for TERM.")

(defun coterm-comint-exec-h ()
  (when-let (((derived-mode-p #'comint-mode))
             (process (get-buffer-process (current-buffer))))
    (setq coterm-t-height (floor (window-screen-lines)))
    (setq coterm-t-width (window-max-chars-per-line))
    (setq-local comint-inhibit-carriage-motion t)

    (add-function :filter-return
                  (local 'window-adjust-process-window-size-function)
                  (lambda (size)
                    (when size
                      (coterm--t-reset-size (cdr size) (car size)))
                    size)
                  '((name . coterm-maybe-reset-size)))

    (add-function :around (process-filter process)
                  #'coterm-t-emulate-terminal))

  ;; After `term-exec-hook', the major mode is often changed (for example into
  ;; `shell-mode', which kills our local variables, so set them again after
  ;; changing major modes.
  (add-hook 'after-change-major-mode-hook #'coterm-comint-exec-h nil t))

(put #'coterm-comint-exec-h 'permanent-local-hook t)

(defvar coterm--old-comint-terminfo-terminal nil)

(define-minor-mode coterm-mode
  "Better terminal emulation in comint processes."
  :global t
  (if coterm-mode
      (progn
        (add-hook 'comint-exec-hook #'coterm-comint-exec-h)
        (when coterm-term-name
          (unless coterm--old-comint-terminfo-terminal
            (setq coterm--old-comint-terminfo-terminal
                  (list comint-terminfo-terminal)))
          (setq comint-terminfo-terminal coterm-term-name)))
    (remove-hook 'comint-exec-hook #'coterm-comint-exec-h)
    (when coterm--old-comint-terminfo-terminal
      (setq comint-terminfo-terminal
            (car coterm--old-comint-terminfo-terminal))
      (setq coterm--old-comint-terminfo-terminal nil))))
