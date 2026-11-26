;;; calfw-plus.el --- A fuller calfw month view  -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Ypot <ypot@publik.slmail.me>
;; Package-Requires: ((emacs "28.1"))
;; Keywords: calendar, org

;;; Commentary:

;; Loaded after calfw and calfw-org, it changes the month view so the
;; tasks of a busy month can be read:
;;
;; - weeks share the screen lines by need, and a cell that cannot show
;;   every task ends in " +N";
;; - a task wraps while its cell has rows to spare, and tasks take their
;;   rows before the bars of multi-day events;
;; - task titles drop their TODO keyword (which gives them its colour),
;;   priority cookie and emphasis markers;
;; - the day number goes to the right of its title line, with the ISO week
;;   number, holidays and annotations at the left, the symbol of the lunar
;;   phase beside the day, and the first short task on the same line;
;; - equinoxes and solstices become annotations instead of days off;
;; - the month loses its toolbar and footer, and calfw buffers draw their
;;   text at `calfw-plus-text-height', task text included, so the grid
;;   keeps its alignment whatever height a TODO keyword face sets.
;;
;; Loading it is enough: it installs its advice on calfw's own functions.

;;; Code:

(require 'cl-lib)
(require 'calfw)
(require 'calfw-org)
(require 'lunar)
(require 'face-remap)

(defvar calfw-plus-text-height 130
  "Height of the text in calfw buffers.
A little smaller than the default, so a six-week month has room for
its tasks.  Set it before loading calfw-plus.")

(defvar-local calfw-plus--text-remap nil
  "Cookie of the remapping that sets the text height of this buffer.")

(defun calfw-plus--text-size ()
  "Draw the current calfw buffer's text at `calfw-plus-text-height'."
  (when calfw-plus--text-remap
    (face-remap-remove-relative calfw-plus--text-remap))
  (setq calfw-plus--text-remap
        (face-remap-add-relative 'default :height calfw-plus-text-height)))

(defun calfw-plus--fit-on-open (&rest _)
  "Redraw the calendar just opened at the size of its own window.
Calfw sizes a new calendar from the window selected before it shows,
whose text is larger than the calendar's."
  (when (derived-mode-p 'calfw-calendar-mode)
    (calfw-refresh-calendar-buffer)))

(defun calfw-plus--bare-month (orig component)
  "Call ORIG with COMPONENT, then remove the toolbar and footer lines.
The toolbar's buttons and the footer's calendar toggles all have keys,
and their lines go to the tasks."
  (let ((start (point-marker))
        (end (copy-marker (point) t)))
    (funcall orig component)
    (save-excursion
      ;; The footer is the last line drawn, after the last week's border.
      (goto-char end)
      (delete-region (max start (1- (line-beginning-position))) end)
      ;; The toolbar is the line after the title.
      (goto-char start)
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2)))
    (set-marker start nil)
    (set-marker end nil)))

(defun calfw-plus--plain-content-face (_text default-face)
  "Return DEFAULT-FACE: tasks take no colour or background from their source."
  default-face)

(defun calfw-plus--todo-keywords ()
  "Return the TODO keywords a task title can start with.
`org-todo-keywords-for-agenda' lists them once an agenda has been
built; before that, they come from `org-todo-keywords', without their
key and logging marks."
  (or org-todo-keywords-for-agenda
      (cl-loop for sequence in org-todo-keywords
               append (cl-loop for keyword in (cdr sequence)
                               unless (equal keyword "|")
                               collect (replace-regexp-in-string
                                        "(.*)\\'" "" keyword)))))

(defun calfw-plus--plain-emphasis (text)
  "Return TEXT without Org emphasis markers, their faces applied instead.
\"/Planning/\" becomes \"Planning\" in italic, as Org shows it with
`org-hide-emphasis-markers'; the markers and faces are those of
`org-emphasis-alist'."
  (dolist (regexp (list org-emph-re org-verbatim-re) text)
    (let ((start 0))
      (while (string-match regexp text start)
        (let* ((begin (match-beginning 2))
               (end (match-end 2))
               (content (copy-sequence (match-string 4 text)))
               (face (cadr (assoc (match-string 3 text) org-emphasis-alist))))
          (when face
            (add-face-text-property 0 (length content) face t content))
          (setq text (concat (substring text 0 begin) content (substring text end))
                start (+ begin (length content))))))))

(defun calfw-plus-org-summary (item)
  "Return ITEM as `calfw-org-summary-format' does, but compact.
The TODO keyword leaves the text and gives its colour to the title,
the priority cookie goes, and so do the emphasis markers, so the
title gets their room."
  (let* ((text (calfw-org-summary-format item))
         (keywords (calfw-plus--todo-keywords))
         (case-fold-search nil))
    (if (not (string-match
              (concat "\\`\\([0-9][0-9]:[0-9][0-9] \\)?"
                      (when keywords
                        (concat "\\(?:\\(" (regexp-opt keywords) "\\) +\\)?"))
                      "\\(?:\\[#[[:alnum:]]\\] *\\)?")
              text))
        (calfw-plus--plain-emphasis text)
      ;; Read the match before `calfw-plus--plain-emphasis' runs its own.
      (let* ((time (or (match-string 1 text) ""))
             (keyword (match-string 2 text))
             (face (and keyword (org-get-todo-face keyword)))
             (color (cond ((facep face) (face-attribute face :foreground nil t))
                          ((consp face) (plist-get face :foreground))))
             (title (calfw-plus--plain-emphasis (substring text (match-end 0)))))
        (when (stringp color)
          (add-face-text-property 0 (length title) (list :foreground color)
                                  t title))
        (concat time title)))))

(defun calfw-plus--wrap-line (line width)
  "Return LINE wrapped at word boundaries into rows no wider than WIDTH.
A word wider than WIDTH is split by characters."
  (cl-loop for row in (let ((sentence-end-double-space nil))
                        ;; Otherwise `fill-region' never breaks after
                        ;; "ESP. " and the word gets split by characters.
                        (calfw-render-line-breaker-wordwrap
                         line width most-positive-fixnum))
           append (if (> (string-width row) width)
                      (calfw-render-line-breaker-simple
                       row width most-positive-fixnum)
                    (list row))))

(defun calfw-plus--split-periods (lines)
  "Return (PERIODS . ITEMS): the leading period rows of LINES and the rest.
Period rows are the bars of events spanning days, and the empty rows
calfw puts above them to keep each bar at the same height all week."
  (let ((n (or (cl-position-if-not
                (lambda (line)
                  (or (equal line "") (get-text-property 0 'cfw:period line)))
                lines)
               (length lines))))
    (cons (cl-subseq lines 0 n) (nthcdr n lines))))

(defun calfw-plus--demand (lines width)
  "Return the rows LINES need in a cell WIDTH wide, every item in full."
  (let ((parts (calfw-plus--split-periods lines)))
    (+ (length (car parts))
       (cl-loop for item in (cdr parts)
                sum (length (calfw-plus--wrap-line item width))))))

(defun calfw-plus--fill-cell (lines width rows)
  "Return LINES laid out in at most ROWS rows WIDTH wide.
Period rows stay first, in the rows the items leave free: an item gets a
row before a bar does, and the bars, seen on the other days of their
span, give way.  Rows left over let the items wrap, in order.  When the
items do not all fit, the last row shows the next item, cut, followed by
\" +N\", N being the items not shown."
  (let* ((parts (calfw-plus--split-periods lines))
         (items (cdr parts))
         (n (length items))
         (keep (min (length (car parts))
                    (if items (max 0 (- rows n)) rows)))
         (periods (cl-subseq (car parts) 0 keep))
         (free (- rows keep))
         (shown (min n free))
         (hidden (- n shown)))
    (cond
     ((or (null items) (<= free 0)) periods)
     ((> hidden 0)
      (let* ((suffix (propertize (format " +%d" hidden) 'face 'bold))
             (room (max 0 (- width (string-width suffix))))
             (next (nth (1- shown) items))
             (last (if (<= (string-width next) room)
                       next
                     (concat (string-trim-right
                              (truncate-string-to-width next (max 0 (1- room))))
                             "…"))))
        (append periods (cl-subseq items 0 (1- shown))
                (list (concat last suffix)))))
     (t
      (let ((extra (- free n)) out)
        (dolist (item items)
          (let* ((wrapped (calfw-plus--wrap-line item width))
                 (take (min (length wrapped) (1+ extra))))
            (setq extra (- extra (1- take)))
            (push (if (< take (length wrapped))
                      ;; The last row it gets carries the rest, which
                      ;; calfw cuts to the cell with an ellipsis.
                      (append (cl-subseq wrapped 0 (1- take))
                              (list (mapconcat #'identity
                                               (nthcdr (1- take) wrapped)
                                               " ")))
                    wrapped)
                  out)))
        (setq out (nreverse out))
        (when (cl-some #'cdr out)
          ;; Some item wraps: mark where each one ends.
          (dolist (rows-of-item (butlast out))
            (calfw--render-add-item-separator-sign rows-of-item)))
        (append periods (apply #'append out)))))))

(defun calfw-plus--allocate (needs wants total)
  "Return the content rows of each week, summing TOTAL.
NEEDS is the rows each week needs to give each task and bar a row of
its own, WANTS the rows it needs to show them in full.  Every week gets
one row; then, one row at a time, the week missing the most of its
NEEDS; the rest goes in proportion to the rows each week WANTS beyond
one, so a light week stays small and a busy one grows even when all
fits.  When no week wants more, the rest goes evenly.  Ties and
rounding favour the earlier week."
  (let* ((n (length needs))
         (rows (make-list n 1))
         (left (- total n)))
    ;; Every task a row of its own first.
    (catch 'served
      (while (> left 0)
        (let ((best nil) (best-gap 0))
          (dotimes (i n)
            (let ((gap (- (nth i needs) (nth i rows))))
              (when (> gap best-gap) (setq best i best-gap gap))))
          (unless best (throw 'served nil))
          (cl-incf (nth best rows))
          (cl-decf left))))
    ;; Then the rest, by how much each week wants beyond one row.
    (when (> left 0)
      (let* ((weights (mapcar (lambda (w) (max 0 (1- w))) wants))
             (sum (apply #'+ weights))
             (weights (if (> sum 0) weights (make-list n 1)))
             (sum (if (> sum 0) sum n))
             (shares (mapcar (lambda (w) (/ (* left w) (float sum))) weights))
             (extra (mapcar #'floor shares))
             (rest (- left (apply #'+ extra)))
             (order (sort (number-sequence 0 (1- n))
                          (lambda (a b)
                            (let ((ra (- (nth a shares) (nth a extra)))
                                  (rb (- (nth b shares) (nth b extra))))
                              (or (> ra rb) (and (= ra rb) (< a b))))))))
        (dolist (i (cl-subseq order 0 rest))
          (cl-incf (nth i extra)))
        (setq rows (cl-mapcar #'+ rows extra))))
    rows))

(defvar calfw-plus--screen-lines nil
  "Screen lines of the month view being drawn, nil in other views.")

(defun calfw-plus--month-lines (orig component)
  "Call ORIG with COMPONENT, letting the weeks know the screen lines."
  (let ((calfw-plus--screen-lines
         (calfw-dest-height (calfw-component-dest component))))
    (funcall orig component)))

(defvar calfw-plus--week-rows nil
  "Content rows of the weeks still to draw, or `measure' while measuring.
While measuring, `calfw-plus--week-demands' collects what each week asks.")

(defvar calfw-plus--week-demands nil
  "(NEED . WANT) of each week, newest first, collected while measuring.
See `calfw-plus--allocate'.")

(defun calfw-plus--fit-weeks (orig model param title-func)
  "Call ORIG with MODEL, PARAM and TITLE-FUNC, sharing rows by need.
The weeks share the content rows calfw gives them all, and in the month
view the screen lines calfw leaves unused, so the month still fits on
the screen: a first run in a scratch buffer measures what each week
asks, and the real one draws each week with its share."
  (let* ((weeks (length (calfw--k 'weeks model)))
         (total (max (* weeks (1- (calfw--k 'cell-height param)))
                     ;; Title, top border, day names and their border take
                     ;; a line each (`calfw-plus--bare-month' removes the
                     ;; toolbar and footer), every week its title and
                     ;; bottom border, and one is left spare.
                     (if calfw-plus--screen-lines
                         (- calfw-plus--screen-lines 5 (* 2 weeks))
                       0)))
         (demands (let ((calfw-plus--week-rows 'measure)
                        (calfw-plus--week-demands nil))
                    (with-temp-buffer (funcall orig model param title-func))
                    (nreverse calfw-plus--week-demands))))
    (if (or (< total weeks) (/= (length demands) weeks))
        (funcall orig model param title-func)
      (let ((calfw-plus--week-rows
             (calfw-plus--allocate (mapcar #'car demands) (mapcar #'cdr demands) total)))
        (funcall orig model param title-func)))))

(defun calfw-plus--week-columns (orig day-columns param)
  "Call ORIG with DAY-COLUMNS and PARAM at the height of this week's share.
While measuring, record what the week asks and draw nothing."
  (cond
   ((eq calfw-plus--week-rows 'measure)
    (let ((width (calfw--k 'cell-width param)))
      (push (cons (cl-loop for (_date _title . lines) in day-columns
                           maximize (length lines))
                  (cl-loop for (_date _title . lines) in day-columns
                           maximize (calfw-plus--demand lines width)))
            calfw-plus--week-demands)))
   ((consp calfw-plus--week-rows)
    (funcall orig day-columns
             (cons (cons 'cell-height (1+ (pop calfw-plus--week-rows)))
                   param)))
   (t (funcall orig day-columns param))))

(defun calfw-plus--season (name)
  "Return the season holiday NAME announces, or nil if it is not one.
`solar-equinoxes-solstices' names an equinox or a solstice after the
seasons of `solar-n-hemi-seasons' or `solar-s-hemi-seasons', followed by
the time."
  (seq-find (lambda (season) (string-prefix-p season name))
            (append (bound-and-true-p solar-n-hemi-seasons)
                    (bound-and-true-p solar-s-hemi-seasons))))

(defun calfw-plus--seasons-as-annotations (data)
  "Return DATA with its equinoxes and solstices moved to its annotations.
DATA is the alist `calfw--view-model-make-common-data' returns.  Calfw
shows a holiday as a day off, in red; an annotation goes quietly beside
the day number, here only the first word of the season name, which
fits beside it (\"Solsticio\"): the month tells which one."
  (let ((annotations (alist-get 'annotations data))
        kept)
    (dolist (holiday (alist-get 'holidays data))
      (let ((season (calfw-plus--season (cadr holiday))))
        (if (not season)
            (push holiday kept)
          (let ((word (car (split-string season)))
                (cell (assoc (car holiday) annotations)))
            (if cell
                (setcdr cell (concat (cdr cell) "/" word))
              (push (cons (car holiday) word) annotations))))))
    (setf (alist-get 'holidays data) (nreverse kept)
          (alist-get 'annotations data) annotations)
    data))

(defun calfw-plus--week-label (date)
  "Return the ISO week number of DATE as \"S48\"."
  (format "S%d" (car (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian date)))))

(defconst calfw-plus--moon-symbols ["○" "◑" "●" "◐"]
  "Symbol of each lunar phase: new, first quarter, full, last quarter.
On a dark background the filled part shows the lit part of the moon.
Cascadia Mono draws all four, so the grid stays aligned.")

(defvar calfw-plus--moon-phases (make-hash-table :test #'equal)
  "Lunar phases of each (MONTH . YEAR) already computed, as (DATE . PHASE).")

(defun calfw-plus--moon (date)
  "Return the lunar phase that falls on DATE, 0 to 3, or nil.
The phases are those of `lunar-phase-list', in the calendar's time zone."
  (require 'lunar)
  (let* ((key (cons (calendar-extract-month date) (calendar-extract-year date)))
         (phases (or (gethash key calfw-plus--moon-phases)
                     (puthash key
                              (mapcar (lambda (phase) (cons (car phase) (nth 2 phase)))
                                      (lunar-phase-list (car key) (cdr key)))
                              calfw-plus--moon-phases))))
    (cdr (assoc date phases))))

(defun calfw-plus--title-parts (title)
  "Return (DAY COUNT HOLIDAY), the parts of the calfw day TITLE.
Calfw writes a day title as \" DAY (COUNT) HOLIDAY\", the count only on a
day with tasks and the holiday only on a holiday, leaving their spaces
when they are missing; a missing part is nil.  Return nil if TITLE has
another form."
  (when (string-match
         "\\` \\([0-9]+\\) *\\(([0-9]+)\\)? *\\(.*?\\) *\\'" title)
    (list (match-string 1 title) (match-string 2 title)
          (let ((holiday (match-string 3 title)))
            (unless (string-empty-p holiday) holiday)))))

(defun calfw-plus--title-layout (date title annotation width)
  "Return (LEFT . RIGHT), the title line of DATE in a cell WIDTH wide.
TITLE and ANNOTATION are what calfw puts at the left and the right.
The right gets the day number, after the symbol of the lunar phase on
the days one falls; the left, the ISO week number in grey on the first
day shown each week, the holiday and the annotation, cut so the right
always fits.  The task count goes: a cell that cannot
show all its tasks says how many are missing."
  (let ((parts (calfw-plus--title-parts title)))
    (if (not parts)
        (cons title annotation)
      (let* ((moon (calfw-plus--moon date))
             (right (if moon
                        (concat (aref calfw-plus--moon-symbols moon) " " (nth 0 parts))
                      (nth 0 parts)))
             (week (and (= (calendar-day-of-week date) (car calfw-week-days-list))
                        (propertize (calfw-plus--week-label date) 'face 'shadow)))
             (left (mapconcat #'identity
                              (delq nil (list week (nth 2 parts)
                                              (and annotation
                                                   (not (string-empty-p annotation))
                                                   annotation)))
                              " "))
             ;; A space opens the line, as in calfw's own titles, and
             ;; one parts left and right.
             (room (max 0 (- width (string-width right) 2))))
        (cons (concat " " (if (> (string-width left) room)
                              (truncate-string-to-width left room nil nil "…")
                            left))
              right)))))

(defun calfw-plus--lift-task (title lines width)
  "Return ((LEFT . RIGHT) . LINES) with the first task in the title, or nil.
TITLE is the day title as `calfw-plus--title-layout' lays it out, LINES
the day's period rows and tasks, WIDTH the cell's.  The first task moves
up to the title line when the left holds nothing but the week number
and the task fits there whole; a longer one stays below, where it can
wrap."
  (let* ((parts (calfw-plus--split-periods lines))
         (task (cadr parts))
         (left (car title))
         (right (cdr title))
         (bare (string-trim left)))
    (when (and task
               (or (string-empty-p bare) (string-match-p "\\`S[0-9]+\\'" bare)))
      (let* ((lead (if (string-empty-p bare) left (concat left " ")))
             ;; Two spaces part the task from the day, so the day
             ;; number does not read as the end of the task.
             (room (- width (string-width right) 2 (string-width lead))))
        (when (<= (string-width task) room)
          (cons (cons (concat lead task) right)
                (append (car parts) (cddr parts))))))))

(defun calfw-plus--layout-titles (args)
  "Return ARGS of `calfw--render-columns' with each day title laid out.
See `calfw-plus--title-layout'.  Every day but today also lifts its
first task into the title when it fits (`calfw-plus--lift-task'): today's
title colours would hide the task's own."
  (let ((width (calfw--k 'cell-width (nth 1 args)))
        (today (calendar-current-date)))
    (list (mapcar (lambda (column)
                    (pcase-let ((`(,date (,title . ,annotation) . ,lines) column))
                      (if (not date)
                          column
                        (let* ((laid (calfw-plus--title-layout date title annotation width))
                               (lifted (and (not (equal date today))
                                            (calfw-plus--lift-task laid lines width))))
                          (if lifted
                              `(,date ,(car lifted) ,@(cdr lifted))
                            `(,date ,laid ,@lines))))))
                  (nth 0 args))
          (nth 1 args))))

;;; Activation

;; Calfw sizes cells by counting characters.  Task text keeps the height of
;; the calendar's text, whatever its TODO keyword face says
;; (`calfw-sanitized-face' already drops their :box), so a cell is as wide
;; on screen as calfw counts.
(set-face-attribute 'calfw-sanitized-face nil :height calfw-plus-text-height)
(add-hook 'calfw-calendar-mode-hook #'calfw-plus--text-size)
(advice-add 'calfw-open-calendar-buffer :after #'calfw-plus--fit-on-open)
;; The month without toolbar and footer: their lines go to the tasks.
(advice-add 'calfw--view-month :around #'calfw-plus--bare-month)
;; Tasks in the default content colour, without the source background.
(advice-add 'calfw--render-get-face-content :override
            #'calfw-plus--plain-content-face)
;; Compact task titles: the TODO keyword as colour, no priority or markers.
(setq calfw-org-schedule-summary-transformer #'calfw-plus-org-summary)
;; Weeks share the screen by need, and a cell that cannot show all its
;; tasks ends in " +N".
(advice-add 'calfw--view-month :around #'calfw-plus--month-lines)
(advice-add 'calfw--render-calendar-cells-weeks :around #'calfw-plus--fit-weeks)
(advice-add 'calfw--render-columns :around #'calfw-plus--week-columns)
(advice-add 'calfw--render-break-lines :override #'calfw-plus--fill-cell)
;; Equinoxes and solstices beside the day number, not as days off.
(advice-add 'calfw--view-model-make-common-data :filter-return
            #'calfw-plus--seasons-as-annotations)
;; Day titles: week number, holiday and annotation at the left, day number
;; at the right.  Added last, so it lays the titles out before
;; `calfw-plus--week-columns' measures the weeks.
(advice-add 'calfw--render-columns :filter-args #'calfw-plus--layout-titles)

(provide 'calfw-plus)
;;; calfw-plus.el ends here
