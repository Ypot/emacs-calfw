;;; test-calfw-plus.el --- Tests of calfw-plus  -*- lexical-binding: t; coding: utf-8 -*-
;; Usage: emacs -Q --batch -l test-calfw-plus.el
;; Loads calfw-plus from this directory and checks how weeks share rows, how
;; a cell lays out its tasks, the compact task titles, the day titles, and
;; the equinoxes and solstices as annotations.
(require 'ert)
(package-activate-all)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'calfw-plus)
(require 'holidays)
(require 'solar)

(defvar displayed-month)
(defvar displayed-year)

;; The seasons as named in Spanish, and holidays with a solstice among them.
(setq solar-n-hemi-seasons '("Equinoccio de Primavera" "Solsticio de Verano"
                             "Equinoccio de Otoño" "Solsticio de Invierno")
      calendar-holidays '((holiday-fixed 12 25 "Fest") (solar-equinoxes-solstices)))

;;; Rows and cells

(defun test--plain (rows) (mapcar #'substring-no-properties rows))

(defun test--fg (face)
  "Return the :foreground of FACE, a plist or a list of faces."
  (cond ((keywordp (car-safe face)) (plist-get face :foreground))
        ((consp face) (cl-some #'test--fg face))))

(ert-deftest calfw-cells/allocate-gives-the-busy-week-the-rows ()
  ;; Light weeks keep one row even when everything would fit.
  (should (equal (calfw-plus--allocate '(1 1 6 1 1 1) '(1 1 6 1 1 1) 12) '(1 1 7 1 1 1)))
  (should (equal (calfw-plus--allocate '(1 4 1 2 1) '(1 4 1 2 1) 15) '(1 9 1 3 1)))
  (should (equal (calfw-plus--allocate '(10 1 1) '(10 1 1) 6) '(4 1 1)))
  (should (equal (calfw-plus--allocate '(3 3) '(3 3) 4) '(2 2)))
  ;; Nobody asks for more: the rest goes evenly.
  (should (equal (calfw-plus--allocate '(0 0 0) '(0 0 0) 7) '(3 2 2)))
  (should (equal (calfw-plus--allocate '(1 1) '(1 1) 5) '(3 2))))

(ert-deftest calfw-cells/allocate-gives-every-task-a-row-before-wrapping ()
  ;; The second week has four short tasks; the first, one task that
  ;; would wrap into six rows.  The four tasks get their rows first.
  (should (equal (calfw-plus--allocate '(1 4) '(6 4) 6) '(2 4)))
  (should (equal (calfw-plus--allocate '(1 4) '(6 4) 5) '(1 4)))
  ;; Too few rows for every task: the week missing the most goes first.
  (should (equal (calfw-plus--allocate '(2 6 2) '(2 8 3) 9) '(2 6 1))))

(ert-deftest calfw-cells/allocate-keeps-the-total ()
  (dolist (case '(((1 1 6 1 1 1) . 12) ((9 0 4 2 0) . 10) ((1 1) . 2) ((5 5 5 5 5 5) . 6)
                  ((2 3 5 7 11 13) . 30) ((1 2 1 2) . 23)))
    (let ((rows (calfw-plus--allocate (car case) (mapcar #'1+ (car case)) (cdr case))))
      (should (= (apply #'+ rows) (cdr case)))
      (should (cl-every (lambda (r) (>= r 1)) rows)))))

(ert-deftest calfw-cells/fill-cell-shows-how-many-are-missing ()
  (let ((items '("Uno" "Dos" "Tres" "Cuatro" "Cinco")))
    (should (equal (test--plain (calfw-plus--fill-cell items 20 1)) '("Uno +4")))
    (should (equal (test--plain (calfw-plus--fill-cell items 20 3)) '("Uno" "Dos" "Tres +2")))
    (should (equal (test--plain (calfw-plus--fill-cell items 20 5)) items))))

(ert-deftest calfw-cells/fill-cell-cuts-the-last-item-to-fit-the-count ()
  (let ((rows (calfw-plus--fill-cell '("Una tarea bastante larga" "Otra") 14 1)))
    (should (equal (test--plain rows) '("Una tarea… +1")))
    (should (<= (string-width (car rows)) 14))))

(ert-deftest calfw-cells/fill-cell-gives-tasks-rows-before-bars ()
  (let ((bar (propertize "(Feria------)" 'cfw:period t)))
    ;; Rows for the bars and the tasks: the bars first, in their place.
    (should (equal (test--plain (calfw-plus--fill-cell (list "" bar "Uno" "Dos") 20 4))
                   '("" "(Feria------)" "Uno" "Dos")))
    ;; Too few: the bars give way to the tasks.
    (should (equal (test--plain (calfw-plus--fill-cell (list "" bar "Uno" "Dos" "Tres") 20 3))
                   '("Uno" "Dos" "Tres")))
    (should (equal (test--plain (calfw-plus--fill-cell (list bar "Uno" "Dos") 20 2))
                   '("Uno" "Dos")))
    (should (equal (test--plain (calfw-plus--fill-cell (list bar "Uno" "Dos" "Tres") 20 2))
                   '("Uno" "Dos +1")))
    ;; No tasks: the bars keep their rows.
    (should (equal (test--plain (calfw-plus--fill-cell (list "" bar) 20 1))
                   '("")))))

(ert-deftest calfw-cells/fill-cell-wraps-with-rows-to-spare ()
  (should (equal (test--plain (calfw-plus--fill-cell '("08:00 Repaso semanal de proyectos") 14 3))
                 '("08:00 Repaso" "semanal de" "proyectos")))
  (should (equal (test--plain (calfw-plus--fill-cell '("08:00 Repaso semanal de proyectos" "Otra") 14 3))
                 '("08:00 Repaso" "semanal de proyectos" "Otra")))
  (let ((rows (calfw-plus--fill-cell '("ESP. Documentacion" "Pagos") 14 3)))
    (should (equal (test--plain rows) '("ESP." "Documentacion" "Pagos")))
    (should (get-text-property 0 'cfw:item-separator (nth 1 rows)))))

(ert-deftest calfw-cells/demand-counts-periods-and-wrapped-items ()
  (should (= (calfw-plus--demand (list (propertize "(Bar)" 'cfw:period t)
                                       "08:00 Repaso semanal de proyectos" "Otra")
                                 14)
             5)))

(ert-deftest calfw-cells/summary-drops-keyword-and-priority ()
  (let ((org-todo-keywords-for-agenda '("SIG." "AD-T" "ESP." "P.ES"))
        (org-todo-keyword-faces '(("AD-T" :foreground "#00ffff" :height 0.75)
                                  ("SIG." :foreground "#00ff00")
                                  ("ESP." :foreground "#ffff00"))))
    (dolist (case '(("10:00 AD-T Comunicación" "10:00 Comunicación" "#00ffff")
                    ("12:00 SIG. [#E] MasterPlan" "12:00 MasterPlan" "#00ff00")
                    ("ESP. /IQ/ Documento" "IQ Documento" "#ffff00")
                    ("08:00 Repaso semanal" "08:00 Repaso semanal" nil)
                    ("[#A] Urgente" "Urgente" nil)
                    ("ESPAÑA no es palabra clave" "ESPAÑA no es palabra clave" nil)))
      (cl-letf (((symbol-function 'calfw-org-summary-format) (lambda (s) (copy-sequence s))))
        (let* ((out (calfw-plus-org-summary (car case)))
               (title-pos (if (string-match "\\`[0-9][0-9]:[0-9][0-9] " out) (match-end 0) 0)))
          (should (equal (substring-no-properties out) (nth 1 case)))
          (should (equal (test--fg (get-text-property title-pos 'face out))
                         (nth 2 case))))))))

(ert-deftest calfw-cells/summary-without-an-agenda-built ()
  ;; Before an agenda is built `org-todo-keywords-for-agenda' is empty:
  ;; the keywords come from `org-todo-keywords'.
  (let ((org-todo-keywords-for-agenda nil)
        (org-todo-keywords '((type "SIG.(s)" "AD-T(t)" "ESP.(e@)" "|" "FINI(h)")))
        (org-todo-keyword-faces '(("AD-T" :foreground "#00ffff"))))
    (should (equal (calfw-plus--todo-keywords) '("SIG." "AD-T" "ESP." "FINI")))
    (cl-letf (((symbol-function 'calfw-org-summary-format) (lambda (s) (copy-sequence s))))
      (let ((out (calfw-plus-org-summary "10:00 AD-T Comunicación")))
        (should (equal (substring-no-properties out) "10:00 Comunicación"))
        (should (equal (test--fg (get-text-property 6 'face out)) "#00ffff"))))))

(ert-deftest calfw-cells/summary-drops-emphasis-markers ()
  (let ((org-todo-keywords-for-agenda '("ESP." "P.ES")))
    (cl-letf (((symbol-function 'calfw-org-summary-format) (lambda (s) (copy-sequence s))))
      (dolist (case '(("09:00 P.ES /Planning/" "09:00 Planning" 6 italic)
                      ("ESP. /IQ/ Documento *urgente*" "IQ Documento urgente" 13 bold)
                      ("Revisar =v2.0= ya" "Revisar v2.0 ya" 8 org-verbatim)
                      ;; Not markers: inside words, or a path.
                      ("Planos_v2 en a/b/c" "Planos_v2 en a/b/c" 0 nil)))
        (let* ((out (calfw-plus-org-summary (car case)))
               (face (get-text-property (nth 2 case) 'face out)))
          (should (equal (substring-no-properties out) (nth 1 case)))
          (should (if (nth 3 case) (memq (nth 3 case) (ensure-list face)) t)))))))

;;; Day titles

(defun test--plain-pair (pair)
  (cons (substring-no-properties (car pair)) (and (cdr pair) (substring-no-properties (cdr pair)))))

(ert-deftest calfw-titles/week-label-is-iso ()
  (should (equal (calfw-plus--week-label '(11 23 2026)) "S48"))
  ;; 2026 starts on a Thursday, so it has 53 ISO weeks.
  (should (equal (calfw-plus--week-label '(12 28 2026)) "S53"))
  (should (equal (calfw-plus--week-label '(1 4 2027)) "S1")))

(ert-deftest calfw-titles/parts ()
  (should (equal (calfw-plus--title-parts " 23 (1)") '("23" "(1)" nil)))
  (should (equal (calfw-plus--title-parts " 24 Puente") '("24" nil "Puente")))
  (should (equal (calfw-plus--title-parts " 7 (2) Puente") '("7" "(2)" "Puente")))
  (should (equal (calfw-plus--title-parts " 3") '("3" nil nil)))
  ;; Calfw leaves the space of a missing count.
  (should (equal (calfw-plus--title-parts " 3 ") '("3" nil nil)))
  (should (equal (calfw-plus--title-parts " 24  Puente") '("24" nil "Puente")))
  (should-not (calfw-plus--title-parts "Monday")))

(ert-deftest calfw-titles/day-right-week-left-on-week-start ()
  (let ((calfw-week-days-list '(1 2 3 4 5 6 0)))
    (should (equal (test--plain-pair (calfw-plus--title-layout '(11 23 2026) " 23 (1)" nil 22))
                   '(" S48" . "23")))
    (should (equal (test--plain-pair (calfw-plus--title-layout '(11 26 2026) " 26 Puente" nil 22))
                   '(" Puente" . "26")))
    (should (equal (test--plain-pair (calfw-plus--title-layout '(12 21 2026) " 21 (1)" "Solsticio" 22))
                   '(" S52 Solsticio" . "21")))
    (should (eq (get-text-property 1 'face (car (calfw-plus--title-layout '(11 23 2026) " 23" nil 22)))
                'shadow))))

(ert-deftest calfw-titles/left-is-cut-so-the-day-fits ()
  (let* ((calfw-week-days-list '(1 2 3 4 5 6 0))
         (pair (calfw-plus--title-layout '(12 7 2026) " 7 (12) Puente de la Inmaculada"
                                         "Solsticio" 22)))
    (should (equal (cdr pair) "7"))
    (should (<= (+ (string-width (car pair)) 1 (string-width (cdr pair))) 22))
    (should (string-suffix-p "…" (car pair)))))

(ert-deftest calfw-titles/moon-phase-beside-the-day ()
  ;; November 2026: new moon on the 9th, first quarter on the 17th, full
  ;; moon on the 24th, last quarter on 1 December, all hours from midnight.
  (let ((calfw-week-days-list '(1 2 3 4 5 6 0)))
    (should (equal (cdr (test--plain-pair (calfw-plus--title-layout '(11 9 2026) " 9" nil 22))) "○ 9"))
    (should (equal (cdr (test--plain-pair (calfw-plus--title-layout '(11 17 2026) " 17" nil 22))) "◑ 17"))
    (should (equal (cdr (test--plain-pair (calfw-plus--title-layout '(11 24 2026) " 24 (1)" nil 22))) "● 24"))
    (should (equal (cdr (test--plain-pair (calfw-plus--title-layout '(12 1 2026) " 1" nil 22))) "◐ 1"))
    (should (equal (cdr (test--plain-pair (calfw-plus--title-layout '(11 25 2026) " 25" nil 22))) "25"))))

(ert-deftest calfw-titles/other-titles-untouched ()
  (should (equal (calfw-plus--title-layout '(11 23 2026) "Monday" "x" 22) '("Monday" . "x"))))

(ert-deftest calfw-titles/columns-lift-a-short-first-task ()
  (let* ((calfw-week-days-list '(1 2 3 4 5 6 0))
         (args (calfw-plus--layout-titles
                (list (list (list '(11 23 2026) (cons " 23 (2)" nil) "10:00 Uno" "Dos")
                            (list nil (cons "" nil)))
                      '((cell-width . 22))))))
    ;; The first task joins the week number; the second stays below.
    (should (equal (test--plain-pair (cadr (car (car args)))) '(" S48 10:00 Uno" . "23")))
    (should (equal (cddr (car (car args))) '("Dos")))
    (should (equal (cadr (car args)) (list nil (cons "" nil))))
    (should (equal (cadr args) '((cell-width . 22))))))

(ert-deftest calfw-titles/lift-task ()
  (let ((bar (propertize "(Feria---)" 'cfw:period t)))
    ;; Bare title: the task goes up whole.
    (should (equal (calfw-plus--lift-task '(" " . "12") '("10:00 Uno" "Dos") 22)
                   '((" 10:00 Uno" . "12") "Dos")))
    ;; The bars keep their rows; the first task after them goes up.
    (should (equal (calfw-plus--lift-task (cons " " "17") (list bar "Uno") 22)
                   (list (cons " Uno" "17") bar)))
    ;; Too long to fit beside the day: it stays, to wrap below.
    (should-not (calfw-plus--lift-task '(" " . "12") '("Cobro upon Approval of Drawings") 22))
    ;; A holiday or an annotation keeps the title to itself.
    (should-not (calfw-plus--lift-task '(" Puente" . "7") '("Uno") 22))
    (should-not (calfw-plus--lift-task '(" S52 Solsticio" . "21") '("Uno") 22))
    ;; It must leave two spaces before the day: the opening space, 17,
    ;; two and the day's 2 fill the 22; one character more does not fit.
    (should (calfw-plus--lift-task '(" " . "12") '("08:00 Repaso sema") 22))
    (should-not (calfw-plus--lift-task '(" " . "12") '("08:00 Repaso seman") 22))
    ;; No tasks: nothing to lift.
    (should-not (calfw-plus--lift-task '(" " . "3") nil 22))))

(ert-deftest calfw-titles/today-keeps-its-title ()
  (let* ((calfw-week-days-list '(1 2 3 4 5 6 0))
         (today (calendar-current-date))
         (args (calfw-plus--layout-titles
                (list (list (list today (cons (format " %d" (calendar-extract-day today)) nil) "Uno"))
                      '((cell-width . 22))))))
    (should (equal (cddr (car (car args))) '("Uno")))))

;;; Seasons

(defun test--december-holidays ()
  (let ((displayed-month 12) (displayed-year 2026))
    (calendar-holiday-list)))

(ert-deftest calfw-seasons/winter-solstice-becomes-an-annotation ()
  (let* ((holidays (test--december-holidays))
         (solstice (seq-find (lambda (h) (string-prefix-p "Solsticio de Invierno" (cadr h)))
                             holidays))
         (data (calfw-plus--seasons-as-annotations
                (list (cons 'holidays holidays) (cons 'annotations nil)))))
    (should solstice)
    (should-not (member solstice (alist-get 'holidays data)))
    (should (equal (cdr (assoc (car solstice) (alist-get 'annotations data)))
                   "Solsticio"))
    (should (assoc '(12 25 2026) (alist-get 'holidays data)))
    (should (= (length (alist-get 'holidays data)) (1- (length holidays))))))

(ert-deftest calfw-seasons/joins-an-existing-annotation ()
  (let ((data (calfw-plus--seasons-as-annotations
               (list (cons 'holidays '(((3 20 2026) "Equinoccio de Primavera 15:46 (CET)")
                                       ((3 19 2026) "Fest")))
                     (cons 'annotations (list (cons '(3 20 2026) "Luna nueva")))))))
    (should (equal (alist-get 'holidays data) '(((3 19 2026) "Fest"))))
    (should (equal (alist-get 'annotations data)
                   '(((3 20 2026) . "Luna nueva/Equinoccio"))))))

(ert-deftest calfw-seasons/other-holidays-stay ()
  (should-not (calfw-plus--season "Fest"))
  (should-not (calfw-plus--season "Puente"))
  (should (equal (calfw-plus--season "Solsticio de Verano 16:24 (CEST)") "Solsticio de Verano"))
  (should (equal (calfw-plus--season "Equinoccio de Otoño 02:05 (CEST)") "Equinoccio de Otoño")))

(ert-run-tests-batch-and-exit)
