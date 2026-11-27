;; -*- lexical-binding: t; -*-
;; Apply feat/calfw-keys: calfw-plus gets `calfw-plus-next-task' and
;; `calfw-plus-previous-task', which go from task to task, day after
;; day, and `calfw-plus-next-task-day' and `calfw-plus-previous-task-day',
;; which go to the first task of the next or previous day with tasks.
;; Evaluates their definitions, read from the branch being applied
;; (`branch-eval-ref').  The init's feat/calfw-keys binds them to n, p, f
;; and b.

(require 'magit)
(require 'calfw-plus)

(with-current-buffer (magit-find-file-noselect branch-eval-ref "calfw-plus.el")
  (save-excursion
    (dolist (head '("(defconst calfw-plus--task-day-window "
                    "(defconst calfw-plus--task-day-reach "
                    "(defun calfw-plus--task-day "
                    "(defun calfw-plus--tasks "
                    "(defun calfw-plus--goto-task-of "
                    "(defun calfw-plus-next-task-day "
                    "(defun calfw-plus-previous-task-day "
                    "(defun calfw-plus-next-task "
                    "(defun calfw-plus-previous-task "))
      (goto-char (point-min))
      (search-forward (concat "\n" head))
      (goto-char (1+ (match-beginning 0)))
      (eval (read (current-buffer)) t))))

(message "calfw-keys applied: days with tasks up to %d days away"
         calfw-plus--task-day-reach)
