;; -*- lexical-binding: t; -*-
;; Undo feat/calfw-keys: removes `calfw-plus-next-task',
;; `calfw-plus-previous-task', `calfw-plus-next-task-day',
;; `calfw-plus-previous-task-day' and what they use.  Undo the init's
;; feat/calfw-keys first, which binds them.

(dolist (fn '(calfw-plus-next-task
              calfw-plus-previous-task
              calfw-plus-next-task-day
              calfw-plus-previous-task-day
              calfw-plus--goto-task-of
              calfw-plus--tasks
              calfw-plus--task-day))
  (fmakunbound fn))
(makunbound 'calfw-plus--task-day-window)
(makunbound 'calfw-plus--task-day-reach)

(message "calfw-keys undone: next task day %S" (fboundp 'calfw-plus-next-task-day))
