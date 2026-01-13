(in-package #:cl-forcats)

(defstruct (factor (:constructor %make-factor))
  data
  levels
  ordered)

(defun make-factor (data &key levels ordered)
  "Create a new factor. DATA is an integer vector, LEVELS is a list of strings."
  (%make-factor :data data :levels (coerce levels 'vector) :ordered ordered))

(defun fct-levels (f)
  (factor-levels f))

(defun (setf fct-levels) (new-levels f)
  (setf (factor-levels f) (coerce new-levels 'vector)))
