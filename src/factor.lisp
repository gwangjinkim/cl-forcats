(in-package #:cl-forcats)

(defstruct (factor (:constructor %make-factor))
  "A categorical variable representation.
DATA is an integer vector where 0 represents NA and 1..N represent levels.
LEVELS is a vector of strings representing the names of the categories.
ORDERED is a boolean indicating if the levels have a meaningful order."
  data
  levels
  ordered)

(defun make-factor (data &key levels ordered)
  "Create a new factor. DATA is an integer vector, LEVELS is a list of strings."
  (%make-factor :data data :levels (coerce levels 'vector) :ordered ordered))

(defun fct-levels (f)
  "Return the levels of factor F as a vector of strings."
  (factor-levels f))

(defun (setf fct-levels) (new-levels f)
  "Set the levels of factor F. NEW-LEVELS should be a sequence of strings."
  (setf (factor-levels f) (coerce new-levels 'vector)))
