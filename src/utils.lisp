(in-package #:cl-forcats)

;; Utility functions for cl-forcats

(defun mean (list)
  "Calculate the average of a list of numbers."
  (if (null list)
      0
      (/ (reduce #'+ list) (length list))))

(defun ensure-string (x)
  "Convert X to a string robustly. Handles symbols and numbers."
  (cond
    ((stringp x) x)
    ((null x) nil)
    (t (princ-to-string x))))

(defun plist-to-pairs (plist)
  "Convert a plist to a list of (key value) pairs."
  (loop for (key val) on plist by #'cddr
        collect (list key val)))
