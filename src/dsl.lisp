(in-package #:cl-forecats)

(defun factor (data &key levels ordered)
  "Create a factor from sequence DATA. 
If LEVELS is nil, unique values are extracted from DATA and sorted alphabetically.
Coerces elements of DATA to strings robustly (handles symbols, numbers).
Treats NIL or CL-VCTRS-LITE:*NA* as missing values."
  (let* ((data-list (coerce data 'list))
         (unique-values (if levels
                            (mapcar #'ensure-string levels)
                            (sort (delete-duplicates 
                                   (mapcar #'ensure-string 
                                           (remove-if (lambda (x) (or (null x) (na-p x))) data-list)) 
                                   :test #'string=) 
                                  #'string<)))
         (levels-vec (coerce unique-values 'vector))
         (int-data (map 'vector
                        (lambda (x)
                          (if (or (null x) (na-p x))
                              0
                              (let ((pos (position (ensure-string x) levels-vec :test #'string=)))
                                (if pos (1+ pos) 0))))
                        data)))
    (make-factor int-data :levels levels-vec :ordered ordered)))

;; Integration with cl-tibble/cl-dplyr
;; We need to define how factors are printed and how they interact with tibbles.

(defmethod print-object ((f factor) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~A levels: ~{~A~^, ~}" 
            (length (factor-data f))
            (coerce (factor-levels f) 'list))))

;; Integration with cl-tibble/cl-dplyr
;; We define methods for cl-tibble to show <fct> and format values correctly.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :cl-tibble)
    (eval `(defmethod ,(intern "TYPE-ABBREVIATION" :cl-tibble) ((x factor)) "fct"))
    (eval `(defmethod ,(intern "FORMAT-COLUMN-VALUE" :cl-tibble) ((x factor) i)
             (let* ((data (factor-data x))
                    (levels (factor-levels x))
                    (val (aref data i)))
               (if (or (null val) (zerop val))
                   cl-vctrs-lite:*na*
                   (aref levels (1- val))))))))
