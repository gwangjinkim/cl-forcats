(in-package #:cl-forcats)

(defun factor (data &key levels ordered)
  "Create a factor from sequence DATA. 
If LEVELS is nil, unique values are extracted from DATA and sorted alphabetically.
Coerces elements of DATA to strings robustly (handles symbols, numbers)."
  (let* ((data-list (coerce data 'list))
         (unique-values (if levels
                            (mapcar #'ensure-string levels)
                            (sort (delete-duplicates (mapcar #'ensure-string (remove nil data-list)) :test #'string=) #'string<)))
         (levels-vec (coerce unique-values 'vector))
         (int-data (map 'vector
                        (lambda (x)
                          (if (null x)
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

;; For cl-tibble to show <fct>
;; (defmethod cl-tibble:type-abbreviation ((x factor)) "fct")
;; (defmethod cl-tibble:format-column-value ((x factor)) ...)
;; Since I don't have cl-tibble loaded here, I'll just provide the DSL for now.
