(in-package #:cl-forcats)

(defun fct-drop (f)
  "Remove unused levels from factor F."
  (let* ((data (factor-data f))
         (levels (factor-levels f))
         (used-indices (sort (delete-duplicates (remove-if-not (lambda (x) (> x 0)) (coerce data 'list))) #'<))
         (new-levels (map 'vector (lambda (idx) (aref levels (1- idx))) used-indices))
         (mapping (make-hash-table :test 'eql))
         (new-data (make-array (length data) :element-type (array-element-type data))))
    
    (loop for old-idx in used-indices
          for new-idx from 1
          do (setf (gethash old-idx mapping) new-idx))
    
    (loop for i from 0 below (length data)
          for x = (aref data i)
          do (setf (aref new-data i) (if (and x (> x 0)) (gethash x mapping 0) x)))
    
    (make-factor new-data :levels new-levels :ordered (factor-ordered f))))

(defun fct-expand (f &rest additional-levels)
  "Add new levels to a factor."
  (let* ((old-levels (coerce (factor-levels f) 'list))
         (new-levels (append old-levels (mapcar #'ensure-string additional-levels))))
    (make-factor (factor-data f) :levels (delete-duplicates new-levels :test #'string= :from-end t) :ordered (factor-ordered f))))

(defun fct-explicit-na (f &key (na-level "(Missing)"))
  "Convert NA (0) to a named level."
  (let* ((data (factor-data f))
         (levels (coerce (factor-levels f) 'list))
         (has-na (some (lambda (x) (or (null x) (zerop x))) data)))
    (if has-na
        (let* ((new-levels (append levels (list (ensure-string na-level))))
               (na-idx (length new-levels))
               (new-data (map 'vector (lambda (x) (if (or (null x) (zerop x)) na-idx x)) data)))
          (make-factor new-data :levels new-levels :ordered (factor-ordered f)))
        f)))
