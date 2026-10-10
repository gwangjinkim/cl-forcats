(in-package #:cl-forcats)

(defun %legacy-count (f &key sort prop)
  "Count occurrences of each level in factor F.
Returns a list of plists: (:level \"name\" :n count [:p proportion]).
If SORT is T, result is sorted by count descending.
If PROP is T, includes proportion :p."
  (let* ((data (factor-data f))
         (levels (factor-levels f))
         (counts (make-hash-table :test 'eql)))
    ;; Initialize counts
    (loop for i from 1 to (length levels)
          do (setf (gethash i counts) 0))
    ;; Count
    (loop for x across data
          do (when (and x (> x 0))
               (incf (gethash x counts))))
    ;; Convert to list of plists or similar structure
    (let ((result (loop for i from 1 to (length levels)
                        for count = (gethash i counts)
                        collect (list :level (aref levels (1- i))
                                      :n count))))
      (when sort
        (setf result (sort result #'> :key (lambda (x) (getf x :n)))))
      (if prop
          (let ((total (length data)))
            (mapcar (lambda (x)
                      (append x (list :p (if (zerop total) 0 (/ (getf x :n) total)))))
                    result))
          result))))

(defun fct-unique (f)
  "R: forcats::fct_unique(). Return every factor level, then implicit missing if present.
The factor's ordered status and unused levels are retained."
  (let* ((f (%check-factor f))
         (codes (append (loop for i from 1 to (length (factor-levels f)) collect i)
                        (when (find 0 (factor-data f)) (list 0)))))
    (make-factor (coerce codes 'vector) :levels (factor-levels f) :ordered (factor-ordered f))))

(defun fct-count (f &key sort prop)
  "R: forcats::fct_count(). Return a tibble of factor F, integer N and optional double P.
Includes unused levels and an implicit missing row. Sorting is stable by descending N."
  (unless (and (or (eq sort t) (null sort)) (or (eq prop t) (null prop)))
    (error "SORT and PROP must be boolean"))
  (let* ((f (%check-factor f)) (unique (fct-unique f))
         (counts (make-array (col-length unique) :initial-element 0))
         (nlevels (length (factor-levels f))))
    (loop for code across (factor-data f) do (incf (aref counts (if (zerop code) nlevels (1- code)))))
    (let* ((indices (loop for i below (length counts) collect i))
           (indices (if sort (stable-sort indices #'> :key (lambda (i) (aref counts i))) indices))
           (ordered-f (make-factor (map 'vector (lambda (i) (aref (factor-data unique) i)) indices)
                                   :levels (factor-levels unique) :ordered (factor-ordered unique)))
           (ordered-n (make-typed-column (map 'vector (lambda (i) (aref counts i)) indices) :int))
           (columns (list ordered-f ordered-n)) (names '("f" "n")))
      (when prop
        (let ((total (col-length f)))
          (setf columns (append columns (list (make-typed-column (map 'vector (lambda (i) (if (zerop total) #+sbcl (sb-kernel:make-double-float #x7ff80000 0) #-sbcl (error "NaN proportions require IEEE float support") (/ (coerce (aref counts i) 'double-float) total))) indices) :double)))
                names (append names '("p")))))
      (cl-tibble:make-tbl (mapcar #'cons names columns)))))
