(in-package #:cl-forcats)

(defun fct-rev (f)
  "R: forcats::fct_rev(). Reverse levels, preserving names and orderedness."
  (let ((f (%check-factor f)))
    (%refactor f (reverse (coerce (factor-levels f) 'list)) (factor-ordered f))))

(defun fct-relevel (f &rest levels)
  "R: forcats::fct_relevel(). Move selected levels after :AFTER (default zero).
A sole callback receives the full level vector. Unknown levels warn and are
ignored; positive infinity inserts at the end. Legacy scalar label coercion is retained."
  (let* ((f (%check-factor f)) (old (coerce (factor-levels f) 'list))
         (option (position :after levels)) (after (if option (nth (1+ option) levels) 0))
         (spec (if option (subseq levels 0 option) levels))
         (move (if (and (= (length spec) 1) (functionp (first spec)))
                   (%characters (funcall (first spec) (make-typed-column (copy-seq (factor-levels f)) :string)))
                   (loop for x in spec append
                         (if (and (not (stringp x)) (or (vectorp x) (and (consp x) (listp x))))
                             (mapcar #'ensure-string (col->list (%input-column x)))
                             (list (ensure-string x)))))))
    (when (and option (/= (+ option 2) (length levels))) (error "Invalid :AFTER options"))
    (unless (and (realp after) (not (%nan-p after)) (>= after 0)
                 (or (= after (%signed-infinity nil)) (= after (truncate after))))
      (error "AFTER must be a nonnegative whole number or positive infinity"))
    (unless (every (lambda (x) (member x old :test #'equal)) move) (warn "Unknown levels in FCT-RELEVEL"))
    (setf move (remove-if-not (lambda (x) (member x old :test #'equal)) move))
    (let* ((remaining (remove-if (lambda (x) (member x move :test #'equal)) old))
           (offset (truncate (min (length remaining) after))))
      (%refactor f (append (subseq remaining 0 offset) move (subseq remaining offset)) (factor-ordered f)))))

(defun fct-infreq (f &key (w nil supplied) (ordered *na*))
  "R: forcats::fct_infreq(). Sort by decreasing (optionally weighted) frequency.
Omit W for default unit weights; supplied NIL is logical FALSE and errors.
W must contain one nonnegative numeric weight per observation. Ties retain level order."
  (let* ((f (%check-factor f)) (size (col-length f))
         (weights (when supplied (%input-column w)))
         (counts (make-array (length (factor-levels f)) :initial-element 0)))
    (when weights
      (unless (and (member (col-type weights) '(:int :double)) (= size (col-length weights)))
        (error "Weights must be numeric and match input length"))
      (dotimes (i size)
        (let ((value (col-ref weights i)))
          (unless (and (realp value) (>= value 0)) (error "Weights must be nonnegative and nonmissing")))))
    (loop for code across (factor-data f) for i from 0
          when (> code 0) do (incf (aref counts (1- code)) (if weights (col-ref weights i) 1)))
    (let ((indices (stable-sort (loop for i below (length counts) collect i) #'> :key (lambda (i) (aref counts i)))))
      (%refactor f (mapcar (lambda (i) (aref (factor-levels f) i)) indices) (%ordered-option ordered f)))))

(defun %median-summary (values)
  (if (null values) *na*
      (if (some (lambda (x) (or (na-p x) (%nan-p x))) values) *na*
          (let* ((sorted (sort (copy-list values) #'<)) (n (length sorted)) (half (floor n 2)))
            (if (oddp n) (nth half sorted) (/ (+ (nth (1- half) sorted) (nth half sorted)) 2d0))))))

(defun fct-reorder (f v &key (fun #'mean) (desc nil) (na-rm nil na-supplied)
                              (default nil default-supplied) args)
  "Reorder levels by a scalar summary. FUN receives a list in observation order.
Legacy MEAN/default empty-group callback behavior is retained until the migration
is approved. :NA-RM T removes missing values; :DEFAULT supplies empty summaries.
:ARGS are passed after the list. Names and orderedness are preserved."
  (%check-boolean desc)
  (when na-supplied (%check-boolean na-rm))
  (let* ((f (%check-factor f)) (v (%input-column v)) (n (col-length f))
         (groups (make-array (length (factor-levels f)) :initial-element nil))
         (summary (make-array (length groups))))
    (unless (= n (col-length v)) (error "F and V must have equal lengths"))
    (dotimes (i n)
      (let ((code (aref (factor-data f) i)))
        (when (and (> code 0) (not (and na-rm (%cell-missing-p v i))))
          (push (col-ref v i) (aref groups (1- code))))))
    (dotimes (j (length groups))
      (let ((values (nreverse (aref groups j))))
        (setf (aref summary j) (%scalar-summary (if (and (null values) default-supplied) default
                                                  (apply fun values args))))))
    (%summary-order f summary desc)))

(defun fct-shift (f &key (n 1))
  "R: forcats::fct_shift(). Rotate factor levels N places to the left."
  (unless (factor-p f) (error "F must be a factor"))
  (unless (and (realp n) (not (%nan-p n)) (= n (truncate n))) (error "N must be a whole number"))
  (let* ((levels (coerce (factor-levels f) 'list)) (len (length levels)))
    (if (zerop len) f
        (let ((shift (mod (truncate n) len)))
          (%refactor f (append (subseq levels shift) (subseq levels 0 shift)) (factor-ordered f))))))
