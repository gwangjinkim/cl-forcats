(in-package #:cl-forcats)

(defun %level-label (label)
  (unless (or (stringp label) (na-p label)) (error "Level label must be a string or NA")) label)

(defun %frequency-add (a b)
  ;; R numeric sums can overflow to infinity rather than trap. Keep the
  ;; floating environment local; no global trap-mode change is made.
  #+sbcl (sb-int:with-float-traps-masked (:overflow :underflow) (+ a b))
  #-sbcl (+ a b))

(defun %frequency-proportion (count total)
  #+sbcl (sb-int:with-float-traps-masked (:overflow :underflow)
           (/ (coerce count 'double-float) (coerce total 'double-float)))
  #-sbcl (/ (coerce count 'double-float) (coerce total 'double-float)))

(defun %frequency-weights (f w supplied)
  (let* ((size (col-length f)) (weights (when supplied (%input-column w)))
         (counts (make-array (length (factor-levels f)) :initial-element 0)))
    (when supplied
      (unless (and (member (col-type weights) '(:int :double)) (= (col-length weights) size))
        (error "W must contain one numeric weight per observation"))
      (dotimes (i size)
        (let ((x (col-ref weights i)))
          (unless (and (realp x) (not (%nan-p x)) (>= x 0))
            (error "Weights must be nonnegative and nonmissing")))))
    (dotimes (i size)
      (let ((code (aref (factor-data f) i)))
        (when (> code 0) (setf (aref counts (1- code)) (%frequency-add (aref counts (1- code)) (if supplied (col-ref weights i) 1))))))
    (values counts (if supplied (reduce #'%frequency-add (col->list weights) :initial-value 0) size))))

(defun %levels-other (f keep other)
  (%level-label other)
  (cond ((every (lambda (x) (eq x t)) keep) f)
        ((not (position nil keep)) (error "Missing lumping decision"))
        (t (let* ((labels (map 'vector (lambda (old yes) (cond ((na-p yes) *na*) (yes old) (t other)))
                              (factor-levels f) keep))
                  (out (lvls-revalue f (make-typed-column labels :string)))
                  (levels (coerce (factor-levels out) 'list)))
             (%refactor out (append (remove other levels :test #'equal) (list other)) (factor-ordered out))))))

(defun %number-option (x &optional minimum)
  (unless (and (realp x) (not (%nan-p x)) (or (null minimum) (>= x minimum)))
    (error "Expected a nonmissing numeric option")) x)

(defun %rank-counts (counts method descending seed supplied random-state)
  (unless (member method '("min" "max" "average" "first" "last" "random") :test #'equal)
    (error "Unsupported TIES-METHOD"))
  (let* ((n (length counts)) (indices (loop for i below n collect i)) (ranks (make-array n))
         (rng (when (and supplied (equal method "random")) (%seed-factor-rng seed)))
         (random-values (when (equal method "random")
                          (unless (or rng (typep random-state 'random-state)) (error "Invalid RANDOM-STATE"))
                          (map 'vector (lambda (i) (declare (ignore i))
                                         (if rng (let ((word (%factor-rng-word rng)))
                                                   (/ (if (zerop word) 0.5d0 word) 4294967296d0))
                                             (random 1d0 random-state))) indices))))
    (setf indices (stable-sort indices
                              (lambda (i j)
                                (let ((a (aref counts i)) (b (aref counts j)))
                                  (if (/= a b) (if descending (> a b) (< a b))
                                      (cond ((equal method "last") (> i j))
                                            (random-values (< (aref random-values i) (aref random-values j)))
                                            (t nil)))))))
    (if (member method '("first" "last" "random") :test #'equal)
        (loop for i in indices for rank from 1 do (setf (aref ranks i) rank))
        (loop for tail on indices for start from 1
              unless (and (> start 1) (= (aref counts (first tail)) (aref counts (nth (- start 2) indices))))
              do (let* ((value (aref counts (first tail)))
                        (size (loop for i in tail while (= value (aref counts i)) count i))
                        (end (+ start size -1))
                        (rank (cond ((equal method "min") start) ((equal method "max") end)
                                    (t (/ (+ start end) 2d0)))))
                   (loop for i in tail repeat size do (setf (aref ranks i) rank)))))
    ranks))

(defun fct-lump-n (f n &key (w nil w-supplied) (other-level "Other") (ties-method "min")
                          (seed nil seed-supplied) (random-state *random-state*))
  "R: forcats::fct_lump_n(). Keep N most frequent levels (least frequent if negative).
Numeric weights apply to observations. TIES-METHOD is min/average/first/last/random/max.
Random ties use an explicit CL RANDOM-STATE or local R-compatible MT stream via SEED.
Implicit missing observations stay missing unless OTHER-LEVEL is an explicit NA level."
  (%number-option n) (%level-label other-level)
  (let* ((f (%check-factor f)) (counts (%frequency-weights f w w-supplied))
         (ranks (%rank-counts counts ties-method (>= n 0) seed seed-supplied random-state)))
    (%levels-other f (map 'vector (lambda (rank) (<= rank (abs n))) ranks) other-level)))

(defun fct-lump-min (f min &key (w nil supplied) (other-level "Other"))
  "R: forcats::fct_lump_min(). Keep levels whose observation weight is at least MIN.
MIN is nonnegative; omit W for unit weights. OTHER-LEVEL is a string or NA, placed last."
  (%number-option min 0) (%level-label other-level)
  (let* ((f (%check-factor f)) (counts (%frequency-weights f w supplied)))
    (%levels-other f (map 'vector (lambda (n) (>= n min)) counts) other-level)))

(defun fct-lump-prop (f prop &key (w nil supplied) (other-level "Other"))
  "R: forcats::fct_lump_prop(). Keep proportions greater than PROP.
Negative PROP keeps proportions at most -PROP. The denominator includes every
observation weight, including implicit missing observations. Zero totals can error."
  (%number-option prop) (%level-label other-level)
  (let ((f (%check-factor f)))
    (multiple-value-bind (counts total) (%frequency-weights f w supplied)
      (%levels-other f
                     (map 'vector (lambda (count)
                                    (cond ((or (zerop total)
                                               (and (= total (%signed-infinity nil)) (= count total))) *na*)
                                          (t (let ((p (%frequency-proportion count total))) (if (< prop 0) (<= p (- prop)) (> p prop)))))) counts)
                     other-level))))

(defun fct-lump-lowfreq (f &key (w nil supplied) (other-level "Other"))
  "R: forcats::fct_lump_lowfreq(). Lump low frequencies while Other stays smallest.
Weights are nonnegative numeric observations. Existing level order and owner metadata remain."
  (%level-label other-level)
  (let* ((f (%check-factor f)) (counts (%frequency-weights f w supplied))
         (indices (stable-sort (loop for i below (length counts) collect i) #'> :key (lambda (i) (aref counts i))))
         (remaining (reduce #'%frequency-add counts :initial-value 0)) (keep (make-array (length counts) :initial-element t)))
    (loop for tail on indices for i = (first tail) for x = (aref counts i)
          do (when (and (= remaining (%signed-infinity nil)) (= x remaining)) (error "Indeterminate infinite frequency cutoff"))
             (decf remaining x)
          when (> x remaining) do (loop for j in (rest tail) do (setf (aref keep j) nil)) (return))
    (%levels-other f keep other-level)))

(defun fct-na-value-to-level (f &key (level *na*))
  "R: forcats::fct_na_value_to_level(). Convert implicit missing cells into LEVEL.
LEVEL defaults to shared NA as an explicit factor level. It is added even when no
missing cells exist; an existing label merges both representations. Names/order remain."
  (%level-label level)
  (let* ((f (%check-factor f)) (old (coerce (factor-levels f) 'list))
         (expanded (%refactor f (%unique-values (append old (list *na*))) (factor-ordered f)))
         (labels (map 'vector (lambda (x) (if (na-p x) level x)) (factor-levels expanded))))
    (lvls-revalue expanded (make-typed-column labels :string))))

(defun fct-na-level-to-value (f &key (extra-levels nil supplied))
  "R: forcats::fct_na_level_to_value(). Convert explicit NA and EXTRA-LEVELS into missing cells.
Omit EXTRA-LEVELS for no additional labels; supplied values must be a character column.
Codes are remapped directly to retain implicit missingness, orderedness and names."
  (let* ((f (%check-factor f)) (remove (cons *na* (when supplied (%characters extra-levels))))
         (old (coerce (factor-levels f) 'list))
         (levels (remove-if (lambda (x) (member x remove :test #'equal)) old)))
    (%copy-factor-codes f (map 'vector (lambda (code)
                                       (if (zerop code) 0
                                           (let ((i (position (nth (1- code) old) levels :test #'equal))) (if i (1+ i) 0))))
                              (factor-data f)) levels)))
