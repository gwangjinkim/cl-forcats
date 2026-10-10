(in-package #:cl-forcats)

(defun %level-pairs (spec)
  (if (and spec (consp (first spec)))
      (progn (unless (every (lambda (x) (and (listp x) (= 2 (length x)))) spec)
               (error "Level specifications must be pairs")) spec)
      (progn (unless (evenp (length spec)) (error "Level specifications must be pairs"))
             (plist-to-pairs spec))))

(defun fct-recode (f &rest new-levels)
  "R: forcats::fct_recode(). Rename or merge levels using new/old pairs.
NIL as a new name removes the corresponding old level. Unknown levels warn.
Legacy scalar old-label coercion remains available pending the parity migration decision."
  (let* ((f (%check-factor f)) (old (coerce (factor-levels f) 'list))
         (labels (copy-list old)) (removed nil))
    (dolist (pair (%level-pairs new-levels))
      (let* ((new (first pair)) (previous (ensure-string (second pair)))
             (i (position previous old :test #'equal)))
        (if i (if (null new) (push previous removed)
                  (setf (nth i labels) (ensure-string new)))
            (warn "Unknown level ~s in FCT-RECODE" previous))))
    (if removed
        (%factor-values (loop for code across (factor-data f) collect
                          (if (or (zerop code) (member (nth (1- code) old) removed :test #'equal))
                              *na* (nth (1- code) labels)))
                        (%unique-values (loop for x in old for label in labels
                                               unless (or (member x removed :test #'equal) (na-p label)) collect label))
                        :ordered (factor-ordered f) :names (col-names f))
        (lvls-revalue f (make-typed-column (coerce labels 'vector) :string)))))

(defun fct-collapse (f &rest group-definitions)
  "R: forcats::fct_collapse(). Merge named groups of existing levels.
:OTHER-LEVEL places unassigned levels in one final level. Unknown levels warn.
The deprecated :GROUP-OTHER option warns whenever supplied."
  (let* ((f (%check-factor f)) (old (coerce (factor-levels f) 'list))
         (labels (copy-list old)) (assigned nil) (spec nil) (other nil) (other-supplied nil)
         (deprecated nil) (deprecated-supplied nil))
    (loop while group-definitions do
      (let ((key (pop group-definitions)))
        (cond ((eq key :other-level) (unless group-definitions (error "Missing OTHER-LEVEL"))
                                     (setf other (pop group-definitions) other-supplied t))
              ((eq key :group-other) (unless group-definitions (error "Missing GROUP-OTHER"))
                                    (setf deprecated (pop group-definitions) deprecated-supplied t))
              ((consp key) (push key spec))
              (t (unless group-definitions (error "Missing group values"))
                 (push (list key (pop group-definitions)) spec)))))
    (when deprecated-supplied (%check-boolean deprecated) (warn "GROUP-OTHER is deprecated")
          (when (and deprecated (not other-supplied)) (setf other "Other")))
    (when (and other (not (or (stringp other) (na-p other)))) (error "OTHER-LEVEL must be a string or NA"))
    (dolist (pair (%level-pairs (nreverse spec)))
      (let* ((name (ensure-string (first pair))) (values (second pair))
             (previous (if (or (stringp values) (numberp values) (symbolp values))
                           (list (ensure-string values))
                           (mapcar #'ensure-string (col->list (%input-column values))))))
        (dolist (x previous)
          (let ((i (position x old :test #'equal)))
            (if i (progn (pushnew x assigned :test #'equal) (setf (nth i labels) name))
                (warn "Unknown level ~s in FCT-COLLAPSE" x))))))
    (when other
      (loop for x in old for i from 0 unless (member x assigned :test #'equal) do (setf (nth i labels) other)))
    (let ((out (lvls-revalue f (make-typed-column (coerce labels 'vector) :string))))
      (if (and other (member other labels :test #'equal))
          (%refactor out (append (remove other (%unique-values labels) :test #'equal) (list other)) (factor-ordered out))
          out))))

(defun fct-lump (f &key n prop (w nil w-supplied) (other-level "Other") (ties-method "min")
                       (seed nil seed-supplied) (random-state *random-state*))
  "R: forcats::fct_lump(). Apply count or proportion criteria through the lumping family.
Legacy omission identity and N-over-PROP precedence remain pending the migration decision.
W weights observations; random ties accept a local SEED or explicit RANDOM-STATE."
  (let* ((f (%check-factor f))
         (options (append (when w-supplied (list :w w))
                         (list :other-level (if (na-p other-level) other-level (ensure-string other-level))))))
    (cond (n (apply #'fct-lump-n f n (append options (list :ties-method ties-method :random-state random-state)
                                           (when seed-supplied (list :seed seed)))))
          (prop (apply #'fct-lump-prop f prop options))
          (t f))))

(defun %legacy-selection-labels (x)
  (if (listp x) (mapcar (lambda (v) (if (na-p v) v (ensure-string v))) x)
      (%characters x)))

(defun fct-other (f &key keep drop (other-level "Other"))
  "R: forcats::fct_other(). Replace selected or unselected levels with OTHER-LEVEL last.
KEEP and DROP accept character columns. Legacy omission identity, KEEP precedence,
and scalar coercion within selection lists remain pending compatibility decisions."
  (let* ((f (%check-factor f)) (levels (factor-levels f))
         (label (if (na-p other-level) other-level (ensure-string other-level))))
    (cond (keep (let ((selected (%legacy-selection-labels keep)))
                  (%levels-other f (map 'vector (lambda (x) (not (null (member x selected :test #'equal)))) levels) label)))
          (drop (let ((selected (%legacy-selection-labels drop)))
                  (%levels-other f (map 'vector (lambda (x) (null (member x selected :test #'equal))) levels) label)))
          (t f))))
