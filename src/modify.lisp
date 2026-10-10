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

(defun fct-lump (f &key n prop (other-level "Other"))
  "Group rare levels into a single 'Other' level.
If N is provided, keeps the top N levels.
If PROP is provided, keeps levels that appear at least PROP fraction of the time."
  (let* ((counts (%legacy-count f :sort t))
         (to-lump nil))
    
    (cond
      (n
       (setf to-lump (mapcar (lambda (x) (getf x :level)) (subseq counts (min n (length counts))))))
      (prop
       (setf to-lump (mapcar (lambda (x) (getf x :level))
                             (remove-if (lambda (x) (>= (getf x :p) prop)) counts))))
      (t
       ;; Default lump if none specified? R lumps the smallest if we don't specify, but let's stick to requirements.
       ))
    
    (if to-lump
        (fct-collapse f (list other-level to-lump))
        f)))

(defun fct-other (f &key keep drop (other-level "Other"))
  "Specifically keep or drop certain levels into 'Other'."
  (let* ((levels (coerce (factor-levels f) 'list))
         (to-drop (cond
                    (keep (remove-if (lambda (l) (member l (mapcar #'ensure-string keep) :test #'string=)) levels))
                    (drop (mapcar #'ensure-string drop))
                    (t nil))))
    (if to-drop
        (fct-collapse f (list other-level to-drop))
        f)))
