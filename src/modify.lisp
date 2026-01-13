(in-package #:cl-forecats)

(defun fct-recode (f &rest new-levels)
  "Rename levels of factor F. 
NEW-LEVELS can be a list of (new-name old-name) or a flattened plist: \"New\" \"Old\"."
  (let* ((levels (coerce (factor-levels f) 'list))
         (mapping (make-hash-table :test 'equal))
         (current-levels (copy-list levels)))
    
    ;; Handle both list of lists and flattened plist
    (let ((pairs (if (listp (car new-levels)) new-levels (plist-to-pairs new-levels))))
      (loop for pair in pairs
            do (let ((new (ensure-string (first pair)))
                     (old (ensure-string (second pair))))
                 (setf (gethash old mapping) new))))
    
    (let ((updated-levels (mapcar (lambda (l) (gethash l mapping l)) current-levels)))
      (make-factor (factor-data f) :levels updated-levels :ordered (factor-ordered f)))))

(defun fct-collapse (f &rest group-definitions)
  "Collapse multiple levels into one. GROUP-DEFINITIONS is (group-name (old-level1 old-level2 ...))."
  (let* ((old-levels (coerce (factor-levels f) 'list))
         (new-levels nil)
         (mapping (make-hash-table :test 'equal)) ; level -> group
         (data (factor-data f))
         (new-data (make-array (length data) :element-type (array-element-type data))))
    
    (let ((groups (if (listp (car group-definitions)) group-definitions (plist-to-pairs group-definitions))))
      (loop for group in groups
            for group-name = (ensure-string (first group))
            for old-list = (mapcar #'ensure-string (alexandria:ensure-list (second group)))
            do (loop for old in old-list
                     do (setf (gethash old mapping) group-name))))
    
    ;; Determine new levels
    (loop for l in old-levels
          for group-name = (gethash l mapping)
          do (if group-name
                 (pushnew group-name new-levels :test #'string=)
                 (pushnew l new-levels :test #'string=)))
    (setf new-levels (nreverse new-levels))
    
    ;; Map data
    (let ((old-idx-to-new-idx (make-hash-table :test 'eql)))
      (loop for old-l in old-levels
            for old-idx from 1
            do (let* ((group-name (gethash old-l mapping old-l))
                      (new-idx (1+ (position group-name new-levels :test #'string=))))
                 (setf (gethash old-idx old-idx-to-new-idx) new-idx)))
      
      (loop for i from 0 below (length data)
            for x = (aref data i)
            do (setf (aref new-data i) (if (and x (> x 0)) (gethash x old-idx-to-new-idx 0) x))))
    
    (make-factor new-data :levels new-levels :ordered (factor-ordered f))))

(defun fct-lump (f &key n prop (other-level "Other"))
  "Group rare levels into a single 'Other' level.
If N is provided, keeps the top N levels.
If PROP is provided, keeps levels that appear at least PROP fraction of the time."
  (let* ((counts (fct-count f :sort t))
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
