(in-package #:cl-forcats)

(defun fct-rev (f)
  "Reverse the order of levels."
  (let* ((old-levels (factor-levels f))
         (n (length old-levels))
         (new-levels (reverse (coerce old-levels 'list)))
         (new-data (map 'vector
                        (lambda (x)
                          (if (and x (> x 0))
                              (- (1+ n) x)
                              x))
                        (factor-data f))))
    (make-factor new-data :levels new-levels :ordered (factor-ordered f))))

(defun fct-relevel (f &rest levels)
  "Manually move LEVELS to the front of the factor levels."
  (let* ((old-levels (coerce (factor-levels f) 'list))
         (to-move (mapcar #'ensure-string levels))
         (remaining (remove-if (lambda (l) (member l to-move :test #'string=)) old-levels))
         (new-levels (append to-move remaining))
         (mapping (make-hash-table :test 'equal))
         (new-data (make-array (length (factor-data f)) :element-type (array-element-type (factor-data f)))))
    
    ;; Create mapping from old index to new index
    ;; old-levels is 0-indexed in the list, so level at index i has index i+1 in factor data
    (loop for level in new-levels
          for new-idx from 1
          do (let ((old-idx-zero (position level old-levels :test #'string=)))
               (when old-idx-zero
                 (setf (gethash (1+ old-idx-zero) mapping) new-idx))))
    
    (loop for i from 0 below (length (factor-data f))
          for x = (aref (factor-data f) i)
          do (setf (aref new-data i) (if (and x (> x 0)) (gethash x mapping 0) x)))
    
    (make-factor new-data :levels new-levels :ordered (factor-ordered f))))

(defun fct-infreq (f)
  "Reorder levels by frequency."
  (let* ((counts (fct-count f :sort t))
         (new-levels (mapcar (lambda (x) (getf x :level)) counts)))
    (apply #'fct-relevel f new-levels)))

(defun fct-reorder (f v &key (fun #'mean) desc)
  "Reorder levels of factor F by another numeric vector V using summary function FUN.
FUN defaults to #'MEAN. If DESC is T, sorts in descending order."
  (let* ((data (factor-data f))
         (levels (factor-levels f))
         (n (length levels))
         (groups (make-hash-table :test 'eql)))
    
    ;; Group values of V by factor levels
    (loop for i from 0 below (length data)
          for level-idx = (aref data i)
          for val = (aref v i)
          do (when (and level-idx (> level-idx 0))
               (push val (gethash level-idx groups))))
    
    ;; Calculate FUN for each group
    (let ((summary (loop for i from 1 to n
                         collect (list :level (aref levels (1- i))
                                       :val (funcall fun (gethash i groups))))))
      ;; Sort levels by summary value
      (setf summary (sort summary (if desc #'> #'<) :key (lambda (x) (getf x :val))))
      (let ((new-levels (mapcar (lambda (x) (getf x :level)) summary)))
        (apply #'fct-relevel f new-levels)))))

(defun fct-shift (f &key (n 1))
  "Shift levels N positions to the left (negative N shifts right)."
  (let* ((levels (coerce (factor-levels f) 'list))
         (len (length levels)))
    (if (zerop len)
        f
        (let* ((shift (mod n len))
               (new-levels (append (subseq levels shift) (subseq levels 0 shift))))
          (apply #'fct-relevel f new-levels)))))
