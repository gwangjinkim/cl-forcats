(in-package #:cl-forcats)

(defun fct-count (f &key sort prop)
  "Count occurrences of each level in factor F."
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
  "Return unique values in order of appearance."
  (let ((seen (make-hash-table :test 'eql))
        (result nil)
        (levels (factor-levels f)))
    (loop for x across (factor-data f)
          do (when (and x (> x 0) (not (gethash x seen)))
               (setf (gethash x seen) t)
               (push (aref levels (1- x)) result)))
    (nreverse result)))
