(in-package #:cl-forcats)

(defun fct-drop (f &key (only nil supplied))
  "R: forcats::fct_drop(). Remove unused levels, optionally restricted by ONLY.
Omit ONLY for all unused levels. Supplied ONLY must be character. Names/order remain."
  (let* ((f (%check-factor f)) (labels (when supplied (%characters only)))
         (counts (%frequency-weights f nil nil))
         (levels (loop for level across (factor-levels f) for count across counts
                       unless (and (zerop count) (or (not supplied) (member level labels :test #'equal))) collect level)))
    (%refactor f levels (factor-ordered f))))

(defun fct-expand (f &rest additional-levels)
  "R: forcats::fct_expand(). Add unique labels after :AFTER (default positive infinity).
Character columns may be spliced among scalar labels. Legacy scalar label coercion
remains available pending its validation migration. Names and orderedness remain."
  (let* ((f (%check-factor f)) (old (coerce (factor-levels f) 'list))
         (option (position :after additional-levels))
         (after (if option (nth (1+ option) additional-levels) (%signed-infinity nil)))
         (spec (if option (subseq additional-levels 0 option) additional-levels))
         (labels (loop for x in spec append
                       (cond ((na-p x) (list x)) ((or (numberp x) (symbolp x)) (list (ensure-string x)))
                             (t (%characters x)))))
         (new (remove-if (lambda (x) (member x old :test #'equal)) (%unique-values labels))))
    (when (and option (/= (+ option 2) (length additional-levels))) (error "Invalid AFTER options"))
    (%number-option after 0)
    (when (and (< after (length old)) (/= after (truncate after)))
      (error "Fractional insertion omits existing levels"))
    (let ((offset (truncate (min (length old) after))))
      (%refactor f (append (subseq old 0 offset) new (subseq old offset)) (factor-ordered f)))))

(defun fct-explicit-na (f &key (na-level "(Missing)"))
  "Convert NA (0) references in factor F to a named level.
The new level is added to the end of the levels vector."
  (let* ((data (factor-data f))
         (levels (coerce (factor-levels f) 'list))
         (has-na (some (lambda (x) (or (null x) (zerop x))) data)))
    (if has-na
        (let* ((new-levels (append levels (list (ensure-string na-level))))
               (na-idx (length new-levels))
               (new-data (map 'vector (lambda (x) (if (or (null x) (zerop x)) na-idx x)) data)))
          (make-factor new-data :levels new-levels :ordered (factor-ordered f)))
        f)))
