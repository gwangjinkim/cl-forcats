(in-package #:cl-forcats)

(defun %copy-factor-codes (f codes levels)
  (let ((out (make-factor codes :levels levels :ordered (factor-ordered f))))
    (when (col-names f) (setf (col-names out) (col-names f)))
    out))

(defun lvls-reorder (f idx &key (ordered *na*))
  "R: forcats::lvls_reorder(). Permute factor levels with zero-based indices.
IDX must contain every existing index once. :ORDERED NA preserves status."
  (let* ((f (%check-factor f)) (col (%input-column idx))
         (indices (col->list col)) (n (length (factor-levels f))))
    (unless (and (member (col-type col) '(:int :double)) (= n (length indices))
                 (every (lambda (i) (and (realp i) (not (%nan-p i)) (<= 0 i) (< i n) (= i (truncate i)))) indices)
                 (= n (length (remove-duplicates indices :test #'=))))
      (error "IDX must contain every zero-based level index exactly once"))
    (%refactor f (mapcar (lambda (i) (aref (factor-levels f) (truncate i))) indices)
               (%ordered-option ordered f))))

(defun lvls-revalue (f new-levels)
  "R: forcats::lvls_revalue(). Replace each level label, merging duplicate labels.
NEW-LEVELS is a character column matching the old level count; NA is a real level."
  (let* ((f (%check-factor f)) (labels (%characters new-levels))
         (unique (%unique-values labels)))
    (unless (= (length labels) (length (factor-levels f)))
      (error "NEW-LEVELS must match the existing level count"))
    (%copy-factor-codes f
                        (map 'vector (lambda (code) (if (zerop code) 0
                                                       (1+ (position (nth (1- code) labels) unique :test #'equal))))
                             (factor-data f)) unique)))

(defun lvls-expand (f new-levels)
  "R: forcats::lvls_expand(). Set a complete level sequence including all old levels."
  (let* ((f (%check-factor f)) (labels (%characters new-levels)))
    (unless (every (lambda (old) (member old labels :test #'equal)) (coerce (factor-levels f) 'list))
      (error "NEW-LEVELS must include all existing levels"))
    (%refactor f labels (factor-ordered f))))

(defun fct-relabel (f fun &rest args)
  "R: forcats::fct_relabel(). Call FUN on the complete level vector, merging labels.
ARGS are passed after that vector. The callback must return character labels."
  (let ((f (%check-factor f)))
    (lvls-revalue f (apply fun (make-typed-column (copy-seq (factor-levels f)) :string) args))))

(defun %cell-missing-p (col i)
  (if (factor-p col) (zerop (aref (factor-data col) i))
      (let ((value (col-ref col i))) (or (na-p value) (%nan-p value)))))

(defun %column-key (col i)
  (if (factor-p col) (aref (factor-data col) i) (col-ref col i)))

(defun %value-less (a b)
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        ((and (member a '(t nil)) (member b '(t nil))) (and (null a) b))
        (t (let* ((pkg (find-package :local-time))
                  (sym (and pkg (find-symbol "TIMESTAMP<" pkg))))
             (if (and sym (fboundp sym)) (funcall sym a b)
                 (error "Unsupported summary/order values ~s and ~s" a b))))))

(defun %owner-subseq (col indices &optional names)
  (let ((out (if (factor-p col)
                 (make-factor (map 'vector (lambda (i) (if i (aref (factor-data col) i) 0)) indices)
                              :levels (factor-levels col) :ordered (factor-ordered col))
                 (restore-column (map 'vector (lambda (i) (if i (col-ref col i) *na*)) indices) (vec-prototype col)))))
    (when names
      (let ((original (col-names col)))
        (when original (setf (col-names out) (map 'vector (lambda (i) (if i (aref original i) *na*)) indices)))))
    out))

(defun %terminal-value (x y desc)
  (let* ((x (%input-column x)) (y (%input-column y))
         (nx (col-length x)) (ny (col-length y))
         (n (if (or (zerop nx) (zerop ny)) 0 (max nx ny)))
         (positions nil))
    (when (and (plusp n) (or (not (zerop (mod n nx))) (not (zerop (mod n ny)))))
      (warn "Longer missing-value mask is not a multiple of shorter input"))
    (dotimes (i n)
      ;; R recycles the missing-value masks before subsetting; beyond-end
      ;; cells in the shorter input are added by that logical subset.
      (unless (or (%cell-missing-p x (mod i nx)) (%cell-missing-p y (mod i ny)))
        (push i positions)))
    (setf positions (nreverse positions))
    (let* ((ordered (stable-sort positions
                                 (lambda (i j)
                                   (cond ((>= i nx) nil) ((>= j nx) t)
                                         (t (if desc (%value-less (%column-key x j) (%column-key x i))
                                                (%value-less (%column-key x i) (%column-key x j))))))))
           (i (first ordered)))
      (%owner-subseq y (list (when (and i (< i ny)) i))))))

(defun first2 (x y)
  "R: forcats::first2(). Return the first Y cell after stable ascending X order.
Rows missing in either input are removed; no remaining rows gives typed NA."
  (%terminal-value x y nil))

(defun last2 (x y)
  "R: forcats::last2(). Return the first Y cell after stable descending X order.
Rows missing in either input are removed; no remaining rows gives typed NA."
  (%terminal-value x y t))

(defun %scalar-summary (x)
  (cond ((factor-p x) (unless (= 1 (col-length x)) (error "Summary must be scalar"))
                         (if (%cell-missing-p x 0) *na* (aref (factor-data x) 0)))
        ((and (column-p x) (not (stringp x)) (not (numberp x)) (not (member x '(t nil)))) (unless (= 1 (col-length x)) (error "Summary must be scalar")) (col-ref x 0))
        ((or (numberp x) (stringp x) (member x '(t nil)) (na-p x)) x)
        (t (error "Summary must be one atomic value"))))

(defun %check-boolean (x)
  (unless (or (eq x t) (null x)) (error "Option must be TRUE or FALSE")) x)

(defun %summary-common (values)
  (let ((strings (find-if #'stringp values)) (numbers (find-if #'numberp values)))
    (map 'vector (lambda (x)
                   (cond ((or (na-p x) (%nan-p x)) x)
                         (strings (cond ((stringp x) x) ((eq x t) "TRUE") ((null x) "FALSE")
                                        ((= x (%signed-infinity t)) "-Inf") ((= x (%signed-infinity nil)) "Inf")
                                        (t (%number-label x))))
                         ((and numbers (member x '(t nil))) (if x 1 0))
                         (t x))) values)))

(defun %summary-order (f summaries desc)
  (let ((indices (loop for i below (length summaries) collect i)))
    (setf summaries (%summary-common summaries))
    (setf indices (stable-sort indices (lambda (i j)
                                       (let ((a (aref summaries i)) (b (aref summaries j)))
                                         (cond ((or (na-p a) (%nan-p a)) nil)
                                               ((or (na-p b) (%nan-p b)) t)
                                               (desc (%value-less b a))
                                               (t (%value-less a b)))))))
    (lvls-reorder f (make-typed-column (coerce indices 'vector) :int))))

(defun fct-reorder2 (f x y &key (fun #'last2) (na-rm nil na-supplied)
                                  (default (%signed-infinity t)) (desc t) args)
  "R: forcats::fct_reorder2(). Reorder levels by a two-column summary callback.
Default LAST2 sorts decreasingly, with -Inf for empty levels. FUN receives
owner-preserving columns in observation order, then :ARGS. Omitted :NA-RM
removes missing X/Y rows with a warning; T is silent, NIL preserves rows."
  (%check-boolean desc)
  (when na-supplied (%check-boolean na-rm))
  (let* ((f (%check-factor f)) (x (%input-column x)) (y (%input-column y))
         (n (col-length f)) (groups (make-array (length (factor-levels f)) :initial-element nil))
         (summary (make-array (length groups))) (missing 0) (retained nil))
    (unless (= n (col-length x) (col-length y)) (error "Inputs must have equal lengths"))
    (dotimes (i n)
      (let ((miss (or (%cell-missing-p x i) (%cell-missing-p y i))) (code (aref (factor-data f) i)))
        (when miss (incf missing))
        (unless (and miss (or na-rm (not na-supplied))) (push i retained))
        (when (and (> code 0) (not (and miss (or na-rm (not na-supplied)))))
          (push i (aref groups (1- code))))))
    (when (and (> missing 0) (not na-supplied)) (warn "FCT-REORDER2 removing missing rows"))
    (dotimes (j (length groups))
      (let ((indices (nreverse (aref groups j))))
        (setf (aref summary j) (%scalar-summary (if indices
                                                  (apply fun (%owner-subseq x indices t) (%owner-subseq y indices t) args)
                                                  default)))))
    (%summary-order (%owner-subseq f (nreverse retained) t) summary desc)))
