(in-package #:cl-forcats)

(defun %input-column (x)
  (cond ((stringp x) (make-typed-column (vector x) :string))
        ((or (numberp x) (eq x t) (null x) (na-p x))
         (make-typed-column (vector x) (if (numberp x) (if (integerp x) :int :double) :bool)))
        ((and (listp x) x (every (lambda (v) (or (stringp v) (na-p v))) x)) (make-typed-column (coerce x 'vector) :string))
        ((column-p x) x)
        (t (error "Expected an atomic column, got ~s" x))))

(defun %characters (x &optional null-ok)
  (if (and null-ok (null x)) nil
      (let ((col (%input-column x)))
        (unless (eq (col-type col) :string) (error "Expected a character vector"))
        (col->list col))))

(defun %unique-values (xs)
  (remove-duplicates xs :test #'equal :from-end t))

(defun %factor-values (values levels &key ordered names strict)
  (when (/= (length levels) (length (%unique-values levels)))
    (error "Duplicated factor levels"))
  (let ((result (make-factor
                 (map 'vector (lambda (x)
                                (let ((i (position x levels :test #'equal)))
                                  (cond (i (1+ i))
                                        ((na-p x) 0)
                                        (strict (error "Unknown factor value ~s" x))
                                        (t 0)))) values)
                 :levels levels :ordered ordered)))
    (when names (setf (col-names result) names))
    result))

(defun fct (&optional (x (vec-init :string 0)) &key levels (na (vec-init :string 0)))
  "R: forcats::fct(). Create a strict factor with levels in appearance order.
LEVELS defaults to inferred levels; :NA lists character values to mark missing."
  (let* ((values (%characters x))
         (missing (%characters na))
         (clean (mapcar (lambda (v) (if (member v missing :test #'equal) *na* v)) values))
         (levels (if levels (%characters levels) (%unique-values (remove-if #'na-p clean)))))
    (%factor-values clean levels :strict t :names (col-names (%input-column x)))))

(defun %nan-p (x)
  #+sbcl (and (floatp x) (sb-ext:float-nan-p x))
  #-sbcl (declare (ignore x))
  #-sbcl nil)

(defun %number< (a b)
  (and (not (%nan-p a)) (or (%nan-p b) (< a b))))

(defun %trim-decimal (s)
  (if (find #\. s) (string-right-trim '(#\.) (string-right-trim '(#\0) s)) s))

(defun %number-label (x)
  (cond ((%nan-p x) "NaN")
        #+sbcl ((and (floatp x) (sb-ext:float-infinity-p x)) (if (minusp x) "-Inf" "Inf"))
        ((integerp x) (format nil "~d" x))
        ((zerop x) "0")
        (t (let* ((raw (string-downcase (format nil "~,14e" (coerce x 'double-float))))
                  (marker (or (position #\d raw) (position #\e raw)))
                  (exponent (parse-integer raw :start (1+ marker)))
                  (mantissa (%trim-decimal (subseq raw 0 marker)))
                  (scientific (format nil "~ae~a~2,'0d" mantissa (if (minusp exponent) "-" "+") (abs exponent)))
                  (fixed (%trim-decimal (format nil "~,vf" (max 0 (- 14 exponent)) (coerce x 'double-float)))))
             (if (< (length scientific) (length fixed)) scientific fixed)))))

(defgeneric as-factor (x)
  (:documentation
   "R: forcats::as_factor(). Convert an atomic column or preserve a factor.
Methods may specialize user types; the default method validates supported
character, numeric and logical columns."))

(defmethod as-factor ((x factor)) x)

(defmethod as-factor ((x t))
  "R: forcats::as_factor(). Preserve factors and infer atomic factor levels.
Characters use first appearance; numeric levels sort numerically; logical
levels always include FALSE then TRUE. Scalar NIL is logical FALSE."
  (when (listp x) (unless (null x) (error "AS-FACTOR has no list method")))
  (if (factor-p x) x
      (let* ((col (%input-column x)) (type (col-type col)) (values (col->list col)))
        (unless (member type '(:string :int :double :bool)) (error "Unsupported factor input"))
        (let* ((known (remove-if #'na-p values))
               (labels (mapcar (lambda (v) (cond ((na-p v) *na*)
                                                 ((eq type :bool) (if v "TRUE" "FALSE"))
                                                 ((stringp v) v) (t (%number-label v)))) values))
               (levels (case type
                         (:bool '("FALSE" "TRUE"))
                         (:string (%unique-values known))
                         (otherwise (%unique-values (mapcar #'%number-label (stable-sort (%unique-values known) #'%number<)))))))
          (%factor-values labels levels :names (col-names col))))))

(defun %check-factor (x)
  (if (factor-p x) x
      (let* ((values (%characters x))
             (levels (sort (%unique-values (remove-if #'na-p values)) #'string<)))
        (%factor-values values levels :names (col-names (%input-column x))))))

(defun %ordered-option (ordered f)
  (cond ((na-p ordered) (factor-ordered f))
        ((or (eq ordered t) (null ordered)) ordered)
        (t (error "ORDERED must be TRUE, FALSE or NA"))))

(defun %refactor (f levels ordered)
  (%factor-values (col->list f) levels :ordered ordered :names (col-names f)))

(defun fct-inorder (f &key (ordered *na*))
  "R: forcats::fct_inorder(). Order levels by first appearance, then unused levels.
:ORDERED defaults to NA, preserving the input ordered status."
  (let* ((f (%check-factor f))
         (seen (%unique-values (remove 0 (coerce (factor-data f) 'list))))
         (indices (append seen (loop for i from 1 to (length (factor-levels f))
                                     unless (member i seen) collect i))))
    (%refactor f (mapcar (lambda (i) (aref (factor-levels f) (1- i))) indices)
               (%ordered-option ordered f))))

(defun %signed-infinity (negative)
  #+sbcl (if negative sb-ext:double-float-negative-infinity sb-ext:double-float-positive-infinity)
  #-sbcl (error "Infinite level coercion requires IEEE float support"))

(defun %level-double (value)
  ;; R's numeric level coercion always produces IEEE doubles. Exact CL
  ;; integers/rationals would otherwise split rounded ties or retain
  ;; hexadecimal values which R underflows to zero.
  #+sbcl (sb-int:with-float-traps-masked (:overflow :underflow :inexact)
            (coerce value 'double-float))
  #-sbcl (handler-case (coerce value 'double-float)
           (floating-point-overflow () (%signed-infinity (minusp value)))
           (floating-point-underflow () (if (minusp value) -0d0 0d0))))

(defun %parse-level-number (x)
  (when (stringp x)
    (let* ((trim (string-trim '(#\Space #\Tab #\Newline #\Return) x))
           (negative (and (plusp (length trim)) (eql (char trim 0) #\-))))
      (cond
        ((member trim '("Inf" "+Inf" "Infinity" "+Infinity") :test #'string-equal) (%signed-infinity nil))
        ((member trim '("-Inf" "-Infinity") :test #'string-equal) (%signed-infinity t))
        ((cl-ppcre:scan "^[+-]?0[xX](?:[0-9a-fA-F]+(?:\\.[0-9a-fA-F]*)?|\\.[0-9a-fA-F]+)(?:[pP][+-]?[0-9]+)?$" trim)
         (let* ((offset (if (find (char trim 0) "+-") 3 2))
                (marker (position-if (lambda (c) (find c "pP")) trim))
                (digits (subseq trim offset marker))
                (point (position #\. digits))
                (whole (if (and point (zerop point)) 0 (parse-integer digits :radix 16 :end point)))
                (fraction (if (and point (< (1+ point) (length digits)))
                              (/ (parse-integer digits :radix 16 :start (1+ point)) (expt 16 (- (length digits) point 1))) 0))
                (exponent (if marker (parse-integer trim :start (1+ marker)) 0)))
           (%level-double (* (if negative -1 1) (+ whole fraction) (expt 2 exponent)))))
        ((cl-ppcre:scan "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$" trim)
         (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
           (handler-case (%level-double (read-from-string trim))
             (reader-error ()
               ;; The decimal grammar has already been checked; overflow in
               ;; CL's float reader corresponds to R's signed infinity.
               (%signed-infinity negative)))))))))

(defun fct-inseq (f &key (ordered *na*))
  "R: forcats::fct_inseq(). Sort levels by their numeric value, then nonnumeric levels."
  (let* ((f (%check-factor f))
         (pairs (loop for level across (factor-levels f) collect (cons level (%parse-level-number level)))))
    (unless (some #'cdr pairs) (error "At least one level must be numeric"))
    (setf pairs (stable-sort pairs (lambda (a b) (and (cdr a) (or (not (cdr b)) (< (cdr a) (cdr b)))))))
    (%refactor f (mapcar #'car pairs) (%ordered-option ordered f))))

(defun %factor-list (fs)
  (let ((xs (cond ((null fs) nil)
                  ((and (listp fs) (every (lambda (x) (and (consp x) (stringp (car x)))) fs)) (mapcar #'cdr fs))
                  ((listp fs) fs)
                  ((and (column-p fs) (eq :list (col-type fs))) (col->list fs))
                  (t (error "Expected a list of factors")))))
    (unless (every #'factor-p xs) (error "Every list element must be a factor"))
    xs))

(defun lvls-union (fs)
  "R: forcats::lvls_union(). Return the union of levels in factor-list order."
  (coerce (%unique-values (mapcan (lambda (f) (coerce (factor-levels f) 'list)) (%factor-list fs))) 'vector))

(defun fct-unify (fs &key (levels nil supplied))
  "R: forcats::fct_unify(). Apply a common complete level set to every factor.
FS is a proper list, string-keyed alist or typed :LIST; names are preserved."
  (let* ((factors (%factor-list fs))
         (levels (if supplied (%characters levels) (coerce (lvls-union fs) 'list)))
         (result (mapcar (lambda (f)
                           (unless (every (lambda (l) (member l levels :test #'equal)) (coerce (factor-levels f) 'list))
                             (error "New levels must include all existing levels"))
                           (%refactor f levels (factor-ordered f))) factors)))
    (cond ((and fs (listp fs) (consp (car fs)) (stringp (caar fs)))
           (mapcar (lambda (entry value) (cons (car entry) value)) fs result))
          ((and fs (not (listp fs)) (eq :list (col-type fs)))
           (let ((out (make-typed-column (coerce result 'vector) :list)))
             (when (col-names fs) (setf (col-names out) (col-names fs))) out))
          (t result))))

(defun fct-c (&rest fs)
  "R: forcats::fct_c(). Concatenate factors and combine their levels.
Use APPLY to splice a list of factors. The result is an unordered factor."
  (let ((levels (coerce (lvls-union fs) 'list)))
    (%factor-values (mapcan #'col->list (%factor-list fs)) levels)))

(defun fct-match (f lvls)
  "R: forcats::fct_match(). Match known levels, returning a logical column.
Unknown nonmissing levels signal an error; NA also matches implicit missing values."
  (let* ((f (%check-factor f))
         (levels (if (na-p lvls) (list *na*) (%characters lvls))))
    (dolist (level levels)
      (unless (or (na-p level) (find level (factor-levels f) :test #'equal))
        (error "Unknown level ~s" level)))
    (make-typed-column (map 'vector (lambda (value) (not (null (member value levels :test #'equal)))) (col->list f)) :bool)))

(defun fct-cross (&rest args)
  "R: forcats::fct_cross(). Cross factors/character columns with scalar recycling.
Trailing :SEP string and :KEEP-EMPTY boolean set separator and unused combinations."
  (let* ((split (position-if #'keywordp args)) (inputs (if split (subseq args 0 split) args))
         (options (if split (subseq args split) nil))
         (sep (getf options :sep ":")) (keep (getf options :keep-empty nil)))
    (unless (and (evenp (length options)) (loop for (key val) on options by #'cddr always (member key '(:sep :keep-empty))))
      (error "Unknown cross option"))
    (unless (stringp sep) (error "SEP must be a string"))
    (unless (or (eq keep t) (null keep)) (error "KEEP-EMPTY must be boolean"))
    (if (null inputs) (make-factor #() :levels nil)
        (let* ((fs (mapcar #'%check-factor inputs))
               (sizes (mapcar #'col-length fs))
               (n (if (member 0 sizes) 0 (apply #'max sizes)))
               (levels nil) (values nil))
          (unless (every (lambda (size) (or (= size n) (= size 1))) sizes)
            (error "Incompatible cross sizes"))
          (labels ((join (xs) (format nil "~{~a~}" (loop for x in xs for first = t then nil append (if first (list x) (list sep x)))))
                   (grid (remaining prefix)
                     (if remaining
                         (loop for level across (factor-levels (car remaining)) do (grid (cdr remaining) (append prefix (list (if (na-p level) "NA" level)))))
                         (push (join prefix) levels))))
            (grid fs nil)
            (dotimes (i n)
              (push (join (mapcar (lambda (f) (let ((v (col-ref f (if (= (col-length f) 1) 0 i)))) (if (na-p v) "NA" v))) fs)) values)))
          (setf values (nreverse values) levels (nreverse levels))
          (unless keep (setf levels (%unique-values (remove-if-not (lambda (l) (member l values :test #'equal)) levels))))
          (%factor-values values levels)))))
