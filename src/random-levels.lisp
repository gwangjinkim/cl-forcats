(in-package #:cl-forcats)

;; Independent mathematical MT19937 implementation. Constants describe its
;; 32-bit recurrence and tempering; no R implementation code is incorporated.
;; Only the explicit seeded path follows R's default MT/rejection protocol.
(defstruct (%factor-rng (:constructor %factor-rng (words))) words (index 624))

(defun %seed-factor-rng (seed)
  (unless (and (integerp seed) (< (- (expt 2 31)) seed) (< seed (expt 2 31)))
    (error "SEED must be a nonmissing signed 32-bit integer"))
  (let ((state (logand seed #xffffffff))
        (words (make-array 624 :element-type '(unsigned-byte 32))))
    (dotimes (i 51) (setf state (logand (1+ (* state 69069)) #xffffffff)))
    (dotimes (i 624)
      (setf state (logand (1+ (* state 69069)) #xffffffff)
            (aref words i) state))
    (%factor-rng words)))

(defun %factor-rng-word (rng)
  (let ((words (%factor-rng-words rng)))
    (when (= (%factor-rng-index rng) 624)
      (dotimes (i 624)
        (let* ((joined (logior (logand (aref words i) #x80000000)
                              (logand (aref words (mod (1+ i) 624)) #x7fffffff)))
               (twist (logxor (ash joined -1) (if (oddp joined) #x9908b0df 0))))
          (setf (aref words i) (logxor (aref words (mod (+ i 397) 624)) twist))))
      (setf (%factor-rng-index rng) 0))
    (let ((value (aref words (%factor-rng-index rng))))
      (incf (%factor-rng-index rng))
      (setf value (logxor value (ash value -11))
            value (logxor value (logand (ash value 7) #x9d2c5680))
            value (logxor value (logand (ash value 15) #xefc60000))
            value (logxor value (ash value -18)))
      (logand value #xffffffff))))

(defun %factor-random-index (n rng)
  (let* ((bits (integer-length (1- n))) (mask (1- (ash 1 bits))))
    (loop
      (let ((value 0))
        (loop for chunk from 0 to bits by 16
              do (setf value (logior (ash value 16) (ash (%factor-rng-word rng) -16))))
        (setf value (logand value mask))
        (when (< value n) (return value))))))

(defun %factor-permutation (n seed supplied random-state)
  (let ((available (coerce (loop for i below n collect i) 'vector))
        (out (make-array n)) (rng (when supplied (%seed-factor-rng seed))))
    (unless supplied
      (unless (typep random-state 'random-state) (error "RANDOM-STATE must be a CL random state")))
    (dotimes (i n out)
      (let* ((left (- n i)) (choice (if rng (%factor-random-index left rng) (random left random-state))))
        (setf (aref out i) (aref available choice)
              (aref available choice) (aref available (1- left)))))))

(defun fct-shuffle (f &key (seed nil supplied) (random-state *random-state*))
  "R: forcats::fct_shuffle(). Randomly permute factor levels, retaining cells.
:SEED reproduces R's default MT19937/rejection stream locally. Otherwise
:RANDOM-STATE is the CL stream to use. No R process or global R state exists."
  (let ((f (%check-factor f)))
    (lvls-reorder f (make-typed-column (%factor-permutation (length (factor-levels f)) seed supplied random-state) :int))))

(defun fct-anon (f &key (prefix "") (seed nil supplied) (random-state *random-state*))
  "R: forcats::fct_anon(). Replace levels with shuffled, zero-padded numeric labels.
:PREFIX is a string. :SEED reproduces R's default MT/rejection stream;
otherwise :RANDOM-STATE supplies a CL random stream. Empty factors error as in R."
  (unless (stringp prefix) (error "PREFIX must be a string"))
  (let* ((f (%check-factor f)) (n (length (factor-levels f)))
         (width (length (princ-to-string n)))
         (labels (coerce (loop for i from 1 to n collect (format nil "~a~v,'0d" prefix width i)) 'vector))
         (permutation (%factor-permutation n seed supplied random-state)))
    (when (zerop n) (error "Cannot anonymise an empty factor"))
    (let ((renamed (lvls-revalue f (map 'vector (lambda (i) (aref labels i)) permutation))))
      (lvls-reorder renamed (map 'vector (lambda (label) (position label (factor-levels renamed) :test #'equal)) labels)))))
