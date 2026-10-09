(in-package #:cl-forcats-tests)
(fiveam:in-suite :cl-forcats-suite)
(fiveam:test x2-factor-column-protocol-and-recoding
  (let* ((f (cl-forcats:make-factor #(1 2 0) :levels '("b" "a")))
         (p (cl-vctrs-lite:make-prototype :factor :levels '("a" "b" "c")))
         (cast (cl-vctrs-lite:vec-cast f p)))
    (fiveam:is (eq :factor (cl-vctrs-lite:col-type f)))
    (fiveam:is (= 3 (cl-vctrs-lite:col-length f)))
    (fiveam:is (equal "b" (cl-vctrs-lite:col-ref f 0)))
    (fiveam:is (cl-vctrs-lite:na-p (cl-vctrs-lite:col-ref f 2)))
    (fiveam:is (equalp #(2 1 0) (cl-forcats:factor-data cast)))
    (fiveam:is (equalp #("a" "b" "c") (cl-forcats:factor-levels cast)))
    (fiveam:is (equalp #(0 1) (cl-forcats:factor-data (cl-vctrs-lite:col-subseq f '(2 0)))))
    (fiveam:is (equalp (vector "b" "a" cl-vctrs-lite:*na*) (cl-vctrs-lite:vec-cast f :string)))))
(fiveam:test x2-factor-union-and-loss
  (let ((f (cl-vctrs-lite:vec-c (cl-forcats:make-factor #(1 0) :levels '("b" "a"))
                                 (cl-forcats:make-factor #(1) :levels '("c" "b")))))
    (fiveam:is (equalp #("b" "a" "c") (cl-forcats:factor-levels f)))
    (fiveam:is (equalp #(1 0 3) (cl-forcats:factor-data f))))
  (fiveam:signals cl-vctrs-lite:cast-error
    (cl-vctrs-lite:vec-cast #("unknown") (cl-vctrs-lite:make-prototype :factor :levels '("a"))))
  (fiveam:signals cl-vctrs-lite:cast-error
    (cl-vctrs-lite:vec-c (cl-forcats:make-factor #(1) :levels '("a" "b") :ordered t)
                           (cl-forcats:make-factor #(1) :levels '("b" "a") :ordered t))))

(fiveam:test x2-ordered-factor-casts-require-identical-order
  (fiveam:signals cl-vctrs-lite:cast-error
    (cl-vctrs-lite:vec-cast (cl-forcats:make-factor #(1) :levels '("a" "b") :ordered t)
      (cl-vctrs-lite:make-prototype :factor :levels '("b" "a") :ordered t)))
  (fiveam:signals cl-vctrs-lite:cast-error
    (cl-vctrs-lite:vec-cast (cl-forcats:make-factor #(1) :levels '("a" "b") :ordered t)
      (cl-vctrs-lite:make-prototype :factor :levels '("a" "b")))))

(fiveam:test x2-cast-matrix-factor-boundary
  (dolist (target '(:bool :int :double :string :date :datetime :time :duration :factor :list))
    (let ((result (handler-case (cl-vctrs-lite:vec-cast (cl-vctrs-lite:vec-init :factor 1) target)
                    (cl-vctrs-lite:cast-error () :error))))
      (fiveam:is (eq (not (eq result :error)) (not (null (member target '(:string :factor)))))))
    (unless (eq target :factor)
      (let ((result (handler-case (cl-vctrs-lite:vec-cast (cl-vctrs-lite:vec-init target 1) :factor)
                      (cl-vctrs-lite:cast-error () :error))))
        (fiveam:is (eq (not (eq result :error)) (eq target :string))))))
)
