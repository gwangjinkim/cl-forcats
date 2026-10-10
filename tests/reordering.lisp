(in-package #:cl-forcats-tests)
(in-suite :cl-forcats-suite)
(test fc2-level-operations-and-randomness
  (let ((f (make-factor #(2 1 0 2) :levels '("a" "b" "unused") :ordered t)))
    (is (equalp #(1 2 0 1) (factor-data (lvls-reorder f #(1 0 2)))))
    (is (equalp #(1 1 0 1) (factor-data (lvls-revalue f #("merged" "merged" "unused")))))
    (is (equalp #("ID-1" "ID-2" "ID-3") (factor-levels (fct-anon f :prefix "ID-" :seed 42))))
    (is (equalp (factor-data (fct-shuffle f :seed 42)) (factor-data (fct-shuffle f :seed 42))))
    (is (factor-ordered (fct-shuffle f :seed 42))))
  (signals error (lvls-reorder (fct #("a" "b")) #(0 0)))
  (signals error (lvls-expand (fct #("a" "b")) #("a")))
  (let ((out (last2 #(2 1 2) #(20 10 30))))
    (is (equalp #(20) out))))

(test fc2-seeded-reference-and-stream-isolation
  ;; Pinned R4.6.1 default MT19937/rejection samples for three levels.
  (let* ((f (make-factor #(2 1 0 2) :levels '("a" "b" "unused") :ordered t))
         (stream (make-random-state t)) (control (make-random-state stream)))
    (is (equalp #("a" "unused" "b") (factor-levels (fct-shuffle f :seed 42 :random-state stream))))
    (is (equalp #(3 1 0 3) (factor-data (fct-anon f :seed 42 :prefix "ID-"))))
    (is (= (random 100000 control) (random 100000 stream)))
    (signals error (fct-shuffle f :seed 2147483648))
    (signals error (fct-anon (make-factor #() :levels nil) :seed 1))))

(test fc2-owner-names-and-distinct-na-paths
  (let ((f (make-factor #(0 2 1 2) :levels (vector "a" cl-vctrs-lite:*na*) :ordered t)))
    (setf (cl-vctrs-lite:col-names f) #("implicit" "explicit" "a" "explicit"))
    ;; Label replacement operates on codes; refactoring can merge implicit NA
    ;; into an explicit missing level, as the pinned R functions do.
    (is (equalp #(0 2 1 2) (factor-data (fct-relabel f #'identity))))
    (is (equalp #(1 1 2 1) (factor-data (fct-rev f))))
    (is (equalp (cl-vctrs-lite:col-names f) (cl-vctrs-lite:col-names (fct-rev f))))
    (is (factor-ordered (fct-rev f)))
    (let ((cell (last2 #(1 2 3 4) f)))
      (is (equalp #(2) (factor-data cell)))
      (is (equalp (factor-levels f) (factor-levels cell)))
      (is (null (cl-vctrs-lite:col-names cell)))))
  (let ((cell (first2 (cl-vctrs-lite:vec-init :double 0) (cl-vctrs-lite:vec-init :string 0))))
    (is (eq :string (cl-vctrs-lite:col-type cell)))
    (is (= 1 (cl-vctrs-lite:col-length cell)))
    (is (cl-vctrs-lite:na-p (cl-vctrs-lite:col-ref cell 0)))))

(test fc2-two-column-summary-and-observation-order
  (let ((f (make-factor #(1 1 2) :levels '("a" "b"))))
    (is (equalp #(1 2) (factor-data (fct-reorder2 f (vector 1 cl-vctrs-lite:*na* 2) #(10 20 5) :na-rm t))))
    (is (= 3 (cl-vctrs-lite:col-length (fct-reorder f (vector 1 cl-vctrs-lite:*na* 2) :na-rm t))))
    (is (equalp #("b" "a")
                (factor-levels (fct-reorder (make-factor #(1 2 1 2) :levels '("a" "b"))
                                           #(10 5 1 20) :fun #'first)))))
  (let ((f (make-factor #(2 3) :levels '("unused" "a" "b"))))
    (is (equalp #("a" "b" "unused") (factor-levels (fct-reorder2 f #(1 2) #("z" "a")))))
    (is (equalp #("a" "b" "unused") (factor-levels (fct-reorder2 f #(1 2) #(t nil)))))))

(test fc2-merge-remove-and-other-placement
  (let ((f (make-factor #(1 2 3 0) :levels '("a" "b" "c"))))
    (is (equalp #(1 1 2 0) (factor-data (fct-recode f "merged" "a" "merged" "b"))))
    (is (equalp #(1 0 2 0) (factor-data (fct-recode f nil "b"))))
    (is (equalp #("group" "rest") (factor-levels (fct-collapse f "group" '("b") :other-level "rest"))))
    (is (equalp #(2 1 2 0) (factor-data (fct-collapse f "group" '("b") :other-level "rest"))))
    (is (equalp #("b" "a" "c") (factor-levels (fct-relevel f "a" :after 1))))
    (signals error (fct-relevel f "a" "a"))))

(test fc2-relevel-whole-float-and-empty-callback
  (is (equalp #("b" "a" "c")
              (factor-levels (fct-relevel (make-factor #(1 2) :levels '("a" "b" "c")) "a" :after 1d0))))
  (let ((out (fct-relevel (make-factor #() :levels nil) #'identity)))
    (is (= 0 (length (factor-levels out))))
    (is (= 0 (length (factor-data out))))))

(test fc2-integer-na-seed-and-na-level-removal
  (let ((f (make-factor #(1 2) :levels '("a" "b"))))
    (signals error (fct-shuffle f :seed -2147483648))
    (signals error (fct-anon f :seed -2147483648)))
  (let* ((f (make-factor #(0 2 1 2) :levels (vector "a" cl-vctrs-lite:*na*)))
         (out (fct-recode f nil "a")))
    (is (= 0 (length (factor-levels out))))
    (is (equalp #(0 0 0 0) (factor-data out)))))
