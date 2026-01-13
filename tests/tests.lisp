(defpackage #:cl-forecats-tests
  (:use #:cl #:cl-forecats #:fiveam))

(in-package #:cl-forecats-tests)

(def-suite :cl-forecats-suite)
(in-suite :cl-forecats-suite)

(test basic-factor-creation
  (let ((f (make-factor #(1 2 1) :levels '("A" "B"))))
    (is (factor-p f))
    (is (equalp (fct-levels f) #("A" "B")))
    (is (equalp (factor-data f) #(1 2 1)))))

(test inspection-api
  (let ((f (make-factor #(1 2 1 0 2) :levels '("A" "B"))))
    (is (equalp (fct-count f) '((:level "A" :n 2) (:level "B" :n 2))))
    (is (equalp (fct-unique f) '("A" "B")))
    (is (equalp (fct-count f :sort t) '((:level "A" :n 2) (:level "B" :n 2))))))

(test reordering-api
  (let ((f (make-factor #(1 2 1) :levels '("A" "B"))))
    (let ((f-rev (fct-rev f)))
      (is (equalp (fct-levels f-rev) #("B" "A")))
      (is (equalp (factor-data f-rev) #(2 1 2))))
    (let ((f-rel (fct-relevel f "B")))
      (is (equalp (fct-levels f-rel) #("B" "A")))
      (is (equalp (factor-data f-rel) #(2 1 2))))))

(test modifying-api
  (let ((f (make-factor #(1 2 1) :levels '("A" "B"))))
    (let ((f-rec (fct-recode f "Alpha" "A" "Beta" "B")))
      (is (equalp (fct-levels f-rec) #("Alpha" "Beta")))
      (is (equalp (factor-data f-rec) #(1 2 1))))
    (let ((f-col (fct-collapse f "Both" '("A" "B"))))
      (is (equalp (fct-levels f-col) #("Both")))
      (is (equalp (factor-data f-col) #(1 1 1))))))

(test utility-api
  (let ((f (make-factor #(1 3 1) :levels '("A" "B" "C"))))
    (let ((f-drop (fct-drop f)))
      (is (equalp (fct-levels f-drop) #("A" "C")))
      (is (equalp (factor-data f-drop) #(1 2 1))))
    (let ((f-exp (fct-expand f "D")))
      (is (equalp (fct-levels f-exp) #("A" "B" "C" "D")))
      (is (equalp (factor-data f-exp) #(1 3 1))))
    (let ((f-na (fct-explicit-na (make-factor #(1 0 1) :levels '("A")))))
      (is (equalp (fct-levels f-na) #("A" "(Missing)")))
      (is (equalp (factor-data f-na) #(1 2 1))))))

(test dsl-api
  (let ((f (factor '("A" "B" "A"))))
    (is (equalp (fct-levels f) #("A" "B")))
    (is (equalp (factor-data f) #(1 2 1))))
  (let ((f (factor '("A" nil "A") :levels '("A" "B"))))
    (is (equalp (fct-levels f) #("A" "B")))
    (is (equalp (factor-data f) #(1 0 1)))))

(test complex-scenarios
  (let ((f (factor '("apple" "banana" "apple" "cherry" "banana" "apple"))))
    ;; Chained: reorder by freq then reverse
    (let ((f-chained (fct-rev (fct-infreq f))))
      (is (equalp (fct-levels f-chained) #("cherry" "banana" "apple")))
      (is (equalp (factor-data f-chained) #(3 2 3 1 2 3))))
    ;; Lump and count
    (let ((f-lump (fct-lump f :n 1)))
      (is (equalp (fct-levels f-lump) #("apple" "Other")))
      (is (equalp (fct-count f-lump) '((:level "apple" :n 3) (:level "Other" :n 3)))))))

(test edge-cases
  ;; Empty factor
  (let ((f (factor nil :levels '("A" "B"))))
    (is (zerop (length (factor-data f))))
    (is (equalp (fct-levels f) #("A" "B"))))
  ;; Only NA
  (let ((f (factor '(nil nil))))
    (is (equalp (factor-data f) #(0 0)))
    (is (zerop (length (fct-levels f)))))
  ;; Single level
  (let ((f (factor '("A" "A"))))
    (is (equalp (fct-levels f) #("A")))
    (is (equalp (factor-data f) #(1 1)))))

(test type-variations
  ;; Symbols should be coerced to strings
  (let ((f (factor '(apple banana apple))))
    (is (equalp (fct-levels f) #("APPLE" "BANANA")))
    (is (equalp (factor-data f) #(1 2 1))))
  ;; Numbers should be coerced to strings
  (let ((f (factor '(1 2 1))))
    (is (equalp (fct-levels f) #("1" "2")))
    (is (equalp (factor-data f) #(1 2 1)))))
