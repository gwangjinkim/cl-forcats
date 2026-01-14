(defpackage #:cl-forcats-na-tests
  (:use #:cl #:cl-forcats #:fiveam #:cl-vctrs-lite))

(in-package #:cl-forcats-na-tests)

(in-suite :cl-forcats-suite)

(test na-creation
  (let ((f (factor (list "A" *na* "B" nil))))
    (is (equalp (factor-data f) #(1 0 2 0)))
    (is (equalp (fct-levels f) #("A" "B")))))

(test na-tibble-integration-logic
  ;; We can test the logic even without cl-tibble by checking ifcl-vctrs-lite:*na*
  ;; would be returned in those conditions.
  (let ((f (factor (list "A" *na*))))
    ;; Verify that index 0 is used for both nil and *na*
    (is (= (aref (factor-data f) 1) 0))
    ;; Verify that fct-levels doesn't contain NA
    (is (not (member "NA" (coerce (fct-levels f) 'list) :test #'string=)))))

(test explicit-na-handling
  (let* ((f (factor (list "A" *na*)))
         (f2 (fct-explicit-na f :na-level "Missing")))
    (is (equalp (fct-levels f2) #("A" "Missing")))
    (is (= (aref (factor-data f2) 1) 2))))
