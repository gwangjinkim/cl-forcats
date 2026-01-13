(asdf:defsystem #:cl-forcats
  :description "Categorical variables (factors) for Common Lisp"
  :author "DeepMind Antigravity"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-vctrs-lite #:cl-ppcre #:alexandria #:serapeum)
  :components ((:file "package")
               (:module "src"
                :components ((:file "utils")
                             (:file "factor")
                             (:file "inspection")
                             (:file "reorder")
                             (:file "modify")
                             (:file "utils-fct")
                             (:file "dsl"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-forcats/tests))))

(asdf:defsystem #:cl-forcats/tests
  :description "Tests for cl-forcats"
  :author "DeepMind Antigravity"
  :license "MIT"
  :depends-on (#:cl-forcats #:fiveam)
  :components ((:module "tests"
                :components ((:file "tests"))))
  :perform (asdf:test-op (op c) (symbol-call :fiveam :run! :cl-forcats-suite)))
