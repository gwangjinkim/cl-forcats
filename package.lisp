(defpackage #:cl-forcats
  (:use #:cl #:cl-vctrs-lite)
  (:export #:factor
           #:factor-p
           #:factor-data
           #:factor-levels
           #:factor-ordered
           #:make-factor
           #:fct-count
           #:fct-unique
           #:fct-levels
           #:fct-relevel
           #:fct-reorder
           #:fct-infreq
           #:fct-rev
           #:fct-shift
           #:fct-recode
           #:fct-collapse
           #:fct-lump
           #:fct-other
           #:fct-drop
           #:fct-expand
           #:fct-explicit-na))
