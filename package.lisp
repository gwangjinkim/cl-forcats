(defpackage #:cl-forcats
  (:use #:cl #:cl-vctrs-lite)
  (:nicknames #:forcats)
  (:export #:fct-lump-n #:fct-lump-prop #:fct-lump-min #:fct-lump-lowfreq
           #:fct-na-value-to-level #:fct-na-level-to-value
           #:fct-anon #:fct-shuffle #:fct-relabel #:fct-reorder2
           #:first2 #:last2 #:lvls-reorder #:lvls-revalue #:lvls-expand
           #:fct #:as-factor #:fct-c #:fct-cross #:fct-unify
           #:lvls-union #:fct-inorder #:fct-inseq #:fct-match
           #:factor
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
