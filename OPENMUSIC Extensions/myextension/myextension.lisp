(screamer:define-screamer-package :t2l 
  (:use :om :system))

(in-package :t2l)

(defvar *source-files* 
  (list (om::om-relative-path '(".") "visual")
        ; (om::om-relative-path '(".") "screamer ext2")
        (om::om-relative-path '(".") "screamer ext3")
        (om::om-relative-path '(".") "general")
        (om::om-relative-path '(".") "paradigms")))

(defvar *package-files* nil)

(defvar *library-files* (append *source-files* *package-files*))

;--------------------------------------------------
; Seting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

  ; using "make-pathname" plus *load-pathname*, allow us to put our library anywhere
(mapc #'om::compile&load *library-files*)
(om::fill-library 
 '(("solver" 
    Nil
    Nil
    (om::?template
     om::make?variables
     om::assert!!
     om::find-any
     om::find-all
     om::solver-input
     om::solver-output
     om::next-solver-input
     om::next-solver-output
     om::reset-solver-registry)
    Nil)

   ("generators"
    Nil
    Nil
    (om::?a-number
     om::?a-real
     om::?a-real-above
     om::?a-real-below
     om::?a-real-between
     om::?an-integer
     om::?an-integer-above
     om::?an-integer-below
     om::?an-integer-between)
    Nil)
   
   ("rules" 
    Nil
    Nil
    (om::?all<
     om::?all>
     om::all<=
     om::all>=
     om::all/=
     om::all/equal
     om::all=
     om::all-between
     om::all-different
     ; om::all?memberv
     ; om::all?!memberv
	 om::?membersof
	 om::!membersof
     om::?items-in
     om::?items!in
     om::?integers-in
     om::?integers!in
     om::?counttrues
     om::?mapprules
     om::?list+
     om::?list-
     om::?list*
     om::?list/
     om::?list%
     om::?listmin
     om::?listmax
     om::?listdx
     om::?list<
     om::?list>
     om::?list<=
     om::?list>=
     om::?list=
     om::?list/=
     om::?+
     om::?-
     om::?*
     om::?/
     om::?1+
     om::?-1
     om::?=
     om::?/=
     om::?<
     om::?>
     om::?<=
     om::?>=       
     om::?and
     om::?or
     om::?not
     om::?abs
     om::?%
     om::?floor
     om::?ceiling
     om::?min
     om::?max
     om::?avg
     om::?member  
     om::?crossw
     om::?symxlat
     om::?xlatsym)
    Nil)  
   
   ("map functions" 
    Nil
    Nil
    (om::map?and om::map2?and om::map3?and om::map4?and om::map5?and om::map?orv om::map2?or om::map3?or om::map4?or om::map5?or om::maplist?and om::maplist2?and om::maplist3?and om::maplist?orv om::maplist2?or om::maplist3?or om::map-func om::map2func om::map3func) 
    Nil)    
   
   ("sequences" 
    Nil
    Nil
    (om::flatten-seqc
     om::mat-trans
     om::group-by-motion-type
     om::fuseseqc
     seqc->poly
     seqc->voices) 
    Nil)

;   ("screamer" 
;    Nil
;    Nil
;    (all-values
;      one-value
;      for-effects
;      ith-value
;      print-values
;      possibly?
;      necessarily?
;      global
;      local
;      make-variable
;      assert!
;      value-of
;      apply-substitution
;      bound?
;      ground?
;      applyv
;      funcallv
;      equalv
;      template
;      a-booleanv
;      booleanpv
;      known?
;      decide
;      notv
;      andv
;      orv
;      count-truesv
;      a-member-ofv
;      memberv
;      a-numberv
;      a-realv
;      a-real-abovev
;      a-real-belowv
;      a-real-betweenv
;      an-integerv
;      an-integer-betweenv
;      numberpv
;      realpv
;      integerpv
;      minv
;      maxv
;      +v
;      -v
;      *v
;      /v
;      <v
;      <=v
;      =v
;      /=v
;      >=v
;      >v
;      memberv
;      notv)
;    Nil)

   ("utility" 
    Nil
    Nil
    (om::print-warnings
     om::hide-warnings
     om::alert2
     write-textfile
     om::alleq
     om::cartx2
     om::nsucc
     om::remove-successive-duplicates
     om::list-excerpt)
    Nil)
   ))
(t2l::start-mp-solver-process)