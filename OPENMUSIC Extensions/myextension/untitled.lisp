(in-package :t2l)
(:export
   #:?template
   #:find-any
   #:find-all
   #:assert!!
   #:?a-number
   #:?a-real
   #:?a-real-above
   #:?a-real-below
   #:?a-real-between
   #:?an-integer
   #:?an-integer-above
   #:?an-integer-below
   #:?an-integer-between
   #:?a-member-of
   #:?avg
   #:?and
   #:?or
   #:?not
   #:?abs
   #:?%
   #:?+
   #:?-
   #:?*
   #:?/
   #:?1+
   #:?-1
   #:?<
   #:?>
   #:?<=
   #:?>=
   #:?=
   #:?/=
   #:?equal
   #:?between
   #:?max
   #:?min
   #:?mapprules
   #:map?and
   #:map2?and
   #:map3?and
   #:map4?and
   #:map5?and
   #:map?or
   #:map2?or
   #:map3?or
   #:map4?or
   #:map5?or
   #:maplist?and
   #:maplist2?and
   #:maplist3?and
   #:maplist?or
   #:maplist2?or
   #:maplist3?or
   #:?listdx
   #:?list+
   #:?list-
   #:?list*
   #:?list/
   #:?list%
   #:?listmin
   #:?listmax
   #:?listdx
   #:?list<
   #:?list<=
   #:?list>
   #:?list>=
   #:?list=
   #:?list/=
   #:?listeq
   #:?list!eq
   #:?all+
   #:?all-
   #:?all*
   #:?all/
   #:?all<
   #:?all<=
   #:?all>
   #:?all>=
   #:?all/=
   #:?all-different
   #:?all-equal
   #:?all/equal
   #:?all-between
   #:?member
   #:?membersof
   #:!membersof
   #:?items-in
   #:?items-in
   #:?items!in
   #:?items!in
   #:?integers-in
   #:?integers!in
   #:?floor
   #:?expt
   #:?crossw
   #:?funcall
   #:?apply
   #:?counttrues
   #:?countof
   #:symxlat
   #:?symxlat
   #:?symxlat-internal
   #:?xlatsym))



(defun process-seqc-merge (input &key partition-fn
                                      region
                                      scan-fns
                                      after-scan-rules
                                      after-merge-rules
                                      final-merge-rules
                                      delete-tied-midic)
  (let* ((init-scan-fns (if scan-fns 
			    (if (listp scan-fns)
				scan-fns 
				(list scan-fns))
			    (list (lambda (x) (let ((r (a-subset-of x)))
                                                (assert! (>=v (lengthv r) 
                                                             (maxv 1 (-v (lengthv x) 2))))
                                                r)))))
         (merge (a-seqc-merger (extract-seqc-regionv input region)
                               :partition-fn partition-fn
                               :scan-fns init-scan-fns
                               :after-scan-rules after-scan-rules
                               :after-merge-rules after-merge-rules
                               :delete-tied-midic delete-tied-midic))
         (merge-mess nil))
         (r (append (extract-seqc-regionv input region t)
                    (if merge (list merge)))))
    (assert! (notv (equalv nil (evaluatev r final-merge-rules))))
    (if delete-tied-midic 
        (map-func #'floor r)
      r))

(cl:defun process-duration-groups (ms timepoints modulus ratio &key proportional-mode)
  :initvals '(((5 8) (6 8) (7 8) (4 8)) (1 2 3 4 5 6 7 8 9 10) 16 (3 2) nil)
  :indoc '("ms" "timepoints" "modulus" "ratio" "proportional-mode") ; an string list with short docs
  :icon 225  ; the icon
  :doc "" 
  (let* ((rratio (/ (car ratio) (cadr ratio)))
         (msdmax (apply #'max (mapcar #'cadr ms)))
         (tscale (/ msdmax modulus))
         (beat-partitions (mapcar #'(lambda (p ms) (om* rratio
                                                        (om* p (/ msdmax (cadr ms)))))
                                  (if proportional-mode
                                      (mapcar #'list (mapcar #'car ms))
                                    (partn-list '(2 3) (mapcar #'car ms)))
                                  ms))
         (timepoints-scaled (mapcar #'(lambda (xs)
                            (cond ((listp xs)
                                   (list (* (car xs) tscale) (cadr xs)))
                                  (t (* xs tscale))))
                        timepoints)))
    ;(print (format nil "process-duration-groups timepoints-scaled ~A" timepoints-scaled))
    ;(print (format nil "important! beat-partitions: ~A" beat-partitions))
    (process-duration-groups-internal (flat beat-partitions) timepoints-scaled)))

(cl:defun process-duration-groups-internal (l segs)
  ;(print (format nil "ENTER process-duration-groups-internal l: ~A segments: ~A" l segs))
  (labels
      ((strip-zeros (xs) (mapcar (lambda (x) (remove 0 x :test 'equalp)) xs))
       (duration (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (abs (car group)))
               ((listp group) 
                ;(print "process-duration-groups-internal#duration is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t (abs group))))
       (group? (input)
         (and (not (null input))
              (listp input)
              (listp (car input))
              (not (listp (cadr input)))))
       (group-value (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (car group))
               ((listp group)
                ;(print "process-duration-groups-internal#group-value is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t group)))
       (process-durations (ll segs-l segs-r)
         ;(print (format nil "process-duration-groups-internal#process-durations ll: ~A segs-l: ~A segs-r: ~A" ll segs-l segs-r))
         (cond ((and (null ll) (not (null segs-l)) (null segs-r)) (list segs-l))
               ((null ll) nil)
               ((and (null segs-l) (null segs-r)) (process-durations ll (list (* (float -1) (float (car ll)))) nil))
               ((null segs-l) (process-durations ll (list (car segs-r)) (cdr segs-r)))
               ((and (null segs-r) (< (reduce #'+ (mapcar #'duration segs-l)) (car ll)))
                (let* ((segs-l-duration (reduce #'+ (mapcar #'duration segs-l)))
                       (diff (float (* -1 (- (car ll) segs-l-duration)))))
                  (process-durations ll (append segs-l (list diff)) nil)))
               (t 
                (let ((diff (- (duration (car ll)) (reduce #'+ (mapcar #'duration segs-l)))))
                  (cond ((> diff 0)
                         (process-durations ll 
                                 (append segs-l (list (car segs-r)))
                                 (cdr segs-r)))
                        ((< diff 0)
                         (let ((next-segment (car (reverse segs-l))))
                           (cond
                          ((listp next-segment)
                           (let* ((a (duration next-segment))
                                  (c (/ (* -1 diff) a))
                                  (b (/ (+ (duration next-segment) diff) a))
                                  (s (reduce #'+ (mapcar #'duration (cadar (reverse segs-l))))) ; sum of the internal subdivisions of the group
                                  (d (* b s))
                                  (e (* c s))
                                  (groups (process-duration-groups-internal (list d e) (cadar (reverse segs-l)))))
                               (let* ((stak (copy-seq (cadar (reverse segs-l)))))
                                 (let ((groups2
                                        (funcall-rec
                                         #'(lambda (x)
                                             (let ((top (pop stak)))
                                               (cond ((floatp top) (float x))
                                                     (t x))))
                                         groups)))
                                  ; (print (format nil "-> process-durations-groups-internal#process-durations a: ~A c: ~A b: ~A s: ~A groups: ~A groups2: ~A next-segment: ~A segments: ~A" a c b s groups groups2 (car (reverse segs-l)) segs-l))
                               (process-durations ll 
                                       (append (butlast segs-l) 
                                               (list (list (+ (duration next-segment) diff)
                                                           (car groups2))))
                                       (append (list (list (* -1 diff) (cadr groups2)))
                                             segs-r))))))
                          (t (process-durations ll
                                     (append (butlast segs-l)
                                             (list (+ (duration (car (reverse segs-l))) diff)))
                                     (append (list (* -1 diff))
                                             segs-r))))))
                        (t
                         (append (list segs-l)
                                 (process-durations (cdr ll)
                                                    nil
                                                    segs-r))))))))
       (process-durations->timees (ps)
         (mapcar ; ((1 2) (1 1))
          #'(lambda (m)
              (mapcar
               #'(lambda (n)
                   (make-timee :value n :flag t))
               m))
          ps))
       (process-timee-flags (re)
         (let ((init-s (map-func #'abs segs)) ; 
               (init-p (flat re)))
           (labels ((prcs (s p &optional (cont nil))
                      (cond ((and (null s) (null p)) nil)
                            ((null p) nil)
                            ((null s) (mapcar #'(lambda (x) 
                                                  (setf (timee-flag x) nil))
                                              p))
                            (t (let ((diff (- (duration (car s))
                                              (duration (timee-value (car p))))))
                                 (cond ((> diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (append (list diff) (cdr s))
                                                (cdr p)
                                                t)))
                                       ((< diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s)
                                                (cdr p))))
                                       (t
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s) (cdr p))))))))))
             (prcs init-s init-p))))
       (expand-timee-groups (re)
         ; (print (format nil "expand-timee-groups re: ~A" re))
         (mapcar #'(lambda (xs)
                     ; (print (format nil "expand-timee-groups calling scale-to-int with ~A" (mapcar #'group-value (mapcar #'timee-value xs))))
                     (let ((x-values (scale-to-int (mapcar #'group-value (mapcar #'timee-value xs)))))
                       (mapcar #'(lambda (struct value)                                 
                                 (progn 
                                   ; (print (format nil "calling setf-timee-value struct: ~A value: ~A" struct value))
                                   (setf-timee-value struct value))
                                   struct)
                               xs
                               x-values)))
                 re))
       (convert-timee-values (re) 
         (mapcar #'(lambda (x)
                     (setf-timee-value x (if (timee-flag x)
                                               (floor (group-value (timee-value x)))
                                             (float (group-value (timee-value x))))))
                 (flat re)))
       (process-timee-signs (re segs)
         (let ((stak (copy-seq segs)))
           (push nil stak)
           (mapcar #'(lambda (x)
                     (if (timee-flag x) (pop stak))
                     (cond ((null stak) x)
                           ((and (< (group-value (car stak)) 0)
                                 (> (timee-value x) 0))
                            (setf (timee-value x) (* -1 (timee-value x)))
                            x)
                           (t x)))
                   (flat re)))))
    (let ((durations (strip-zeros (process-durations l nil segs))))
      (let ((duration-obj-list (process-durations->timees durations)))
        (process-timee-flags duration-obj-list)
        (expand-timee-groups duration-obj-list)
        (process-timee-signs duration-obj-list segs)
        (convert-timee-values duration-obj-list)
        (map-func #'timee-value duration-obj-list)))))

(cl:defun partn-list (keys input) ; TODO rename
  (if (null input)
      nil
    (append
     (list
      (partn-list-elem (car input) keys))
     (partn-list keys (cdr input)))))

(cl:defun partn-list-elem (e keys)
  (labels
      ((find-sumof (xs) (apply #'+ (flat xs))))
  (cond ((null e) nil)
        (t (screamer:one-value 
            (an-expanded-list keys #'(lambda (x) (= (find-sumof x) e))))))))

















(defvar *sk-seteql-functions* (list #'ommaxv 
                                    #'omminv
                                    #'omandv
                                    #'omorv
                                    #'om=v
                                    #'om/=v
                                    #'omequalv))
(defvar *sk-seteql-function* 
  #'(lambda (list1 list2)
      (and (equalp (car list1) (car list2))
           (seteql (cdr list1) (cdr list2)))))
(defvar *sk-member-of-functions* (list #'member-of-sequencev
                                       #'member-of-number-sequencev
                                       #'not-member-of-sequencev
                                       #'not-member-of-number-sequencev
                                       #'all-items-inv
                                       #'all-integer-items-inv
                                       #'all-items-!inv
                                       #'all-integer-items-!inv
                                       #'ommemberv))
(defvar *sk-member-of-function*
  #'(lambda (list1 list2)
      (and (equalp (car list1) (car list2))
           (equalp (cadr list1) (cadr list2))
           (seteql (cddr list1) (cddr list2)))))
(defvar *sk-unordered-list-functions* (list #'om+v
                                            #'om*v
                                            #'omavgv
                                            #'distinct-values-countv))
(defvar *sk-unordered-list-function*
  #'(lambda (list1 list2)
      (and 
       (equalp (car list1) (car list2))
       (cond
        ((and (null list1) (null list2)) T)
        ((null list1) nil)
        ((null list2) nil)
        ((not (= (length list1) (length list2))) nil)
        (T
         (labels ((indexof-1st-match (x list)
                    (let ((i (loop named loop-1 for i from 0 to (1- (length list))
                                   do (if (equal x (elt list i))
                                          (return-from loop-1 i)))))
                      (cond ((null i) nil)
                            ((= i (length list)) nil)
                            (T i))))
                  (remove-element-at-index (i list)
                    (cond ((= i 0) (cdr list))
                          ((= i (1- (length list))) (butlast list))
                          (T (append (subseq list 0 i)
                                     (subseq list (1+ i) (length list))))))
                  (compare (list1 list2)
                    (cond ((null list1) (null list2))
                          ((null list2) nil)
                          (T (let ((i (indexof-1st-match (car list1) list2)))
                               (if i
                                   (compare (cdr list1) (remove-element-at-index i list2))
                                 nil))))))                          
           (compare (cdr list1) (cdr list2))))))))
(cl:defun vregister (make-variable-fn key-fn &rest key-fn-arguments)
  (or (apply #'lookup-solver-key (append (list key-fn) key-fn-arguments))
      (apply #'register-solver-key (append (list key-fn (funcall make-variable-fn)) key-fn-arguments))))



























(defmacro-compile-time best-values
    (form1 objective-form &optional (form2 nil form2?))
  (let ((values (gensym "VALUES"))
        (last-value-cons (gensym "LAST-VALUE-CONS")))
    `(let ((,values '())
           (,last-value-cons nil))
       (let ((bound (gensym "BOUND-"))
             (best (gensym "BEST-"))
             (objective (gensym "OBJECTIVE-")))
         `(let ((,bound nil)
                (,best nil)
                (,objective (variablize ,objective-form)))
       (attach-noticer!
        #'(lambda ()
            (if (and ,bound (< (variable-upper-bound ,objective) ,bound)) (fail)))
        ,objective)
       (for-effects
         (let ((value ,form1))
           (global (setf ,bound (variable-upper-bound ,objective))
                   (setf ,best value))))
       (if ,bound (list ,best ,bound) ,(if form2? form2 '(fail))))))













(let ((x (an-integer-betweenv 1 10)) (y (an-integer-betweenv 1 10)))
  (best-value
   (solution (list x y) (static-ordering #'linear-force))
   (/v (-v x y) 2)))
(defun find-all2 (i form1 &key points-system catalog force-function cost-fun terminate-test order)
  (cond
   ((and i (>= i 1))
    (reset-solver-output catalog)
    (setf-solver-input form1 catalog)
    (let ((output
           (n-values i
                     (solution form1
                               (generate-ordering-force-function 
                                :force-function force-function
                                :cost-fun cost-fun
                                :terminate-test terminate-test
                                :order order)))))
      (global (setf-solver-output output catalog))))  
   ;;;; 
   (t
      (reset-solver-output catalog)
      (setf-solver-input form1 catalog)

      ;; console message
      (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
          (get-decoded-time)
        (print (format nil "FIND-ALL started at ~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d"
                       hour minute second month date)))                       

      (let ((points-system-function (if points-system
                                        points-system
                                      (let ((counter 0))
                                        #'(lambda (xs)
                                            (global (incf counter))
                                            counter))))
            (points-total (a-realv))
            (solution-recorded-time nil))
        (best-value
         (let ((solution 
                (solution form1 (generate-ordering-force-function 
                                 :force-function force-function
                                 :cost-fun cost-fun
                                 :terminate-test terminate-test
                                 :order order))))

           (assert! (equalv points-total (funcall points-system-function solution)))

           ;; console message
           (multiple-value-bind
               (second minute hour date month year day-of-week dst-p tz)
               (get-decoded-time)
             (let ((mclib-current-internal-real-time (get-internal-real-time)))
               (cond ((or (null solution-recorded-time)
                          (> (- mclib-current-internal-real-time solution-recorded-time) 1000))
                      (progn   
                     (print
                      (format nil "[~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d] d:~Ams, ~A: ~A'"
                              hour
                              minute
                              second
                              month
                              date
                              (if solution-recorded-time 
                                  (- mclib-current-internal-real-time 
                                     solution-recorded-time)
                                0)
                              points-total
                              solution))                      
                     (global
                       (setf solution-recorded-time 
                             mclib-current-internal-real-time)))))))

           (cond ((and i (= i 0))
                  (progn 
                    (setf-solver-output solution catalog)))
                 (t (push-to-solver-output solution catalog))))
         points-total)
        (cdr (assoc (or catalog :backup) *backup-solver-output*))))))

(progn ; functions in *solver-keys*
    (mapcar #'print (mapcar #'(lambda (fn) (list fn (length (remove-if-not #'(lambda (entry) (equal fn (caar entry))) t2l::*solver-keys*)))) (remove-duplicates (mapcar #'caar t2l::*solver-keys*))))
    "OK")



















(mapcar #'(lambda (fn) (list fn (length (remove-if-not #'(lambda (xs) (equal fn (caar xs))) t2l::*solver-keys*)))) fns)



(defmethod! omminv (&rest xs)
  :icon 209
  (or (apply #'lookup-solver-key 
             (append (list #'omminv) xs))
      (apply #'register-solver-key 
             (append (list #'omminv
                           (apply #'screamer:minv (remove nil (flat xs))))
                     xs))))
(defun %v (n d)
  (or (lookup-solver-key #'%v n d)
      (register-solver-key #'%v        
                           (let ((x (an-integerv)))
                             (assert! (om<v x d))
                             (assert! (om>=v x 0))
                             (assert! (om=v x (om-v n (om*v d (an-integerv)))))
                             x)
                           n
                           d)))
'(ommaxv
  omminv
  
  omavgv

  ommemberv
  om-count-truesv
  absv
  %v
  omandv
  omorv
  omnotv
  om+v
  om-v
  om*v
  om/v
  om<v
  om>v
  om<=v
  om>=v
  om=v
  om/=v)

(defvar *sk-seteql-functions* (list #'ommaxv 
                                    #'omminv
                                    #'omandv
                                    #'omorv
                                    #'om=v
                                    #'om/=v))


(apply #'lookup-solver-key 
       (append (list #'omminv) '(1 2 3)))

(defvar *sk-unordered-list-functions* (list #'om+v
                                            #'om*v
                                            #'omavgv
                                            #'meanv
                                            #'om-count-truesv))




















(setq voice1 '(x1 (x2 x2 x3) x4 x5) 
      voice2 '(y1 (y2 y3 y3) y4 y5))
(setq voice1 '(x3 x3 x4) voice2 '(y4 y5 y5))
(setq voice1 '(x1 x2 (x3 x3 x4)) voice2 '(y1 y2 (y3 y4 y4)))
(setq voice1 '(x1 (x2 x2 x3) x4)  
      voice2 '(y1 (y2 y3 y3) (y4 y5)))
(setq voice1 '(0 1 1 1) voice2 '(8 3 4 5))
(setq voice1 '(x1 x2 (x3 x4 x5)) voice2 '(y1 y2 y3))
(setq voice1 '(x1 x2 x3) voice2 '(y1 y2 (y3 y4 y5)))
(setq voice1 '(x1 x2 (x3 x4 x5) x6 x7) voice2 '(y1 y2 y3 y4 y5))
(setq voice1 '(x1 x2 x3) voice2 '(y1 y2 (y3 y4 y5)))
(setq voice1 '(x1 x2 x3 x4 x5) voice2 '(y1 (y2 y3 y4) y5 y6 y7))
(setq voice1 '(x1 x2 (x3 x3 x4) (x5 x6 x7 x8) x9) 
      voice2 '(y1 (y2 y3) (y4 y5 y5) y6 y7))
(setq voice1 '(x1 x2 x3)
      voice2 '((nil y1) (y2 y3) (y4 y5)))
(setq voice1 '(x1 x2 x3)
      voice2 '((y0 nil) (y2 y3) (y4 y5)))
(setq voice1 '(x1 x2 x3)
      voice2 '((y0 y1) (y2 y3) (y4 y5)))
(list voice1 voice2)
(print-warnings)
(hide-warnings)
(progn
  (print "==========================")
  (print "==========================")
  (print (format nil "sequence voice1: ~A" voice1))
  (print (format nil "sequence voice2: ~A" voice2))
  (mapcar #'print (group-by-motion-type voice1 voice2))
  "OK")










(labels
    ((group-chords (sequence)
       (group-chords-internal nil (mat-trans (flatten-seqc (list voice1 voice2)))))
     (group-chords-internal (groups chords)
       (cond
        ((null chords) groups)
        
        ; suspensions
        ((and (cddr chords)
              (let ((xs (mat-trans (first-n chords 3))))
                (and (not (has-null-values xs))
                     (or
                   ; suspension - x1x1x2 y1y2y2
                      (and (equal (caar xs) (cadar xs)) ; x1=x1
                           (equal (cadadr xs) (caddr (cadr xs))) ; y2=y2
                           (not (equal (cadar xs) (caddar xs)))  ; x1!=x2
                           (not (equal (caadr xs) (cadadr xs)))) ; y1!=y2
                   ; suspension - x1x2x2 y1y1y2
                      (and (equal (caar xs) (cadar xs)) ; x2=x2
                           (equal (cadadr xs) (caddr (cadr xs))) ; y1=y1
                           (not (equal (caar xs) (cadar xs))) ; x1!=x2
                           (not (equal (cadadr xs) (caddr (cadr xs))))))))) ; y1!=y2
         (group-chords-internal
          (append-chords-to-new-group 
           (append-chords-to-last-group groups (list (car chords)))
           (first-n chords 3))
          (cdddr chords)))

        ; 1-to-X
        ((alleq (append (car (last-group-as-sequence groups)) (list (caar chords)))) ; x's =
         (group-chords-internal (append-chords-to-last-group groups (list (car chords)))
                                (cdr chords)))
           
        ; 1-to-1 
        ((and (not (equal (car (last (last-group groups))) ; x1
                          (caar chords))) ; x2
              (not (equal (cadr (last (last-group groups))) ;y1
                          (cadar chords)))) ; y2            
         (group-chords-internal
          (append-chords-to-new-group 
           (if groups 
               (append-chords-to-last-group groups (list (car chords)))
             nil)
           (list (car chords)))
          (cdr chords)))
        
        ))
     (alleq (xs)
       (print (format nil "alleq: ~A" xs))
       (notany #'null
               (mapcar #'(lambda (xs) (equal (car xs) (cadr xs))) (cartx2 xs))))
     (append-chords-to-last-group (groups chords)
       (cond ((null chords) groups)
             ((null groups) (list chords))
             ((cdr groups)
              (append (butlast groups)
                      (list
                       (append (car (last groups)) chords))))
             (t
              (list (append (car groups) chords)))))
     (append-chords-to-new-group (groups chords)
       (cond ((null chords) groups)
             ((null groups) (list chords))
             (t
              (append groups (list chords)))))
     (last-group (groups) (cond ((null groups) (list))
                                (t (car (reverse groups)))))
     (last-group-as-sequence (groups) (mat-trans (last-group groups))))
  
  
            
  (setf groups nil)
  (setf groups (append-chords-to-new-group groups '((x1 y1) (x2 y2))))
  (setf groups (append-chords-to-new-group groups '((x3 y3) (x4 y4))))
  (setf groups (append-chords-to-last-group groups '((x5 y5))))
  (setf groups (append-chords-to-last-group groups '((x6 y6) (x7 y7))))
  (setf groups (append-chords-to-new-group groups '((x8 y8) (x9 y9) (x10 y10))))
  (print (last-group-as-sequence groups))
  (print (car (last (last-group groups)))) ; x10 y10
  (print "=============")
  (print (group-chords (list voice1 voice2)))  
  (print "=============")

  (mapcar #'mat-trans (group-chords (list voice1 voice2))))














'(ommemberv absv %v floorv ceilingv omandv omorv om+v om-v om*v om/v om<v om>v om<=v om>=v om=v om/=v ommaxv omminv)

















(setq sequence 
      '((55 (57 59) 60)
        ((59 60) 62 60)
        ((67 (66 (64 62)) 67))))
(setq sequence '(((55) ((57 59)) 60)
                 ((59 60) 62 60)
                 ((67 (66 (64 62)) 67))))
(setq timepoints '((2 (1 2) 1)
                   ((1 1) 3 1)
                   ((2))))
(defun init-sequence (sequence) 
  (labels ((unnest (x)
             (cond ((null x) nil)
                   ((atom x) x)
                   ((and (listp x) (= 1 (length x)))
                    (unnest (car x)))
                   ((listp x)
                    (mapcar #'unnest x))
                   (t x))))
    (mapcar #'unnest sequence)))
(init-sequence sequence)
(setq sequence 
      (init-sequence '((55 (57 59) 60)
                       ((59 60) 62 60)
                       ((67 (66 (64 62)) 67)))))
(setq sequence
      (init-sequence '((60 65)
                       (72 (74 75)))))
(setq timepoints
      (map-func #'(lambda (x)
                    (if x (an-integer-abovev 1) nil))
                sequence))
(flatten-seqc timepoints)

(defmethod! om-seqc2timepoints-basic (sequence)
            :icon 160
            (let* ((timepoints (map-func #'(lambda (x)
                                             (if x (an-integer-abovev 1) nil))
                                         sequence))
                   (map (mapcar #'(lambda (variable) (cons variable 0)) (remove nil (flat (map-func #'(lambda (x) (if (and x (screamer::variable? x)) x nil)) timepoints))))))
              (print "build map")
              ;;;; build map
              (mapcar #'(lambda (voice)
                          (map-func #'(lambda (x)
                                        (if x
                                            (rplacd (assoc x map)
                                                    (1+ (cdr (assoc x map))))))
                                    voice))
                      (flatten-seqc timepoints))
  
              ;;;; constrain variable values using map
              (mapcar #'(lambda (xs)
                          (assert! (=v (car xs) (cdr xs))))
                      map)

              (om-solution timepoints)))

(om-solution timepoints)

(om-seqc2timepoints-basic sequence)


(mapcar #'(lambda (voice) (mapcar #'(lambda (x) (if (atom x) (list x) x)) voice)) sequence)                   
(setq sequence (init-sequence sequence))
(labels
    ((lists-of-1->atoms (list)
       (mapcar #'(lambda (x) (cond ((null x) nil)
                                   ((and (listp x)
                                         (= (length x) 1))
                                    (car x))
                                   ((listp x)
                                    (lists-of-1->atoms x))
                                   (t x)))
               list)))
  (mapcar #'(lambda (voice) (lists-of-1->atoms voice))
          sequence))
            
(mat-trans (init-sequence sequence))
(mat-trans sequence)







(defmethod! om-seqc2timepoints-basic ((tree list) &optional modulus)
            :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
            :icon 160 ; the icon
            :doc ""
            (labels
                ((atoms2list (x) (cond ((null x) (list x))
                                       ((listp x) x)
                                       ((atom x) (list x))
                                       (t x)))
                 (sequence-atoms2list (sequence) (mapcar #'atoms2list sequence))
                 (process-timepoints (sequence) T))
              (let ((sequence-list (mat-trans tree)))
                (let ((timepoint-values
                       (mapcar
                        #'flat
                        (mat-trans
                         (mapcar #'(lambda (sequence)
                                     (let ((duration-max (apply #'max (mapcar #'length sequence)))
                                           (timepoints
                                            (map-func
                                             #'(lambda (x) (an-integer-abovev 1))
                                             (mapcar #'flat sequence))))
                                       (assert! (map-andv
                                                 #'(lambda (voice-timepoints)
                                                     (om=v (apply #'om+v 
                                                                  (map-func #'(lambda (x) (if x (absv x) nil))
                                                                            voice-timepoints))
                                                           duration-max))
                                                 timepoints))
                                       (one-value (solution timepoints (static-ordering #'linear-force)))))
                                 (mapcar #'sequence-atoms2list sequence-list))))))
        ; (print (format nil "timepoint-values: ~A" timepoint-values))
                  (map2func
                   #'(lambda (timepoint midinote)
             ;(print (format nil "map2func input timepoint: ~A midinote: ~A" timepoint midinote))
                       (if midinote
                           timepoint
                         (* -1 timepoint)))        
                   timepoint-values
                   (mapcar #'flat tree))))))

(cl:defun make-variable-for-voice-sequence-list (sequences &rest rules-fn)
 (labels
     ((list-contains-atoms (xs) (not (every #'listp xs)))
      (atoms->list (xs) (mapcar #'(lambda (x) 
                                    (cond ((or (null x) (not (listp x))) (list x))
                                          (t x))) xs)))
 (let ((position-idx 0)
       (merged-sequences (mat-trans sequences))
       (variables nil))
   (let ((output 
          (reduce 
           #'(lambda (x y)
               ;(print (format nil "x: ~A y: ~A" x y))

               (let (; (merged-sequence (list x y))
                     (last-chord (car (reverse (mat-trans (flatten-seqc x)))))
                     (next-chord (car (mat-trans (flatten-seqc y)))))
          ;(print (format nil "merged-sequence: ~A" merged-sequence))
                 (push                  
                  (map-andv
                   #'(lambda (rules-fn)
                       (if (null rules-fn) 
                           t
                         (funcall rules-fn 
                                  x
                                  y
                                  position-idx
                                  last-chord
                                  next-chord
                                  (mapcar #'(lambda (x y) (-v y x)) last-chord next-chord)
                                  merged-sequences)))
                   rules-fn)
                  variables)
          ; (print (format nil "rules-fn: ~A" rules-fn))
                 (incf position-idx)
                 (cond ((list-contains-atoms x)
                        (mat-trans (list x y)))
                       (t
                        (mat-trans (append (mat-trans x) (list y)))))))
           sequences)))
     (apply #'andv variables)))))

; 'sequence' objects
(cl:defun om-join-seqc-list (list)
  (join-seqc-list list))

(cl:defun join-seqc-list (list)
  (let ((countmax (list-max (mapcar #'length list))))
    (apply #'mapcar 
           #'append
           (loop for s in list
                 collect (append s (if (= countmax (length s))
                                       nil
                                     (list 
                                      (make-sequence 'list 
                                                     (- countmax (length s))
                                                     :initial-element '(-60)))))))))