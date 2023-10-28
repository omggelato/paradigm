(in-package :t2l)

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
