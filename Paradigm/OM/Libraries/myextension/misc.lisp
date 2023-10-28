(in-package :om)







(defun list-depth (list)
  (let ((depth 0))
    (labels
        ((list-depth-internal (tree level)
           (cond ((null tree) nil)
                 ((not (consp tree)) (if (> level depth)
                                         (setf depth level)))
                 ((consp (car tree))
                  (list-depth-internal (car tree) (1+ level))
                  (list-depth-internal (cdr tree) level))
                 (t
                  (if (> level depth) (setf depth level))
                  (list-depth-internal (cdr tree) level)))))
      (list-depth-internal list 0)
      depth)))



(defun flatten-seqc (list &optional enable-suspensions)
  (let ((r (let* ((ll (map-func #'(lambda (x)
                                       (cond ((null x) "nil")
                                             (t x)))
                                   list)))
             (labels 
                 ((prcs-sublist (x) 
                    (cond ((contains-list x) 
                           (match-sublist-lens (atoms2list x)
                                               (find-largest-sublist-len (atoms2list x))
                                               (not enable-suspensions)))
                          (t (mapcar 'list x))))

                  (contains-list (ll)
                    (cond ((null ll) nil)
                          ((listp ll) (or (listp (car ll))
                                          (contains-list (cdr ll))))
                          (t nil)))
                  
                  (convert-mnlist-repeats (mnl)
                    (reverse (convert-mnlist-repeats-rec (reverse mnl))))

                  (convert-mnlist-repeats-rec (mnl)
                    (cond ((null mnl) nil)
                          ((cdr mnl) (append (list (cond ((equalp (car mnl) (cadr mnl)) 
                                                          (om* (car mnl) 1.0))
                                                         (t (car mnl))))
                                             (convert-mnlist-repeats-rec (cdr mnl))))
                          (t (list (car mnl)))))
                  
                      (flatn (x)
                        (cond ((null x) nil)
                              (t (append (car x) (flatn (cdr x))))))

                      (find-largest-sublist-len (x) 
                        (cond ((null (car x)) 0)
                              ((listp (car x)) (max (length (car x)) 
                                                    (find-largest-sublist-len (cdr x))))
                              ((atom (car x)) (max 1 
                                                   (find-largest-sublist-len (cdr x))))
                              (t 0)))

                      (match-sublist-lens (x len &optional ignore-midi-ties) 
                        (cond ((null (car x)) nil)
                              ((< (length (car x)) len) 
                               (append (list (append (car x)
                                                     (make-sequence 'list
                                                                    (- len (length (car x)))
                                                                    :initial-element (if ignore-midi-ties 
                                                                                         (car (reverse (flat (car x))))
                                                                                       (car (reverse (flat (car x))))))))
                                       (match-sublist-lens (cdr x) len ignore-midi-ties)))
                              (t (append (list (car x)) (match-sublist-lens (cdr x) len ignore-midi-ties)))))
                      
                      (atoms2list (x) 
                        (cond ((null (car x)) nil)
                              ((listp (car x)) (append (list (car x)) (atoms2list (cdr x))))
                              ((atom (car x)) (append (list (list (car x))) (atoms2list (cdr x))))
                              (t nil)))
                      
                      ) ; end of functions
               (let ((flat (mapcar #'flatn
                                   (mat-trans (let ((a 
                                                     (mapcar #'(lambda (x) 
                                                                 
                                                                 (cond ((contains-list (flat x 1)) 
                                                                        (flatten-seqc x enable-suspensions))
                                                                       (t (prcs-sublist x))))
                                                             (mat-trans (prcs-sublist ll)))))
                                                a)))))
                 (if enable-suspensions           
                     (mapcar 'convert-mnlist-repeats flat)
                   flat))))))
    (map-func #'(lambda (x)
                     (cond ((and (stringp x)
                                 (string= x "nil"))
                            nil)
                           (t x)))
                 r)))

(defun flat1 (list)
  (cond ((null list) nil)
        ((and (listp list)
              (every #'listp list))
         (apply #'append list))
        ((listp list)
         (flat1 (mapcar #'(lambda (x) (cond ((listp x) x)
                                            (t (list x))))
                        list)))
        (t list)))

(defmethod! last-item (list)
  :icon 235
  (cond ((null list) nil)
        ((not (listp list)) list)
        (t
         (car (reverse list)))))

(defun combinations-of2 (xs) ; delete
  (let ((stack nil))
    (labels
        ((internal (xs)
           (cond ((null xs) nil)
                 ((cdr xs)
                  (mapcar #'(lambda (y)
                              (push (list (car xs) y)
                                    stack))
                          (cdr xs))
                  (internal (cdr xs)))
                 (t t))))
      (internal xs)
      (reverse stack))))

(defmethod! group-by-motion-type (voice1 voice2 &key mode)
  :icon 235
  (t2l::group-by-motion-type voice1 voice2 :mode mode))

(defmethod! alleq (list &key test)
  :icon 235
  (every #'(lambda (xs) (funcall (or test #'equal) (car xs) (cadr xs)))
         (t2l::combinations-of2 list)))

(defmethod! write-textfile (input label ext &optional timezone) (write-textilfe input label ext timezone))

(defmethod! cartx2 (xs) :icon 235 (t2l::cartx2 xs))

(defmethod! nsucc (input n &key step list-padding pad-character exhaust-list) :icon 235 (t2l::nsucc input n :step step :list-padding list-padding :pad-character pad-character :exhaust-list exhaust-list))

(defmethod! remove-successive-duplicates (list &key test) :icon 235 (t2l::remove-successive-duplicates list :test test))

(defmethod! list-excerpt (list percent &optional items)
  :icon 235
  (cond 
   ((null list) list)
   ((= (length list) 1) list)
   (t
    (let* ((start-index (min 
                         (round (float (* (float (/ (min (abs percent)) 100.0))
                                          (length list))))
                         (1- (length list))))
           (end-index (if items                         
                          (min (length list) (+ start-index items))
                        (length list))))
      (subseq list start-index end-index)))))

(defmethod! alert2 (input &key label print-label-only) :icon 129 (t2l::alert2 input :label label :print-label-only print-label-only))
(defmethod! print-warnings (&optional x) :icon 129 (t2l::print-warnings x))
(defmethod! hide-warnings () :icon 129 (t2l::hide-warnings))