;;;; Author: N-Critser
;;;; File:  missionaries.lisp

(defun trace-depth-no-dupe ()
  (depth-first-search-with-duplicate-node-detection *missionaries-and-cannibals*))

(defun a-star-mc ()
  (a-star-search *missionaries-and-cannibals*))

;; FIXME  MAKE STATES CHANGE IN ONE MOVE NOT TWO!!!!!!!!!!!!!!!!!!!
;;  MISS-AND-CANNIB-BOAT-TO-GOAL  : ONE MISS AND ONE CANNIBAL TAKE BOAT FROM START TO GOAL
;;  check : boat at start, even cannibals and missionaries on both coasts. 

;; FIXME: Need to fill in state args!!!
(defclass shore-state (state)
  ((boat-start
    :initarg :boat-start
    :initform nil
    :accessor boat-start
    :documentation "boats present at start shore")
   (missionaries-start
    :initarg :missionaries-start
    :initform nil
    :accessor missionaries-start
    :documentation "missionaries on start shore")
   (cannibals-start
    :initarg :cannibals-start
    :initform nil
    :accessor cannibals-start
    :documentation "cannibals on start shore")
   (boat-goal
    :initarg :boat-goal
    :initform nil
    :accessor boat-goal
    :documentation "boat present at goal")
   (missionaries-goal
    :initarg :missionaries-goal
    :initform nil
    :accessor missionaries-goal
    :documentation "missionaries at goal shore")
   (cannibals-goal
    :initarg :cannibals-goal
    :initform nil
    :accessor cannibals-goal
    :documentation "cannibals at goal shor")))

(defparameter *missionaries-and-cannibals*
  (make-instance 'problem
                 :start-state (make-instance 'shore-state
                                             :boat-start t
                                             :missionaries-start 3
                                             :cannibals-start 3
                                             :boat-goal nil
                                             :missionaries-goal 0
                                             :cannibals-goal 0)
                 :goal-test 'all-at-goal-shore
                 :operators '( two-cannib-boat-to-goal
                              one-miss-boat-to-goal  one-miss-boat-to-start
                              cannib-and-miss-boat-to-goal   cannib-and-miss-boat-to-start
                              two-miss-boat-to-start two-cannib-boat-to-start
                              one-cannib-boat-to-goal one-cannib-boat-to-start
                              two-miss-boat-to-goal
                             )
                              ;miss-and-cannib-to-start cannib-to-start  miss-to-start eat-miss)
                 :name "missionaries-and-cannibals"))

(defmethod equal-states ((self shore-state) (other shore-state))
  (and (equal (missionaries-start self) (missionaries-start other))
       (equal (cannibals-start self) (cannibals-start other))
       (equal (missionaries-goal self) (missionaries-goal other))
       (equal (cannibals-goal self) (cannibals-goal other))))

(defmethod copy ((self shore-state))
  (make-instance 'shore-state
                 :missionaries-start (missionaries-start self)
                 :cannibals-start (cannibals-start self)
                 :missionaries-goal (missionaries-goal self)
                 :cannibals-goal (cannibals-goal self)))

(defmethod all-at-goal-shore ((self shore-state))
  (and (eql t (boat-goal self))
       (>= (missionaries-goal self) 3)
       (>= (cannibals-goal self) 3)))

(defmethod estimated-distance-from-goal ((self shore-state))
  (declare (ignore self))
  1)

;;;;**********************************************************
;;;;  OPERATORS AND OTHER GOODIES
;;;;        two-miss-boat-to-goal  two-cannib-boat-to-goal
;;;;        one-miss-boat-to-goal  one-miss-boat-to-start
;;;;        cannib-and-miss-boat-to-goal   cannib-and-miss-boat-to-start
;;;;        two-miss-boat-to-start  two-cannib-boat-to-start  
;;;;      one-cannib-boat-to-goal one-cannib-boat-to-start
;;;;     
;;;;                             
;;;;**********************************************************

;(defmethod start-two-miss-to-boat ((self shore-state))
;  (when (and (eql (boat-start self) t)
;             (> (missionaries-start self) (cannibals-start self)))
;    (let ((copy (copy self))
;          (old-miss-start (missionaries-start self))
;          (old-miss-boat  (missionaries-boat self)))
;      (setf (missionaries-boat copy) (+ old-miss-boat 2))
;      (setf (missionaries-start copy) (- old-miss-start 2))
;      copy)))


;;;   MOVE THE CONIDITIONALS INTO THE LET PORTION TO ONLY BREAK ON THE BOAT LOCATION!!!

(defmethod two-miss-boat-to-goal ((self shore-state))
  (when (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      (when (and
             (>=  old-miss-start 2)
             (<=  old-cann-start 1)
             (< old-cann-goal 3)
             )
      (setf (boat-start copy) nil)
      (setf (boat-goal copy ) t)
      (setf (missionaries-start copy) (- old-miss-start 2))
      (setf (missionaries-goal copy) (+ old-miss-goal 2))
      copy))))

(defmethod two-miss-boat-to-start ((self shore-state))
  (when (eql (boat-goal self) t)
            ; (>= (missionaries-goal self) 2)
             
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-miss-goal (missionaries-goal self)))
      (when  (>= old-miss-goal 2)
        (setf (boat-start copy) t)
        (setf (boat-goal copy ) nil)
        (setf (missionaries-start copy) (+ old-miss-start 2))
        (setf (missionaries-goal copy) (- old-miss-goal 2))
        copy))))



;(defmethod start-one-miss-to-boat ((self shore-state))
;  (when (and (eql (boat-start self) t)
;             (> (missionaries-start self) (cannibals-start self)))
;    (let ((copy (copy self))
;          (old-miss-start (missionaries-start self)))
;      (setf (missionaries-boat copy)  1)
;      (setf (missionaries-start copy) (- 1 old-miss-start))
;      copy)))

(defmethod one-miss-boat-to-goal ((self shore-state))
  (when  (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      (when (and
             (>= old-miss-start 1)
             (> old-miss-start old-cann-start)
             (<= old-cann-goal old-miss-goal)
             )
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (missionaries-start copy) (- old-miss-start 1))
        (setf (missionaries-goal copy) (+ old-miss-goal 1))
        copy))))

(defmethod one-cannib-boat-to-goal ((self shore-state))
  (when  (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self)))
      (when (and
             (>= old-cann-start 1)
             (or (< old-cann-goal old-miss-goal)
                 (= old-miss-goal 0))
             )
      (setf (boat-start copy) nil)
      (setf (boat-goal copy) t)
      (setf (cannibals-start copy) (- old-cann-start 1))
      (setf (cannibals-goal copy) (+ old-cann-goal 1))
      copy))))
  
  
;(defmethod goal-one-miss-to-boat ((self shore-state))
;  (when (and (eql (boat-goal self) t)
;             (> (missionaries-goal self) (cannibals-goal self)))
;    (let ((copy (copy self))
;          (old-miss-goal (missionaries-start self)))
;      (setf (missionaries-boat copy) 1)
;      (setf (missionaries-goal copy) (- 1 old-miss-goal))
;      copy)))

(defmethod one-miss-boat-to-start ((self shore-state))
  (when (and (eql (boat-goal self) t)
            ; (>= (missionaries-goal self) 1)
            ; (> (missionaries-goal self) (cannibals-goal self))
            ; (<= (cannibals-start self) (missionaries-start self))
             )
    (let ((copy (copy self))
          (old-miss-goal (missionaries-goal self))
          (old-miss-start (missionaries-start self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      (when (and
             (>= old-miss-goal 1)
             (>= old-miss-goal old-cann-goal)
            ; (<= old-cann-start old-miss-start))
             )
        (setf (boat-goal copy) nil)
        (setf (boat-start copy) t)
        (setf (missionaries-goal copy) (- old-miss-goal 1))
        (setf (missionaries-start copy) (+ old-miss-start 1))
        copy))))
          
(defmethod one-cannib-boat-to-start ((self shore-state))
  (when (and (eql (boat-goal self) t)
            ; (>= (cannibals-goal self) 1)
            ; (< (cannibals-start self) (missionaries-start self))
             )
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self)))
      (when (and
             (>= old-cann-goal 1)
             (or (< old-cann-start old-miss-start)
                 (= old-miss-start 0)))
        (setf (boat-start copy) t)
        (setf (boat-goal copy) nil)
        (setf (cannibals-start copy) (+ old-cann-start 1))
        (setf (cannibals-goal copy) (- old-cann-goal 1))
        copy))))
               

;(defmethod start-two-cannib-to-boat ((self shore-state))
;  (when (eql (boat-start self) t)
;    (when (and (= (missionaries-boat self) 0)
;               (= (cannibals-boat self) 0)
;               (> (cannibals-start self) 1))
;      (let ((copy (copy self))
;            (old-cann-start (cannibals-start self))
;            (old-cann-boat (cannibals-boat self)))
;        (setf (cannibals-boat copy) (+ old-cann-boat  2))
;        (setf (cannibals-start copy) (- old-cann-start 2))
;        copy)))

(defmethod two-cannib-boat-to-goal ((self shore-state))
  (when  (boat-start self)
    (let ((copy (copy self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self)))
      (when 
          (and
           (> old-cann-start 1))
           
         ;  (or (=  old-miss-goal 0)
         ;      (> old-miss-goal (+ (cannibals-goal self) 2))
         ;      )
             
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (cannibals-start copy) (- old-cann-start 2))
        (setf (cannibals-goal copy) (+ old-cann-goal 2))
        copy))))


(defmethod two-cannib-boat-to-start ((self shore-state))
  (when (and (eql (boat-goal self) t)
            ; (> (cannibals-goal self) 1)
            ; (or (= (missionaries-start self) 0)
            ;     (> (missionaries-start self) (+ (cannibals-start self) 2)))
             ) ; THIS AND ( OR ) MAY BE A PROBLEM
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self)))
      
      (when (and
             (> old-cann-goal 1))
             ;(or (= old-miss-start 0)
              ;   (> old-miss-start (+ old-cann-start 2))))
        (setf (boat-start copy) t)
        (setf (boat-goal copy) nil)
        (setf (cannibals-start copy) (+ old-cann-start 2))
        (setf (cannibals-goal copy) (- old-cann-goal 2))
        copy))))




;; FIXME : TAKE THE CONDITIONALITY OUT WHERE UNNECESSARY
;(defmethod start-cannib-and-miss-to-boat ((self shore-state))
;  (when (eql (boat-start self) t)
;    (when (and (= (cannibals-boat self) 0)
;               (= (missionaries-boat self) 0)
;               (= (missionaries-start self) (cannibals-start self)))
;      (let ((copy (copy self))
;            (old-cann-start (cannibals-start self))
;            (old-miss-start (missionaries-start self)))
;        (setf (cannibals-boat copy) 1)
;        (setf (missionaries-boat copy) 1)
 ;       (setf (cannibals-start copy) (- old-cann-start 1))
;        (setf (missionaries-start copy) (- old-miss-start 1))
;        copy))))

(defmethod cannib-and-miss-boat-to-goal ((self shore-state))
  (when  (eql (boat-start self) t)
            ; (>= (missionaries-start self) 1)
            ; (>= (cannibals-start self) 1)
            ; (>= (missionaries-goal self) (cannibals-goal self))
            ; (>= (missionaries-start self) (cannibals-start self))
             
    (let ((copy (copy self))
          (old-cann-start (cannibals-start self))
          (old-miss-start (missionaries-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self)))
      (when (and
             (>= old-miss-start 1)
             (>= old-cann-start 1)
             (>= old-miss-goal old-cann-goal)
             (>= old-miss-start old-cann-start))
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (cannibals-start copy) (- old-cann-start 1))
        (setf (missionaries-start copy) (- old-miss-start 1))
        (setf (cannibals-goal copy) (+ old-cann-goal 1 ))
        (setf (missionaries-goal copy) (+ old-miss-goal 1))
        copy))))

;(defmethod goal-cannib-and-miss-to-boat ((self shore-state))
;  (when (eql (boat-goal self) t)
;    (let ((copy (copy self))
;          (old-cann-goal (cannibals-goal self))
;          (old-miss-goal (missionaries-goal self)))
;      (setf (cannibals-boat copy) 1)
;      (setf (missionaries-boat copy) 1)
; 
;      (setf (cannibals-goal copy) (- 1 old-cann-goal))
;      (setf (missionaries-goal copy) (- 1 old-miss-goal))
;      copy)))

(defmethod cannib-and-miss-boat-to-start ((self shore-state))
  (when (eql (boat-goal self) t)
     (let ((copy (copy self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-start (cannibals-start self))
          (old-miss-start (missionaries-start self)))
      (when
          (and
           (>= old-miss-goal 1)
           (>= old-cann-goal 1))
      (setf (boat-goal copy) nil)
      (setf (boat-start copy) t)
      (setf (cannibals-goal copy) (- old-cann-goal 1))
      (setf (missionaries-goal copy) (- old-miss-goal 1))
      (setf (cannibals-start copy) (+ old-cann-start 1))
      (setf (missionaries-start copy) (+ old-miss-start 1))
      copy))))
          
          
;(defmethod miss-pickup-cannib ((self shore-state))
;  (when (or (> (missionaries-goal self) (cannibals-goal self))
;            (= (missionaries-goal self) 0))
;    (let 


;(start-two-cannib-to-boat (start-state *missionaries-and-cannibals*))
;(describe (two-cannib-boat-to-goal (start-state *missionaries-and-cannibals*)))
;(describe (two-miss-boat-to-goal (start-state  *missionaries-and-cannibals*)))
;(describe (two-cannib-boat-to-start (start-state *missionaries-and-cannibals*)))
(describe (cannib-and-miss-boat-to-goal  (start-state *missionaries-and-cannibals*)))
(describe (cannib-and-miss-boat-to-start  (cannib-and-miss-boat-to-goal  (start-state *missionaries-and-cannibals*))))
;(describe (one-cannib-boat-to-goal (start-state *missionaries-and-cannibals*)))
(describe (one-cannib-boat-to-start (one-cannib-boat-to-goal (start-state *missionaries-and-cannibals*))))

;(describe (one-cannib-boat-to-start (two-cannib-boat-to-goal (start-state *missionaries-and-cannibals*))))

(describe (two-miss-boat-to-goal (one-cannib-boat-to-start (two-cannib-boat-to-goal (one-miss-boat-to-start (cannib-and-miss-boat-to-goal  (start-state *missionaries-and-cannibals*)))))))