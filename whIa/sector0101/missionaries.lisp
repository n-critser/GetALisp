;;;; Author: N-Critser
;;;; File:  missionaries.lisp


;; FIXME: Need to fill in state args!!!
(defclass shore-state (state)
  ((missionaries-start :initarg :missionaries-start :initform nil :accessor missionaries-start :documentation "missionaries on start shore")
   (cannibals-start :initarg :cannibals-start :initform nil :accessor cannibals-start :documentation "cannibals on start shore")
   (missionaries-boat :initarg :missionaries-boat)
   (cannibals-boat :initare :cannibals-boat)
   (missionaries-goal :initarg :missionaries-goal)
   (cannibals-goal :intitarg :cannibals-goal)))