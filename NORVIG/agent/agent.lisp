;;;; -*- Mode: Lisp; Syntax: Common-Lisp;
;;;; copied from Peter Norvigs original lisp code files from AIMA
;;;; This is the closure for an Agent Program.  It is separate from
;;;; the agent body which it is connected to but logically and state wise
;;;; separated from.  As such, only precepts delivered from the environment
;;;; can affect the agent program.  




;;(defstruct agent 
;  "an agent "
;  (progam #'nothing)
;  (body (make-agent-body))
;  (score 0)
;  (percept nil)
;  (action nil)
;  (name nil))



;; "an agent that asks the user to type in an action"
(defclass agent-body ( )
  ((percept :accessor agent-precept
            :initarg :percept
            :documentation "a precept for the agent  ")
   (name :accessor agent-name
         :initarg :name
         :initform (error "Must supply an agent name")
         :documentation "agents name")
   (program :accessor agent-program )))



(defun make-agent (percept )
  "default function to make an instance of an agent
    use read function to get interactive data from user "
  (make-instance 'agent-body
                 :name name
                 :percept percept ))

(defmethod  initialize-agent :after ((agent agent-body) &key)
       (let ((percept (slot-value agent 'percept)))
         (setf (slot-value agent 'program)
               (ask-user percept))))

(defparameter *agent* (make-instance 'agent-body
					      :percept 'see-red
                                              :name 'bill))

(defun ask-user (percept)
  "ask the user what action to take."
  (format t "~&Percept is ~A; action? " percept)
  (read))

(defgeneric percept(agent-body))

(defmethod percept ((agent agent-body))
  (slot-value agent 'percept))


(defmethod print-structure ((agent agent) stream)
  "agents are printed with name (body) and score"
  (format stream "[~A = ~D]" (or (agent-name agent) (agent-body agent))
          (agent-score agent)))
#|
(defun initialize-agent-names (env)
  "name the agents 1,2, .. if no name"
  (for each agent in (environment-agents env) do
       (when (null (agent-name agent))
         (let ((i (+1 (position agent (environment-agents env))))
               (body (agent-body agent)))
           (setf (agent-name agent) i)
           (when (and body (null (object-name body)))
             (setf (object-name body) i))))))
|#