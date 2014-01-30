;;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: N-Critser
;;;; copied from Peter Norvigs original lisp code files from AIMA
;;;; This is the closure for an Agent Program.  It is separate from
;;;; the agent body which it is connected to but logically and state wise
;;;; separated from.  As such, only precepts delivered from the environment
;;;; can affect the agent program.  

;;(defstruct (ask

(defstruct employee
  age
  first-name
  last-name
  sex
  children)


(defstruct spy-agent (name "007" :type string))
(make-spy-agent :name "james")
(make-spy-agent)

(defstruct agent 
  "an agent "
  (progam #'nothing)
  (body (make-agent-body))
  (score 0)
  (percept nil)
  (action nil)
  (name nil))

(defstruct (ask-user-agent (:include agent (program 'ask-user)))
  "an agent that asks the user to type in an action")

(defun ask-user (percept)
  "ask the user what action to take."
  (format t "~&Percept is ~A; action? " percept)
  (read))

(defmethod print-structure ((agent agent) stream)
  "agents are printed with name (body) and score"
  (format stream "[~A = ~D]" (or (agent-name agent) (agent-body agent))
          (agent-score agent)))

(defun initialize-agent-names (env)
  "name the agents 1,2, .. if no name"
  (for each agent in (environment-agents env) do
       (when (null (agent-name agent))
         (let ((i (+1 (position agent (environment-agents env))))
               (body (agent-body agent)))
           (setf (agent-name agent) i)
           (when (and body (null (object-name body)))
             (setf (object-name body) i))))))