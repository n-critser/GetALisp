;;; -*- Mode: Lisp ; Syntax: Common-Lisp; -*-

;;;; Environment sim

;;;Usage: 
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;

;; Basic environment class
(defclass environment ()
  ((agents :accessor env-agents
           :initform '()
           :initarg :agents)
   (step   :accessor env-step
           :initform 0)
   (max-steps :initform 1000
              :initarg max-steps)
   (stream   :initform t) ;; TODO: CHECK THIS ONE ?
   (initialized :initform nil
                :initarg :initialized)
   (state  :accessor env-state
           :initform nil
           :initarg :state)))  
                
           
(defclass agent ()
  ((program :accessor agent-program       
            ;:initform #'nothing
            :initarg program)
   (body :accessor agent-body)
         ;:initform (make-agent-body))
   (score :accessor agent-score
          :initform 0)
   (percept :accessor agent-percept
            :initform nil
            :initarg percept)
   (action :accessor agent-action
           :initform nil
           :initarg action)
   (name :accessor agent-name
         :initform nil
         :initarg name)))

(defun run-environment (env)
  "Base function for running the simulated environment"
  (initialize env)
  (display-environment env)
  (dotimes (i (environment-max-steps env))
    (incf (environment-step env))
    (for each agent in (environement-agents env) do
         (setf (agent-percept agent) (get-percept env agent))
         (setf (agent-action agent)
               (funcall (agent-program agent) (agent-percept agent))))
    ;;Execute actions ---> update world state
    (update-fn env)
    (for each agent in (environment-agents env) do
         (setf (agent-score agent) (performance=measure env agent)))
    (display-environment env)
    (when (termination? env) (return)))
  env)


;;;; EVERYTHING BELOW HERE IS DIRECTLY FROM NORVIG
;;;; IF IT GETS CHANGED IT GOES ABOVE!!!!!!!!!!!!!!!!!
;;;; Generic Functions that must be defined for each environment

;;; For each new type of environment you want to define, you will need a
;;; defstructure that inherits from (includes) ENVIRONMENT, and you will need
;;; to write new methods (or inherit existing methods) for each of the
;;; following eight functions.  Here are the ones that will change for each
;;; new environment:

(defmethod get-percept ((env environment) agent)
  "Return the percept for this agent."
  (declare-ignore env agent)
  nil)

(defmethod update-fn ((env environment))
  "Modify the environment, based on agents actions, etc."
  (execute-agent-actions env))

(defmethod legal-actions ((env environment))
  "A list of the action operators that an agent can do."
  nil)

(defmethod performance-measure ((env environment) agent)
  "Return a number saying how well this agent is doing."
  ;; The default is to subtract one point for each time step.
  (declare-ignore agent)
  (- (environment-step env)))

;;; Here are the ones that can usually be inherited:

(defmethod initialize (env environment)
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (initialize-agent-names env)
  (setf (environment-initialized env) t)
  env)

(defmethod termination? ((env environment))
  "Return true if the simulation should end now."
  nil)

(defmethod display-environment ((env environment))
  "Display the current state of the environment."
  ;; You probably won't need to specialize this, unless you want to do
  ;; a fancy graphical user interface
  (let ((stream (environment-stream env)))
    (when stream 
      (format stream "~&At Time step ~D:~%" (environment-step env))
      (when (> (environment-step env) 0)
	(for each agent in (environment-agents env) do
	     (format stream 
		     "~&Agent ~A perceives ~A~%~6Tand does ~A~%"
		     agent (agent-percept agent)
		     (agent-action agent))))
      (display-environment-snapshot env))))

(defmethod display-environment-snapshot ((env environment))
  "Display a 'picture' of the current state of the environment."
  ;; This is what you will specialize 
  (print env (environment-stream env)))

;;;; Auxiliary Functions

(defun run-eval-environment (env)
  "Basic environment simulator; the same as run-environment. [p 48]
  We decided it was silly to run the environment and NOT eval the agents,
  so in this code, and in future editions of the book, we will only have
  RUN-ENVIRONMENT, and it will evaluate the agents."
  (run-environment env))

(defun agent-trial (environment-fn agent-type env-gen-random-state n)
  "Run n environments with an identical agent in each, and average the scores."
  ;; By binding *random-state* to env-gen-random-state, we hope to reproduce
  ;; the same set of environments each time AGENT-TRIAL is called with the
  ;; same environment-fn.
  (let ((total 0) (score 0))
    (for i = 1 to n do
	 (let* ((env (let ((*random-state* env-gen-random-state))
		       (funcall environment-fn 
				:stream nil
				:aspec (list agent-type)))))
	   (run-environment env)
	   (incf total (agent-score (first (environment-agents env))))))
    (setf score (float (/ total n)))
    (format t "~&~10,2F average for ~A" score agent-type)
    score))

(defun execute-agent-actions (env)
  "Each agent (if the agent is alive and has specified a legal action)
  takes the action."
  (for each agent in (environment-agents env) do
       (let ((act (agent-action agent)))
	 (when (member (op act) (legal-actions env))
	   (apply (op act) env (agent-body agent) (args act))))))

(defmethod print-structure ((env environment) stream)
  (format stream "#<~A; Step: ~D, Agents:~{ ~A~}>"
	  (type-of env) (environment-step env)
	  (environment-agents env)))

