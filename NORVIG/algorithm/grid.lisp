;;;;grid code from Norvig

(defun grid-contents (env loc)
  "return a list of objects in this location"
  (aref (grid-environment-grid env) (xy-x loc) (xy-y loc)))

(defsetf grid-contents (env loc) (val)
  `(setf (aref (grid-environment-grid ,env) (xy-x, loc) (xy-y ,loc))
         ,val))

(defun move-object-to (object loc env)
  "move an object to a location and return that location. but fail
   if the locartion is blocked"
  (cond ((find-object-if #'obstacle-p loc env)
         (setf (object-bump object) 'bump)
         nil)
        (t (remove-object object env)
           (place-object object loc env)
           loc)))

(defun place-object (object loc env &optional (initial? t))
  "put the object in its initial pos or a new pos"
  (when (agent-p object)
    (pushnew object (environment-agents env))
    (setf object (agent-body object)))
  (setf (object-loc object) loc)
  (pushnew object (grid-contents env loc))
  (when initial?
    (push object (grid-environment-objects env)))
  object)

(defun place-in-container (object container env)
  "put the object inside conatiner"
  (when (< (+ (object-size object)
              (sum (object-contents container) #'object-size))
           (object-max-contents object))
    (remove-object object env)
    (setf (object-container object) container)
    (setf (object-loc object) (object-loc container))
    (pushnew object (object-contents container))
    object))

(defun remove-object (object env)
  "remove the obj from its location"
  (let ((loc (object-loc object))
        (old-container (object-container object)))
    (deletef object (grid-contents env loc))
    (when old-container
      (deletef object (object-contents old-container))
      (setf (object-container object) nil))))

(defun find-object-if (predicate loc env)
  "return an object in this location that satisfies this predicate"
  (find-if predicate (grid-contents env loc)))

(defun find-neighbor-if (predicate loc env)
  "return an object in a neighboring sqr that satisfies predicate"
  (let ((x (xy-x loc))
        (y (xy-y loc)))
    (or (find-object-if predicate (@ X (+ y 1)) env)
        (find-object-if predicate (@ x (- y 1)) env)
        (find-object-if predicate (@ (+ x 1) y) env)
        (find-object-if predicate (@ (- x 1) y) env))))


(defun find-object-or-neighbor-if (predicate loc env)
  "Return an object either in loc or a neighboring square that satisfies
  the predicate."
  (or (find-object-if predicate loc env)
      (find-neighbor-if predicate loc env)))

(defun near? (loc1 loc2 &optional (tolerance 1))
  "Are the two locations nearby each other?"
  (and (<= (abs (- (xy-x loc1) (xy-x loc2))) tolerance)
       (<= (abs (- (xy-y loc1) (xy-y loc2))) tolerance)))

;;;; Maintaining and manipulating orientation

(defun add-locs (&rest locations)
  "Coordinate-wise addition of locations: (add-locs '(1 2) '(10 20)) = (11 22)"
  (apply #'mapcar #'+ locations))

(defun subtract-locs (&rest locations)
  "Coordinate-wise subtraction of locations."
  (apply #'mapcar #'- locations))

(defun heading->string (heading)
  "Convert a heading like (0 1) to a depictive string like ^."
  (cond ((equal heading '(1 0)) ">")
	((equal heading '(0 1)) "^")
	((equal heading '(-1 0)) "<")
	((equal heading '(0 -1)) "V")
	(t "?")))

(defun absolute-loc (agent-body offset)
  "Return an absolute location given an offset from an agent, taking the
  agent's orientation into account.  An offset of (1 2) means 1 square to
  the right and two ahead of the agent, given its present orientation."
  (let ((x (xy-x offset))
	(y (xy-y offset))
	(heading (agent-body-heading agent-body)))
    (add-locs (object-loc agent-body)
	      (cond ((equal heading '(1 0)) (@ y (- x)))
		    ((equal heading '(0 1)) offset)
		    ((equal heading '(-1 0)) (@ (- y) x))
		    ((equal heading '(0 -1)) (@ (- x) (- y)))
		    (t "?")))))

(defun offset-loc (agent-body loc)
  "Return an offset from an agent that corresponds to the absolute loc."
  (let ((x (- (xy-x loc) (xy-x (object-loc agent-body))))
	(y (- (xy-y loc) (xy-y (object-loc agent-body))))
	(heading (agent-body-heading agent-body)))
    (cond ((equal heading '(1 0)) (@ (- y) (+ x)))
	  ((equal heading '(0 1)) (@ x y))
	  ((equal heading '(-1 0)) (@ (+ y) (- x)))
	  ((equal heading '(0 -1)) (@ (- x) (- y)))
	  (t "?"))))
