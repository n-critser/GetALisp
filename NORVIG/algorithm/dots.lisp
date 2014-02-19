;;;; This file contains functions and definitions to play the game of DOTS.
;;;; More information on the game can be found in the handout for project 2.
;;;; The top level functions you will be using from this file are
;;;; playgame to play a game and draw-edge (or draw-left, draw-right, draw-
;;;; top, and draw-bottom) to make a single move.

;;;; To complete this game, you will need to write an evaluation function,
;;;; and a playing function that chooses and makes a move based on the
;;;; current board.  The function you write should have the form:
;;;; (defun play (player array depth)...) where player is the initial of the
;;;; current player, array is the array of box structures, and depth is the
;;;; maximum number of plies to search using your minimax algorithm.

;;;; Written by Ellen Walker, 9/93
;;;; Updated by Ellen Walker, 9/95

;;; "Public" functions
(export '(make-box copy-box box-p box-name box-top box-bottom box-left
	  box-right
	  init-dots print-dots draw-edge draw-top draw-left draw-right
          draw-bottom check-complete next-player count-boxes copy-dots-array
	  top left bottom right))


;;; Structure for a box

(defstruct box
  ;; name (initial) inside box
  (name nil)
  ;; flag for each edge - T if the edge is "filled in"
  (top nil)
  (bottom nil)
  (left nil)
  (right nil))

;;; Initialize the playing field.  It is a nxn array of box structures.

(defun init-dots (n)
  (let ((boxarray (make-array (list n n))))
    (dotimes (row n)
      (dotimes (col n)
	(setf (aref boxarray row col) (make-box))))
    boxarray))
	
;;; This function prints an ASCII representation of the playing field, by
;;; traversing the array of boxes.  Note that the top row must be traversed
;;; an extra time to get the top edges.

(defun print-dots (boxarray)
  ;; print the top edges of the top row
  (format t "~a" ".")
  (dotimes (col (array-dimension boxarray 1))
    (format t "~a" (if (box-top (aref boxarray 0 col)) "_." " .")))
  (format t "~%")
  ;; print a row of boxes
  (dotimes (row (array-dimension boxarray 0))
    ;; print leftmost vertical edge
    (format t "~a" (if (box-left (aref boxarray row 0)) "|" " "))
    ;; print box labels & vertical edges
    (dotimes (col (array-dimension boxarray 1))
      (format t "~a" (or (box-name (aref boxarray row col)) " "))
      (format t "~a" (if (box-right (aref boxarray row col)) "|" " ")))
    (format t "~%")
    ;; print horizontal edges below the box
    (format t "~a" ".")
    (dotimes (col (array-dimension boxarray 1))
      (format t "~a" (if (box-bottom (aref boxarray row col)) "_." " .")))
    (format t "~%"))
  (format t "~%"))

;;; draw-edge draws a single edge of a box, and checks whether the box is
;;; complete.  Note that for each edge interior to the playing field, two box
;;; boundaries must be set and two boxes must be checked for completeness.
;;; Actually draw-edge is a "wrapper" for 4 similar functions, draw-top, draw-
;;; bottom, draw-left, and draw-right

(defun draw-edge (boxarray player row col edge)
  (case edge
    (top    (draw-top boxarray player row col))
    (bottom (draw-bottom boxarray  player row col))
    (left   (draw-left boxarray player row col))
    (right  (draw-right boxarray player row col))))
	    

;;; Each of the 4 following functions draws a particular edge of the box with
;;; the given coordinates.  All have the same form:  box is the current box,
;;; next is the box on the other side of the edge to be drawn (if any),
;;; and res is the result to be returned (T if a box was completed).
;;; First check if the edge is already filled in.  If so, drop into the
;;; debugger in a continuable error so that :c gives the player another chance.
;;; Otherwise, fill in the edge and see if the box is complete.  If there is
;;; another box to check (next), fill in its equivalent edge and check if it's
;;; complete.  Return T if either "box" or "next" has been completed.

(defun draw-top (boxarray player row col)
  (let ((box (aref boxarray row col))
	(next (if (> row 0)
		   (aref boxarray (1- row) col)))
	(res nil))
  (cond ((box-top box)
	 (cerror "Try another edge" "This edge is already played")
	 ;;Note: return T so the player gets another attempt at this turn
	 T) 
	(t
	 (setf (box-top box) t)
	 (setf res (check-complete player box))
	 (if next (setf (box-bottom next) t))
	 (or (check-complete player next) res)))))


(defun draw-left (boxarray player row col)
  (let ((box (aref boxarray row col))
	(next (if (> col 0)
		   (aref boxarray row (1- col))))
	(res nil))
  (cond ((box-left box)
	 (cerror "Try another edge" "This edge is already played")
	 ;;Note: return T so the player gets another attempt at this turn
	 T) 
	(t
	 (setf (box-left box) t)
	 (setf res (check-complete player box))
	 (if next (setf (box-right next) t))
	 (or (check-complete player next) res)))))


(defun draw-right (boxarray player row col)
  (let ((box (aref boxarray row col))
	(next (if (< (1+ col) (array-dimension boxarray 1))
		   (aref boxarray row (1+ col))))
	(res nil))
  (cond ((box-right box)
	 (cerror "Try another edge" "This edge is already played")
	 ;;Note: return T so the player gets another attempt at this turn
	 T) 
	(t
	 (setf (box-right box) t)
	 (setf res (check-complete player box))
	 (if next (setf (box-left next) t))
	 (or (check-complete player next) res)))))

(defun draw-bottom (boxarray player row col)
  (let ((box (aref boxarray row col))
	(next (if (< (1+ row) (array-dimension boxarray 0))
		   (aref boxarray (1+ row) col)))
	(res nil))
  (cond ((box-bottom box)
	 (cerror "Try another edge" "This edge is already played")
	 ;;Note: return T so the player gets another attempt at this turn
	 T) 
	(t
	 (setf (box-bottom box) t)
	 (setf res (check-complete player box))
	 (if next (setf (box-top next) t))
	 (or (check-complete player next) res)))))

	 
;;; Check whether a particular box has been completed. If so, then fill in
;;; the player's initial.
(defun check-complete (player box)
  (and box
       (box-left box)
       (box-right box)
       (box-top box)
       (box-bottom box)
       (setf (box-name box) player)))
       
;;; This function plays a game of dots between player 1 and player 2.  It
;;; uses LISP's ability to pass functions as arguments to pass the actual
;;; playing functions for the two players and call them as needed.  A sample
;;; playing function (human-play) is included in this file.  You can use it
;;; to play against your program or to play against each other, e.g.
;;; (playgame 5 'human-play 'a 'human-play 'b)
;;; The keyword arguments p1-init and p2-init allow you to specify an
;;; initialization function for each player.  This function, if given, will
;;; be called (with no arguments) once before the game is played.  You can
;;; use it to set up global variables, etc.

(defun playgame (n depth p1-fun player1 p2-fun player2 &key p1-init p2-init)
  (if p1-init (funcall p1-init n depth player1 player2 t))
  (if p2-init (funcall p2-init n depth player2 player1 nil))
  (do ((boxarray (init-dots n))
       ;; lookup table to look up the function from the player's initial
       (func-table `((,player1 . ,p1-fun)
		     (,player2 . ,p2-fun)))
       ;; Who is the player?  Start with player 1, then determine the next
       ;; player based on the results of the first player's turn
       ;; Note that the call to apply actually lets the current player play
       (player player1
	       (next-player player
			    (apply (cdr (assoc player func-table))
				   (list player boxarray depth))
			    (list player1 player2)))
       )
      ;; When there are no free boxes (labeled NIL) the game is over.
      ;; Print the final board and compute the score.
      ((zerop (count-boxes boxarray nil))
       (print-dots boxarray)
       (format t "Game over:  ~%")
       (dolist (i (list player1 player2))
	 (format t "~t~s - ~d~%" i (count-boxes boxarray i))))
    ;; This is the body of the loop  It prints the current board.
    (print-dots boxarray)
))

;;; Determine who the next player is, given the current player, a logical
;;; value whether that player gets a free turn, and the two players.
;;; When it's not a free turn, return the other player.
(defun next-player (current free-turn players)
  (if free-turn
      current
      (car (remove current players))))     

;;; Count the boxes in the array with the given label, by traversing the

;;; array. 
(defun count-boxes (boxarray label)
  (let ((ct 0))
    (dotimes (row (array-dimension boxarray 0) ct)
      (dotimes (col (array-dimension boxarray 1))
	(if (eq (box-name (aref boxarray row col)) label)
	    (setf ct (1+ ct)))))))
	
	
;;; This is a dummy game playing routine
;;; Simply read in the coordinates of the box and which edge to fill in, then
;;; fill it in.
(defun human-play (player boxarray depth)
  ;; No searching, so ignore the depth parameter
  (declare (ignore depth))
  (format t "Player ~a's turn:~%" player)
  (format t "Enter row # and column # of box: ")
  (let* ((row (read))
	 (col (read))
	 box)
    ;; Make sure the coordinates are valid before accessing the box
    (cond ((and (numberp row)
		(> row -1)
		(< row (array-dimension boxarray 0))
		(numberp col)
		(> col -1)
		(< col (array-dimension boxarray 0)))
	   (setq box (aref boxarray row col))
	   ;; Determine which edge to fill in
	   (format t "Which edge? Enter the initial: T(op) L(eft) B(ottom) or R(ight) ")
	   ;; Call appropriate function to fill in the edge
	   (funcall
	    (cdr (assoc (read) '((t . draw-top)
				 (l . draw-left)
				 (b . draw-bottom)
				 (r . draw-right))))
	    ;; The following are arguments passed to the chosen function
	    boxarray player row col))
	  ;; If invalid coordinates, recursively call human-play with the
	  ;; same parameters
	  (t
	   (cerror "Try again"
		   "Sorry, ~s ~s is not a valid address" row col)
	   (human-play player boxarray 0)))))

	
;;; Utility function for copying a two-dimensional "boxarray".  Use it to keep
;;; from modifying the "real" boxarray while searching future positions.

(defun copy-dots-array (array)
  (let ((new-array (make-array (array-dimensions array))))
    (dotimes (i (array-dimension new-array 0))
       (dotimes (j (array-dimension new-array 1))
	 (setf (aref new-array i j)
	       (copy-box (aref array i j)))))
    new-array))
	     
