(defun simple-grid( env)
  " create a 2 d array that can represent the
    grid in a graphical way"
  (loop
     for  i from 0 to env
       for j from 0 to env
     do (print '- )))

;; (SLOT-VALUE instance slot-name)
(defclass box2 ()
  ((rows :accessor box2-rows
         :initform 3
         :initarg :rows)
   (cols :accessor box2-cols
         :initform 3
         :initarg :cols)
   (rep  :accessor box2-rep 
         :initarg :rep)
   (row-char :initform '-)
   (col-char :initform '\|)))

(defun make-box2 (rows cols)
  (make-instance 'box2 :rows rows :cols cols))

(defun print-box2 (box2)
  (slot-value box2 'rows)
  (slot-value box2 'cols))


;; makes 2 d array n x n and prints x n^2 times
(defun make-bins(n)
  (let ((boxarray (make-array (list n n))))
    (dotimes (row n)
      (princ #\Newline)
      (dotimes (col n)
        (princ '*)))
    boxarray))


;;; returns the initial tic-tac-toe board
(defun make-board ()
  '(nil nil nil nil nil nil nil nil nil))

;;; print a tic-tac-toe board
(defun print-board (board)
  (format t " ~%")
  (format t "    0|1|2~%")
  (format t " ~%")
  (format t "0   ~A|~A|~A~%" (token 0 board) (token 1 board) (token 2 board))
  (format t "    ------~%")
  (format t "1   ~A|~A|~A~%" (token 3 board) (token 4 board) (token 5 board))
  (format t "    ------~%")
  (format t "2   ~A|~A|~A~%" (token 6 board) (token 7 board) (token 8 board))
  (format t " ~%"))
;;; used by print-board
(defun token (index board)
  (let ((item (nth index board)))
    (if item item " "))) ; converts nils to blanks, but leaves 'Xs and 'Os alone



(defun init-dots (n)
  (let ((boxarray (make-array (list n n))))
    (dotimes (row n)
      (dotimes (col n)
	(setf (aref boxarray row col) (make-box))))
    boxarray))
	

#|
(defmethod print-box (box2)
  (let ((b (make-array (list box2.rows box2.cols))))
    (dotimes (i 
|#
(defstruct box top left owner)

(defun make-random-boxes (rows cols)
  (let ((b (make-array (list rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref b i j)
              (make-box
               :top (> (random 100) 50)
               :left (> (random 100) 50)
               :owner (and (> (random 100) 50) "A")))))
    b))

(defun print-boxes (boxes)
  "Print the boxes array to *standard-output*"

  (let ((rows (array-dimension boxes 0))
        (cols (array-dimension boxes 1)))
    (dotimes (r rows)
      ;; print tops then sides
      (dotimes (c cols)
        (with-slots (top) (aref boxes r c)
          (princ "+")
          (if (< (1+ c) cols)
            (princ (if top
                       "-"
                       " "))
            (princ #\Newline))))
      (when (< (1+ r) rows)
        (dotimes (c cols)
          (with-slots (left owner) (aref boxes r c)
            (princ (if left "|" " "))
            (if (< (1+ c) cols)
                (princ (or owner " "))
                (princ #\Newline))))))))