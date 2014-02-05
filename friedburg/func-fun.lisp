;;; func-fun.lisp

(defun change-head (l1 l2)
  (cons (first l2) (rest l1)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun volume-of-cuboid (a b c)
  (* a b c))


(defun area-of-square (s)
  (* s s))


(defun rectangle-area (a b)
  (* a b))

(defun cuboid-volume-new (l)
  (* (first l) (my-second l) (my-third l)))

(defun my-second (l)
  (first (rest l)))

(defun my-third (l)
  (first (rest (rest l))))

(defun weight-of-purchase (l)
  (+ (my-second (first l)) (my-second (my-second l))))