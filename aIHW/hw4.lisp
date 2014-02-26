;;;; File: hw4.lisp
;;;; Author: N-Critser
;;;; -----------------------------
;;;;
(defun load-hw4()
  (load "hw4.lisp"))



;;;; square product
;; FIXME 
(defun sqr-prod (Alist Blist)
  (let ((result nil))
    (loop for x in Alist
       for y in Blist
       ;;if (and (equal x nil) (equal y nil)
       when  (and (symbolp x) (symbolp y))
       do (setq result (list x x y y))
       when (and (numberp x) (numberp y))
       do (setq result (* (* x y) (* x y)))
       ;;when (and (not x) (not y))
       ;;do (print "x and y are nil")
       when (or (and (not (symbolp x)) (symbolp y))
                (and (symbolp x) (not (symbolp y))))
         do (format t  "~A  and ~A don't match type ~%" x y)
         collect result )))
       


;; Function: all-mom (list)
;; ------------------------
;; usage: (all-mom '( x 3 fancy what nil)) ----------> (MOM MOM MOM MOM NIL)
(defun all-mom (list)
  (cond ((endp list) nil)
        ((consp (first list))
         (cons (all-mom (first list))(all-mom (rest list))))
        ((equal (first list) nil) (cons nil (all-mom (rest list))))
        (t  (cons 'mom (all-mom (rest list))))))


;;
(defun surface (elem list)
  (cond ((endp list) 0)
        ((equal  elem (first list))
         (+ 1 (surface elem (rest list))))
        (t (surface elem (rest list)))))


(defun deep(elem list)
  (cond ((endp list) 0)
        ((equal elem (first list))
         (+ 1 (deep elem (rest list))))
        ((consp (first list))
         (deep elem (first list)))
        (t (deep elem (rest list)))))