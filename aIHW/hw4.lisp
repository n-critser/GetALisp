;;;; File: hw4.lisp
;;;; Author: N-Critser
;;;; -----------------------------
;;;;




;;;; square product
;; FIXME 
(defun sqr-prod (Alist Blist)
  (let ((result nil))
    (loop for x in Alist
       for y in Blist
         (if 
       do (setq result (* (* x x) (* y y)))
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