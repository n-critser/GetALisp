;;;; File: hw4.lisp
;;;; Author: N-Critser
;;;; -----------------------------
;;;;
(defun load-hw4()
  (load "hw4.lisp"))

(define-condition arg-error (error)
  ((message
    :documentation "Text message of Argument Error"
    :initarg :message
    :initform "An argument error has occured"
    :reader arg-error-message
    :accessor message)
   (arg
    :accessor arg
    :initarg :arg
    :initform nil
    :reader arg-error-arg
    :documentation "The value that signaled the error"))
  (:report (lambda (condition stream)
             (format stream " ERROR-in: ~A  MESSAGE: ~A  ~%"
                     (arg-error-arg condition)
                     (arg-error-message condition)))))

(defun reset-list()
  (format t "Reformat to a list  " )) 
  

;(defmethod print-arg-error ((object arg-error) stream )
;  (print-unreadable-object (self stream :type t :identity t)
;    (format stream  "~@[~A ~] ~@[: ~A ~]~%"
;            (arg-error-message object)
;            (arg-error-arg object)))

;(defun arg-error (message &key arg )
;  (error 'arg-error
;         :message message
;         :arg arg))

(defparameter *test-list* '((1 2 3 x y z)
                            "hi" 
                            (r foo bar baz zab)
                            (2 nil nil 22 22)
                            (pi (* x x) r^2)))



(defun invalid-list (alist)
  (format t " INVALID-LIST : ~A ~%" alist ))
  ;;(format t " INVALID-LIST : ~{ ~A~^ ~}~~%" list))

;;;; square product
;; FIXME 
(defun sqr-prod (Alist Blist)
;  (restart-bind ((nil #'(lambda)() (invalid-list Alist))
;                 :report-function
;                 #'(lambda (stream)
;                     (format stream "Invalid List.")))
  
  (let ((result nil))
    (if (and  (consp  Alist) (consp Blist))
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
           do
             (format t  "[~A] and [~A] Type Mismatch ~%" x y)
             (setq result '---)
           collect result )
        ;else
        ;(format t "[ERROR: ~A is not a valid list]" "Your List"))))
        (cerror "Try again."
         'arg-error :message "Gack!! Somethings Wrong with my Thing"))))


(defparameter *sqr1* '( (1 2 3 6)  (a b c) (1 2 c) (a b 9 4) ("hi" "di" "ho") () ))
(defparameter *sqr2* '( (3 2 1)  (w h o) (x 2 f a) (bu 23 y) ( a 2 h i 2 )))

(defun test-sqr(list1 list2)
  ;(let ((out nil))
  (loop for l1 in list1
     for l2 in list2
     do (format t "[~{~A~^,~}]  [~{ ~A~^,~}] = Sqr-Result=~A~%"
                l1
                l2
                (handler-case (sqr-prod l1 l2)
                  (arg-error () nil)))))
               


(defun test-sqr-prod (lists) 
  (let ((out nil))
    (loop for list in lists
         do (setq out (handler-case (sqr-prod list list)
                        (arg-error () (format t "~A" * ))))
         collect out )))


;; Function: all-mom (list)
;; ------------------------
;; usage: (all-mom '( x 3 fancy what nil)) ----------> (MOM MOM MOM MOM NIL)
(defun all-mom (list)
  ;(when (not (consp list))
  ;  (format t "~A is not a list ~%" list))
  (when (consp list)
    (cond ((endp list) nil)
          ((consp (first list))
           (cons (all-mom (first list))(all-mom (rest list))))
          ((equal (first list) nil) (cons nil (all-mom (rest list))))
          (t  (cons 'mom (all-mom (rest list)))))))


(defun test-mom (lists)
  (loop for list in lists
       do (format t "[~{~A~^,~}] :  all-mom =~A~%"
               list
                (handler-case (all-mom list)
                  (arg-error () nil)))))

;;
(defun surface (elem list)
;;  (when (or (not elem) (not  list))
;;    (error 'arg-error))
   (cond ((endp list) 0)
         ((equal  elem (first list))
          (+ 1 (surface elem (rest list))))
         (t (surface elem (rest list)))))

;; defvar is not reloaded during a slime session
;; defparameter is  
(defparameter *elems* '(a b "hi" d 5 d d g d ) )
(defparameter *lists* '((a b c) (x y (b "what" f brick house) bb b B z) (j "hi" ("HI") g) (l m o) (1 2 34 5) ("hello" "back" "ddd" d d d 'd)))
                                                             

(defun test-surface (elems lists)
  (loop for elem in elems
     for list in lists
     ;;collect  (list elem list)
     do (format t "surface-count=~A :  elem= ~A    list= ~A~%"
                (handler-case (surface elem list)
                  (arg-error () nil))        
                elem
                list)))
      
    ;; collect (handler-case (surface elem list)
    ;;           (arg-error () nil))))
     
       

(defun deep(elem list)
  (cond ((endp list) 0)
        ((equal elem (first list))
         (+ 1 (deep elem (rest list))))
        ((consp (first list))
         (deep elem (first list)))
        (t (deep elem (rest list)))))

(defun test-deep (elems lists)
  (loop for elem in elems
     for list in lists
     do (format t "deep-count=~A :  elem= ~A    list= ~A~%"
                (handler-case (deep elem list)
                  (arg-error () nil))
                  elem
                  list)))

(defun test-deep-surface ()
  (test-deep *elems* *lists*)
  (test-surface *elems* *lists*))

;;     collect  (list elem list)
       ;;do (format t "elem=~A list=~A ~%" elem list)
;;     collect (handler-case (deep elem list)
;;               (arg-error () nil))))