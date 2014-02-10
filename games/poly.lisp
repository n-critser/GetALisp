;;;; polynomial things in lisp
;;;; author: n-critser

(defun load-poly ()
  (load "poly.lisp"))

;; defclass info from "Practical Common Lisp"
;; using (defclass name (direct-superclass-name*)
;;(slot-specifier*))
(defclass term ()
  ((variable-char
   :initarg :variable-char
   :initform 'x)
   (coefficient
    :initarg :coefficient
    :initform 1)
   (exponent
    :initarg :exponent
    :initform 0)))


;; Needs a list of terms instead of just a term
(defclass poly()
  ((term1
    :initarg :term1)))

(defmethod add-term (poly term)
  (list (slot-value poly 'term1)
        term))


(defparameter *sqr* (make-instance 'term :variable-char 'x' :coefficient 2 :exponent 2))
;(setf (slot-value *sqr* 'variable-char) 'x)
;(setf (slot-value *sqr* 'coefficient) 2)
;(setf (slot-value *sqr* 'exponent) 2)
(defmethod coeff (a-term)
  (slot-value a-term 'coefficient))

(defparameter *poly1*
    (make-instance 'poly :term1 *sqr*))

(defmethod print-term (a-term)
  (format t "~d~A*~d"
          (slot-value a-term 'coefficient)
          (slot-value a-term 'variable-char)
          (slot-value a-term 'exponent)))

