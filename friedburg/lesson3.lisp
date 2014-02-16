;;Predicates in Lisp
(defun l3-call ()
  (load "lesson3.lisp"))

;; TYPE TESTING 
(atom 'a)       ;; T if its an atom 
(consp '(a b c));; T if its an non-empty list
(listp '(a b c));; T if its a list of any flavor
(numberp 12)    ;; T if its a number atom
(symbolp 'x)    ;; T if its a symbol nil and t included
(endp '())      ;; T if <list> is nil or empty list
(zerop 0)       ;; T if <arg> is 0 or evals to 0 --bitwise | or with 0

;; SPECIAL
(member 'i '(a i o u)) ;; (I O U) rest of list after member found Shallow


;; EQUALITY TESTING
(equal '(a b c) '(a b c))  ;; object wise equal 



;; ARITHMETIC PREDICATES
'(> < =)       ;;
(< 23 23)      ;; NIL


;; LOGICAL PREDICATES
'(not and or)
(not (not 2))  ;; T if nil

(or 'a 'b)     ;; returns the value of first True object  A

(and (first '(a b c)) (rest '(e f g h)))  
;; (F G H) ---> returns value of Truth maker


;; CONTROL STRUCTURES

;; IF 
(if (equal 'x 'y)
    'equal
    'not-equal) ;; ----> NOT-EQUAL

;; COND
(cond ((consp 2) 'liste) ; (cond ((<test>) <consequence>)
      ((symbolp 2) 'atom);       ((<test>)) if T evalutes to test value
      (t 'zahl))
;;ZAHL



;;;;   FUNCTIONS WITH TESTS

(defun inserts (item li)
  (cond ((member item li) li)
        (t (cons item li))))


(defun compare (x y)
  (and (numberp x)
       (numberp y))
  (cond ((< x y) 'erste-kleiner)
        ((> x y) 'erste-groesser)
        (t 'beide-gleich)))

(defun what-is-it (it)
  (cond ((listp it) 'list)
        ((symbolp it) 's-atom)
        ((numberp it) 'number)
        (t 'do-not-know)))

(defun weather-test (weather)
  (if (equal weather 'good)
      'go-swimming
      'study-at-home))
;; (weather-test 'good)  GO-SWIMMING


;; Test if ar == cold
(defun cold-p (ar)
  (equal ar 'cold))

(defun larger-15-p (number)
  (> number 15))

(defun palindrome-p (li)
  (equal li (reverse li)))

(defun one-p (li)
  (= 1 (length li)))

(defun more-than-two-p (li)
  (< 2 (length li)))

(defun nrest (item li)
  (- 
   (length (member item li)) 1))

(defun not-numberp (num)
  (not (numberp num)))

(defun number-or-list-p (li)
  (or
   (numberp li)
   (consp li)))

(defun safe-greaterp (x y)
  (and (numberp x)
       (numberp y)
       (> x y)))

(defun safe-lessp (x y)
  (and (numberp x)
       (numberp y)
       (< x y)))