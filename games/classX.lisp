;;;;classX.lisp
;;;;http://www.aiai.ed.ac.uk/~jeff/clos-guide.html
;;;;code below comes mostly from the link above 

(defclass person ()
  ((name :accessor person-name
         :initform 'bill
         :initarg :name)
   (age :accessor person-age
        :initform 40
        :initarg :age)))

(defun make-person (name age)
  (make-instance 'person :name name :age age))

;;(setq pers12 (make-person 'joni 44))

(defclass teacher (person)
  ((subject :accessor teacher-sub
            :initarg :subject)))

(defclass math-teacher (teacher)
  ((subject :initform "math")))