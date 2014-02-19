;; M-- M-x slime choose ccl64
(defun load-lesson4 ()
  (load "lesson4.lisp"))

(defun say-fuck-yeah ()
  (format t "FUCK YEAH" ))

(defun my-member (expression list)
  (cond ((endp list) nil)                 ;base case 1
        ((equal expression
                (first list)) list)       ;base case 2
        (t (my-member expression (rest list)))))  ;recursive case

(defun top-level-list-p (list)
  (cond ((endp list) t)
        ((consp (first list)) nil)
        (t (top-level-list-p (rest list)))))
;; 
(defun contains-number-p (list)
  (cond ((endp list) nil)
        ((numberp (first list)) t)
        (t (contains-number-p (rest list)))))

;;
(defun monotonic-decreasing-p (number-list)
  (cond ((endp number-list) t) ;; t case
        ((endp (rest number-list)) t)  ;; if list has size<2 then t
        ((< (first number-list) (first (rest number-list))) nil);; nil case
        (t (monotonic-decreasing-p (rest number-list)))))


;; simple-and
(defun simple-and (list)
  (cond ((endp list) t)
    ((and (first list) (simple-and (rest list))))))


;; simple-or
(defun simple-or (list)
  (cond ((endp list) nil)
        ((or (first list) (simple-or (rest list))))))

;; x-in-list-p
(defun x-in-list-p (list)
  (cond ((endp list) nil)
        ((equal 'x (first list)) t)
        (t (x-in-list-p (rest list)))))

;; ALL-SAME-AS-ELEMENT 
(defun all-same-as-element( elm list)
  (cond ((endp list) t)
        ((and (equal elm (first list))
              (all-same-as-element elm (rest list))))))


;; mono-increasing
(defun monotonic-increasing-p (number-list)
  (cond ((endp number-list) t) ;; t case
        ((endp (rest number-list)) t)  ;; if list has size<2 then t
        ((> (first number-list) (first (rest number-list))) nil);; nil case
        (t (monotonic-increasing-p (rest number-list)))))
