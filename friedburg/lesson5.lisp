;;;; lesson5.lisp
;;;; friedburg-lesson5-toplevel recurssion

;;count-atoms
(defun count-atoms (list)
  (cond ((endp list) 0)
        ((atom (first list))
         (+ 1 (count-atoms (rest list))))
        (t (count-atoms (rest list)))))


;; my-length
(defun my-length (list)
  (cond ((endp list) 0)
        ((consp (first list))
         (+ 1 (my-length (rest list))))
        ((atom (first list))
         (+ 1 (my-length (rest list))))
        (t (my-length (rest  list)))))


;; count-sublists !!!!CRAZY WEIRD LISP SHIT !!!
(defun count-sublists (list)
  (cond ((endp list) 0)
        ((consp (first list))
         (+ 1 (count-sublists (rest list))))
        ((not (consp (first list)))         ;; could just say 
         (+ 0 (count-sublists (rest list))))))  ;; (t (count-sublists (rest list)))

;; my-remove
(defun my-remove ( expr list)
  (cond ((endp list) nil)
        ((equal (first list) expr)
          (my-remove expr (rest list)))
        (t (cons (first list)
                 (my-remove expr (rest list))))))
;; test-list-for-item
(defun test-list-for-item (expr list)
  (cond ((endp list) nil)
        ((equal (first list) expr)
         (cons t (test-list-for-item expr (rest list))))
        (t (cons nil
                 (test-list-for-item expr (rest list))))))

;;remove-first
(defun remove-first( expr list)
  (cond ((endp list) nil)
        ((equal (first list) expr) (rest list))
        (t (cons (first list)
           (remove-first expr (rest list))))))
         


;;
(defun my-replace (expr1 expr2 list)
  (cond ((endp list) nil)
        ((equal (first list) expr1)
         (cons expr2
               (my-replace expr1 expr2 (rest list))))
        (t
         (cons (first list)
               (my-replace expr1 expr2 (rest list))))))

