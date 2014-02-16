;;; func-fun.lisp

(defun call ()
  (load "func-fun.lisp"))

(defun change-head (l1 l2)
  (cons (first l2) (rest l1)))

(defun last-element (li)
  (first (reverse li)))


(defun replace-third (item li)
  (cons (first li)
        (cons (first (rest li))
              (cons item (rest (rest (rest li)))))))

;;(defun shopping (li)
;;  (cons
;;   (first (first li))
;;   (list (first (first (rest li))))))

(defun shopping (li)
  (list
   (first (first li))
   (first (first (rest li)))))

(defun insert-behind (l1 l2)
  (reverse
   (cons l1 (reverse l2))))

(defun remove-last (li)
  (reverse (rest (reverse li))))

(defun replace-last (item li)
  (reverse
   (cons item
         (rest (reverse li)))))

(defun add-lengths (l1 l2)
  (+ (length l1) (length l2)))


(defun add-length-to-list (li)
  (append li (list (length li))))

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