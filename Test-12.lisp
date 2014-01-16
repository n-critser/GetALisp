;;;; Test-12.lisp




(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))


;(defmacro combine-results (&body form)
;  (with-gensyms (result)
;    `(let ((,result t))
;       ,@(form  `(unless ,form (setf ,result nil)))
;       ,result)))


(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))



(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(defun test-* ()
  (check
   (= (* 2 2) 4)
   (= (* 3 5) 15)))

(defun test-arithmetic ()
   (test-+)
   (test-*))
