(defun make-cd(title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Improving the User Interaction
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; make some cd's with terminal input
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;; loop for input until exited
(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))(return))))

;;saving the database
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; load the database 
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; query database simple
(defun select-by-artist (artist)
  (remove-if-not
   #' (lambda (cd) (equal (getf cd :artist) artist))
      *db*))
;; query general
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; selector
(defun artist-selector (artist)
  #' (lambda (cd) (equal (getf cd :artist) artist)))

;; general query function generator
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      ;; logical and of the arguments
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
       