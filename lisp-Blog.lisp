;;;; lisp-Blog.lisp
;;;; A lisp program to spit out static html for a blogger on github.io

;;;; (closure-template:compile-cl-templates #P"~/Dev/Lisp/GetALisp/template-test.tmpl")
;;;; ( namespace:main '(plist keys and values))
;;;; (closure-hello.view:main '(:title "hello" :body "<h1>world</h1>"))
;;;; (hello.view:main)

;;;; (load "lisp-Blog.lisp")

(ql:quickload "closure-template")

(closure-template:compile-cl-templates #P"~/Dev/Lisp/GetALisp/template-test.tmpl")

(defvar *db* nil)

(defun push-post (post)
  (push post *db*))

;;;      (add-post "pid_0" "fuckedy-fuck" "<h1>wakawakawaka</h1>")
(defun add-post(post_id title body)
  (push-post (list post_id
                  (make-post title body))))

(defun dump-db ()
  (dolist (post *db*)
    (format t "~{~a:~10t~a~%~}~%" post)))

(defun make-post( title body)
  "make a template with title and body output as html"
   (blog.post:main
    (list :title title
          :body body)))

