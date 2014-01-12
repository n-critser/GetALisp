;;;;hello-app.lisp

(ql:quickload "restas")

(restas:define-module #:hello-app
    (:use :cl :restas))

(in-package #:hello-app)

(define-route hello-app ("")
  "Hello fucking World of Lisp")

(start '#:hello-app :port 8080)