;;; Adapted by:
;;; Original Author: http://chaitanyagupta.com/lisp/restarts.html
;;; 


(define-condition csv-error (error)
  ((message
    :initarg :message
    :accessor csv-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor csv-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (line-number
    :initarg :line-number
    :accessor csv-error-line-number
    :initform nil
    :documentation "The line number of the row in for the error was signalled.")))


;; Do something more useful than the default printer behaviour
(defmethod print-object ((object csv-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (csv-error-line-number object)
            (csv-error-message object)
            (csv-error-value object))))


;; We use this function to signal our validation error
(defun csv-error (message &key value line-number)
  (error 'csv-error
         :message message
         :value value
         :line-number line-number))