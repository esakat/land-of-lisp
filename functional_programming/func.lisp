(defun add-widget (database widget)
  (cons widget database))

(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
        (setf *database* (add-widget *database* (read)))
        (format t "The database contains the following: ~a~%" *database*)))