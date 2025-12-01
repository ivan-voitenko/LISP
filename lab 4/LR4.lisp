(defun add-next-fn (&key (transform #'identity))
  (let ((previous nil))
    (lambda (current)
      (if previous  
          (progn (rplacd previous (funcall transform current))
                 (setf previous (cons (cdr previous) nil)))
          (setf previous (cons (funcall transform current) nil))))))



(defun check-add-next-fn (name input expected &key (transform #'identity))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (mapcar (add-next-fn :transform transform) input) expected)
          name))


(defun test-add-next-fn ()
  (check-add-next-fn "Test 1" '(1 2 3) '((1 . 2) (2 . 3) (3 . NIL)))
  (check-add-next-fn "Test 2" '(1 2 3) '((2 . 3) (3 . 4) (4 . NIL)) :transform #'1+)
  (check-add-next-fn "Test 3" '() '())  
  (check-add-next-fn "Test 4" '(5) '((5 . NIL))))

