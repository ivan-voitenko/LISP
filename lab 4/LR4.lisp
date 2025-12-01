(defun sort-constructive (list &key (key #'identity) (test #'<))
  (recursive-sort list key test))

(defun recursive-sort (list key test)
  (multiple-value-bind (new-sorted-list is-sorted)
      (sort-iteration list nil key test)
    (if is-sorted
        new-sorted-list
        (recursive-sort new-sorted-list key test))))

(defun sort-iteration (list swapped key test)
  (if (or (null list) (null (cdr list)))
      (values list (not swapped))
      (let* ((first (car list))
             (second (cadr list))
             (first-key (funcall key first))
             (second-key (funcall key second))
             (rest (cddr list)))
        (if (funcall test second-key first-key)
            (let ((new-list (cons second (cons first rest))))
              (multiple-value-bind (sorted-tail is-sorted)
                  (sort-iteration (cdr new-list) t key test)
                (values (cons (car new-list) sorted-tail) is-sorted)))
            (multiple-value-bind (sorted-tail is-sorted)
                (sort-iteration (cdr list) swapped key test)
              (values (cons first sorted-tail) is-sorted))))))


(defun check-sort-constructive (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-constructive input :key key :test test) expected)
          name))

(defun test-sort-constructive ()
  (check-sort-constructive "Test 1" '(10 8 6 4 2) '(2 4 6 8 10))
  (check-sort-constructive "Test 2" '(2 4 6 8 10) '(10 8 6 4 2) :test #'>)
  (check-sort-constructive "Test 3" '((3 . C) (1 . A) (2 . B)) '((1 . A) (2 . B) (3 . C)) :key #'car)
  (check-sort-constructive "Test 4" '("abc" "a" "ab") '("a" "ab" "abc") :key #'length)
  (check-sort-constructive "Test 5" '(-5 2 -3 1) '(1 2 -3 -5) :key #'abs))



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
