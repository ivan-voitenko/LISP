(defun sort-constructive (list)
  (recursive-sort list))

(defun recursive-sort (list)
  (multiple-value-bind (new-sorted-list is-sorted)
      (sort-iteration list nil)
    (if is-sorted
        new-sorted-list
        (recursive-sort new-sorted-list))))

(defun sort-iteration (list swapped)
  (if (or (null list) (null (cdr list)))
      (values list (not swapped))
      (let* ((first (car list))
             (second (cadr list))
             (rest (cddr list)))
        (if (> first second)
            (let ((new-list (cons second (cons first rest))))
              (multiple-value-bind (sorted-tail is-sorted)
                  (sort-iteration (cdr new-list) t)
                (values (cons (car new-list) sorted-tail) is-sorted)))
            (multiple-value-bind (sorted-tail is-sorted)
                (sort-iteration (cdr list) swapped)
              (values (cons first sorted-tail) is-sorted))))))



(defun check-sort-constructive (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-constructive input) expected)
          name))

(defun test-sort-constructive ()
  (check-sort-constructive "Test 1" '(2 4 6 8 10) '(2 4 6 8 10))
  (check-sort-constructive "Test 2" '(10 8 6 4 2) '(2 4 6 8 10))
  (check-sort-constructive "Test 3" '(10 2 8 4 6 6) '(2 4 6 6 8 10))
  (check-sort-constructive "Test 4" '(10 10 8 8 6) '(6 8 8 10 10))
  (check-sort-constructive "Test 5" '(10 -2 4 -6 8 -10) '(-10 -6 -2 4 8 10)))




(defun sort-imperative (list)
  (let ((copy (copy-list list))
        (limit (- (length list) 1))
        (last-swap 0))
    (loop while (> limit 0) do
          (setf last-swap 0)
          (loop for i from 0 below limit do
                (let ((current (nth i copy))
                      (next (nth (1+ i) copy)))
                  (when (> current next)
                    (rotatef (nth i copy) (nth (1+ i) copy))
                    (setf last-swap i))))
          (setf limit last-swap))
    copy))


(defun check-sort-imperative (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-imperative input) expected)
          name))

(defun test-sort-imperative ()
  (check-sort-imperative "Test 1" '(2 4 6 8 10) '(2 4 6 8 10))
  (check-sort-imperative "Test 2" '(10 8 6 4 2) '(2 4 6 8 10))
  (check-sort-imperative "Test 3" '(10 2 8 4 6 6) '(2 4 6 6 8 10))
  (check-sort-imperative "Test 4" '(10 10 8 8 6) '(6 8 8 10 10))
  (check-sort-imperative "Test 5" '(10 -2 4 -6 8 -10) '(-10 -6 -2 4 8 10)))
