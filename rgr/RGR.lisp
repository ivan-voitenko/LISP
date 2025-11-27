;Рекурсивна функція обрахунку F(i)
(defun calculate-f (i)
  (cond
    ((= i 1) 1.0)
    ((= i 11) 1.0)

    ((and (>= i 2) (<= i 10))
     (* (calculate-f (- i 1)) (log i)))

    ((and (>= i 12) (<= i 20))
     (* (calculate-f (- i 1)) (sqrt i)))
    
    (t (format t "The calculate-f function is not defined for the index: ~a~%" i))))


(defun check-calculate-f (name input expected)
(let ((result (calculate-f input))
      (epsilon 0.0001))
  (format t "~:[FAILED~;PASSED~]... ~a: Expected = ~a, Result = ~a~%~%"
          (< (abs (- result expected)) epsilon)
          name
          expected
          result)))

(defun test-calculate-f ()
 
  (check-calculate-f "Test1: i=1"   1 1.0)
  (check-calculate-f "Test2: i=2"   2 0.6931)
  (check-calculate-f "Test3: i=10" 10 62.3216)
  (check-calculate-f "Test4: i=11" 11 1.0)
  (check-calculate-f "Test5: i=12" 12 3.4641)
  (check-calculate-f "Test6: i=20" 20 246879.1704)
  (format t "Test7: i=-1: ")
  (calculate-f -1)
  (format t "~%Test8: i=-100: ")
  (calculate-f 100))

(test-calculate-f)
