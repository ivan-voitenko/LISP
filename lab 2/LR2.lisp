;Перше завдання: 
;Написати функцію group-pairs, яка групує послідовні пари елементів у списки

(defun group-pairs (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list (list (car lst))))
    (t (cons (list (car lst) (cadr lst))
             (group-pairs (cddr lst))))))

;Функції тестування
(defun check-group-pairs (name input  expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (group-pairs input) expected)
          name))

(defun test-group-pairs ()
  (check-group-pairs "Test 1.1" '(a b c d e f g) '((A B) (C D) (E F) (G)))
  (check-group-pairs "Test 1.2" '(1 2 3 4 5 6)  '((1 2) (3 4) (5 6))) 
  (check-group-pairs "Test 1.3" '(A) '((A)))
  (check-group-pairs "Test 1.4" () '()))               


;(test-group-pairs)



;Друге завдання
;Написати функцію list-set-union , яка визначає об'єднання двох множин,заданих списками атомів:

(defun check-member (item lst)
  (cond
    ((null lst) nil)
    ((eq item (car lst)) t)
    (t (check-member item (cdr lst)))))

(defun list-set-union (list1 list2)
  (cond    
    ((null list1) list2)   
    ((check-member (car list1) list2)
     (list-set-union (cdr list1) list2))
    (t (cons (car list1) (list-set-union (cdr list1) list2)))))

;Функції тестування
(defun check-list-set-union (name list1 list2  expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-union list1 list2) expected)
          name))

(defun test-list-set-union ()
  (check-list-set-union "Test 2.1" '(1 2 3) '(2 3 4) '(1 2 3 4))
  (check-list-set-union "Test 2.2" '(1 2 3) nil '(1 2 3)) 
  (check-list-set-union "Test 2.3" '(a b c) '(a b c) '(a b c))
  (check-list-set-union "Test 2.4" () () ()))
