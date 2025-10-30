<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
Войтенко Іван КВ-21</p>

<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 3

1. Написати функцію group-pairs , яка групує послідовні пари елементів у списки:
```lisp
CL-USER> (group-pairs '(a b c d e f g))
((A B) (C D) (E F) (G))
```
2. Написати функцію list-set-union , яка визначає об'єднання двох множин,
заданих списками атомів:
```lisp
CL-USER> (list-set-union '(1 2 3) '(2 3 4))
(1 2 3 4) ; порядок може відрізнятись
```

## Лістинг функції group-pairs
```lisp
(defun group-pairs (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list (list (car lst))))
    (t (cons (list (car lst) (cadr lst))
             (group-pairs (cddr lst))))))
```
### Тестові набори та утиліти
```lisp
(defun check-group-pairs (name input  expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (group-pairs input) expected)
          name))

(defun test-group-pairs ()
  (check-group-pairs "Test 1.1" '(a b c d e f g) '((A B) (C D) (E F) (G)))
  (check-group-pairs "Test 1.2" '(1 2 3 4 5 6)  '((1 2) (3 4) (5 6))) 
  (check-group-pairs "Test 1.3" '(A) '((A)))
  (check-group-pairs "Test 1.4" () '()))             
```
### Тестування
```lisp
CL-USER> (test-group-pairs)
passed... Test 1.1
passed... Test 1.2
passed... Test 1.3
passed... Test 1.4
NIL
```
## Лістинг функції list-set-union
```lisp
(defun check-member (item lst)
  (cond
    ((null lst) nil)
    ((eql item (car lst)) t)
    (t (check-member item (cdr lst)))))

(defun list-set-union (list1 list2)
  (cond    
    ((null list1) list2)   
    ((check-member (car list1) list2)
     (list-set-union (cdr list1) list2))
    (t (cons (car list1) (list-set-union (cdr list1) list2)))))
```
### Тестові набори та утиліти
```lisp
(defun check-list-set-union (name list1 list2  expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-union list1 list2) expected)
          name))

(defun test-list-set-union ()
  (check-list-set-union "Test 2.1" '(1 2 3) '(2 3 4) '(1 2 3 4))
  (check-list-set-union "Test 2.2" '(1 2 3) nil '(1 2 3)) 
  (check-list-set-union "Test 2.3" '(a b c) '(a b c) '(a b c))
  (check-list-set-union "Test 2.4" () () ()))
```
### Тестування
```lisp
CL-USER> (test-list-set-union)
passed... Test 2.1
passed... Test 2.2
passed... Test 2.3
passed... Test 2.4
NIL
```

