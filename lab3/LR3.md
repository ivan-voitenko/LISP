<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до роботи зі списками""<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Войтенко Іван КВ-21</p>

<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку.
Не допускається використання: псевдо-функцій, деструктивних операцій, циклів,
функцій вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).

Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон звіту
наведені в п. 3.2.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 3
  Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
CL-USER> (test-sort-constructive)
passed... Test 1
passed... Test 2
passed... Test 3
passed... Test 4
passed... Test 5
NIL
```
## Лістинг функції з використанням імперативного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
CL-USER> (test-sort-imperative)
passed... Test 1
passed... Test 2
passed... Test 3
passed... Test 4
passed... Test 5
NIL
```


