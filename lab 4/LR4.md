<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Войтенко Іван КВ-21</p>

<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
- використати функції вищого порядку для роботи з послідовностями (де це
доречно);
- додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини (варіант 3)
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг реалізації першої частини завдання
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
### Тестові набори та утиліти першої частини
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
### Тестування першої частини
```lisp
CL-USER> (test-sort-constructive)
passed... Test 1
passed... Test 2
passed... Test 3
passed... Test 4
passed... Test 5
NIL
```
## Варіант другої частини 3
Написати функцію add-next-fn , яка має один ключовий параметр — функцію
transform . add-next-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення наступного елемента списку.
Якщо функція transform передана, тоді значення поточного і наступного елементів, що
потраплять у результат, мають бути змінені згідно transform . transform має
виконатись мінімальну кількість разів.

```lisp
CL-USER> (mapcar (add-next-fn) '(1 2 3))
((1 . 2) (2 . 3) (3 . NIL))
CL-USER> (mapcar (add-next-fn :transform #'1+) '(1 2 3))
((2 . 3) (3 . 4) (4 . NIL))
```
## Лістинг реалізованої програми
```lisp
(defun add-next-fn (&key (transform #'identity))
  (let ((previous nil))
    (lambda (current)
      (if previous  
          (progn (rplacd previous (funcall transform current))
                 (setf previous (cons (cdr previous) nil)))
          (setf previous (cons (funcall transform current) nil))))))
```
### Тестові набори та утиліти
```lisp
(defun check-add-next-fn (name input expected &key (transform #'identity))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (mapcar (add-next-fn :transform transform) input) expected)
          name))


(defun test-add-next-fn ()
  (check-add-next-fn "Test 1" '(1 2 3) '((1 . 2) (2 . 3) (3 . NIL)))
  (check-add-next-fn "Test 2" '(1 2 3) '((2 . 3) (3 . 4) (4 . NIL)) :transform #'1+)
  (check-add-next-fn "Test 3" '() '())  
  (check-add-next-fn "Test 4" '(5) '((5 . NIL))))  
```
### Тестування
```lisp
CL-USER> (test-add-next-fn)
passed... Test 1
passed... Test 2
passed... Test 3
passed... Test 4
NIL
```



