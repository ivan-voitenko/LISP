<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Войтенко Іван КВ-21</p>

<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 3
База даних: Виробництво дронів
Тип записів: Асоціативний список
## Лістинг реалізації завдання
```lisp
;Визначення кількості та типів колонок таблиць
(defparameter *manufacturer* '(:number :string))
(defparameter *drones* '(:number :string :string))

;Реалізація читання з csv і представлення у вигляді таблиці
(defun read-table-from-csv (path)
  (if (and path (stringp path))
      (with-open-file (stream path :if-does-not-exist :error)
        (loop with table = '()
              for line = (read-line stream nil nil)
              while line
              do (push (uiop:split-string (string-trim '(#\Return) line) :separator '(#\;)) table)
              finally (return (reverse table))))
      (error "Invalid path: ~A" path)))


;Реалізація перетворення таблиці на асоц. список
(defun table-to-alist (table)
  (mapcan (lambda (x) (list (reverse (pairlis (car table) x)))) (cdr table)))

;Реалізація прив'язки типів до елементів асоц. списку
(defun assign-types (elems types)
  (if (null elems)
      '()
      (let* ((key (intern (string-upcase (car (car elems))) :keyword))
             (value (case (car types)
                      (:number (read-from-string (cdr (car elems))))
                      (:string (cdr (car elems)))
                      (otherwise (error "Invalid keyword in conf-list: ~s.~%" (car types))))))
        (cons (cons key value)
              (assign-types (cdr elems) (cdr types))))))


;Реалізація функції select
(defun select (file-path column-config)
  "`column-config` must match the number of table columns.
    Accepts a list of keyword symbols, distinguishing only `:number` and `:string`; others are invalid."

  (let ((data-alist (table-to-alist (read-table-from-csv file-path))))
    (if (eql (length (car data-alist)) (length column-config))
        (let ((processed-alist (mapcan (lambda (row) (list (assign-types row column-config))) data-alist))
              (filtered-results '())
              (match-status t))
          (lambda (&rest filters)
            (if (evenp (length filters))
                (progn
                  (dolist (row processed-alist)
                    (loop for filter-args on filters by #'cddr
                          while (and filter-args match-status)
                          do
                          (unless (equal (cdr (assoc (first filter-args) row))
                                         (second filter-args))
                            (setf match-status nil)))
                    (if match-status
                        (push row filtered-results)
                        (setf match-status t)))
                  (reverse filtered-results))
                (error "Filters must have an even number of arguments.~% 
                        Each filter should consist of a keyword symbol followed by a value."))))
        
        (error "The length of `column-config` must match the number of columns in the table file.
                ~% Number of columns: ~a~% Length of `column-config`: ~a~%"
               (length (car data-alist)) (length column-config)))))

;Допоміжні функції для запису в файл
(defun is-empty-line (text)
  (if (string= text "")
      nil
      ";"))

(defun stringify (value)
  (if (stringp value)
      value
      (write-to-string value)))

;Реалізація запису в файл
(defun write-to-csv (path typed-alist)
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((header-line ""))
      (dolist (header (car typed-alist))
        (setf header-line (concatenate 'string header-line (is-empty-line header-line) (string-capitalize (string (car header))))))
      (format s "~a~%" header-line)
      
      (dolist (row typed-alist)
        (let ((data-line ""))
          (dolist (cell row)
            (setf data-line (concatenate 'string data-line (is-empty-line data-line) (stringify (cdr cell)))))
          (format s "~a~%" data-line))))))

;Реалізація перетворення асоц. списку в хеш таблицю
(defun alist-to-hash (alist)
  (let ((hash-tables '())
        (current-table nil))
    (dolist (entry alist)
      (setf current-table (make-hash-table :test 'eq))
      (dolist (key-value entry)
        (setf (gethash (car key-value) current-table) (cdr key-value)))
      (push current-table hash-tables))
    (reverse hash-tables)))

;Реалізація виводу асоц. списку
(defun print-alist (alist)
  (dolist (var (car alist))
    (format t "~20a " (string-capitalize (string (car var))))
    
    )
  (format t "~%")
  (dolist (var alist)
    (dolist (elem var)
      (format t "~20s " (cdr elem))
      )
    (format t "~%")
    )
  )

;Реалізація виводу асоц. списку
(defun print-hash-list (hash-list)
  (dolist (hash hash-list)
    (maphash (lambda (key value)
               (format t "~20a ~a~%" (string-capitalize (string key)) value))
             hash)
    (format t "---~%")))


```
### Тестові набори та утиліти
```lisp
(defun test-read-csv-and-print ()
  (let ((manufacturer-data (funcall (select "projects/lab5/Manufacturer.csv" *manufacturer*)))
        (filtered-by-manufacturer (funcall (select "projects/lab5/Drones.csv" *drones*) :manufacturer "DJI"))
        (filtered-by-drones (funcall (select "projects/lab5/Drones.csv" *drones*)  :dronename "Parrot Disco")))

    (format t "Results for manufacturer (all rows):~%")
    (print-alist manufacturer-data)
    (format t "~%")

    (format t "Results filtered by manufacturer 'DJI':~%")
    (print-alist filtered-by-manufacturer)
    (format t "~%")

    (format t "Results filtered by drones 'Parrot Disco' :~%")
    (print-alist filtered-by-drones)
    (format t "~%")))

(defun test-alist-to-hash-table ()
  (let* ((alist '(((:DRONEID . 1) (:DRONENAME . "DJI Mavic Air 2") (:MANUFACTURER . "DJI1"))
                  ((:DRONEID . 2) (:DRONENAME . "DJI Mavic Mini 2") (:MANUFACTURER . "DJI"))))
         (hash-list (alist-to-hash alist)))
    
    (format t "Converted Hash Tables:~%")
    (print-hash-list hash-list)))

(defun test-write-to-csv ()
  (let* ((input-file "projects/lab5/Drones.csv")
         (output-file "projects/lab5/Parrot_Drones.csv")
         (conf-list *drones*) 
         (selector (select input-file conf-list))
         (filtered-alist (funcall selector :manufacturer "Parrot")))
   
    (write-to-csv output-file filtered-alist)
    (format t "Data where :manufacturer 'Parrot' has been written to ~a~%" output-file))
    (format t "Data from Parrot_Drones.csv ~%")
    (print-alist (funcall (select "projects/lab5/Parrot_Drones.csv" *drones*))))

(defun test-all ()
  (test-read-csv-and-print)
  (test-alist-to-hash-table)
  (test-write-to-csv))

```
### Вміст тестових файлів

Manufacturer.csv
```
ID	Manufacturer
1	DJI
2	Parrot
3	Autel Robotics

```

Drones.csv
```
DRONEID	DRONENAME	        MANUFACTURER
1	    DJI Mavic Air 2	    DJI
2	    DJI Mavic Mini 2	DJI
3	    DJI Inspire 2	    DJI
4	    Parrot Anafi	    Parrot
5	    Parrot Bebop 2	    Parrot
6	    Parrot Disco	    Parrot
7	    Autel EVO II Pro	Autel Robotics
8   	Autel EVO Nano+	    Autel Robotics
9	    Autel X-Star 	    Autel Robotics

```

### Тестування
```lisp
CL-USER> (test-all)
Results for manufacturer (all rows):
Id                   Manufacturer         
1                    "DJI"                
2                    "Parrot"             
3                    "Autel Robotics"     

Results filtered by manufacturer 'DJI':
Droneid              Dronename            Manufacturer         
1                    "DJI Mavic Air 2"    "DJI"                
2                    "DJI Mavic Mini 2"   "DJI"                
3                    "DJI Inspire 2"      "DJI"                

Results filtered by drones 'Parrot Disco' :
Droneid              Dronename            Manufacturer         
6                    "Parrot Disco"       "Parrot"             

Converted Hash Tables:
Droneid              1
Dronename            DJI Mavic Air 2
Manufacturer         DJI1
---
Droneid              2
Dronename            DJI Mavic Mini 2
Manufacturer         DJI
---
Data where :manufacturer 'Parrot' has been written to projects/lab5/Parrot_Drones.csv
Data from Parrot_Drones.csv 
Droneid              Dronename            Manufacturer         
4                    "Parrot Anafi"       "Parrot"             
5                    "Parrot Bebop 2"     "Parrot"             
6                    "Parrot Disco"       "Parrot"             
NIL
```

