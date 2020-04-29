(write-line "Максим Травинцев 401-И Вариант # 15")

; № 5
; Опредtлить функцию, которая увеличивает элементы исходного списка на единицу.
; [1, 2, 3, 4] => [2, 3, 4, 5]

(defun add_one (arr) 
    (mapcar #'(lambda (x) (+ x 1)) arr)
)

(write-line "# 5")
; TEST
(print (add_one '(1 2 3 4)))
(write-line "")
(write-line "-------------------")

; №7
; Определите функцию, удаляющую из исходного списка элементы с четными номерами.
; [1, 2, 3, 4] => [1, 3]

(defun delete_even (x)
	(if (cadr x)
	    (cons 
            (cadr x)
	        (delete_even (cddr x))
        )
    )
)

(write-line "# 7")
(print (delete_even '(0 1 2 3 4 5 6)))
(write-line "")
(write-line "-------------------")

; № 11
; Определите функцию, осуществляющую разделение исходного списка на два подсписка.
; В первый из них должно попасть указанное количество элементов с начала списка,
; во второй — оставшиеся элементы.
; [1, 2, 3, 4, 5, 6] 3 => ([1, 2, 3] [4, 5, 6])

(defun divide_by_n (list n)
    (if list
        (if (zerop n) (cons nil (cons list nil))
            (
                (lambda (elem result)
                    (cons
                        (cons elem (car result))
                        (cdr result)
                    )
                )
                (car list)
                (divide_by_n  (cdr list) (- n 1))
            )
        )
    )
)

(write-line "# 11")

; TEST 1
(print (divide_by_n '(1 2 3 4 5 6) 3))
; TEST 2
(print (divide_by_n '(1 2 3 4 5 6) 6))
; TEST 3
(print (divide_by_n '(1 2 3 4 5 6) 0))
; TEST 4
(print (divide_by_n '() 10))

(write-line "")
(write-line "-------------------")

; № 12
; Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.
(defun remove_repeating (lst)
    (
        (lambda (x y)    
            (cond
                ((null y)
                    lst
                )
                ((equal x (cadr lst))
                    (cons x (remove_repeating (cddr lst)))
                )
                (t
                    (cons x (remove_repeating y ))
                )
            )
        )  
        (car lst) 
        (cdr lst)
    )
)

(write-line "# 12")

; TEST 1
(print (remove_repeating '(1 2 3 4 5 6)))
; TEST 2
(print (remove_repeating '(1 1 2 2 3 3 4 4 5 5 6 6)))
; TEST 3
(print (remove_repeating '()))
; TEST 4
(print (remove_repeating '(1 1 1 2)))

(write-line "")
(write-line "-------------------")

; № 13
; Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.

(defun list_to_set(lst)
    (
        (lambda (x y)
            (cond
                ((null lst)
                    nil
                )
                ((member x y)
                    (list_to_set y)
                )
                (t
                    (cons x (list_to_set y))
                )
            )
        )
        (car lst)
        (cdr lst)
    )
)

(write-line "# 13")

; TEST 1
(print (list_to_set '(1 2 3 4 5 6)))
; TEST 2
(print (list_to_set '(1 1 2 2 3 3 4 4 5 5 6 6)))
; TEST 3
(print (list_to_set '()))
; TEST 4
(print (list_to_set '(1 1 1 2)))

(write-line "")
(write-line "-------------------")

; № 19
; Определите функцию (ЛУКОВИЦА n), строящую N-уровневый вложенный список,
; элементом которого на самом глубоком уровне является N.
; 3 => (((3)))

(defun recursion (a n)
    (cond
        ((zerop n)
            a
        )
        (t
            (cons (recursion a (- n 1)) nil)
        )
    )
)

(defun ЛУКОВИЦА (a)
    (recursion a a)
)

(write-line "# 19")

; TEST 1
(print(ЛУКОВИЦА 3))
; TEST 2
(print(ЛУКОВИЦА 2))
; TEST 3
(print(ЛУКОВИЦА 0))

(write-line "")
(write-line "-------------------")

; № 31
;  Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый
; элемент, входящий в оба списка х и у, в противном случае NIL

(defun includes (lst val)
    (cond
        ((null lst)
            nil
        )
        ((eq val (car lst))
            val
        )
        (t
            (includes (cdr lst) val)
        )
    )
)
    
(defun ПЕРВЫЙ-СОВПАДАЮЩИЙ (lst1 lst2)
    (cond
        ((null lst1)
            nil
        )
        ((includes lst2 (car lst1))
            (car lst1)
        )
        (t
            (ПЕРВЫЙ-СОВПАДАЮЩИЙ (cdr lst1) lst2)
        )
    )
)

(write-line "# 31")

; TEST 1
(print(ПЕРВЫЙ-СОВПАДАЮЩИЙ '(1 2 3) '(1 2 3)))
; TEST 2
(print(ПЕРВЫЙ-СОВПАДАЮЩИЙ '(1 2 3) '(4 5 6)))
; TEST 3
(print(ПЕРВЫЙ-СОВПАДАЮЩИЙ '() '()))

(write-line "")
(write-line "-------------------")

; № 47
; Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства
; символа.

(setf (get 'obj 'val1) 1)
(setf (get 'obj 'val2) 2)
(setf (get 'obj 'val3) 3)
(setf (get 'obj 'val4) 4)

(defun УДАЛИТЬ-ВСЕ-СВОЙСТВА (object) 
    (
        (lambda (keys) 
            (cond 
                ((null keys)
                    nil
                ) 
                (t
                    (remprop object (car keys))
                    (УДАЛИТЬ-ВСЕ-СВОЙСТВА object)
                )
            )
        )
        (symbol-plist object)
    ) 
)

(write-line "# 47")

(print (symbol-plist 'obj))

(УДАЛИТЬ-ВСЕ-СВОЙСТВА 'obj)

(print (symbol-plist 'obj))

(write-line "")
(write-line "-------------------")

; № 48
; Функция GET возвращает в качестве результата NIL в том случае, если у символа нет данного свойства,
; либо если значением этого свойства является NIL.
; Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в списке свойств.
; Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.

(setf (get 'obj 'val1) 1)
(setf (get 'obj 'val2) NIL)
(setf (get 'obj 'val3) 3)
(setf (get 'obj 'val4) 4)

(defun checkif (keys key) 
    (cond 
        ((null keys)
            nil
        ) 
        ((eq (car keys) key)
            t
        )
        (t
            (checkif (cddr keys) key)
        )
    )
)

(defun ИМЕЕТ-СВОЙСТВО (object key) 
    (checkif (symbol-plist object)  key) 
)

(write-line "# 48")

(print (symbol-plist 'obj))

(print (ИМЕЕТ-СВОЙСТВО 'obj 'val1))
(print (ИМЕЕТ-СВОЙСТВО 'obj 'val2))
(print (ИМЕЕТ-СВОЙСТВО 'obj 'val8))

(write-line "")
(write-line "-------------------")
