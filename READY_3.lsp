; № 1
; Определить макрос, который возвращает свой вызов.

(defmacro selfCall (&rest x)
    `(quote (selfCall ,@x))
)

(write-line "")
(write-line "# 1")

; Test #1
(print (selfCall "hello world"))

(write-line "")
(write-line "-------------------")

; № 2
; Определите макрос (POP стек), который читает из стека верхний элемент и
; меняет значение переменной стека.
(defmacro pop2 (стек)
    `(let tail
        ; Разворачиваем список, берем первый элемент => последний в исходном
        (setq tail (car (reverse ,стек)))
        ; Разворачиваем список, берем все кроме первого, разворачиаем обратно => вытащили из исходного последний
        (setq ,стек (reverse (cdr (reverse ,стек))))
        tail
    )
)

(setq stack `(1 2 3 4 5))

(write-line "")
(write-line "# 2")

(print (pop2 stack))
(print stack)
(print (pop2 stack))
(print stack)
(print (pop2 stack))
(print stack)

(write-line "")
(write-line "-------------------")

; № 3
; Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro if2 (условие p q)
    `(cond
        (,условие ,p)
        (t ,q)
    )
)

(write-line "")
(write-line "# 3")

; Test #1
(print (if2 (> 1 2) "yep" "nope"))
; Test #2
(print (if2 (< 1 2) "yep" "nope"))

(write-line "")
(write-line "-------------------")

; № 4
; Определите в виде макроса форму (FIF тест отр нуль полож).

(defmacro fif (тест отр нуль полож)
    `(cond
        ((< ,тест 0) ,отр)
        ((> ,тест 0) ,полож)
        (t ,нуль)
    )
)

(write-line "")
(write-line "# 4")

; Test #1
(print (fif (- 1 2) "neg" "zero" "pos"))
; Test #2
(print (fif (+ 1 2) "neg" "zero" "pos"))
; Test #3
(print (fif (- 1 1) "neg" "zero" "pos"))

(write-line "")
(write-line "-------------------")


