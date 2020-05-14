; № 1
; Определите FUNCALL через функционал APPLY

(defun funcapply (func &rest args) 
    (apply func args)
)

(write-line "")
(write-line "# 1")

; Test #1
(print (funcapply 'max 1 2 3))

; Test #2
(print (funcapply 'cons 1 '(2 3 4)))

(write-line "")
(write-line "-------------------")

; № 3
; Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка

(defun apl-apply (funcs args)
    (mapcar 'apply funcs args)
)

(write-line "")
(write-line "# 3")

; Test #1
(print (apl-apply '(max -) '((1 2 3) (10 5))))

; Test #2
(print (apl-apply '(* * *) '((1 2 3) (10 5) (100 0 10))))

(write-line "")
(write-line "-------------------")

; № 5
; Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда,
; являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.

(defun НЕКОТОРЫЙ (pred args)
	(not (null (mapcan pred args)))
)

(write-line "")
(write-line "# 5")

; Test #1
(print (НЕКОТОРЫЙ (lambda (x) (> x 2)) '(1 2 3 4)))

; Test #2
(print (НЕКОТОРЫЙ 'evenp `(1 3 5 2)))

; Test #3
(print (НЕКОТОРЫЙ 'evenp `(1 3 5)))

(write-line "")
(write-line "-------------------")

; № 7

; Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
; все элементы, которые не обладают свойством, наличие которого проверяет
; предикат пред.

(defun filter (pred list)
    (mapcan
        (lambda (x)
            (cond
                ((funcall pred x) (list x))
                (t nil)
            )
        )
        list
    )
)

(write-line "")
(write-line "# 7")

; Test #1
(print (filter (lambda (x) (> x 2)) '(1 2 3 4)))

; Test #2
(print (filter 'evenp `(1 3 5 2)))

; Test #3
(print (filter 'evenp `(1 3 5)))

(write-line "")
(write-line "-------------------")

; #11
; Определите фукнционал МНОГОФУН, который использует функции, являющиеся
; аргументами, по следующей схеме:
; (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).


(defun multifun (funcs args)
    (mapcar (lambda (f) (apply f args)) funcs)
)

(write-line "")
(write-line "# 11")

; Test #1
(print (multifun '(+ -) '(1 2 3 4 5)))

; Test #2
(print (multifun '(min max) '(1 2 3 4 5)))

(write-line "")
(write-line "-------------------")
