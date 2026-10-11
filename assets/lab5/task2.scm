;; =====================================================================
;; ЛР5, варіант 3, задача 3.2. Список комплексних чисел в алгебраїчній формі a + ib.
;; Виведення списку; обчислення суми та добутку всіх чисел списку.
;; Комплексне число — пара (a . b): a — дійсна частина, b — уявна. Вбудовані комплексні
;; типи й бібліотеки не використовуються.
;;   (a + ib) + (c + id) = (a + c) + i(b + d)
;;   (a + ib) * (c + id) = (ac - bd) + i(ad + bc)
;; Нейтральні елементи: 0 + 0i для суми, 1 + 0i для добутку.
;; =====================================================================

(define (re z) (car z))
(define (im z) (cdr z))
(define (make-complex a b) (cons a b))

(define (c+ z w) (make-complex (+ (re z) (re w)) (+ (im z) (im w))))
(define (c* z w)
  (make-complex (- (* (re z) (re w)) (* (im z) (im w)))
                (+ (* (re z) (im w)) (* (im z) (re w)))))

;; Сума та добуток списку — рекурсивне згортання
(define (sum-list lst)
  (if (null? lst) (make-complex 0 0) (c+ (car lst) (sum-list (cdr lst)))))
(define (product-list lst)
  (if (null? lst) (make-complex 1 0) (c* (car lst) (product-list (cdr lst)))))

;; Вхідні дані: список пар (a b)
(define (list->complex lst)
  (if (null? lst)
      '()
      (cons (make-complex (car (car lst)) (cadr (car lst))) (list->complex (cdr lst)))))

;; ---------- Виведення ----------
;; Формат "a+bi" / "a-bi"; наприклад 3+2i, 1-4i, 0+0i
(define (complex->string z)
  (string-append (number->string (re z))
                 (if (< (im z) 0) "-" "+")
                 (number->string (if (< (im z) 0) (- (im z)) (im z)))
                 "i"))

(define (print-list lst)
  (cond ((null? lst) (display "(порожній список)"))
        ((null? (cdr lst)) (display (complex->string (car lst))))
        (else (display (complex->string (car lst))) (display ",  ") (print-list (cdr lst)))))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Список комплексних чисел у вигляді ((a b) (c d) ...): ")
  (run (list->complex (read))))

(define (run lst)
  (newline)
  (display "Список:  ") (print-list lst) (newline)
  (display "Сума:    ") (display (complex->string (sum-list lst))) (newline)
  (display "Добуток: ") (display (complex->string (product-list lst))) (newline))
