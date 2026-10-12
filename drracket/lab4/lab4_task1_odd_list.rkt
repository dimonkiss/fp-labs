#lang r5rs
;; Файл для Dr.Racket: натисніть Run (F5) — програма сама запустить (main) і запитає дані.
;; =====================================================================
;; ЛР4, варіант 3, задача 3.1. Робота зі списком непарних натуральних чисел
;;  - згенерувати список перших k непарних чисел (1, 3, 5, ...) і вивести його;
;;  - додати в кінець списку m нових елементів (продовження послідовності непарних чисел);
;;  - інвертувати список із заданої позиції p до кінця; а також від початку до позиції p;
;;  - сформувати новий список із квадратів чисел вхідного списку.
;; Усі процедури — рекурсивні, без зміни стану (set!, let не використовуються).
;; =====================================================================

;; ---------- Базові процедури над списками ----------
(define (my-length lst)
  (if (null? lst) 0 (+ 1 (my-length (cdr lst)))))

(define (my-append a b)
  (if (null? a) b (cons (car a) (my-append (cdr a) b))))

;; Інверсія списку хвостовою рекурсією з акумулятором
(define (my-reverse lst)
  (reverse-acc lst '()))
(define (reverse-acc lst acc)
  (if (null? lst) acc (reverse-acc (cdr lst) (cons (car lst) acc))))

;; Перші p елементів і решта (позиції рахуються з 1)
(define (take lst p)
  (if (or (= p 0) (null? lst)) '() (cons (car lst) (take (cdr lst) (- p 1)))))
(define (drop lst p)
  (if (or (= p 0) (null? lst)) lst (drop (cdr lst) (- p 1))))

(define (last-element lst)
  (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

;; ---------- Генерація ----------
;; k непарних чисел, починаючи з first (first — непарне)
(define (odds-from first k)
  (if (= k 0) '() (cons first (odds-from (+ first 2) (- k 1)))))

(define (odd-list k) (odds-from 1 k))

;; ---------- Додавання елементів у кінець ----------
;; Нові елементи продовжують послідовність: після останнього x йдуть x+2, x+4, ...
(define (add-to-end lst m)
  (my-append lst (odds-from (if (null? lst) 1 (+ 2 (last-element lst))) m)))

;; ---------- Інверсія частини списку ----------
;; Від позиції p до кінця: (e1 ... e_(p-1)) + інверсія (e_p ... e_n)
(define (reverse-from lst p)
  (my-append (take lst (- p 1)) (my-reverse (drop lst (- p 1)))))

;; Від початку до позиції p: інверсія (e1 ... e_p) + (e_(p+1) ... e_n)
(define (reverse-to lst p)
  (my-append (my-reverse (take lst p)) (drop lst p)))

;; ---------- Квадрати ----------
(define (squares lst)
  (if (null? lst) '() (cons (* (car lst) (car lst)) (squares (cdr lst)))))

;; ---------- Інтерфейс ----------
(define (show label lst)
  (display label) (display lst) (newline))

(define (main)
  (display "Кількість непарних чисел k: ") (ask-m (read)))
(define (ask-m k)
  (display "Скільки елементів додати в кінець (m): ") (ask-p k (read)))
(define (ask-p k m)
  (display "Позиція p для інверсії (1..довжина списку): ") (run k m (read)))

(define (run k m p)
  (newline)
  (steps (odd-list k) m p))

(define (steps lst m p)
  (show "Початковий список:                    " lst)
  (steps2 (add-to-end lst m) m p))
(define (steps2 ext m p)
  (display "Додано елементів: ") (display m) (newline)
  (show "Список після додавання:               " ext)
  (display "Позиція p = ") (display p) (newline)
  (show "Інверсія від позиції p до кінця:      " (reverse-from ext p))
  (show "Інверсія від початку до позиції p:    " (reverse-to ext p))
  (show "Новий список — квадрати елементів:    " (squares ext)))


(main)
