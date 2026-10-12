#lang r5rs
;; Файл для Dr.Racket: натисніть Run (F5) — програма сама запустить (main) і запитає дані.
;; =====================================================================
;; ЛР3, варіант 3, задача 3.1. Корінь нелінійного рівняння  x^2 = e^(-x)
;; на інтервалі [a, b] двома методами: бісекції та хорд.
;; Рівняння зводимо до вигляду f(x) = x^2 - e^(-x) = 0.
;; Використано форми lambda (функція рівняння) і let (локальні прив'язки c, h); ітерації —
;; хвостова рекурсія з параметрами, присвоєння set! не потрібне.
;; =====================================================================

;; ---------- Функція рівняння ----------
(define f (lambda (x) (- (* x x) (exp (- x)))))

;; ---------- Допоміжні ----------
(define (same-sign? u v) (> (* u v) 0))

(define (print-iter label i x fx)
  (display label) (display " ітерація ") (display i)
  (display ": x = ") (display x)
  (display ",  f(x) = ") (display fx) (newline))

;; ---------- Метод бісекції (поділу навпіл) ----------
;; Інтервал [a, b], f(a)*f(b) < 0. Середина c = (a+b)/2; залишаємо половину зі зміною знаку.
;; Зупинка: довжина інтервалу (b - a)/2 < eps. Результат — список (корінь  кількість-ітерацій).
(define (bisection a b eps i show?)
  (let ((c (/ (+ a b) 2)))
    (cond (show? (print-iter "бісекція" i c (f c))))
    (cond ((or (= (f c) 0) (< (/ (- b a) 2) eps)) (list c i))
          ((same-sign? (f a) (f c)) (bisection c b eps (+ i 1) show?))
          (else (bisection a c eps (+ i 1) show?)))))

;; ---------- Метод хорд ----------
;; Хорда через точки (a, f(a)) і (b, f(b)) перетинає вісь x у точці
;;    c = a - f(a) * (b - a) / (f(b) - f(a)).
;; Новий інтервал — той кінець, де знак f збігається зі знаком f(c), замінюється на c.
;; Зупинка: |c - c_prev| < eps або f(c) = 0.
(define (chord a b eps i show? c-prev)
  (let ((c (chord-point a b)))
    (cond (show? (print-iter "хорди  " i c (f c))))
    (cond ((or (= (f c) 0) (< (abs (- c c-prev)) eps)) (list c i))
          ((same-sign? (f a) (f c)) (chord c b eps (+ i 1) show? c))
          (else (chord a c eps (+ i 1) show? c)))))

(define (chord-point a b)
  (- a (/ (* (f a) (- b a)) (- (f b) (f a)))))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Ліва межа a: ") (ask-b (exact->inexact (read))))
(define (ask-b a)
  (display "Права межа b: ") (ask-eps a (exact->inexact (read))))
(define (ask-eps a b)
  (display "Точність (0.001 ... 0.000001): ") (ask-show a b (read)))
(define (ask-show a b eps)
  (display "Показувати проміжні ітерації? (1 — так, 0 — ні): ")
  (run a b eps (= (read) 1)))

(define (run a b eps show?)
  (newline)
  (if (same-sign? (f a) (f b))
      (begin (display "На інтервалі [") (display a) (display ", ") (display b)
             (display "] кореня немає (f(a) і f(b) мають однаковий знак).") (newline))
      (compare a b eps show?)))

(define (compare a b eps show?)
  (compare-results (bisection a b eps 1 show?) (chord a b eps 1 show? (+ b 1)) eps))

(define (compare-results rb rc eps)
  (newline)
  (display "Метод бісекції: x = ") (display (car rb))
  (display ",  f(x) = ") (display (f (car rb)))
  (display ",  ітерацій: ") (display (cadr rb)) (newline)
  (display "Метод хорд:    x = ") (display (car rc))
  (display ",  f(x) = ") (display (f (car rc)))
  (display ",  ітерацій: ") (display (cadr rc)) (newline)
  (display "Різниця коренів |x_біс - x_хорд| = ") (display (abs (- (car rb) (car rc)))) (newline)
  (display "Швидший метод (менше ітерацій): ")
  (display (cond ((< (cadr rc) (cadr rb)) "хорд")
                 ((< (cadr rb) (cadr rc)) "бісекції")
                 (else "однаково")))
  (newline))


(main)
