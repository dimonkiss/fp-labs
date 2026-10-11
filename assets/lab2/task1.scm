;; =====================================================================
;; ЛР2, варіант 3, задача 3.1. Табуляція функції
;;            | e^(-x) + e^(-2x),     0 <= x <= 2
;;     Y(x) = |
;;            | 1 / e^(-(x+5)),       x > 2
;; де експонента e^z обчислюється розвиненням у ряд Тейлора:
;;     e^z = 1 + z + z^2/2! + ... + z^k/k! + ...
;; Рекурентне співвідношення для членів ряду:  a_0 = 1,  a_(k+1) = a_k * z / (k + 1)
;; (степінь і факторіал не обчислюються заново — кожен член утворюється з попереднього).
;; Точність eps вводиться з клавіатури (10^-2 ... 10^-6): сума обривається, коли |a_k| < eps.
;; Тип рекурсії — ХВОСТОВА (акумулятори: поточний член, номер, сума).
;; Обмеження: без let / let* / set! / циклів.
;; =====================================================================

;; ---------- e^z за рядом Тейлора (хвостова рекурсія) ----------
(define (exp-taylor z eps)
  (exp-sum z eps 1.0 0 0.0))

;; term — поточний член a_k, k — його номер, sum — сума попередніх членів
(define (exp-sum z eps term k sum)
  (if (< (abs term) eps)
      sum
      (exp-sum z eps (/ (* term z) (+ k 1)) (+ k 1) (+ sum term))))

;; e^(-x), спосіб 1 (прямий): ряд зі знакозмінними членами, z = -x.
;; При x > 2 члени ряду досягають великих значень з різними знаками, і сума втрачає
;; значущі цифри (катастрофічне скорочення) — див. аналіз у звіті.
(define (exp-neg-alt x eps)
  (exp-taylor (- x) eps))

;; e^(-x), спосіб 2 (стійкий): e^(-x) = 1 / e^x, де e^x обчислюється рядом із членами
;; одного знака (при x >= 0) — скорочення немає, відносна похибка мала для всіх x.
(define (exp-neg-stable x eps)
  (/ 1 (exp-taylor x eps)))

;; Вибір способу: mode = 1 — прямий, mode = 2 — стійкий
(define (exp-neg x eps mode)
  (if (= mode 1) (exp-neg-alt x eps) (exp-neg-stable x eps)))

;; ---------- Розгалуження: функція Y ----------
;; Повертає число або символ 'undefined, якщо x поза областю визначення
(define (y-taylor x eps mode)
  (cond ((and (>= x 0) (<= x 2))
         (+ (exp-neg x eps mode) (exp-neg (* 2 x) eps mode)))
        ((> x 2)
         (/ 1 (exp-neg (+ x 5) eps mode)))
        (else 'undefined)))

;; Еталон — стандартна функція exp
(define (y-standard x)
  (cond ((and (>= x 0) (<= x 2)) (+ (exp (- x)) (exp (* -2 x))))
        ((> x 2) (/ 1 (exp (- (+ x 5)))))
        (else 'undefined)))

;; ---------- Округлення для виведення ----------
(define (round-to v digits)
  (/ (round (* v (expt 10 digits))) (expt 10 digits)))

;; ---------- Виведення одного рядка таблиці ----------
(define (print-row x eps mode)
  (print-row-value x (y-taylor x eps mode) (y-standard x)))

;; yt — значення за рядом Тейлора, ys — стандартне значення (обчислені один раз)
(define (print-row-value x yt ys)
  (if (eq? yt 'undefined)
      (begin (display (round-to x 6)) (display "\t")
             (display "Функція не визначена") (newline))
      (begin (display (round-to x 6)) (display "\t")
             (display (round-to yt 8)) (display "\t")
             (display (round-to ys 8)) (display "\t")
             (display (abs (- yt ys))) (newline))))

;; ---------- Рекурсивна табуляція ----------
;; i — номер кроку; x = x0 + i*step (так уникаємо накопичення похибки додавання)
(define (tabulate x0 x1 step eps mode i)
  (if (<= (+ x0 (* i step)) (+ x1 (/ step 1000)))
      (begin (print-row (+ x0 (* i step)) eps mode)
             (tabulate x0 x1 step eps mode (+ i 1)))))

(define (print-header)
  (display "аргумент\tрозклад Тейлора\tстандартне\tпохибка") (newline))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Початкове значення x0: ") (ask-x1 (read)))
(define (ask-x1 x0)
  (display "Кінцеве значення x1: ") (ask-step x0 (read)))
(define (ask-step x0 x1)
  (display "Крок: ") (ask-eps x0 x1 (read)))
(define (ask-eps x0 x1 step)
  (display "Точність eps (від 0.01 до 0.000001): ") (ask-mode x0 x1 step (read)))
(define (ask-mode x0 x1 step eps)
  (display "Спосіб e^(-x): 1 — прямий ряд, 2 — через 1/e^x: ") (run x0 x1 step eps (read)))
(define (run x0 x1 step eps mode)
  (newline)
  (print-header)
  (tabulate x0 x1 step eps mode 0))
