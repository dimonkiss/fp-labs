#lang r5rs
;; Файл для Dr.Racket: натисніть Run (F5) — програма сама запустить (main) і запитає дані.
;; =====================================================================
;; ЛР3, варіант 3, задача 3.2. Визначений інтеграл функції f(x) = 1/(1 + x^2)
;; на відрізку [a, b] методами прямокутників (середніх) та трапецій.
;; Процедури інтегрування отримують функцію f, межі a, b та кількість інтервалів n
;; і повертають значення інтегралу. Підінтегральна функція задана lambda-виразом, крок h та
;; сума оформлені через let; цикли — хвостова рекурсія (set! не потрібне).
;; =====================================================================

;; ---------- Підінтегральна функція ----------
(define f (lambda (x) (/ 1 (+ 1 (* x x)))))

;; ---------- Метод прямокутників (середніх) ----------
;; I ~ h * sum_{i=0}^{n-1} f(a + (i + 1/2)*h),  h = (b - a)/n
(define (rectangles g a b n)
  (let ((h (/ (- b a) n)))
    (* h (sum-mid g a h n 0 0))))

(define (sum-mid g a h n i acc)
  (if (= i n)
      acc
      (sum-mid g a h n (+ i 1) (+ acc (g (+ a (* (+ i 0.5) h)))))))

;; ---------- Метод трапецій ----------
;; I ~ h * ( (f(a) + f(b))/2 + sum_{i=1}^{n-1} f(a + i*h) )
(define (trapezoids g a b n)
  (let ((h (/ (- b a) n)))
    (* h (+ (/ (+ (g a) (g b)) 2) (sum-inner g a h n 1 0)))))

(define (sum-inner g a h n i acc)
  (if (= i n)
      acc
      (sum-inner g a h n (+ i 1) (+ acc (g (+ a (* i h)))))))

;; ---------- Автоматичний добір n за точністю ----------
;; Подвоюємо n, доки |I_2n - I_n| >= eps (правило Рунге). Результат: (значення  n)
(define (auto method g a b n eps)
  (if (< (abs (- (method g a b (* 2 n)) (method g a b n))) eps)
      (list (method g a b (* 2 n)) (* 2 n))
      (auto method g a b (* 2 n) eps)))

;; ---------- «Точне» значення ----------
;; Метод трапецій із дуже малим кроком h = (b-a)/10^6; для контролю — аналітичний результат atan(b) - atan(a).
(define exact-n 1000000)
(define (exact-value g a b) (trapezoids g a b exact-n))
(define (analytic a b) (- (atan b) (atan a)))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Ліва межа a: ") (ask-b (exact->inexact (read))))
(define (ask-b a)
  (display "Права межа b: ") (ask-eps a (exact->inexact (read))))
(define (ask-eps a b)
  (display "Точність (0.001 ... 0.000001): ") (ask-n a b (read)))
(define (ask-n a b eps)
  (display "Кількість розбиттів n: ") (run a b eps (read)))

(define (run a b eps n)
  (newline)
  (report a b eps n (exact-value f a b)))

(define (report a b eps n ex)
  (report-values a b eps n ex (rectangles f a b n) (trapezoids f a b n)))

;; r, t — значення інтегралу методами прямокутників і трапецій для заданого n
(define (report-values a b eps n ex r t)
  (display "«Точне» значення (трапеції, n = 1000000) = ") (display ex) (newline)
  (display "Аналітичне значення atan(b) - atan(a)    = ") (display (analytic a b)) (newline)
  (newline)
  (display "--- Для заданого n = ") (display n) (display " ---") (newline)
  (print-method "прямокутники" r ex)
  (print-method "трапеції    " t ex)
  (newline)
  (display "--- Для заданої точності eps = ") (display eps) (display " (n добирається автоматично) ---") (newline)
  (print-auto "прямокутники" (auto rectangles f a b 1 eps) ex)
  (print-auto "трапеції    " (auto trapezoids f a b 1 eps) ex)
  (newline)
  (display "Висновок: точнішим є метод ")
  (display (if (< (abs (- r ex)) (abs (- t ex))) "прямокутників (середніх)" "трапецій"))
  (display " (за однакового n)") (newline))

(define (print-method name value ex)
  (display name) (display ": I = ") (display value)
  (display ",  похибка = ") (display (abs (- value ex))) (newline))

(define (print-auto name r ex)
  (display name) (display ": I = ") (display (car r))
  (display ",  n = ") (display (cadr r))
  (display ",  похибка = ") (display (abs (- (car r) ex))) (newline))


(main)
