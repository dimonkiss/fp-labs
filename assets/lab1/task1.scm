;; =====================================================================
;; ЛР1, варіант 3, задача 3.1. Функція Аккермана A(m, n)
;;   A(0, n) = n + 1
;;   A(m, 0) = A(m-1, 1)                    при m > 0
;;   A(m, n) = A(m-1, A(m, n-1))            при m > 0, n > 0
;; Реалізації: 1) звичайна рекурсія; 2) хвостова рекурсія з акумулятором-стеком;
;; 3) ітеративний процес (константна глибина керуючого стеку).
;; Також: підрахунок викликів і глибини рекурсії та порівняння часу виконання.
;; Обмеження: без let / let* / set! / циклів — лише define, cond, if, lambda.
;; =====================================================================

;; ---------- 1. Звичайна рекурсія ----------
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

;; ---------- 2. Хвостова рекурсія ----------
;; Акумулятор stack зберігає «відкладені» значення m (замість стеку викликів).
(define (ack-tail m n stack)
  (cond ((= m 0)
         (if (null? stack)
             (+ n 1)
             (ack-tail (car stack) (+ n 1) (cdr stack))))
        ((= n 0) (ack-tail (- m 1) 1 stack))
        (else (ack-tail m (- n 1) (cons (- m 1) stack)))))

;; ---------- 3. Ітеративний процес ----------
;; Для m <= 3 використовуються замкнені формули (без жодної рекурсії):
;;   A(0,n)=n+1; A(1,n)=n+2; A(2,n)=2n+3; A(3,n)=2^(n+3)-3.
;; Для m >= 4 процес виконується хвостовим циклом із явним стеком.
(define (ack-iter m n)
  (cond ((= m 0) (+ n 1))
        ((= m 1) (+ n 2))
        ((= m 2) (+ (* 2 n) 3))
        ((= m 3) (- (expt 2 (+ n 3)) 3))
        (else (ack-tail m n '()))))

;; ---------- Статистика: кількість викликів і глибина рекурсії ----------
;; Результат — список (значення  кількість-викликів  максимальна-глибина)
(define (ack-stat m n d)
  (cond ((= m 0) (list (+ n 1) 1 d))
        ((= n 0) (add-call (ack-stat (- m 1) 1 (+ d 1))))
        (else (outer-step (ack-stat m (- n 1) (+ d 1)) m d))))
(define (add-call r) (list (car r) (+ 1 (cadr r)) (caddr r)))
(define (outer-step inner m d)
  (combine inner (ack-stat (- m 1) (car inner) (+ d 1))))
(define (combine inner outer)
  (list (car outer) (+ 1 (cadr inner) (cadr outer)) (max (caddr inner) (caddr outer))))

;; Те саме для хвостової версії: (значення  кількість-кроків  максимальна-довжина-стеку)
(define (ack-tail-stat m n stack len steps maxlen)
  (cond ((= m 0)
         (if (null? stack)
             (list (+ n 1) (+ steps 1) maxlen)
             (ack-tail-stat (car stack) (+ n 1) (cdr stack) (- len 1) (+ steps 1) maxlen)))
        ((= n 0) (ack-tail-stat (- m 1) 1 stack len (+ steps 1) maxlen))
        (else (ack-tail-stat m (- n 1) (cons (- m 1) stack) (+ len 1) (+ steps 1) (max maxlen (+ len 1))))))

;; ---------- Вимірювання часу ----------
;; Викликає (f m n) reps разів і повертає середній час у мілісекундах.
(define (time-of f m n reps)
  (/ (time-loop f m n reps (current-inexact-milliseconds)) reps))
(define (time-loop f m n reps start)
  (if (= reps 0)
      (- (current-inexact-milliseconds) start)
      (begin (f m n) (time-loop f m n (- reps 1) start))))
(define (ack-tail0 m n) (ack-tail m n '()))

;; ---------- Інтерфейс ----------
(define (show label value) (display label) (display value) (newline))

(define (report m n)
  (show "Вхідні дані: m = " m)
  (show "              n = " n)
  (newline)
  (report-stats m n (ack-stat m n 1) (ack-tail-stat m n '() 0 0 0)))

;; s = (значення викликів глибина) для звичайної рекурсії; t = (значення кроків довжина-стеку) для хвостової.
;; Статистика обчислюється один раз і передається параметрами (без повторних викликів).
(define (report-stats m n s t)
  (display "1) Звичайна рекурсія")
  (newline)
  (show "   A(m, n)               = " (car s))
  (show "   кількість викликів    = " (cadr s))
  (show "   глибина рекурсії      = " (caddr s))
  (display "2) Хвостова рекурсія (акумулятор — стек)")
  (newline)
  (show "   A(m, n)               = " (car t))
  (show "   кількість кроків      = " (cadr t))
  (display "   глибина керуючого стеку = 1 (усі виклики хвостові)")
  (newline)
  (show "   макс. довжина стеку-акумулятора = " (caddr t))
  (display "3) Ітеративний процес")
  (newline)
  (show "   A(m, n)               = " (ack-iter m n))
  (newline)
  (show "Перевірка збігу результатів: "
        (and (= (car s) (car t)) (= (car s) (ack-iter m n)) (= (car s) (ack m n)))))

(define (main)
  (display "Функція Аккермана. Введіть m: ")
  (ask-n (read)))
(define (ask-n m)
  (display "Введіть n: ")
  (report-after-input m (read)))
(define (report-after-input m n)
  (newline)
  (report m n))

;; Порівняння ефективності на великих значеннях
(define (compare m n reps)
  (display "A(") (display m) (display ", ") (display n) (display ")  результат = ")
  (display (ack-iter m n)) (newline)
  (show "   звичайна рекурсія:  час (мс) = " (time-of ack m n reps))
  (show "   хвостова рекурсія:  час (мс) = " (time-of ack-tail0 m n reps))
  (show "   ітеративний процес: час (мс) = " (time-of ack-iter m n reps)))
