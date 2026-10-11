;; =====================================================================
;; ЛР6, варіант 3, задача 3.1. Два вектори, заповнені псевдовипадковими цілими числами.
;; Знайти найменший серед елементів ПЕРШОГО вектора, які збігаються зі значеннями
;; елементів ДРУГОГО вектора. Вивести вектори, знайдений елемент та його індекс
;; (індекс у першому векторі, відлік з нуля; береться перше входження).
;; Використано дозволені процедури: vector, vector-ref, vector-length, list->vector.
;; Датчик випадкових чисел — власний лінійний конгруентний генератор.
;; =====================================================================

;; ---------- Псевдовипадкові числа ----------
(define (next-seed s) (remainder (+ (* s 1103515245) 12345) 2147483648))
(define (rand-value s max) (+ 1 (remainder (quotient s 65536) max)))   ; 1..max

;; Список із n псевдовипадкових чисел 1..max (seed — початковий стан)
(define (rand-list n seed max)
  (if (= n 0)
      '()
      (cons (rand-value (next-seed seed) max)
            (rand-list (- n 1) (next-seed seed) max))))

(define (random-vector n seed max)
  (list->vector (rand-list n seed max)))

;; ---------- Пошук ----------
;; Чи входить x у вектор v (перевірка індексів від i)
(define (vector-member? v x i)
  (cond ((= i (vector-length v)) #f)
        ((= (vector-ref v i) x) #t)
        (else (vector-member? v x (+ i 1)))))

;; Перебір першого вектора: best — найменший збіг (або #f), best-i — його індекс
;; Результат: (значення . індекс) або #f
(define (find-min v1 v2 i best best-i)
  (cond ((= i (vector-length v1))
         (if best (cons best best-i) #f))
        ((and (vector-member? v2 (vector-ref v1 i) 0)
              (or (not best) (< (vector-ref v1 i) best)))
         (find-min v1 v2 (+ i 1) (vector-ref v1 i) i))
        (else (find-min v1 v2 (+ i 1) best best-i))))

;; ---------- Виведення ----------
(define (print-vector v) (display v) (newline))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Довжина першого вектора: ") (ask-n2 (read)))
(define (ask-n2 n1)
  (display "Довжина другого вектора: ") (ask-max n1 (read)))
(define (ask-max n1 n2)
  (display "Максимальне значення елементів: ") (ask-seed n1 n2 (read)))
(define (ask-seed n1 n2 max)
  (display "Початкове число генератора (seed): ") (run n1 n2 max (read)))

(define (run n1 n2 max seed)
  (newline)
  (report (random-vector n1 seed max) (random-vector n2 (+ seed 1) max)))

(define (report v1 v2)
  (display "Вектор 1: ") (print-vector v1)
  (display "Вектор 2: ") (print-vector v2)
  (print-answer (find-min v1 v2 0 #f 0)))

(define (print-answer r)
  (if r
      (begin (display "Найменший елемент першого вектора, що є у другому: ") (display (car r)) (newline)
             (display "Його індекс у першому векторі: ") (display (cdr r)) (newline))
      (begin (display "Спільних елементів у векторах немає.") (newline))))
