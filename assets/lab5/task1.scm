;; =====================================================================
;; ЛР5, варіант 3, задача 3.1. Список раціональних чисел (дробів) a/b.
;; Дріб — пара (a . b) із b > 0. Бібліотечні функції НОД/НСК/abs не використовуються —
;; усі потрібні процедури написані нижче. Дії:
;;  1) два нових списки: дроби менші за заданий і дроби більші за заданий;
;;  2) приведення дробів до спільного знаменника та перевірка, чи утворюють вони
;;     арифметичну прогресію;
;;  3) пошук пари дробів, сума яких найближча до 1.
;; =====================================================================

;; ---------- Арифметика цілих ----------
(define (iabs x) (if (< x 0) (- x) x))
(define (gcd2 a b) (if (= b 0) (iabs a) (gcd2 b (remainder a b))))
(define (lcm2 a b) (quotient (* (iabs a) (iabs b)) (gcd2 a b)))

;; ---------- Дроби ----------
(define (num f) (car f))
(define (den f) (cdr f))
(define (make-frac a b) (cons a b))
(define (reduce f) (make-frac (quotient (num f) (gcd2 (num f) (den f))) (quotient (den f) (gcd2 (num f) (den f)))))

;; a1/b1 < a2/b2  <=>  a1*b2 < a2*b1  (знаменники додатні)
(define (frac<? f g) (< (* (num f) (den g)) (* (num g) (den f))))
(define (frac>? f g) (frac<? g f))

(define (frac+ f g)
  (make-frac (+ (* (num f) (den g)) (* (num g) (den f))) (* (den f) (den g))))

;; Відстань від суми до 1, у вигляді дробу |ad + cb - bd| / (bd)
(define (dist-to-one f g)
  (make-frac (iabs (- (+ (* (num f) (den g)) (* (num g) (den f))) (* (den f) (den g))))
             (* (den f) (den g))))

;; Вхідні дані: список пар (a b) → список дробів
(define (list->fracs lst)
  (if (null? lst) '() (cons (make-frac (car (car lst)) (cadr (car lst))) (list->fracs (cdr lst)))))

;; ---------- Виведення ----------
(define (frac->string f)
  (string-append (number->string (num f)) "/" (number->string (den f))))
(define (print-fracs lst)
  (cond ((null? lst) (display "(порожній список)"))
        ((null? (cdr lst)) (display (frac->string (car lst))))
        (else (display (frac->string (car lst))) (display "  ") (print-fracs (cdr lst)))))
(define (show label lst) (display label) (print-fracs lst) (newline))

;; ---------- Завдання 1: менші / більші за заданий дріб ----------
(define (select-less lst g)
  (cond ((null? lst) '())
        ((frac<? (car lst) g) (cons (car lst) (select-less (cdr lst) g)))
        (else (select-less (cdr lst) g))))
(define (select-greater lst g)
  (cond ((null? lst) '())
        ((frac>? (car lst) g) (cons (car lst) (select-greater (cdr lst) g)))
        (else (select-greater (cdr lst) g))))

;; ---------- Завдання 2: спільний знаменник, арифметична прогресія ----------
(define (common-denominator lst)
  (if (null? lst) 1 (lcm2 (den (car lst)) (common-denominator (cdr lst)))))

(define (to-denominator lst d)
  (if (null? lst)
      '()
      (cons (make-frac (* (num (car lst)) (quotient d (den (car lst)))) d)
            (to-denominator (cdr lst) d))))

;; Чисельники однакового знаменника утворюють арифметичну прогресію, якщо різниці сусідніх рівні
(define (arithmetic? lst)
  (or (null? lst) (null? (cdr lst)) (null? (cddr lst))
      (and (= (- (num (cadr lst)) (num (car lst))) (- (num (caddr lst)) (num (cadr lst))))
           (arithmetic? (cdr lst)))))

;; Сортування вставками (щоб перевірити прогресію й у впорядкованому вигляді)
(define (insert-sorted f sorted)
  (cond ((null? sorted) (list f))
        ((frac<? f (car sorted)) (cons f sorted))
        (else (cons (car sorted) (insert-sorted f (cdr sorted))))))
(define (sort-fracs lst)
  (if (null? lst) '() (insert-sorted (car lst) (sort-fracs (cdr lst)))))

;; ---------- Завдання 3: пара із сумою, найближчою до 1 ----------
;; Результат — список (f g відстань) або '() , якщо дробів менше двох
(define (better a b) (cond ((null? a) b) ((null? b) a) ((frac<? (caddr b) (caddr a)) b) (else a)))
(define (best-with f lst)
  (if (null? lst)
      '()
      (better (list f (car lst) (dist-to-one f (car lst))) (best-with f (cdr lst)))))
(define (best-pair lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (better (best-with (car lst) (cdr lst)) (best-pair (cdr lst)))))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Список дробів у вигляді ((a b) (c d) ...): ") (ask-given (read)))
(define (ask-given lst)
  (display "Заданий дріб (a b): ") (run (list->fracs lst) (read)))

(define (run fracs g)
  (newline)
  (show "Вихідний список:            " fracs)
  (part1 fracs (make-frac (car g) (cadr g)))
  (part2 fracs)
  (part3 fracs))

(define (part1 fracs g)
  (newline)
  (display "1) Заданий дріб ") (display (frac->string g)) (newline)
  (show "   менші за заданий:       " (select-less fracs g))
  (show "   більші за заданий:      " (select-greater fracs g)))

(define (part2 fracs)
  (newline)
  (display "2) Спільний знаменник = ") (display (common-denominator fracs)) (newline)
  (part2b (to-denominator fracs (common-denominator fracs))))
(define (part2b common)
  (show "   дроби зі спільним знаменником: " common)
  (display "   арифметична прогресія (у заданому порядку): ")
  (display (if (arithmetic? common) "так" "ні")) (newline)
  (display "   арифметична прогресія (після впорядкування): ")
  (display (if (arithmetic? (sort-fracs common)) "так" "ні")) (newline))

(define (part3 fracs)
  (newline)
  (display "3) Пара дробів із сумою, найближчою до 1: ")
  (print-pair (best-pair fracs)))
(define (print-pair r)
  (if (null? r)
      (begin (display "недостатньо дробів") (newline))
      (begin (display (frac->string (car r))) (display " + ") (display (frac->string (cadr r)))
             (display " = ") (display (frac->string (reduce (frac+ (car r) (cadr r)))))
             (display ",  відстань до 1: ") (display (frac->string (reduce (caddr r))))
             (newline))))
