#lang r5rs
;; Файл для Dr.Racket: натисніть Run (F5) — програма сама запустить (main) і запитає дані.
;; =====================================================================
;; ЛР4, варіант 3, задача 3.2. Модель усного іспиту в режимі «реального часу».
;; Допущені до іспиту студенти утворюють список (ідентифікатори — порядкові номери).
;; Екзаменатор викликає перших двох студентів зі списку. Поки перший відповідає
;; екзаменатору (задаються 2 запитання), другий готує письмову відповідь; потім вони
;; міняються. Спілкування з парою триває m хвилин, після чого викликаються наступні двоє.
;; Хто не склав — потрапляє до списку на перездачу.
;;
;; Час моделюється змінною clock (хвилини від початку іспиту, початок о 09:00).
;; Оцінки студентів — псевдовипадкові (лінійний конгруентний генератор), початкове
;; значення seed вводиться з клавіатури, тому прогін відтворюваний.
;; Зміна стану не використовується: список, годинник, генератор та результати
;; передаються параметрами рекурсивної процедури.
;; =====================================================================

;; ---------- Список студентів: 1, 2, ..., n ----------
(define (students-from i n)
  (if (> i n) '() (cons i (students-from (+ i 1) n))))

;; ---------- Псевдовипадкові числа ----------
(define (next-seed s) (remainder (+ (* s 1103515245) 12345) 2147483648))
(define (score-of s) (remainder (quotient s 65536) 101))      ; оцінка 0..100
(define pass-threshold 60)

;; ---------- Допоміжне ----------
(define (my-append a b)
  (if (null? a) b (cons (car a) (my-append (cdr a) b))))

(define (pad2 x) (if (< x 10) (string-append "0" (number->string x)) (number->string x)))
(define (time->string clock)
  (string-append (pad2 (+ 9 (quotient clock 60))) ":" (pad2 (remainder clock 60))))

(define (say clock text)
  (display "[") (display (time->string clock)) (display "] ") (display text) (newline))

;; ---------- Відповідь одного студента ----------
;; Студент відповідає екзаменатору протягом dur хвилин, починаючи з моменту start.
;; Повертає список (склав?  новий-seed).
(define (examine id start dur seed)
  (say start (string-append "Студент №" (number->string id) " відповідає екзаменатору (2 запитання)"))
  (verdict id (+ start dur) (score-of (next-seed seed)) (next-seed seed)))

(define (verdict id clock score seed)
  (say clock (string-append "Студент №" (number->string id) ": оцінка " (number->string score)
                            (if (>= score pass-threshold) " — ЗАРАХОВАНО" " — НЕ ЗАРАХОВАНО")))
  (list (>= score pass-threshold) seed))

;; ---------- Внесення результату до списків ----------
;; Повертає (passed retake) після додавання id до відповідного списку
(define (add-result passed? id passed retake)
  (if passed?
      (list (my-append passed (list id)) retake)
      (list passed (my-append retake (list id)))))

;; ---------- Основна рекурсія ----------
;; queue — черга студентів; m — тривалість спілкування з парою (хв); clock — годинник;
;; passed, retake — накопичені списки; seed — стан генератора.
(define (run-exam queue m clock passed retake seed)
  (cond ((null? queue) (list passed retake clock))
        ((null? (cdr queue)) (run-single (car queue) m clock passed retake seed))
        (else (run-pair (car queue) (cadr queue) (cddr queue) m clock passed retake seed))))

;; Пара студентів a та b: спершу відповідає a (b готується), потім b
(define (run-pair a b rest m clock passed retake seed)
  (say clock (string-append "Екзаменатор викликає студентів №" (number->string a) " і №" (number->string b)))
  (say clock (string-append "Студент №" (number->string b) " готує письмову відповідь"))
  (pair-second a b rest m clock passed retake (examine a clock (quotient m 2) seed)))

(define (pair-second a b rest m clock passed retake res-a)
  (pair-finish a b rest m clock (add-result (car res-a) a passed retake)
               (examine b (+ clock (quotient m 2)) (quotient m 2) (cadr res-a))))

(define (pair-finish a b rest m clock lists res-b)
  (say (+ clock m) (string-append "Спілкування з парою (№" (number->string a) ", №" (number->string b) ") завершено"))
  (newline)
  (next-round rest m (+ clock m) (add-result (car res-b) b (car lists) (cadr lists)) (cadr res-b)))

(define (next-round rest m clock lists seed)
  (run-exam rest m clock (car lists) (cadr lists) seed))

;; Останній студент без пари (непарна кількість)
(define (run-single a m clock passed retake seed)
  (say clock (string-append "Екзаменатор викликає студента №" (number->string a) " (без пари)"))
  (single-finish a m clock passed retake (examine a clock (quotient m 2) seed)))

(define (single-finish a m clock passed retake res)
  (newline)
  (finish-single-lists (+ clock (quotient m 2)) (add-result (car res) a passed retake)))

(define (finish-single-lists clock lists)
  (list (car lists) (cadr lists) clock))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Кількість студентів, допущених до іспиту: ") (ask-m (read)))
(define (ask-m n)
  (display "Тривалість спілкування з парою m (хв, парне): ") (ask-seed n (read)))
(define (ask-seed n m)
  (display "Початкове число генератора (seed): ") (run n m (read)))

(define (run n m seed)
  (newline)
  (say 0 "Іспит розпочато")
  (newline)
  (print-result (run-exam (students-from 1 n) m 0 '() '() seed)))

(define (print-result r)
  (display "Іспит завершено о ") (display (time->string (caddr r))) (newline)
  (display "Склали іспит:      ") (display (car r)) (newline)
  (display "Призначена перездача: ") (display (cadr r)) (newline))


(main)
