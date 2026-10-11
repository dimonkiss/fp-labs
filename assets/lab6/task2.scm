;; =====================================================================
;; ЛР6, варіант 3, задача 3.2. Черга символів (FIFO).
;;  1) Користувач вводить символи по одному (кожен — в окремому рядку, порожній рядок —
;;     кінець введення). Після кожного введення виводиться стан черги: новий символ додається в кінець.
;;  2) Обробка вихідної черги: символ береться з голови; якщо це англійська літера, він
;;     додається в нову чергу, інакше пропускається. Після кожної операції показується, який символ
;;     вилучено, та стан обох черг.
;;  3) Після обробки виводяться підсумкові черги; нова черга перетворюється на список,
;;     впорядкований за алфавітом.
;; Чергу подано списком: голова черги — перший елемент, додавання — в кінець.
;; Бібліотек обробки черг не використовується.
;; =====================================================================

;; ---------- Операції черги ----------
(define (make-queue) '())
(define (queue-empty? q) (null? q))
(define (queue-head q) (car q))
(define (dequeue q) (cdr q))
(define (enqueue q x)                       ; додавання в кінець черги
  (if (null? q) (list x) (cons (car q) (enqueue (cdr q) x))))

;; ---------- Символи ----------
(define (english-letter? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))))

(define (lower c)                           ; нижній регістр для англійських літер
  (if (and (char>=? c #\A) (char<=? c #\Z))
      (integer->char (+ (char->integer c) 32))
      c))

;; ---------- Виведення черги ----------
(define (print-queue q)
  (display "[")
  (print-items q)
  (display "]"))
(define (print-items q)
  (cond ((null? q) #t)
        ((null? (cdr q)) (print-char (car q)))
        (else (print-char (car q)) (display ", ") (print-items (cdr q)))))
(define (print-char c) (display "'") (display c) (display "'"))

;; ---------- Введення ----------
;; Читає один рядок: повертає перший символ рядка або #f, якщо рядок порожній
(define (read-symbol)
  (read-symbol-from (read-char)))
(define (read-symbol-from c)
  (if (or (eof-object? c) (char=? c #\newline))
      #f
      (begin (skip-line (read-char)) c)))
(define (skip-line c)
  (if (or (eof-object? c) (char=? c #\newline))
      #t
      (skip-line (read-char))))

;; Заповнення вихідної черги
(define (fill-queue q)
  (display "Введіть символ (порожній рядок — завершити): ")
  (fill-with q (read-symbol)))
(define (fill-with q c)
  (if c
      (begin (display "   додано в кінець черги: ") (print-char c) (display ";  черга: ")
             (print-queue (enqueue q c)) (newline)
             (fill-queue (enqueue q c)))
      q))

;; ---------- Обробка ----------
(define (process src dst)
  (if (queue-empty? src)
      (list src dst)
      (step (queue-head src) (dequeue src) dst)))

(define (step c src dst)
  (display "Вилучено з голови: ") (print-char c)
  (if (english-letter? c)
      (begin (display " — англійська літера, додається в нову чергу") (newline)
             (show-state src (enqueue dst c))
             (process src (enqueue dst c)))
      (begin (display " — не англійська літера, пропускається") (newline)
             (show-state src dst)
             (process src dst))))

(define (show-state src dst)
  (display "   вихідна черга: ") (print-queue src) (newline)
  (display "   нова черга:    ") (print-queue dst) (newline))

;; ---------- Сортування за алфавітом ----------
;; «менше або дорівнює» за кодом нижнього регістру: символ ставиться перед рівними — сортування стабільне
(define (before? a b) (<= (char->integer (lower a)) (char->integer (lower b))))
(define (insert-sorted c sorted)
  (cond ((null? sorted) (list c))
        ((before? c (car sorted)) (cons c sorted))
        (else (cons (car sorted) (insert-sorted c (cdr sorted))))))
(define (sort-chars lst)
  (if (null? lst) '() (insert-sorted (car lst) (sort-chars (cdr lst)))))

;; ---------- Головна процедура ----------
(define (main)
  (display "=== Введення символів ===") (newline)
  (run-process (fill-queue (make-queue))))

(define (run-process src)
  (newline)
  (display "=== Обробка черги ===") (newline)
  (final (process src (make-queue))))

(define (final r)
  (newline)
  (display "=== Підсумок ===") (newline)
  (display "Вихідна черга: ") (print-queue (car r)) (newline)
  (display "Нова черга (англійські літери): ") (print-queue (cadr r)) (newline)
  (display "Список, впорядкований за алфавітом: ") (display (sort-chars (cadr r))) (newline))
