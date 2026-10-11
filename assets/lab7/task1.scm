;; =====================================================================
;; ЛР7, варіант 3, задача 3.1. Робота з текстовими файлами.
;;  1) Програма генерує n рядків тексту й записує їх у файл text_input.txt;
;;  2) рядки зчитуються з файлу та виводяться на екран;
;;  3) у кожному реченні всі входження заданого слова замінюються на нове слово;
;;  4) результат записується в новий файл text_output.txt.
;; Слово — максимальна послідовність літер (знаки пунктуації та пробіли його не входять),
;; порівняння слів не залежить від регістру. Використано стандартні R5RS-процедури
;; введення-виведення: call-with-output-file, call-with-input-file, read-char, display.
;; Мова: R5RS (Dr.Racket / Guile). Без let / set! / циклів.
;; =====================================================================

;; ---------- Генерація тексту ----------
(define subjects '("Студент" "Програміст" "Викладач" "Дослідник" "Інженер"))
(define verbs    '("вивчає" "описує" "перевіряє" "використовує" "створює"))
(define objects  '("функціональне програмування" "рекурсивні процедури" "списки та вектори"
                   "символьні обчислення" "чисельні методи"))
(define endings  '("щодня" "з цікавістю" "у лабораторії" "разом із командою" "вдома"))

(define (next-seed s) (remainder (+ (* s 1103515245) 12345) 2147483648))
(define (pick lst seed) (list-ref lst (remainder (quotient seed 65536) (length lst))))

;; Один рядок: «Підмет дієслово додаток закінчення.»
;; Кожен компонент вибирається зі свого списку наступним значенням генератора.
(define (make-line seed)
  (string-append (pick subjects (next-seed seed)) " "
                 (pick verbs (next-seed (next-seed seed))) " "
                 (pick objects (next-seed (next-seed (next-seed seed)))) " "
                 (pick endings (next-seed (next-seed (next-seed (next-seed seed))))) "."))

;; Список із n рядків
(define (make-lines n seed)
  (if (= n 0)
      '()
      (cons (make-line seed) (make-lines (- n 1) (next-seed (next-seed (next-seed (next-seed (next-seed seed)))))))))

;; ---------- Запис рядків у файл ----------
(define (write-lines lines port)
  (if (null? lines)
      #t
      (begin (display (car lines) port) (newline port) (write-lines (cdr lines) port))))

(define (save-lines lines filename)
  (call-with-output-file filename (lambda (port) (write-lines lines port))))

;; ---------- Читання рядків із файлу ----------
;; Повертає список рядків; рядок накопичується як зворотний список символів
(define (read-lines port)
  (read-lines-from port (read-char port) '() '()))

(define (read-lines-from port c current acc)
  (cond ((eof-object? c)
         (reverse (if (null? current) acc (cons (list->string (reverse current)) acc))))
        ((char=? c #\newline)
         (read-lines-from port (read-char port) '() (cons (list->string (reverse current)) acc)))
        (else (read-lines-from port (read-char port) (cons c current) acc))))

(define (load-lines filename)
  (call-with-input-file filename read-lines))

;; ---------- Заміна слова ----------
;; replace-in-chars проходить по символах рядка: літери накопичуються у слові word;
;; на першому нелітерному символі слово порівнюється із цільовим і дописується в результат.
(define (same-word? chars target)
  (string=? (string-downcase (list->string chars)) (string-downcase target)))

(define (flush-word word target replacement out)
  ;; word і out — зворотні списки символів; повертає новий out
  (cond ((null? word) out)
        ((same-word? (reverse word) target) (append (reverse (string->list replacement)) out))
        (else (append word out))))

(define (replace-in-chars chars target replacement word out)
  (cond ((null? chars) (reverse (flush-word word target replacement out)))
        ((char-alphabetic? (car chars))
         (replace-in-chars (cdr chars) target replacement (cons (car chars) word) out))
        (else
         (replace-in-chars (cdr chars) target replacement '()
                           (cons (car chars) (flush-word word target replacement out))))))

(define (replace-word line target replacement)
  (list->string (replace-in-chars (string->list line) target replacement '() '())))

(define (replace-all lines target replacement)
  (if (null? lines)
      '()
      (cons (replace-word (car lines) target replacement)
            (replace-all (cdr lines) target replacement))))

;; ---------- Виведення на екран ----------
(define (print-lines lines i)
  (if (null? lines)
      #t
      (begin (display "  ") (display i) (display ". ") (display (car lines)) (newline)
             (print-lines (cdr lines) (+ i 1)))))

;; ---------- Інтерфейс ----------
(define input-file "text_input.txt")
(define output-file "text_output.txt")

(define (main)
  (display "Кількість рядків n: ") (ask-seed (read)))
(define (ask-seed n)
  (display "Початкове число генератора: ") (ask-word n (read)))
(define (ask-word n seed)
  (display "Слово, яке потрібно замінити (у лапках): ") (ask-new n seed (read)))
(define (ask-new n seed old)
  (display "Нове слово (у лапках): ") (run n seed old (read)))

(define (run n seed old new)
  (newline)
  (save-lines (make-lines n seed) input-file)
  (display "Файл ") (display input-file) (display " записано.") (newline)
  (display "Прочитано з файлу:") (newline)
  (process (load-lines input-file) old new))

(define (process lines old new)
  (print-lines lines 1)
  (newline)
  (display "Заміна слова «") (display old) (display "» на «") (display new) (display "»:") (newline)
  (finish (replace-all lines old new)))

(define (finish result)
  (print-lines result 1)
  (save-lines result output-file)
  (newline)
  (display "Результат записано у файл ") (display output-file) (newline)
  (display "Перевірка: файл містить ") (display (length (load-lines output-file))) (display " рядків.") (newline))
