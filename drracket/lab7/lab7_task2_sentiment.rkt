#lang racket
;; Файл для Dr.Racket: натисніть Run (F5) — програма сама запустить (main) і запитає дані.
;; =====================================================================
;; ЛР7, варіант 3, задача 3.2*. Тональність (емоційне забарвлення) тексту.
;; Етапи:
;;  1) зчитування файлу в список рядків;
;;  2) токенізація (поділ на слова) і нормалізація (нижній регістр, пунктуація відкидається);
;;  3) видалення стоп-слів стандартною функцією filter;
;;  4) словник емоцій (слова з вагами);
;;  5) оцінка кожного слова з урахуванням підсилювачів («дуже», «надзвичайно», «сильно», «вкрай»)
;;     та заперечень («не», «ні»), накопичення балів і лічильника емоційних слів;
;;  6) нормалізація: тональність = сума балів / кількість емоційних слів;
;;  7) інтерпретація за порогами: > 0.2 — позитивний, < -0.2 — негативний, інакше нейтральний.
;; Мова: «The Racket Language» у Dr.Racket (потрібна стандартна filter) або Guile. Без let / set! / циклів.
;; =====================================================================

;; ---------- 1. Читання файлу ----------
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

;; ---------- 2. Токенізація і нормалізація ----------
;; Рядок → список слів (літери переводяться у нижній регістр, усе інше — роздільник)
(define (tokenize-chars chars word out)
  (cond ((null? chars) (reverse (flush word out)))
        ((char-alphabetic? (car chars))
         (tokenize-chars (cdr chars) (cons (char-downcase (car chars)) word) out))
        (else (tokenize-chars (cdr chars) '() (flush word out)))))
(define (flush word out)
  (if (null? word) out (cons (list->string (reverse word)) out)))
(define (tokenize line) (tokenize-chars (string->list line) '() '()))

;; Список рядків → єдиний список слів
(define (tokenize-lines lines)
  (if (null? lines) '() (append (tokenize (car lines)) (tokenize-lines (cdr lines)))))

;; ---------- 3. Стоп-слова ----------
;; Слова «не», «ні», «дуже» тощо навмисно НЕ є стоп-словами — вони потрібні для оцінки.
(define stop-words
  '("і" "й" "та" "в" "у" "на" "з" "із" "до" "що" "це" "як" "але" "проте" "я" "ми" "ви"
    "він" "вона" "воно" "вони" "то" "же" "ж" "а" "б" "би" "ось" "такі" "такий" "просто"))

(define (remove-stop-words words)
  (filter (lambda (w) (not (member w stop-words))) words))

;; ---------- 4. Словники ----------
(define emotion-dictionary
  '(("гарний" . 1) ("чудовий" . 2) ("чудова" . 2) ("цікавий" . 1) ("приємна" . 1) ("люблю" . 1)
    ("радий" . 1) ("прекрасний" . 2) ("добрий" . 1) ("чудові" . 2) ("цікава" . 1)
    ("поганий" . -1) ("жахливий" . -2) ("жахлива" . -2) ("нудний" . -1) ("слабкі" . -1)
    ("розчарований" . -2) ("погана" . -1) ("нудна" . -1) ("жахливі" . -2)))

(define amplifiers '(("дуже" . 1.5) ("надзвичайно" . 2) ("сильно" . 1.5) ("вкрай" . 2)))
(define negations '("не" "ні"))

;; Пошук у асоціативному списку: значення або #f
(define (lookup word table)
  (cond ((null? table) #f)
        ((string=? word (car (car table))) (cdr (car table)))
        (else (lookup word (cdr table)))))

;; ---------- 5. Оцінка слів ----------
;; Стан: mult — множник від підсилювачів, neg — -1 після заперечення (інакше 1),
;;       total — сума балів, count — кількість емоційних слів.
;; Підсилювач/заперечення діють на найближче наступне емоційне слово; будь-яке інше слово
;; скидає їх. Результат: список (total count).
(define (score-words words mult neg total count)
  (cond ((null? words) (list total count))
        ((lookup (car words) amplifiers)
         (score-words (cdr words) (* mult (lookup (car words) amplifiers)) neg total count))
        ((member (car words) negations)
         (score-words (cdr words) mult (* neg -1) total count))
        ((lookup (car words) emotion-dictionary)
         (score-words (cdr words) 1 1
                      (+ total (* mult neg (lookup (car words) emotion-dictionary)))
                      (+ count 1)))
        (else (score-words (cdr words) 1 1 total count))))

;; ---------- 6. Нормалізація ----------
(define (normalized-score r)
  (if (= (cadr r) 0) 0 (/ (car r) (cadr r))))

;; ---------- 7. Інтерпретація ----------
(define (classify score)
  (cond ((> score 0.2) "позитивний")
        ((< score -0.2) "негативний")
        (else "нейтральний")))

;; ---------- Інтерфейс ----------
(define (main)
  (display "Ім'я текстового файлу (у лапках): ")
  (run (read)))

(define (run filename)
  (newline)
  (analyse (load-lines filename)))

(define (analyse lines)
  (display "Прочитано рядків: ") (display (length lines)) (newline)
  (analyse2 (remove-stop-words (tokenize-lines lines))))

(define (analyse2 words)
  (display "Слів після видалення стоп-слів: ") (display (length words)) (newline)
  (display "Слова: ") (display words) (newline)
  (analyse3 (score-words words 1 1 0 0)))

(define (analyse3 r)
  (display "Сума балів: ") (display (car r)) (newline)
  (display "Емоційних слів: ") (display (cadr r)) (newline)
  (analyse4 (normalized-score r)))

(define (analyse4 score)
  (display "Нормалізована тональність: ") (display score) (newline)
  (display "Категорія: ") (display (classify score)) (newline))


(main)
