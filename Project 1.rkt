#lang racket

; 1. Function "before?"
(define (before? date1 date2)
  (if (< (car date1) (car date2))
      #t
      (if (= (car date1) (car date2))
          (if (null? (cdr date1))
              #f
              (if (null? (cdr date2))
                  #f
                  (before? (cdr date1) (cdr date2))))
          #f)))

; 2. Function "number-in-month"
(define (number-in-month dates month)
  (if (null? dates)
      0(+ (if (= (cadr (car dates)) month)
           1 0)(number-in-month (cdr dates) month))))

; 3. Function "number in months"
(define (number-in-months dates months)
  (if (null? months)
      0(+ (number-in-month dates (car months))
         (number-in-months dates (cdr months)))))

; 4. Function "dates-in-month"
(define (dates-in-month dates month)
  (if (null? dates)
      '()
      (let ((date (car dates)))
        (if (= (cadr date) month)
            (cons date (dates-in-month (cdr dates) month))
            (dates-in-month (cdr dates) month)))))

; 5. Function "dates-in-months"
(define (dates-in-months dates months)
  (if (null? months)
      '()(append (dates-in-month dates (car months))
              (dates-in-months dates (cdr months)))))

; 6. Function "Get n-th"
(define (get-nth lst n)
  (if (= n 1)
      (car lst)(get-nth (cdr lst) (- n 1))))

; 7. Function "date->string"
(define months '( "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(define (date->string date)
  (let ((month (get-nth months (- (cadr date) 0))))
    (string-append month " " (number->string (car date)) ", " (number->string (caddr date)))))

; 8. Function "number-before-reaching-sum"
(define (number-before-reaching-sum sum lst)
  (if (null? lst)
      0(if (<= sum (car lst))
          1(+ 1 (number-before-reaching-sum (- sum (car lst)) (cdr lst))))))

; 9. Function "what-month"
(define (what-month day)
  (define month-lengths '(31 28 31 30 31 30 31 31 30 31 30 31))
  (number-before-reaching-sum day month-lengths))

; 10. Function "month-range"
(define (month-range day1 day2)
  (define month-lengths '(31 28 31 30 31 30 31 31 30 31 30 31))
  (if (> day1 day2)
      '()
      (cons (what-month day1) (month-range (+ day1 1) day2))))

; 11. Function earliest
(define (earliest dates)
  (if (null? (cdr dates))
      (car dates)
      (let ((date (car dates)))
        (if (before? date (earliest (cdr dates)))
            date(earliest (cdr dates))))))
