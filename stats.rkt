#lang racket/gui

(require plot
         "database.rkt"
         racket/date
         racket/list)
(provide plot-weekly-stats
         plot-yearly-stats)
(plot-new-window? #t)


(module+ test (require rackunit)
  (define mock-stats '(#("Personal" "0" 1)
                       #("Personal" "6" 1)
                       #("Study" "0" 3)
                       #("Study" "5" 1)
                       #("Study" "6" 2)
                       #("Work" "6" 1)))

  (define mock-yearly-stats
    '(#("Study" "2022-10" 13)
      #("Study" "2022-11" 3)
      #("Study" "2022-12" 12)
      #("Personal" "2022-13" 2)
      #("Study" "2022-13" 8))))

(define (stats-board->hash stats board)
  (make-immutable-hash (map
                (lambda (v) (cons (vector-ref v 1) (vector-ref v 2)))
                (filter
                 (lambda (v) (equal? (vector-ref v 0) board))
                 stats))))
(module+ test
  (check-equal? (stats-board->hash mock-stats "Study") (hash "6" 2 "5" 1 "0" 3)))


(define (weekly-board-stats stats board)
  (define week-hash (stats-board->hash stats board))
  (map (lambda (day-n day-label)
         (list day-label (hash-ref week-hash day-n 0)))
       '("1" "2" "3" "4" "5" "6" "0")
       '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
(module+ test
  (check-equal?
   '(("Mon" 0)
     ("Tue" 0)
     ("Wed" 0)
     ("Thu" 0)
     ("Fri" 1)
     ("Sat" 2)
     ("Sun" 3))
   (weekly-board-stats mock-stats "Study")))

(define (rotate lst n) (append (drop lst n) (take lst n)))
(module+ test
  (check-equal?
   (rotate '(1 2 3 4) 0) '(1 2 3 4))
  (check-equal?
   (rotate '(1 2 3 4) 2) '(3 4 1 2)))

(define (weekly-board-stats! stats board)
  (rotate (weekly-board-stats stats board) (date-week-day (current-date))))

(define (yearly-board-stats stats board)
  (map (lambda (y)
         (list  (string-trim (substring  (vector-ref y 1)  5) "0" #:left? #t #:right? #f) 
                (vector-ref y 2)))
       (filter (lambda (x) (equal? (vector-ref x 0) board)) stats)))
(module+ test
  (check-equal?
   '(("10" 13) ("11" 3) ("12" 12) ("13" 8))
   (yearly-board-stats mock-yearly-stats "Study")))

(define (stats->boards stats)
  (set->list (list->set (map (lambda (v) (vector-ref v 0)) stats))))
(module+ test
  (check-equal?  (list->set (stats->boards mock-stats))
                 (list->set '("Work" "Personal" "Study"))))

(define (plot-weekly-stats)
  (let* ([stats (weekly-stats)]
         [boards (stats->boards stats)])
    (if (not (empty? boards)) 
        (plot (map (lambda (board n)
                 (discrete-histogram
                  (weekly-board-stats! stats board)
                  #:skip (length boards)
                  #:color n
                  #:line-color n
                  #:x-min n
                  #:label (~a board "=" (apply + (map second (weekly-board-stats stats board))))))
               boards  (sequence->list (range 1 (+ 1 (length boards)))))
          #:x-label "Day" #:y-label "Number of daily pomodoro"
          #:title "Weekly Stats"
          #:y-max 20
          #:width 800)
        (message-box "Weekly Stats"
                     "There are no saved sessions yet"
                     #f
                     '(ok no-icon)))))

(define (plot-yearly-stats)
  (let* ([stats (yearly-stats)]
         [boards (stats->boards stats)])
    (if (not (empty? boards)) 
        (plot (map (lambda (board n)
                 (discrete-histogram
                  (yearly-board-stats stats board)
                  #:skip (length boards)
                  #:color n
                  #:line-color n
                  #:x-min n))
               boards  (sequence->list (range 1 (+ 1 (length boards)))))
          #:x-label "Week" #:y-label "Pomodoros for week"
          #:title "Yearly Stats"
          #:y-max 50
          #:width 800)
        (message-box "Yearly Stats"
                     "There are no saved sessions yet"
                     #f
                     '(ok no-icon)))))
