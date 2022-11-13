#lang racket/gui

(require plot
         "database.rkt")
(provide plot-weekly-stats)
(plot-new-window? #t)


(module+ test (require rackunit)
  (define mock-stats '(#("Personal" "0" 1)
                  #("Personal" "6" 1)
                  #("Study" "0" 3)
                  #("Study" "5" 1)
                  #("Study" "6" 2)
                  #("Work" "6" 1))))

(define (stats-board->hash stats board)
  (make-hash (map
                (lambda (v) (cons (vector-ref v 1) (vector-ref v 2)))
                (filter
                 (lambda (v) (equal? (vector-ref v 0) board))
                 stats))))
(module+ test
  (check-equal?
   (hash-keys (stats-board->hash mock-stats "Study")) '("6" "5" "0"))
  (check-equal?
   (hash-values (stats-board->hash mock-stats "Study")) '(3 1 2)))


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

(define (stats->boards stats)
  (set->list (list->set (map (lambda (v) (vector-ref v 0)) stats))))
(module+ test
  (check-equal? (stats->boards mock-stats) '("Work" "Personal" "Study")))

(define (plot-weekly-stats)
  (let* ([stats (weekly-stats)]
         [boards (stats->boards stats)])
    (if (not (empty? boards)) 
        (plot (map (lambda (board n)
                 (discrete-histogram
                  (weekly-board-stats stats board)
                  #:skip 5
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
