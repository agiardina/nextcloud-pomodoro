#lang racket

(require db)
(provide add-to-logs
         weekly-stats)

(define conn (sqlite3-connect
              #:database "ncpomodoro.db"
              #:mode 'create))

(unless (table-exists? conn "logs")
  (query-exec conn
   "CREATE TABLE logs(
id INTEGER PRIMARY KEY,
board TEXT,
stack TEXT,
card TEXT,
timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)"))

(define (add-to-logs board stack card)
  (query-exec
   conn
   "INSERT INTO logs (board,stack,card) VALUES (?,?,?)" board stack card))

(define (weekly-stats)
  (query-rows conn "SELECT board,strftime('%w',timestamp) as day,count(*)
FROM logs 
WHERE timestamp >= DATE('now', 'weekday 1', '-7 days') 
GROUP BY board,day"))
