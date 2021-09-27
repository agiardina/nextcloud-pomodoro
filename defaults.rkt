#lang racket 

(require framework/preferences)
(provide defaults)

(define defaults
  (hash
   'server "https://cloud.example.com"
   'user ""
   'pass ""))

(for ([(key value) (in-hash defaults)])
  (preferences:set-default key value string?))
