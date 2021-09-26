#lang racket 
(require framework/preferences)

(preferences:set-default 'server "https://cloud.example.com" string?)
(preferences:set-default 'user "" string?)
(preferences:set-default 'pass "" string?)
