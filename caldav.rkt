#lang racket

(require net/base64
         net/http-client
         net/url
         xml
         xml/path)

(module+ test (require rackunit))

(define user-principal-data "<d:propfind xmlns:d=\"DAV:\">
                 <d:prop>
                   <d:current-user-principal />
                 </d:prop>
               </d:propfind>")

(define (make-auth-header user pass)
  (string-trim (string-append
       "Authorization: Basic "
       (bytes->string/utf-8
        (base64-encode
         (string->bytes/utf-8
          (string-append user ":" pass)))))))
(module+ test
  (check-equal? (make-auth-header "demo" "p@55w0rd") "Authorization: Basic ZGVtbzpwQDU1dzByZA=="))


(define (get-path full-url)
  (let* ([parts (url-path (string->url full-url))]
         [parts-str (map path/param-path parts)])
    (string-append "/" (string-join parts-str "/"))))
(module+ test
  (check-equal? (get-path "https://www.mysite.com/my/long/path/") "/my/long/path/"))

(define (http-config server-url username password)
  (hash "ssl?" (if (equal? (url-scheme (string->url server-url)) "https") #t #f)
        "host" (url-host (string->url server-url))
        "path" (get-path server-url)
        "auth-header" (make-auth-header username password)))
(module+ test
  (let ([conf (http-config "https://www.mysite.com" "user" "pass")])
    (check-equal? (hash-ref conf "ssl?") #t)
    (check-equal? (hash-ref conf "host") "www.mysite.com")
    (check-pred string? (hash-ref conf "path"))
    (check-pred string? (hash-ref conf "auth-header"))))

(define (xml->current-user-href s)
  (se-path* '(d:current-user-principal d:href)
            (string->xexpr s)))
(module+ test
  (let ([s "<?xml version=\"1.0\"?>\n
<d:multistatus xmlns:d=\"DAV:\" xmlns:s=\"http://sabredav.org/ns\" xmlns:oc=\"http://owncloud.org/ns\" xmlns:nc=\"http://nextcloud.org/ns\">
<d:response><d:href>/remote.php/dav/</d:href><d:propstat>
<d:prop><d:current-user-principal><d:href>/remote.php/dav/principals/users/myuser/</d:href></d:current-user-principal></d:prop>
<d:status>HTTP/1.1 200 OK</d:status>
</d:propstat></d:response>
</d:multistatus>\n"])
    (check-equal? (xml->current-user-href s) "/remote.php/dav/principals/users/myuser/")))

(define (call-caldav! url username password method depth data)
  (let ([conf (http-config url username password)])
    (let-values ([(status header content)
                  (http-sendrecv (hash-ref conf "host")
                   (hash-ref conf "path")
                   #:ssl? (hash-ref conf "ssl?")
                   #:method method
                   #:headers (list "Content-Type: text/xml"
                                   (~a "Depth: " depth)
                                   (hash-ref conf "auth-header"))

                   #:data data)])
      (port->string content))))

(define (current-user-principal! url username password)
  (xml->current-user-href (call-caldav! url username password "PROPFIND" 0 user-principal-data)))
