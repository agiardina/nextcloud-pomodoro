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

(define calendar-home-set-data "<d:propfind xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">                                                           <d:prop>    
                   <c:calendar-home-set />                                        
                 </d:prop>
               </d:propfind>")

(define calendars-data "<d:propfind xmlns:d=\"DAV:\"  xmlns:cal=\"urn:ietf:params:xml:ns:caldav\">
                 <d:prop>
                   <d:displayname/>
                   <d:current-user-privilege-set/>
                   <cal:supported-calendar-component-set/>
                 </d:prop>
               </d:propfind>")

(define todos-data "<c:calendar-query xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">
    <d:prop>
        <d:getetag />
        <c:calendar-data>
          <c:comp name=\"VTODO\">
            <c:prop name=\"SUMMARY\"/>
          </c:comp>
        </c:calendar-data>
    </d:prop>
    <c:filter>
        <c:comp-filter name=\"VCALENDAR\">
            <c:comp-filter name=\"VTODO\" />
        </c:comp-filter>
    </c:filter>
</c:calendar-query>")

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

(define (xml->user-calendar-home-href s)
  (se-path* '(cal:calendar-home-set d:href)
            (string->xexpr s)))
(module+ test
  (let ([s "<?xml version=\"1.0\"?>\n<d:multistatus xmlns:d=\"DAV:\" xmlns:s=\"http://sabredav.org/ns\" xmlns:cal=\"urn:ietf:params:xml:ns:caldav\" xmlns:cs=\"http://calendarserver.org/ns/\" xmlns:card=\"urn:ietf:params:xml:ns:carddav\" xmlns:oc=\"http://owncloud.org/ns\" xmlns:nc=\"http://nextcloud.org/ns\"><d:response><d:href>/remote.php/dav/principals/users/myuser/</d:href><d:propstat><d:prop><cal:calendar-home-set><d:href>/remote.php/dav/calendars/myuser/</d:href></cal:calendar-home-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>\n"])
    (check-equal? (xml->user-calendar-home-href s) "/remote.php/dav/calendars/myuser/")))

(define (absolute-url calling-url href)
  (let ([cu (string->url calling-url)]
        [hrefu (string->url href)])
    (if (url-scheme hrefu)
        href ;is already an absolute url
        (url->string (struct-copy url cu [path (url-path hrefu)])))))
(module+ test
  (check-equal? (absolute-url "http://www.myoldsite.com/d/e/f/" "https://www.mysite.com/a/b/c/")
                "https://www.mysite.com/a/b/c/")
  (check-equal? (absolute-url "http://www.mysite.com/a/b/c/" "/d/e/f/")
                "http://www.mysite.com/d/e/f/"))

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
  (absolute-url url (xml->current-user-href (call-caldav! url username password "PROPFIND" 0 user-principal-data))))

(define (calendar-home-set! url username password)
  (absolute-url url (xml->user-calendar-home-href (call-caldav! url username password "PROPFIND" 0 calendar-home-set-data))))

(define (writable? r)
  (if (member '(d:write ()) (se-path*/list '(d:privilege) r))
      #t
      #f))

(define (todos? r)
  (if (member '(cal:comp ((name "VTODO")))
              (se-path*/list '(cal:supported-calendar-component-set) r))
      #t
      #f))

(define (events? r)
  (if (member '(cal:comp ((name "VEVENT")))
              (se-path*/list '(cal:supported-calendar-component-set) r))
      #t
      #f))

(define (response->calendar r calling-url)
  (hash "url" (absolute-url calling-url (se-path* '(d:href) r))
        "name" (se-path* '(d:displayname) r)
        "writable?" (writable? r)
        "todos?" (todos? r)
        "events?" (events? r)))
(module+ test
  (let* ([response '(d:response 
                     ()
                     (d:href () "/remote.php/dav/calendars/demo/todo/")
                     (d:propstat
                      ()
                      (d:prop
                       ()
                       (d:displayname () "Todo")
                       (d:current-user-privilege-set
                        ()
                        (d:privilege () (d:write ()))
                        (d:privilege () (d:write-properties ()))
                        (d:privilege () (d:write-content ()))
                        (d:privilege () (d:unlock ()))
                        (d:privilege () (d:bind ()))
                        (d:privilege () (d:unbind ()))
                        (d:privilege () (d:write-acl ()))
                        (d:privilege () (d:read ()))
                        (d:privilege () (d:read-acl ()))
                        (d:privilege () (d:read-current-user-privilege-set ()))
                        (d:privilege () (cal:read-free-busy ())))
                       (cal:supported-calendar-component-set () (cal:comp ((name "VTODO")))))
                      (d:status () "HTTP/1.1 200 OK")))]
         [calendar (response->calendar response "http://www.mysite.com")])
    (check-equal? (hash-ref calendar "url") "http://www.mysite.com/remote.php/dav/calendars/demo/todo/")
    (check-equal? (hash-ref calendar "name") "Todo")
    (check-equal? (hash-ref calendar "writable?") #t)
    (check-equal? (hash-ref calendar "todos?") #t)
    (check-equal? (hash-ref calendar "events?") #f)))

(define (xml->calendars s calling-url)
  (let* ([res (se-path*/list '(d:multistatus) (string->xexpr s))]
         [res->calendar (lambda (r) (response->calendar r calling-url))]
         [all-calendars (map res->calendar res)])
    (filter (lambda (c) (if (hash-ref c "name")
                            #t
                            #f))
            all-calendars)))
(module+ test
  (let* ([s "<?xml version=\"1.0\"?>\n<d:multistatus xmlns:d=\"DAV:\" xmlns:s=\"http://sabredav.org/ns\" xmlns:cal=\"urn:ietf:params:xml:ns:caldav\" xmlns:cs=\"http://calendarserver.org/ns/\" xmlns:oc=\"http://owncloud.org/ns\" xmlns:nc=\"http://nextcloud.org/ns\"><d:response><d:href>/remote.php/dav/calendars/demo/</d:href><d:propstat><d:prop><d:current-user-privilege-set><d:privilege><d:write/></d:privilege><d:privilege><d:write-properties/></d:privilege><d:privilege><d:write-content/></d:privilege><d:privilege><d:unlock/></d:privilege><d:privilege><d:bind/></d:privilege><d:privilege><d:unbind/></d:privilege><d:privilege><d:write-acl/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege></d:current-user-privilege-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><d:displayname/><cal:supported-calendar-component-set/></d:prop><d:status>HTTP/1.1 404 Not Found</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/personal/</d:href><d:propstat><d:prop><d:displayname>Personal</d:displayname><d:current-user-privilege-set><d:privilege><d:write/></d:privilege><d:privilege><d:write-properties/></d:privilege><d:privilege><d:write-content/></d:privilege><d:privilege><d:unlock/></d:privilege><d:privilege><d:bind/></d:privilege><d:privilege><d:unbind/></d:privilege><d:privilege><d:write-acl/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege><d:privilege><cal:read-free-busy/></d:privilege></d:current-user-privilege-set><cal:supported-calendar-component-set><cal:comp name=\"VEVENT\"/></cal:supported-calendar-component-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/todo/</d:href><d:propstat><d:prop><d:displayname>Todo</d:displayname><d:current-user-privilege-set><d:privilege><d:write/></d:privilege><d:privilege><d:write-properties/></d:privilege><d:privilege><d:write-content/></d:privilege><d:privilege><d:unlock/></d:privilege><d:privilege><d:bind/></d:privilege><d:privilege><d:unbind/></d:privilege><d:privilege><d:write-acl/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege><d:privilege><cal:read-free-busy/></d:privilege></d:current-user-privilege-set><cal:supported-calendar-component-set><cal:comp name=\"VTODO\"/></cal:supported-calendar-component-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/inbox/</d:href><d:propstat><d:prop><d:current-user-privilege-set><d:privilege><cal:schedule-deliver/></d:privilege><d:privilege><cal:schedule-deliver-invite/></d:privilege><d:privilege><cal:schedule-deliver-reply/></d:privilege><d:privilege><cal:schedule-query-freebusy/></d:privilege><d:privilege><d:unbind/></d:privilege><d:privilege><d:write-properties/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege></d:current-user-privilege-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><d:displayname/><cal:supported-calendar-component-set/></d:prop><d:status>HTTP/1.1 404 Not Found</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/outbox/</d:href><d:propstat><d:prop><d:current-user-privilege-set><d:privilege><cal:schedule-send/></d:privilege><d:privilege><cal:schedule-send-invite/></d:privilege><d:privilege><cal:schedule-send-reply/></d:privilege><d:privilege><cal:schedule-send-freebusy/></d:privilege><d:privilege><cal:schedule-post-vevent/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege></d:current-user-privilege-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><d:displayname/><cal:supported-calendar-component-set/></d:prop><d:status>HTTP/1.1 404 Not Found</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/trashbin/</d:href><d:propstat><d:prop><d:current-user-privilege-set><d:privilege><d:all/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:write/></d:privilege><d:privilege><d:write-properties/></d:privilege><d:privilege><d:write-content/></d:privilege><d:privilege><d:unlock/></d:privilege><d:privilege><d:bind/></d:privilege><d:privilege><d:unbind/></d:privilege><d:privilege><d:write-acl/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege></d:current-user-privilege-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><d:displayname/><cal:supported-calendar-component-set/></d:prop><d:status>HTTP/1.1 404 Not Found</d:status></d:propstat></d:response><d:response><d:href>/remote.php/dav/calendars/demo/app-generated--deck--board-7/</d:href><d:propstat><d:prop><d:displayname>Deck: Personal</d:displayname><d:current-user-privilege-set><d:privilege><d:write-properties/></d:privilege><d:privilege><d:read/></d:privilege><d:privilege><d:read-acl/></d:privilege><d:privilege><d:read-current-user-privilege-set/></d:privilege><d:privilege><cal:read-free-busy/></d:privilege></d:current-user-privilege-set><cal:supported-calendar-component-set><cal:comp name=\"VTODO\"/></cal:supported-calendar-component-set></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>\n"]
         [calling-url "https://www.mysite.com"]
         [calendars (xml->calendars s calling-url)])
    (check-equal? (length calendars) 3)))

(define (filter-todo-calendars l)
  (filter (lambda (c) (hash-ref c "todos?")) l))
(module+ test
  (let ([calendars (list (hash "todos?" #t) (hash "todos?" #f) (hash "todos?" #t))])
    (check-equal? (length (filter-todo-calendars calendars)) 2)))

(define (calendars! url username password)
  (xml->calendars (call-caldav! url username password "PROPFIND" 1 calendars-data) url))

(define (todo-calendars! url username password)
  (filter-todo-calendars (calendars! url username password)))

(define (todo->summary todo)
  (let ([summary-lines 
          (filter (lambda (x) 
              (equal? (car x) "SUMMARY")) 
              (map (lambda (x) (regexp-split ":" x)) 
                  (regexp-split "\n" todo)))])
    (if (pair? summary-lines) 
        (string-join (cdar summary-lines) ":") 
        "")))

(define (todos->summaries todos)
  (filter 
    (lambda (x) (positive? (string-length x))) 
    (map todo->summary todos)))

(define (todos! url username password)
 (todos->summaries (se-path*/list 
      '(cal:calendar-data) 
      (string->xexpr (call-caldav! url username password "REPORT" 1 todos-data)))))