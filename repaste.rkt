#lang racket
(require irc
         racket/async-channel
         net/url
         net/head
         json
         html-parsing
         racket/serialize)

(define config (file->value "config.rkt"))
(define (config-value key)
  (hash-ref config key #f))

(define (filter-cr str)
  (string-replace str "\r" ""))

(define (call/check-status url port f)
  (define headers (purify-port port))
  (define match (regexp-match #px"HTTP/\\d\\.\\d (\\d{3})" headers))
  (when (not match)
    (raise-user-error (format "Can't read ~a: Unexpected HTTP headers: ~a"
                              url headers)))
  (when (not (= (string->number (second match)) 200))
    (raise-user-error (format "Couldn't read ~a: HTTP status ~a"
                              url (second match))))
  (f port))

(define (wrap url f)
  (lambda (port) (call/check-status url port f)))

(define (get url)
  (call/input-url (string->url url) get-impure-port (wrap url port->string)))

(define (get-xexp url)
  (call/input-url (string->url url) get-impure-port (wrap url html->xexp)))

(define (get-json url)
  (call/input-url (string->url url) get-impure-port (wrap url read-json)))

(define (post url data)
  (call/input-url (string->url url)
                  (lambda (u) (post-impure-port u data))
                  (wrap url port->string)))

(define jsexpr->bytes (compose string->bytes/utf-8 jsexpr->string))

(define compile-flags
  "g++ -std=c++17 -O2 -Wall -Wextra -pedantic -pthread main.cpp && ./a.out")
(define (post-to-coliru code)
  (define result-hash
    (post "http://coliru.stacked-crooked.com/share"
          (jsexpr->bytes
           (make-hash `((cmd . ,compile-flags)
                        (src . ,code))))))
  (string-append "http://coliru.stacked-crooked.com/a/"
                 (string-trim result-hash))) ; result-hash has an \n at the end

(define (match-url m) (first m))
(define (match-hash m) (second m))

(define (handle-simple-pastebin match raw-format)
  (define content (get (format raw-format (match-hash match))))
  (values (match-hash match) (post-to-coliru content)))

(define (make-simple-handler raw-format)
  (lambda (match)
    (handle-simple-pastebin match raw-format)))

(define handle-pastebin (make-simple-handler "http://pastebin.com/raw/~a"))
(define handle-fedora-paste (make-simple-handler
                             "https://paste.fedoraproject.org/paste/~a/raw"))
(define handle-hastebin (make-simple-handler "https://hastebin.com/raw/~a"))
(define handle-bpaste (make-simple-handler "https://bpaste.net/raw/~a"))
(define handle-paste-ee (make-simple-handler "https://paste.ee/r/~a/0"))
;; #Python paste? Really?
(define handle-pound-python (make-simple-handler
                             "https://paste.pound-python.org/raw/~a/"))
(define handle-dpaste (make-simple-handler "http://dpaste.com/~a.txt"))
(define handle-debian-paste (make-simple-handler
                             "http://paste.debian.net/plain/~a"))
(define handle-ptpb (make-simple-handler "https://ptpb.pw/~a"))

(define (handle-irccloud match)
  (define content (get (format "https://www.irccloud.com/pastebin/raw/~a"
                               (match-hash match))))
  ;; For some reason, the paste begins with "# Pastebin <hash>"
  (define stripped-content (string-join (cdr (string-split content "\n"))
                                        "\n"))
  (values (match-hash match) (post-to-coliru stripped-content)))

(define (get-raw-gist-url url)
  (define document (get-xexp url))
  (define (process expr done)
    (match expr
      [(list 'a (list-no-order '@ (list 'href href) _ ...) "Raw")
       (done href)]
      [(list _ body ...) (for ([e body]) (process e done))]
      [_ (void)]))
  (call/ec (lambda (raw) (process document raw))))

(define (handle-gist match)
  (define raw-url (string-append "https://gist.githubusercontent.com"
                                 (get-raw-gist-url (match-url match))))
  (values (match-hash match) (post-to-coliru (get raw-url))))

(define (get-paste-of-code-raw hash)
  (hash-ref (get-json (format "https://paste.ofcode.org/~a/json" hash))
            'code))

(define (handle-paste-of-code match)
  (values (match-hash match)
          (post-to-coliru (get-paste-of-code-raw (match-hash match)))))

(define (get-ubuntu-paste-raw url)
  (define document (get-xexp url))
  (define (strip-entities html)
    (match html
      [(list '@ _ ...) ""]
      [(list _ elements ...)
       (string-join (for/list ([elem elements]) (strip-entities elem)) "")]
      [_ html]))
  (define (process expr done)
    (match expr
      [(list 'div (list '@ (list 'class "paste")) body)
       (done (strip-entities body))]
      [(list _ body ...) (for ([e body]) (process e done))]
      [_ (void)]))
  (call/ec (lambda (raw) (process document raw))))

(define (handle-ubuntu-paste match)
  (values (match-hash match)
          (post-to-coliru (get-ubuntu-paste-raw (match-url match)))))

(define nick-counts-file "counts.rktd")
(define nick-counts (make-hash))
(with-handlers ([exn:fail:filesystem? void])
  (call-with-input-file nick-counts-file
    (lambda (in)
      (set! nick-counts (deserialize (read in))))))

(define nick-counts-sema (make-semaphore 1))
(define (get-and-increment-nick-count nick)
  (call-with-semaphore
   nick-counts-sema
   (lambda ()
     (define previous (hash-ref nick-counts nick 0))
     (define new (add1 previous))
     (hash-set! nick-counts nick new)
     (call-with-output-file nick-counts-file #:exists 'replace
       (lambda (out)
         (write (serialize nick-counts) out)))
     new)))

(define ordinal-units (list "zeroeth" "first" "second" "third" "fourth" "fifth"
                            "sixth" "seventh" "eighth" "ninth"))
(define ordinal-irregular (list "tenth" "eleventh" "twelfth" "thirteenth"
                                "fourteenth" "fifteenth" "sixteenth"
                                "seventeenth" "eighteenth" "nineteenth"))
(define ordinal-tens (list "tenth" "twentieth" "thirtieth" "fortieth" "fiftieth"
                           "sixtieth" "seventieth" "eightieth" "ninetieth"))
(define cardinal-tens (list "ten" "twenty" "thirty" "forty" "fifty" "sixty"
                            "seventy" "eighty" "ninety"))
(define (number->english/ordinal n)
  (define (units-index n) (remainder n 10))
  (define (tens-index n) (sub1 (quotient n 10)))
  (cond
    [(< n 10) (list-ref ordinal-units n)]
    [(< n 20) (list-ref ordinal-irregular (- n 10))]
    [(and (< n 100) (= (units-index n) 0))
     (list-ref ordinal-tens (tens-index n))]
    [(< n 100) (string-append (list-ref cardinal-tens (tens-index n))
                              "-"
                              (list-ref ordinal-units (units-index n)))]
    [else
     (string-append (number->string n)
                    (case (remainder n 10)
                      [(1) "st"]
                      [(2) "nd"]
                      [(3) "rd"]
                      [else "th"]))]))

(define repaste-format-first
  (string-append "Paste ~a was moved to ~a ~a, please avoid paste sites that "
                 "can't even compile your code."))
(define repaste-format-subsequent
  (string-append "Paste ~a was moved to ~a ~a, for the ~a time, do not use "
                 "paste sites that can't compile code."))

(define (repaste user match handler)
  (define-values (id result-url) (handler match))
  (define count (get-and-increment-nick-count user))
  (case count
    [(1) (format repaste-format-first id result-url user)]
    [else (format repaste-format-subsequent
                  id result-url user (number->english/ordinal count))]))

(define handlers
  `((#px"pastebin\\.com/(\\w+)" . ,handle-pastebin)
    (#px"paste\\.fedoraproject\\.org/paste/([a-zA-Z0-9_-]+)"
     . ,handle-fedora-paste)
    (#px"hastebin\\.com/(\\w+)\\.\\w+" . ,handle-hastebin)
    (#px"bpaste\\.net/show/(\\w+)" . ,handle-bpaste)
    (#px"paste\\.ee/p/(\\w+)" . ,handle-paste-ee)
    (#px"paste\\.pound-python\\.org/show/(\\w+)/" . ,handle-pound-python)
    (#px"dpaste\\.com/(\\w+)" . ,handle-dpaste)
    (#px"paste\\.debian\\.net/(\\d+)/" . ,handle-debian-paste)
    (#px"ptpb\\.pw/([^/&# ]+)" . ,handle-ptpb)
    (#px"www\\.irccloud\\.com/pastebin/(\\w+)/" . ,handle-irccloud)
    (#px"https://gist\\.github\\.com/[^/]+/(\\w+)" . ,handle-gist)
    (#px"paste\\.ofcode\\.org/(\\w+)" . ,handle-paste-of-code)
    (#px"https://paste\\.ubuntu\\.com/p/(\\w+)/" . ,handle-ubuntu-paste)))

(define (handle-privmsg connection target user message)
  (for ([h handlers])
    (define pattern (car h))
    (define handler (cdr h))
    (define match (regexp-match pattern (filter-cr message)))
    (when match
      (thread
       (lambda ()
         (send-privmsg target (repaste user match handler)))))))

(define irc-thread #f)
(define (send-privmsg channel message)
  (cond
    [(and (thread? irc-thread) (thread-running? irc-thread))
     (thread-send irc-thread (cons channel message))]
    [else (error "IRC thread not running!")]))

(define (extract-nick prefix)
  (first (string-split prefix "!")))

(define (ignore? nick)
  (member nick (config-value 'ignore) string-ci=?))

(define (run)
  (define-values (connection ready)
    (irc-connect (config-value 'server)
                 (config-value 'port)
                 (config-value 'nick)
                 (config-value 'username)
                 (config-value 'real-name)
                 #:return-eof #t))
  (void (sync ready))
  (set! irc-thread (current-thread))

  (with-handlers ([exn:break? (lambda (e)
                                (displayln "Quitting...")
                                (irc-quit connection))])
    (when (config-value 'password)
      (irc-send-message connection
                        "NickServ"
                        (format "IDENTIFY ~a" (config-value 'password))))
    (irc-join-channel connection (config-value 'channel))

    (define incoming (irc-connection-incoming connection))
    (define outgoing (thread-receive-evt))
    (let loop ()
      (define message (sync incoming outgoing))
      (cond
        [(eq? message outgoing)
         (define msg (thread-receive))
         (irc-send-message connection (car msg) (cdr msg))
         (loop)]
        [else
         (match message
           [(irc-message prefix command parameters content)
            (printf "~a~n" (filter-cr content))
            (match message
              [(irc-message _ "PRIVMSG" (list target body) _)
               (define user (extract-nick prefix))
               (when (and (string-ci=? target (config-value 'channel))
                          (not (ignore? user)))
                 (handle-privmsg connection target user body))]
              [_ '()])
            (loop)]
           [eof (displayln "Disconnected")])]))))

(module* main #f
  (run))
