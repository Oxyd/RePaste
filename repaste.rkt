#lang racket
(require irc racket/async-channel net/url net/head json html-parsing)

(define config (file->value "config.rkt"))
(define (config-value key)
  (hash-ref config key))

(define (filter-cr str)
  (string-replace str "\r" ""))

(define (get url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (get-xexp url)
  (call/input-url (string->url url) get-pure-port html->xexp))

(define (get-json url)
  (call/input-url (string->url url) get-pure-port read-json))

(define (post url data)
  (call/input-url (string->url url)
                  (lambda (u) (post-pure-port u data))
                  port->string))

(define jsexpr->bytes (compose string->bytes/utf-8 jsexpr->string))

(define (post-to-coliru code)
  (define result-hash
    (post "http://coliru.stacked-crooked.com/share"
          (jsexpr->bytes
           (make-hash `((cmd . ,(string-append "g++ -std=c++17 -O2 -Wall "
                                               "-pedantic -pthread main.cpp "
                                               "&& ./a.out"))
                        (src . ,code))))))
  (string-append "http://coliru.stacked-crooked.com/a/"
                 (string-trim result-hash))) ; result-hash has an \n at the end

(define (handle-simple-pastebin match raw-format)
  (define hash (second match))
  (define content (get (format raw-format hash)))
  (values hash (post-to-coliru content)))

(define (handle-pastebin match)
  (handle-simple-pastebin match "http://pastebin.com/raw/~a"))

(define (handle-fedora-paste match)
  (handle-simple-pastebin match "https://paste.fedoraproject.org/paste/~a/raw"))

(define (get-raw-gist url)
  (define document (get-xexp url))
  (define (process expr done)
    (match expr
      [(list 'a (list '@ (list 'href href) _ ...) "Raw")
       (done href)]
      [(list _ body ...) (for ([e body]) (process e done))]
      [_ (void)]))
  (call/ec (lambda (raw) (process document raw))))

(define (handle-gist match)
  (define url (first match))
  (define hash (second match))
  (define raw-url (string-append "https://gist.githubusercontent.com"
                                 (get-raw-gist url)))
  (values hash (post-to-coliru (get raw-url))))

(define (get-paste-of-code-raw hash)
  (hash-ref (get-json (format "https://paste.ofcode.org/~a/json" hash))
            'code))

(define (handle-paste-of-code match)
  (define hash (second match))
  (values hash (post-to-coliru (get-paste-of-code-raw hash))))

(define (repaste connection target match handler)
  (define-values (id result-url) (handler match))
  (irc-send-message connection target
                    (format "Paste ~a moved to ~a" id result-url)))

(define handlers
  `((#px"pastebin\\.com/(\\w+)" . ,handle-pastebin)
    (#px"paste\\.fedoraproject\\.org/paste/(\\w+)" . ,handle-fedora-paste)
    (#px"https://gist\\.github\\.com/[^/]+/(\\w+)" . ,handle-gist)
    (#px"paste\\.ofcode\\.org/(\\w+)" . ,handle-paste-of-code)))

(define (handle-privmsg connection target message)
  (for ([h handlers])
    (match h
      [(cons pattern handler)
       (define match (regexp-match pattern message))
       (when match
         (thread (lambda ()
                   (repaste connection target match handler))))])))

(define (run)
  (define-values (connection ready)
    (irc-connect (config-value 'server)
                 (config-value 'port)
                 (config-value 'nick)
                 (config-value 'username)
                 (config-value 'real-name)
                 #:return-eof #t))
  (void (sync ready))

  (with-handlers ([exn:break? (lambda (e)
                                (displayln "Quitting...")
                                (irc-quit connection))])
    (irc-send-message connection
                      "NickServ"
                      (format "IDENTIFY ~a" (config-value 'password)))
    (irc-join-channel connection (config-value 'channel))

    (define incoming (irc-connection-incoming connection))
    (let loop ()
      (define message (async-channel-get incoming))
      (match message
        [(irc-message prefix command parameters content)
         (printf "~a~n" (filter-cr content))
         (match message
           [(irc-message _ "PRIVMSG" (list target body) _)
            (when (equal? target (config-value 'channel))
              (handle-privmsg connection target body))]
           [_ '()])
         (loop)]
        [eof (displayln "Disconnected")]))))

(module* main #f
  (run))
