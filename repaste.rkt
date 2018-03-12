#lang racket
(require irc racket/async-channel net/url net/head json)

(define config (file->value "config.rkt"))
(define (config-value key)
  (hash-ref config key))

(define (filter-cr str)
  (string-replace str "\r" ""))

(define (get url)
  (call/input-url (string->url url) get-pure-port port->string))

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

(define (handle-pastebin match)
  (define hash (second match))
  (define content (get (format "http://pastebin.com/raw/~a" hash)))
  (values hash (post-to-coliru content)))

(define (repaste connection target match handler)
  (define-values (id result-url) (handler match))
  (irc-send-message connection target
                    (format "Paste ~a moved to ~a" id result-url)))

(define handlers
  `((#px"pastebin\\.com/(\\w+)" . ,handle-pastebin)))

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
