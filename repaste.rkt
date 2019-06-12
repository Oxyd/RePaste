#lang racket
(require irc
         racket/async-channel
         net/url
         net/head
         net/base64
         json
         html-parsing
         racket/serialize
         sxml
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         openssl/libcrypto
         file/gunzip)

(define config (file->value "config.rkt"))
(define (config-value key)
  (hash-ref config key #f))

(define (filter-cr str)
  (string-replace str "\r" ""))

(define (extract-status-and-headers port)
  (define headers (purify-port port))
  (define match (regexp-match #px"HTTP/\\d\\.\\d (\\d{3})" headers))
  (when (not match)
    (raise-user-error (format "Can't read ~a: Unexpected HTTP headers: ~a"
                              url headers)))
  (values (string->number (second match)) headers))

(define (extract-location headers)
  (cond
    [(extract-field "Location" headers) => identity]
    [else (raise-user-error "Got HTTP 302, but no location")]))

(define (request url connect handle)
  (define (process-http-response url port retry connect handle)
    (define-values (status headers) (extract-status-and-headers port))
    (case status
      [(200)
       (handle port)]
      [(301 302)
       (retry (extract-location headers) connect handle)]
      [else
       (raise-user-error (format "Couldn't read ~a: HTTP status ~a"
                                 url status))]))
  (call/input-url (string->url url)
                  connect
                  (lambda (port)
                    (process-http-response url port request connect handle))))

(define (get-redirect-target url)
  (call/input-url (string->url url)
                  get-impure-port
                  (λ (port)
                    (define-values (status headers) (extract-status-and-headers port))
                    (case status
                      [(301 302) (extract-location headers)]
                      [else #f]))))

(define (get url)
  (request url get-impure-port port->string))

(define (get-bytes url)
  (request url get-impure-port port->bytes))

(define (get-xexp url)
  (request url get-impure-port html->xexp))

(define (get-json url)
  (request url get-impure-port read-json))

(define (post url data [header null])
  (request url
           (lambda (u) (post-impure-port u data header))
           port->string))

(define (post-json url data)
  (request url
           (lambda (u) (post-impure-port u data))
           read-json))

(define jsexpr->bytes (compose string->bytes/utf-8 jsexpr->string))

(struct named-file
  (name contents))

(struct paste-contents
  (id main-file-contents other-files))

(define (make-paste-contents id main-file-contents [other-files '()])
  (paste-contents id main-file-contents other-files))

(define (post-to-wandbox contents)
  (define files-to-compile
    (filter (λ (file) (not (string-contains? (named-file-name file) ".h")))
            (paste-contents-other-files contents)))
  (define result-json
    (post-json "https://wandbox.org/api/compile.json"
               (jsexpr->bytes
                (make-hash
                 `((compiler . "gcc-head")
                   (code . ,(paste-contents-main-file-contents contents))
                   (codes . ,(for/list ([file (in-list (paste-contents-other-files contents))])
                               (make-hash
                                `((file . ,(named-file-name file))
                                  (code . ,(named-file-contents file))))))
                   (options . "c++2a,warning,optimize,boost-nothing-gcc-head")
                   (compiler-option-raw . ,(string-join
                                            (append '("-pedantic" "-pthread")
                                                    (if (empty? files-to-compile)
                                                        '()
                                                        (cons "-xc++" (map named-file-name files-to-compile))))
                                            "\n"))
                   (save . #t))))))
  (hash-ref result-json 'url))

(define (match-url m) (first m))
(define (match-hash m) (second m))

(define (handle-simple-pastebin match raw-format)
  (define content (get (format raw-format (match-hash match))))
  (make-paste-contents (match-hash match) content))

(define (make-simple-handler raw-format)
  (lambda (match)
    (handle-simple-pastebin match raw-format)))

(define (handle-irccloud match)
  (define content (get (format "https://www.irccloud.com/pastebin/raw/~a"
                               (match-hash match))))
  ;; For some reason, the paste begins with "# Pastebin <hash>"
  (define stripped-content (string-join (cdr (string-split content "\n"))
                                        "\n"))
  (make-paste-contents (match-hash match) stripped-content))

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
  (define url (string-append "https://" (match-url match)))
  (define raw-url (string-append "https://gist.githubusercontent.com"
                                 (get-raw-gist-url url)))
  (make-paste-contents (match-hash match) (get raw-url)))

(define (get-paste-of-code-raw hash)
  (hash-ref (get-json (format "https://paste.ofcode.org/~a/json" hash))
            'code))

(define (handle-paste-of-code match)
  (make-paste-contents (match-hash match)
                       (get-paste-of-code-raw (match-hash match))))

(define (strip-tags html)
  (match html
    [(list '@ _ ...) ""]
    [(list elements ...)
     (string-join
      (for/list ([elem elements])
        (strip-tags elem)) "")]
    [(? string?)
     html]
    [_ ""]))

(define (json-path jsexpr path)
  (if (empty? path)
      jsexpr
      (if (hash-has-key? jsexpr (car path))
          (json-path (hash-ref jsexpr (car path))
                     (cdr path))
          #f)))

(define (handle-ubuntu-paste match)
  (define url (format "https://paste.ubuntu.com/p/~a/" (match-hash match)))
  (define content
    (strip-tags ((sxpath "//td[@class='code']/div[@class='paste']//pre")
                 (get-xexp url))))
  (make-paste-contents (match-hash match) content))

(define (choose-proxy)
  (define json (get-json (string-append "https://api.getproxylist.com/proxy"
                                        "?protocol[]=http"
                                        "&maxConnectTime=1"
                                        "&maxSecondsToFirstByte=1")))
  (list (hash-ref json 'protocol)
        (hash-ref json 'ip)
        (hash-ref json 'port)))

(define (handle-crna-cc match)
  (parameterize ([current-proxy-servers (cons (choose-proxy)
                                              (current-proxy-servers))])
    (define url (format "http://crna.cc/~a" (match-hash match)))
    (define pre-contents ((sxpath "//div[@class='pasted']//pre")
                          (get-xexp url)))
    (when (empty? pre-contents)
      (raise-user-error "No paste contents found"))
    (define content (strip-tags pre-contents))
    (make-paste-contents (match-hash match) content)))

(define (handle-paste-all match)
  (define id (match-hash match))
  (define url (format "http://pasteall.org/~a" id))
  (define content (string-join ((sxpath "//pre[@id='originalcode']//text()")
                                (get-xexp url))
                               "\n"))
  (make-paste-contents id content))

(define (handle-paste-org-ru match)
  (define id (match-hash match))
  (define url (format "http://paste.org.ru/?~a" id))
  (define content (strip-tags ((sxpath "//ol[@id='code']")
                                (get-xexp url))))
  (make-paste-contents id content))

(define (handle-paste-org match)
  (define id (match-hash match))
  (make-paste-contents id
                       (string-join ((sxpath "//textarea/text()")
                                     (get-xexp (format "https://www.paste.org/~a" id)))
                                    "")))

(define (handle-paste-gg m)
  (define main-xexp (get-xexp (string-append "https://" (match-url m))))
  (define file-boxes ((sxpath "//div[@class='box']") main-xexp))
  (define (box-raw-url xexp)
    (define relative (match (first ((sxpath "//a") xexp))
                       [`(a ,(list-no-order '@ `(href ,url) _ ...) ,_) url]))
    (string-append "https://paste.gg"
                   ;; XXX: This should probably use some actual HTML entity
                   ;; decoder instead of string-replace.
                   (string-replace relative "&#x2F;" "/")))
  (cond
    [(empty? file-boxes)
     (raise-user-error "No files in paste")]
    [(= (length file-boxes) 1)
     (make-paste-contents (match-hash m)
                          (get (box-raw-url (first file-boxes))))]
    [else
     (define files
       (for/list ([box (in-list file-boxes)])
         (named-file (first ((sxpath "//h2/span/text()") box))
                     (get (box-raw-url box)))))
     (make-paste-contents (match-hash m) "" files)]))

(define-ffi-definer define-crypto libcrypto)
(define-crypto ERR_get_error (_fun -> _long))
(define-crypto ERR_error_string
  (_fun _long (_buf : (_bytes o 512)) -> _bytes))

(define (check-crypto-result name r)
  (unless (= r 1)
    (error (format "Crypto error in ~a: ~a"
                   name
                   (ERR_error_string (ERR_get_error))))))

(define-cpointer-type _EVP_CIPHER)
(define-cpointer-type _EVP_CIPHER_CTX)
(define-cpointer-type _EVP_MD)
(define-cpointer-type _ENGINE)
(define-crypto EVP_CIPHER_CTX_free
  (_fun _EVP_CIPHER_CTX -> _void)
  #:wrap (deallocator))
(define-crypto EVP_CIPHER_CTX_new
  (_fun -> _EVP_CIPHER_CTX)
  #:wrap (allocator EVP_CIPHER_CTX_free))
(define-crypto EVP_aes_128_ccm (_fun -> _EVP_CIPHER))
(define-crypto EVP_aes_192_ccm (_fun -> _EVP_CIPHER))
(define-crypto EVP_aes_256_ccm (_fun -> _EVP_CIPHER))
(define-crypto EVP_sha256 (_fun -> _EVP_MD))
(define-crypto PKCS5_PBKDF2_HMAC
  (_fun (pass : _bytes)
        (_int = (bytes-length pass))
        (salt : _bytes)
        (_int = (bytes-length salt))
        _int
        _EVP_MD
        (keylen : _int)
        (out : (_bytes o keylen))
        -> (r : _int)
        -> (begin
             (check-crypto-result "PKCS5_PBKDF2_HMAC" r)
             out)))

(define EVP_CTRL_CCM_SET_IVLEN #x9)
(define EVP_CTRL_CCM_SET_TAG #x11)
(define-crypto EVP_CIPHER_CTX_ctrl
  (_fun _EVP_CIPHER_CTX _int _int _bytes -> (r : _int)
        -> (check-crypto-result "EVP_CIPHER_CTX_ctrl" r)))

(define-crypto EVP_CIPHER_CTX_block_size
  (_fun _EVP_CIPHER_CTX -> _int))
(define-crypto EVP_DecryptInit_ex
  (_fun _EVP_CIPHER_CTX _EVP_CIPHER/null _ENGINE/null _bytes _bytes
        -> (r : _int)
        -> (check-crypto-result "EVP_DecryptInit_ex" r)))
(define-crypto EVP_DecryptUpdate
  (_fun (ctx : _EVP_CIPHER_CTX)
        (out : (_bytes o (+ (bytes-length in) (EVP_CIPHER_CTX_block_size ctx))))
        (outl : (_ptr o _int))
        (in : _bytes)
        (_int = (bytes-length in))
        -> (r : _int)
        -> (begin
             (check-crypto-result "EVP_DecryptUpdate" r)
             (subbytes out 0 outl))))
(define-crypto EVP_DecryptUpdate/size-only
  (_fun _EVP_CIPHER_CTX
        (_bytes = #f)
        (_ptr o _int)
        (_bytes = #f)
        _int
        -> (r : _int)
        -> (check-crypto-result "EVP_DecryptUpdate/size-only" r))
  #:c-id EVP_DecryptUpdate)

(define-crypto SHA512
  (_fun (in : _bytes)
        (_int = (bytes-length in))
        (out : (_bytes o 64))
        -> _void
        -> out))

(define (decrypt-aes-ccm ciphertext key iv tag key-size)
  (define ctx (EVP_CIPHER_CTX_new))
  (define length-of-length
    (cond
      [(>= (bytes-length ciphertext) (expt 2 24)) 4]
      [(>= (bytes-length ciphertext) (expt 2 16)) 3]
      [else 2]))
  (EVP_DecryptInit_ex ctx
                      (case (* 8 key-size)
                        [(128) (EVP_aes_128_ccm)]
                        [(192) (EVP_aes_192_ccm)]
                        [(256) (EVP_aes_256_ccm)]
                        [else (error "Invalid key size")])
                      #f #f #f)
  (EVP_CIPHER_CTX_ctrl ctx EVP_CTRL_CCM_SET_IVLEN (- 15 length-of-length) #f)
  (EVP_CIPHER_CTX_ctrl ctx EVP_CTRL_CCM_SET_TAG (bytes-length tag) tag)
  (EVP_DecryptInit_ex ctx #f #f key iv)
  (EVP_DecryptUpdate/size-only ctx (bytes-length ciphertext))
  (begin0
      (EVP_DecryptUpdate ctx ciphertext)
    (EVP_CIPHER_CTX_free ctx)))

(define (get-zerobin-payload url)
  ;; The payload is formatted like [{"data": "another JSON object encoded as a
  ;; string"}]. We are interested in the inner JSON.

  (call-with-input-string
   (hash-ref (car (call-with-input-string
                   (car ((sxpath "//div[@id='cipherdata']/text()")
                         (get-xexp url)))
                   read-json))
             'data)
   read-json))

(define (decrypt-sjcl data password)
  (unless (and (string=? (hash-ref data 'mode "ccm") "ccm")
               (string=? (hash-ref data 'cipher "aes") "aes"))
    (error "Invalid encryption scheme"))

  (define (data-bytes key)
    (string->bytes/utf-8 (hash-ref data key)))

  (define tag-length (/ (hash-ref data 'ts 64) 8))
  (define ct+tag (base64-decode (data-bytes 'ct)))
  (define ciphertext-length (- (bytes-length ct+tag) tag-length))
  (define ciphertext (subbytes ct+tag 0 ciphertext-length))
  (define tag (subbytes ct+tag ciphertext-length))
  (define key-size (/ (hash-ref data 'ks 128) 8))
  (define key (PKCS5_PBKDF2_HMAC password
                                 (base64-decode (data-bytes 'salt))
                                 (hash-ref data 'iter 1000)
                                 (EVP_sha256)
                                 key-size))
  (decrypt-aes-ccm ciphertext
                   key
                   (base64-decode (data-bytes 'iv))
                   tag
                   key-size))

(define (inflate-bytes in)
  (call-with-output-bytes
   (lambda (out)
     (call-with-input-bytes in (lambda (in) (inflate in out))))))

(define (handle-zerobin match)
  (define id (second match))
  (define password (string->bytes/utf-8 (third match)))
  (define url (format "https://zerobin.hsbp.org/?~a" id))
  (define compressed-content (decrypt-sjcl (get-zerobin-payload url)
                              password))
  (make-paste-contents id (bytes->string/utf-8
                           (inflate-bytes (base64-decode compressed-content)))))

(define (handle-0bin match)
  (define id (second match))
  (define password (string->bytes/utf-8 (third match)))
  (define url (format "https://0bin.net/paste/~a" id))
  (define payload
    (call-with-input-string
     (string-join ((sxpath "//pre[@id='paste-content']/code//text()")
                   (get-xexp url)))
     read-json))
  (make-paste-contents id (bytes->string/utf-8
                           (base64-decode (decrypt-sjcl payload password)))))

(define (bytes-map in f)
  (define result (bytes-copy in))
  (for ([i (in-range (bytes-length result))])
    (define b (bytes-ref in i))
    (bytes-set! result i (f (bytes-ref result i))))
  result)

(define (base64-url-decode in)
  (define (f b)
    (cond
      [(equal? b (char->integer #\-)) (char->integer #\+)]
      [(equal? b (char->integer #\_)) (char->integer #\/)]
      [else b]))
  (base64-decode (bytes-map in f)))

(define (base64-url-encode in)
  (define (f b)
    (cond
      [(equal? b (char->integer #\+)) (char->integer #\-)]
      [(equal? b (char->integer #\/)) (char->integer #\_)]
      [else b]))
  (bytes-map (base64-encode in #"") f))

(define (handle-riseup match)
  (define seed (base64-url-decode (string->bytes/utf-8 (match-hash match))))
  (define out (SHA512 seed))
  (define key (subbytes out 0 (/ 256 8)))
  (define iv (subbytes out (/ 256 8) (/ 384 8)))
  (define ident (base64-url-encode (subbytes out (/ 384 8) (/ 512 8))))
  (define ident* (string-trim (bytes->string/utf-8 ident) "=" #:repeat? #t))

  (define payload (get-bytes (format "https://share.riseup.net/i/~a" ident*)))

  ;; Payload begins with a 4-byte header of #"UP1\0" for some reason.
  (define payload* (subbytes payload 4))
  (define tag-length (/ 64 8))
  (define ciphertext-length (- (bytes-length payload*) tag-length))
  (define ciphertext (subbytes payload* 0 ciphertext-length))
  (define tag (subbytes payload* ciphertext-length))
  (define plaintext (decrypt-aes-ccm ciphertext key iv tag (bytes-length key)))

  ;; The plaintext payload is not just the paste, though. It begins with UTF-16
  ;; encoded JSON, followed by a null terminator (so, two null bytes), followed
  ;; by UTF-8 encoded paste contents.

  (define paste (second (string-split (bytes->string/utf-8 plaintext)
                                      "\u0000\u0000")))
  (make-paste-contents (match-hash match) paste))

(define (handle-paste-kde-org m)
  (define url (string-append "https://" (match-url m)))
  (define (raw-anchor? a)
    (match a
      [(list 'a (list '@ _ ...) "Raw") #t]
      [_ #f]))
  (define raw-url-anchor
    (let ([anchors (filter raw-anchor?
                           ((sxpath "//a[@class='btn btn-success']")
                            (get-xexp url)))])
      (unless (eq? (cdr anchors) null)
        (raise-user-error "Found more than one raw anchor"))
      (first anchors)))
  (define raw-url
    (match raw-url-anchor
      [(list 'a (list-no-order '@ (list 'href href) _ ...) _ ...)
       href]))
  (make-paste-contents (match-hash m) (get raw-url)))

(define (handle-kopy-io match)
  (make-paste-contents (match-hash match)
                       (hash-ref (get-json (format "https://kopy.io/documents/~a"
                                                   (match-hash match)))
                                 'data)))

(define (handle-codeshare-io match)
  ;; codeshare.io is a piece of junk because it ends in .io. It likes to timeout
  ;; often, usually giving some other reply than what we're expecting. We'll
  ;; simply retry a few times if that happens.

  (let retry ([attempt 5])
    (when (= attempt 0)
      (raise-user-error "codeshare.io failed too many times"))

    (define bootstrap
      (string->jsexpr
       (string-join ((sxpath "//script[@id='bootstrapData']/text()")
                     (get-xexp (format "https://codeshare.io/~a"
                                       (match-hash match)))))))
    (cond
      [(>= (length bootstrap) 2)
       (define value (json-path (second bootstrap)
                                '(response codeshare codeCheckpoint value)))
       (cond
         [(list? value)
          (make-paste-contents (match-hash match) (string-join value))]
         [else
          (retry (sub1 attempt))])]
      [else
       (retry (sub1 attempt))])))

(define (remove-bom s)
  (if (and (> (string-length s) 0)
           (char=? (string-ref s 0) #\uFEFF))
      (substring s 1)
      s))

(define (handle-tail-ml match)
  (make-paste-contents (match-hash match)
                       (remove-bom
                        (string-join (map (λ (line) (string-trim line #:left? #f))
                                          ((sxpath "//code[@id='p']/text()")
                                           (get-xexp (string-append "https://"
                                                                    (match-url match)))))
                                     "\n"))))

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

(define (repaste user match handler [id-override #f])
  (define contents (handler match))
  (define result-url (post-to-wandbox contents))
  (define count (get-and-increment-nick-count user))
  (define id
    (cond
      [id-override => identity]
      [else (paste-contents-id contents)]))
  (case count
    [(1) (format repaste-format-first id result-url user)]
    [else (format repaste-format-subsequent
                  id result-url user (number->english/ordinal count))]))

(define handlers
  `((#px"pastebin\\.com/(?:raw/)?(\\w+)"
     . ,(make-simple-handler "http://pastebin.com/raw/~a"))
    (#px"paste\\.fedoraproject\\.org/paste/([a-zA-Z0-9_~-]+)"
     . ,(make-simple-handler "https://paste.fedoraproject.org/paste/~a/raw"))
    (#px"hastebin\\.com/(\\w+)\\.\\w+"
     . ,(make-simple-handler "https://hastebin.com/raw/~a"))
    (#px"bpaste\\.net/show/(\\w+)"
     . ,(make-simple-handler "https://bpaste.net/raw/~a"))
    (#px"paste\\.ee/p/(\\w+)"
     . ,(make-simple-handler "https://paste.ee/r/~a/0"))
    (#px"paste\\.pound-python\\.org/show/(\\w+)/"
     . ,(make-simple-handler "https://paste.pound-python.org/raw/~a/"))
    (#px"dpaste\\.com/(\\w+)"
     . ,(make-simple-handler "http://dpaste.com/~a.txt"))
    (#px"dpaste\\.de/(\\w+)"
     . ,(make-simple-handler "https://dpaste.de/~a/raw"))
    (#px"paste\\.debian\\.net/(\\d+)/"
     . ,(make-simple-handler "http://paste.debian.net/plain/~a"))
    (#px"paste\\.debian\\.net/plain/(\\d+)"
     . ,(make-simple-handler "http://paste.debian.net/plain/~a"))
    (#px"paste\\.debian\\.net/hidden/(\\w+)/"
     . ,(make-simple-handler "http://paste.debian.net/plainh/~a"))
    (#px"ptpb\\.pw/([^/&# ]+)"
     . ,(make-simple-handler "https://ptpb.pw/~a"))
    (#px"thepasteb\\.in/p/(\\w+)"
     . ,(make-simple-handler "https://thepasteb.in/raw/~a"))
    (#px"cfp\\.vim-cn\\.com/(\\w+)"
     . ,(make-simple-handler "https://cfp.vim-cn.com/~a"))
    (#px"paste\\.awesom\\.eu/(\\w+)"
     . ,(make-simple-handler "http://paste.awesom.eu/raw/~a"))
    (#px"lpaste\\.net/(\\d+)"
     . ,(make-simple-handler "http://lpaste.net/raw/~a"))
    (#px"termbin\\.com/(\\w+)" . ,(make-simple-handler "http://termbin.com/~a"))
    (#px"la\\.wentropy\\.com/(\\w+)"
     . ,(make-simple-handler "https://la.wentropy.com/~a"))
    (#px"ix\\.io/(\\w+)" . ,(make-simple-handler "http://ix.io/~a"))
    (#px"paste\\.touhou\\.fm/(\\w+).cpp"
     . ,(make-simple-handler "https://paste.touhou.fm/raw/~a"))
    (#px"ghostbin\\.com/paste/([a-zA-Z0-9]+)"
     . ,(make-simple-handler "https://ghostbin.com/paste/~a/raw"))
    (#px"nopaste\\.chaoz-irc\\.net/view/(\\w+)"
     . ,(make-simple-handler "https://nopaste.chaoz-irc.net/view/raw/~a"))
    (#px"pastiebin\\.com/(\\w+)"
     . ,(make-simple-handler "https://www.pastiebin.com/v/~a"))
    (#px"pastecode\\.xyz/view/(\\w+)"
     . ,(make-simple-handler "https://pastecode.xyz/view/raw/~a"))
    (#px"paste\\.suut\\.in/(\\w+)"
     . ,(make-simple-handler "http://paste.suut.in/raw/~a"))
    (#px"p\\.teknik\\.io/([a-zA-Z0-9]+)"
     . ,(make-simple-handler "https://p.teknik.io/Raw/~a"))
    (#px"netpipe\\.ca/paste/paste\\.php\\?id=(\\d+)"
     . ,(make-simple-handler "http://www.netpipe.ca/paste/paste.php?raw&id=~a"))
    (#px"pb\\.lericson\\.se/p/([a-zA-Z0-9]+)/"
     . ,(make-simple-handler "http://pb.lericson.se/p/~a/text/"))
    (#px"qbin\\.io/([a-zA-Z0-9-]+)"
     . ,(make-simple-handler "https://qbin.io/~a/raw"))
    (#px"pastebin\\.osuosl\\.org/(\\d+)"
     . ,(make-simple-handler "https://pastebin.osuosl.org/~a/raw/"))
    (#px"alarmpi\\.no-ip\\.org/kamokan/(\\w+)\\?cpp"
     . ,(make-simple-handler "https://alarmpi.no-ip.org/kamokan/~a"))
    (#px"paste\\.rs/([a-zA-Z0-9]+).*"
     . ,(make-simple-handler "https://paste.rs/~a"))
    (#px"sprunge\\.us/([a-zA-Z0-9]+)"
     . ,(make-simple-handler "http://sprunge.us/~a"))
    (#px"wklejto\\.pl/(\\d+)" . ,(make-simple-handler "http://wklejto.pl/txt~a"))
    (#px"del\\.dog/(\\w+)" . ,(make-simple-handler "https://del.dog/raw/~a"))
    (#px"paste\\.opensuse\\.org/view/+(\\d+)" . ,(make-simple-handler "http://paste.opensuse.org/view/raw/~a"))
    (#px"www\\.irccloud\\.com/pastebin/([^/]+)" . ,handle-irccloud)
    (#px"gist\\.github\\.com/(?:[^/]+/)?(\\w+)" . ,handle-gist)
    (#px"paste\\.ofcode\\.org/(\\w+)" . ,handle-paste-of-code)
    (#px"paste\\.ubuntu\\.com/p/(\\w+)" . ,handle-ubuntu-paste)
    (#px"crna\\.cc/([^/&# ]+)" . ,handle-crna-cc)
    (#px"pasteall\\.org/(\\d+)" . ,handle-paste-all)
    (#px"paste\\.org\\.ru/\\?(\\w+)" . ,handle-paste-org-ru)
    (#px"paste\\.org/(\\d+)" . ,handle-paste-org)
    (#px"paste\\.gg/p/[^/]+/([a-zA-Z0-9]+)" . ,handle-paste-gg)
    (#px"zerobin\\.hsbp\\.org/\\?([^#]+)#([^=]+=)" . ,handle-zerobin)
    (#px"0bin\\.net/paste/([^#]+)#([a-zA-Z0-9_+-]+)" . ,handle-0bin)
    (#px"share\\.riseup\\.net/#([a-zA-Z0-9_-]+)" . ,handle-riseup)
    (#px"paste\\.kde\\.org/(\\w+)" . ,handle-paste-kde-org)
    (#px"kopy\\.io/([a-zA-Z0-9]+)" . ,handle-kopy-io)
    (#px"codeshare\\.io/([a-zA-Z0-9]+)" . ,handle-codeshare-io)
    (#px"tail\\.ml/p/([a-zA-Z0-9]+)\\.cpp" . ,handle-tail-ml)
    ))

(define shorteners
  '(#px"bit\\.ly/([a-zA-Z0-9]+)"
    #px"tinyurl\\.com/([a-zA-Z0-9-]+)"
    ))

(define (try-handle-message target user message [id-override #f])
  (define msg (filter-cr message))
  (let/ec return
    (for ([h (in-list handlers)])
      (define pattern (car h))
      (define handler (cdr h))
      (define match (regexp-match pattern msg))
      (when match
        (thread
         (lambda ()
           (dynamic-wind
             void
             (lambda () (send-privmsg target (repaste user match handler id-override)))
             (lambda () (collect-garbage)))))
        (return #t))))
  #f)

(define (handle-privmsg target user message)
  (unless (try-handle-message target user message)
    (define msg (filter-cr message))
    (let/ec return
      (for ([shortener (in-list shorteners)])
        (cond
          [(regexp-match shortener msg)
           => (λ (match)
                (define link-target (get-redirect-target (string-append "https://" (first match))))
                (when (try-handle-message target user link-target (second match))
                  (return (void))))])))))

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
  (define (connect)
    (define-values (connection ready)
      (irc-connect (config-value 'server)
                   (config-value 'port)
                   (config-value 'nick)
                   (config-value 'username)
                   (config-value 'real-name)
                   #:return-eof #t))
    (sync ready)
    connection)

  (define ping-interval (* 60 1000))
  (define max-ping (* (* 3 60) 1000))
  (define (make-pinger-evt)
    (alarm-evt (+ (current-inexact-milliseconds) ping-interval)))

  (set! irc-thread (current-thread))
  (define connection (connect))
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
    (define pinger-evt (make-pinger-evt))
    (define last-pong-received (current-milliseconds))
    (let loop ()
      (define message (sync incoming outgoing pinger-evt))
      (cond
        [(eq? message outgoing)
         (define msg (thread-receive))
         (irc-send-message connection (car msg) (cdr msg))
         (loop)]
        [(eq? message pinger-evt)
         (define delta (- (current-milliseconds) last-pong-received))
         (cond
           [(> delta max-ping)
            (displayln "--- Ping timeout! ---")
            (irc-quit connection)]
           [else
            (irc-send-command connection "PING"
                              (number->string (current-milliseconds)))])
         (set! pinger-evt (make-pinger-evt))
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
                 (handle-privmsg target user body))]
              [(irc-message _ "PONG" _ _)
               (set! last-pong-received (current-milliseconds))]
              [_ '()])
            (loop)]
           [eof
            (displayln "--- Disconnected ---")
            (run)])]))))

(module* main #f
  (run))
