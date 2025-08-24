#lang racket

(require toml "./utilities.rkt")

#|

%inv (X): Invariant pre-condition on X
invT% (X): Invariant post-condition on X

X :- (E, F, D)
E = Environment variable
F = File
D = Directory

Function: env-compliant?
1. check for :>anther-home-env<:
   1a. If it does exist,
       [return] :<anther-home-success>:
   1b. If it does not exist,
       [effect] create environment-variable named "anther-home" with value :>anther-home-env<:
[inv% (E): "anther-home" <= :>anther-home-env<:]

Function: anther-setup

1. if :>anther-home<: exists,
    1a. if :>rel-conf-path<: exists inside :>anther-home<:,
        [effect] print :>already-exists<:
        [return] <:anther-setup-success:>
    1b. if :>rel-conf-path<: does not exist, but :>anther-home<: is not empty:
        [error] throw exn <:anther-setup-failed-ene:> with >:exists-not-empty:<
    1c. if :>rel-conf-path<: does not exist, and :>anther-home<: is empty,
        [effect] print :>msg.empty-home-found<:
        [effect] print :>msg.beginning-setup<:
        [effect] create file at :>anther-home<:/:>rel-conf-path<: with content :>toml-conf<:
        [effect] print :>msg.finished-setup<:
        [return] <:setup-anther-success:>
2. If :>anther-home<: does not exist,
   [effect] print :>msg.beginning-setup<:
   [effect] create directory :>anther-home<:
   [effect] create file at :>rel-conf-path<: with content :>toml-conf<:
   [effect] print :>finished-setup<:

[inv% (R): :>anther-home<: exists and has only :>rel-conf-path<: with content :>toml-conf<:]
[In-Flow: (path;abs| anther-home), (path;rel| rel-conf-path), (toml| toml-conf),
          (str| empty-home-found, already-exists, exists-not-empty, beginning-setup, finished-setup)
]
[Out-Flow: <:anther-setup-failed-ene:>, <:anther-setup-success:>] 
|#

;; +------------------------------------------+
;; | THE LIBRARY                              |
;; +------------------------------------------+

;; create default TOML config
(define default-anther-config
  (hash
   'user (hash 'name "" 'email "")
   'paths (hash 'projects "" 'cache "")
   )
  )

;; A struct to hold all the messages for the setup function
(struct setup-messages (already-done exists-but-not-empty beginning-setup finished-setup) #:transparent)

(define (grab-msg symbol xs)
  (match symbol
    ['alr (setup-messages-already-done xs)]
    ['ene (setup-messages-exists-but-not-empty xs)]
    ['beg (setup-messages-beginning-setup xs)]
    ['fin (setup-messages-finished-setup xs)]
    [_ (error 'grab-msg "Invalid symbol!")]
    )
  )

;; A struct to hold all the options for the setup function
(struct setup-opts (home-dir rel-config-path default-config messages) #:transparent)

(define (grab-opt symbol xs)
  (match symbol
    ['hd (setup-opts-home-dir xs)]
    ['rcp (setup-opts-rel-config-path xs)]
    ['conf (setup-opts-default-config xs)]
    ['msg (setup-opts-messages xs)]
    [_ (error 'grab-opt "Invalid symbol!")]
    )
  )

;; command line setup

(define anther-home-param
  (make-parameter
   (if anther-home-env
       (interpret-path anther-home-env)
       (interpret-path fallback-home-env)
       )
   )
  )

(define parser
  (command-line
   #:program "anther setup"
   #:usage-help
   "Takes one optional argument, --home-dir."
   #:once-each
   [("--home-dir") hd "Set the home directory for Anther"
                   (anther-home-param (interpret-path hd))
                   (putenv "anther-home" hd)]
   )
  )

(define anther-home (anther-home-param))
(define anther-top-config (build-path anther-home anther-top-config-rel))

;; Core setup logic

(define (anther-setup setup-opts)
(cond
  [(file-exists? anther-top-config)
   (printf "Anther is already set up at ~a. Exiting. 🥳\n" anther-home)]
  [(and (directory-exists? anther-home) (not (null? (directory-list anther-home))))
   (error 'setup-anther
          "Directory '~a' already exists and is not empty. Please delete it or choose a different path."
          anther-home)]
  [else
   (printf "Setting up anther at ~a...\n" anther-home)
   (unless (directory-exists? anther-home)
     (make-directory anther-home))
   (define anther-config-toml-string (tomlexpr->string default-anther-config))
   (with-output-to-file anther-top-config
     (lambda () (printf "~a" anther-config-toml-string))
     #:exists 'error)
   (printf "Anther setup complete! A default configuration file has been created.\n")])
  )


;; +------------------------------------------+
;; | THE TEST SUITE                           |
;; +------------------------------------------+

(module+ test
  (require rackunit rackunit/text-ui raco/testing)

  (define test-alr "Already done!")
  (define test-ene "Exists but not empty!")
  (define test-beg "Beginning setup!")
  (define test-fin "Finished setup!")
  (define test-msgs (setup-messages
                    test-alr
                    test-ene
                    test-beg
                    test-fin)
    )

  (define 

  (run-tests
   (test-suite "grab-msg tests"
               (test-case "Testing 'alr grabbing"
                          (check-equal? (grab-msg 'alr test-msgs) test-alr))
               (test-case "Testing 'ene grabbing"
                          (check-equal? (grab-msg 'ene test-msgs) test-ene))
               (test-case "Testing 'beg grabbing"
                          (check-equal? (grab-msg 'beg test-msgs) test-beg))
               (test-case "Testing 'fin grabbing"
                          (check-equal? (grab-msg 'fin test-msgs) test-fin))
               (test-case "Testing error using 'err"
                          (check-exn exn:fail? (lambda () (grab-msg 'err test-msgs))))
     
               )
   )

  (run-tests
   (test-suite "grab-opt tests"
               (test-case 

    )
  )

  (test-suite
   "Tests for anther-setup"

   )

  )

;; +------------------------------------------+
;; | THE EXECUTABLE                           |
;; +------------------------------------------+


(module+ main
  (define anther-home-param
    (make-parameter
     (if anther-home-env
         (interpret-path anther-home-env)
         (interpret-path fallback-home-env)
         )
     )
    )

  (define parser
    (command-line
     #:program "anther setup"
     #:usage-help
     "Takes one optional argument, --home-dir."
     #:once-each
     [("--home-dir") hd "Set the home directory for Anther"
                     (anther-home-param (interpret-path hd))
                     (putenv "anther-home" hd)]
     )
    )

  (define anther-home (anther-home-param))
  (define anther-top-config (build-path anther-home anther-top-config-rel))

  (define final-already-done ("Anther is already set up at ~a. Exiting. 🥳\n" anther-home))
  (define final-exists-but-not-empty
    ("Directory '~a' already exists and is not empty. Please delete it or choose a different path."
     anther-home))
  (define final-beginning-setup ("Setting up anther at ~a...\n" anther-home))
  (define final-finished-setup ("Anther setup complete! A default configuration file has been created.\n"))
  
  (define final-messages (setup-messages
                          final-already-done
                          final-exists-but-not-empty
                          final-beginning-setup
                          final-finished-setup))
  
  (define final-opts (setup-opts
                      anther-home
                      anther-top-config-rel
                      default-anther-config
                      final-messages))

  (anther-setup final-opts)

  )