#lang racket

(require toml)

#|
How setup will work (version 1.0):
- check for environment variable "anther-home"
  - set it if it doesn't exist
- check to make sure "anther-home" exists
  - if it exists, check if .anther.toml exists
    - if anther-home exists but .anther.toml doesn't, error out
    - if anther-home exists and .anther.toml exists, do nothing
  - create anther-home if it doesn't exist, then create .anther.toml
|#

;; Helpers

(define (interpret-path p-txt)
  (define exploded-path (explode-path (string->path p-txt)))
  (define toplvl (car exploded-path))
  (define usr-home (find-system-path 'home-dir))
        
  (if (equal? toplvl (string->path "~"))
      (apply build-path (append (list usr-home) (cdr exploded-path)))
      (apply build-path exploded-path)
      )
  )

;; Parameters & paths

(define fallback-home-env "~/.anther")
(define anther-home-env (getenv "anther-home"))

(define anther-home-param
  (make-parameter
   (if anther-home-env
       (interpret-path anther-home-env)
       (interpret-path fallback-home-env)
       )
   )
  )

;; create default TOML config
(define default-anther-config
  (hash
   'user (hash 'name "" 'email "")
   'paths (hash 'projects "" 'cache "")
   )
  )

(define anther-top-config-rel (string->path ".anther.toml"))

;; command line setup

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