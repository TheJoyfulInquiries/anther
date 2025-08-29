#lang racket

(provide (all-defined-out))
(require racket/hash racket/format racket/generic racket/contract)

#|
function: interpret-path
|#

(define (interpret-path p-txt)
  (define exploded-path (explode-path (string->path p-txt)))
  (define toplvl (car exploded-path))
  (define usr-home (find-system-path 'home-dir))
        
  (if (equal? toplvl (string->path "~"))
      (apply build-path (append (list usr-home) (cdr exploded-path)))
      (apply build-path exploded-path)
      )
  )

;; interpret-path tests

(module+ test

  (require rackunit rackunit/text-ui raco/testing)

  (define docs-dir (build-path (find-system-path 'home-dir) "Documents"))

  (run-tests
   (test-suite "interpret-path tests"
               (test-case "check for tilde expansion"
                          (check-equal? (interpret-path "~/Documents") docs-dir))
               )
   )
  )


#|
collection of list utilities
|#

(define (empty? xs)
  (equal? '() xs)
  )

(define (non-empty? xs)
  (not (empty? xs))
  )

(define (boolean-upsert bkt x base-case)
  (if (hash-has-key? bkt x)
      (hash-update! bkt x (lambda (v) (not base-case)))
      (hash-set! bkt x base-case)
      )
  )

;; only property needed for flag-compare is hashability
;; base-case is probably a kludge that can be engineered away but not a huge priority

(define (flag-compare xs ys [base-case #f])
  (define bkt (make-hash))
  (for-each (lambda (x) (hash-set! bkt x base-case)) xs)
  (for-each (lambda (x) (boolean-upsert bkt x base-case)) ys)
  bkt
  )

(define (identical-lists? xs ys)
  (define bkt (flag-compare xs ys))
  (andmap (lambda (x) (and #t x)) (hash-values bkt))
  )

;; tests for list utilities

(module+ test

  (define-simple-check (check-identical-lists? xs ys)
    (identical-lists? xs ys)
    )

  (define-simple-check (check-empty? xs)
    (empty? xs)
    )

  (define-simple-check (check-non-empty? xs)
    (non-empty? xs)
    )
  
  (run-tests
   (test-suite "check tests"
               (test-case "ensuring check-identical-lists works"
                          (check-identical-lists? '(1 2 3) '(3 2 1))
                          )
               (test-case "ensuring check-empty works"
                          (check-empty? '())
                          )
               (test-case "ensuring check-non-empty works"
                          (check-non-empty? '(1 2 3))
                          )
               ))
  )

#|
function: list-downcup
- computes list intersection
|#

(define (list-downcup xs ys)
  (define bkt (flag-compare xs ys))
  (hash-keys (hash-filter bkt (lambda (k v) v)))
  )

(module+ test

  (define l1 '(1 2 3 4 5 6))
  (define l2 '(4 5 6 7 8 9))
  (define l3 '(10 11 12 13))
  (define correct-downcup '(4 5 6))
  
  (run-tests
   (test-suite "list-intersections test"
               (test-case "check for correct intersection"
                          (check-identical-lists? (list-downcup l1 l2) correct-downcup)
                          )
               (test-case "check for correct intersection + commutativity"
                          (check-identical-lists? (list-downcup l2 l1) correct-downcup)
                          )
               (test-case "check for correct non-intersection"
                          (check-empty? (list-downcup l1 l3))
                          )
               (test-case "ensure intersection is empty when one argument is null"
                          (check-empty? (list-downcup l1 '()))
                          )

               )
   )
  )

#|
function: list-delta
- computes symmetric difference
|#

(define (list-delta xs ys)
  (define bkt (flag-compare xs ys))
  (hash-keys (hash-filter bkt (lambda (k v) (not v))))
  )

(module+ test

  (define correct-delta '(1 2 3 7 8 9))

  (run-tests
   (test-suite "list-delta tests"
               (test-case "check for correct delta"
                          (check-identical-lists? (list-delta l1 l2) correct-delta))
               (test-case "check for correct delta + commutativity"
                          (check-identical-lists? (list-delta l2 l1) correct-delta))
               (test-case "check for correct non-delta"
                          (check-empty? (list-delta l1 l1))
                          )
               (test-case "ensure delta is identical to xs when ys is null"
                          (check-identical-lists? l1 (list-delta l1 '()))
                          )
               )
   )
  )

#|
function: list-slash
- computes a\b, asymmetric difference
|#

(define (list-slash xs ys)
  (define bkt (make-hash))
  (for-each (lambda (x) (hash-set! bkt x #t)) xs)
  (for-each (lambda (y) (hash-remove! bkt y)) ys)
  (hash-keys bkt)
  )

(module+ test

  (define correct-slash-1 '(1 2 3))
  (define correct-slash-2 '(7 8 9))

  (run-tests
   (test-suite "list-slash tests"
               (test-case "check for correct slash 1"
                          (check-identical-lists? (list-slash l1 l2) correct-slash-1)
                          )
               (test-case "check for correst slash 2"
                          (check-identical-lists? (list-slash l2 l1) correct-slash-2)
                          )
               (test-case "check for correct empty slash"
                          (check-empty? (list-slash l1 l1))
                          )
               (test-case "ensure list-slash is identical to xs when ys is null"
                          (check-identical-lists? l1 (list-slash l1 '()))
                          )

               )
   )
  )

#|
boolean checks for set properties
|#

(define (strict-superset? xs ys)
  (and (identical-lists? (list-downcup xs ys) ys) (not (identical-lists? xs ys)))
  )

(define (strict-subset? xs ys)
  (and (identical-lists? (list-downcup xs ys) xs) (not (identical-lists? xs ys)))
  )

(define (mixed-bag? xs ys)
  (and (non-empty? (list-downcup xs ys)) (non-empty? (list-slash xs ys)) (non-empty? (list-slash ys xs)))
  )

(define (disjoint? xs ys)
  (empty? (list-downcup xs ys))
  )

(module+ test

  (define-simple-check (check-strict-superset? xs ys)
    (strict-superset? xs ys)
    )

  (define-simple-check (check-not-strict-superset? xs ys)
    (not (strict-superset? xs ys))
    )

  (define-simple-check (check-strict-subset? xs ys)
    (strict-subset? xs ys)
    )

  (define-simple-check (check-not-strict-subset? xs ys)
    (not (strict-subset? xs ys))
    )

  (define-simple-check (check-mixed-bag? xs ys)
    (mixed-bag? xs ys)
    )

  (define-simple-check (check-not-mixed-bag? xs ys)
    (not (mixed-bag? xs ys))
    )

  (define-simple-check (check-disjoint? xs ys)
    (disjoint? xs ys)
    )

  (define-simple-check (check-not-disjoint? xs ys)
    (not (disjoint? xs ys))
    )

  (run-tests
   (test-suite "superset tests"
               (test-case "test that l1 is a superset of correct-slash-1"
                          (check-strict-superset? l1 correct-slash-1)
                          )
               (test-case "testing anti-commutativity"
                          (check-not-strict-superset? correct-slash-1 l1)
                          )
               (test-case "test that l1 is a superset of null"
                          (check-strict-superset? l1 '())
                          )
               (test-case "test that l1 is not a superset of itself"
                          (check-not-strict-superset? l1 l1)
                          )
               (test-case "test that l1 is not a superset of l2"
                          (check-not-strict-superset? l1 l2)
                          )
               )
   )

  (run-tests
   (test-suite "subset tests"
               (test-case "test that correct-slash-1 is a subset of l1"
                          (check-strict-subset? correct-slash-1 l1)
                          )
               (test-case "testing anti-commutativity"
                          (check-not-strict-subset? l1 correct-slash-1)
                          )
               (test-case "test that null is a subset of l1"
                          (check-strict-subset? '() l1)
                          )
               (test-case "test that l1 is not a subset of l1"
                          (check-not-strict-subset? l1 l1)
                          )
               (test-case "test that l1 is not a subset of l2"
                          (check-not-strict-subset? l1 l2)
                          )
               )
   )

  (run-tests
   (test-suite "mixed-bag tests"
               (test-case "test that correct-delta is a mixed-bag with l1"
                          (check-mixed-bag? correct-delta l1)
                          )
               (test-case "same but commutativity"
                          (check-mixed-bag? l1 correct-delta)
                          )
               (test-case "test that null is not a mixed bag with l1"
                          (check-not-mixed-bag? '() l1)
                          )
               (test-case "test that l1 is not a mixed bag of l1"
                          (check-not-mixed-bag? l1 l1)
                          )
               (test-case "test that l1 is not a mixed bag of l2"
                          (check-not-mixed-bag? l2 l2)
                          )
               )
   )

  (run-tests
   (test-suite "disjoint tests"
               (test-case "test that l1 and l3 are disjoint"
                          (check-disjoint? l1 l3)
                          )
               (test-case "same but commutativity"
                          (check-disjoint? l3 l1)
                          )
               (test-case "check that l1 is not disjoint with itself"
                          (check-not-disjoint? l1 l1)
                          )
               (test-case "check that l1 is disjoint with null"
                          (check-disjoint? l1 '())
                          )
               )
   )
  )

                         

#|
function: qualify-lists
- outputs 'identical if the two lists are the same
- outputs 'superset if xs > ys
- outputs 'subset if xs < ys
- outputs 'mixed-bag if xs contains some but not all of the elements in ys, plus others
- outputs 'disjoint if xs is disjoint from ys
|#

(define (qualify-lists xs ys)
  (cond
    [(identical-lists? xs ys) 'identical]
    [(strict-superset? xs ys) 'strict-superset]
    [(strict-subset? xs ys) 'strict-subset]
    [(mixed-bag? xs ys) 'mixed-bag]
    [(disjoint? xs ys) 'disjoint]
    )
  )

(module+ test

  ;; commutativity does not need to be tested, only that the decision branches are appropriately reached
  
  (run-tests
   (test-suite "qualify-lists tests"
               (test-case "l1 & l1 'identical" #t)
               (test-case "l1 & correct-slash-1 'strict-superset" #t)
               (test-case "correct-slash-1 & l1 'strict-subset" #t)
               (test-case "correct-delta & l1 'mixed-bag" #t)
               (test-case "l1 & l3 'disjoint" #t)
               )
   )
  )

#|
file utilities
|#

(struct file-def (name content) #:transparent)

(define file-def/c (struct/c file-def string? string?))

(define (file-def->bytes def)
  (string->bytes/utf-8 (file-def-content def))
  )

(module+ test

  (run-tests
   (test-suite "file-def->bytes test"
               (test-case "ensure file-def works correctly for string content"
                          (check-equal? (string->bytes/utf-8 "test") (file-def->bytes (file-def "test-file-1" "test")))
                          )
               )
   )
  )

(define (make-file-def name [content ""])
  (cond
    [(and (string? name) (string? content)) (file-def name content)]
    [else (error 'make-file-def
                 (string-append "File definitions must have string for both content and name!"
                                (format "make-file-def was called with (name: ~a) (content: ~a)" name content)))]
    )
  )

(module+ test

  (define tf-1 (make-file-def "test-file-1"))
  (define tf-1-with-content (make-file-def "test-file-1" "test"))
  (define tf-2 (make-file-def "test-file-2"))

  (define-simple-check (check-file-exists? fp)
    (file-exists? fp)
    )

  (define-simple-check (check-file-does-not-exist? fp)
    (not (file-exists? fp))
    )

  (run-tests
   (test-suite "make-file-def tests"
               (test-case "check content optional"
                          (equal? tf-1 (make-file-def "test-file-1" ""))
                          )
               (test-case "check content supplied"
                          (equal? tf-1 (make-file-def "test-file-2" "test"))
                          )
               (test-case "check error when supplied content not-string"
                          (check-exn
                           exn:fail?
                           (lambda () (make-file-def "test" 1)))
                          )
               (test-case "check error when supplied name not-string"
                          (check-exn
                           exn:fail?
                           (lambda () (make-file-def 1 "test")))
                          )
               (test-case "check error when supplied name and content not-string"
                          (check-exn
                           exn:fail?
                           (lambda () (make-file-def 1 1)))
                          )
               )
   )
  
  )

(define (file-from-def def [base-dir (interpret-path "~")] [overwrite? #f])
  (define fp (build-path base-dir (file-def-name def)))
  (cond
    [(and (file-exists? fp) (not overwrite?))
     'file-already-exists]
    [else
     (let ([result (if (file-exists? fp) 'successfully-overwritten 'successfully-created)])
       (call-with-atomic-output-file fp (lambda (out tf) (display (file-def-content def) out)))
       result)]))

(module+ test

  (define fp-1 (build-path (interpret-path "~") (file-def-name tf-1)))
  (define fp-2 (build-path (interpret-path "~/Documents") (file-def-name tf-1)))

  (run-tests
   (test-suite "file-from-def tests"
               #:after (
                        lambda () 
                         (when (file-exists? fp-1) (delete-file fp-1))
                         (when (file-exists? fp-2) (delete-file fp-2))         
                         )
               (test-case "test-file-1 successfully created with default base-path"
                          (let ([fp (build-path (interpret-path "~") (file-def-name tf-1))]
                                [result (file-from-def tf-1)])
                            (check-true (and (file-exists? fp) (equal? result 'successfully-created)))
                            )
                          )
               (test-case "test-file-1 successfully created with supplied base-path"
                          (let ([fp (build-path (interpret-path "~/Documents") (file-def-name tf-1))]
                                [result (file-from-def tf-1 (interpret-path "~/Documents"))])
                            (check-true (and (file-exists? fp) (equal? result 'successfully-created)))
                            )
                          )
               (test-case "test-file-1 successfully created with supplied base-path & overwritten"
                          (let ([fp (build-path (interpret-path "~/Documents") (file-def-name tf-1))]
                                [intermediate (file-from-def tf-1 (interpret-path "~/Documents"))]
                                [result (file-from-def tf-1 (interpret-path "~/Documents") #t)])
                            (check-true (and (file-exists? fp) (equal? result 'successfully-overwritten)))
                            )
                          )
               (test-case "outputs 'file-already-exists when overwrite? #f"
                          (let ([fp (build-path (interpret-path "~/Documents") (file-def-name tf-1))]
                                [intermediate (file-from-def tf-1 (interpret-path "~/Documents"))]
                                [result (file-from-def tf-1 (interpret-path "~/Documents"))])
                            (check-true (and (file-exists? fp) (equal? result 'file-already-exists)))
                            )
                          )
               )
   )
  )

(define (verify-file-by-def def [base-dir (interpret-path "~")] #:strict? [strict? #f])
  (define fp (build-path base-dir (file-def-name def)))
  (cond
    [(or
     (and (file-exists? fp) (not strict?))
     (and (file-exists? fp) strict? (equal? (file->bytes fp) (file-def->bytes def))))
     'success]
    [(and (file-exists? fp) strict? (not (equal? (file->bytes fp) (file-def->bytes def))))
     'exists-but-different]
    [(not (file-exists? fp))
     'does-not-exist]
    )
  )

(module+ test

  (run-tests
   (test-suite "verify-file-by-def tests"
               #:after (
                        lambda ()
                         (when (file-exists? fp-1) (delete-file fp-1))
                         )               
               (test-case "verify-file-by-def 'success test not-strict"
                          (file-from-def tf-1)
                          (check-equal? 'success (verify-file-by-def tf-1))
                          )
               (test-case "verify-file-by-def 'success test strict"
                          (file-from-def tf-1)
                          (check-equal? 'success (verify-file-by-def tf-1 #:strict? #t))
                          )
               (test-case "verify-file-by-def 'exists-but-different"
                          (file-from-def tf-1)
                          (check-equal? 'exists-but-different
                                        (verify-file-by-def (make-file-def "test-file-1" "oopsie") #:strict? #t))
                          )
               (test-case "verify-file-by-def 'does-not-exist"
                          (check-equal? 'does-not-exist (verify-file-by-def (make-file-def "fakey")))
                          )
               )
   )
  )

(define (delete-file-from-def file-def [base-dir (interpret-path "~")] [strict? #f])
  (define fp (build-path base-dir (file-def-name file-def)))
  (define verification (verify-file-by-def file-def base-dir strict?))
  (when (equal? verification 'success) (delete-file fp))
  verification
  )

(module+ test

  (run-tests
   (test-suite "delete-file-from-def tests"
               (test-case "delete-file-from-def 'success not-strict" #t)
               (test-case "delete-file-from-def 'success strict" #t)
               (test-case "delete-file-from-def 'exists-but-different" #t)
               (test-case "delete-file-from-def 'does-not-exist" #t)
               )
   )

  )
       

#|
dir-tree utilities
|#

(struct dir-tree (name files children) #:transparent)

(define (list-of-file-def? xs)
  (andmap (lambda (x) (file-def? x)) xs)
  )

(define (list-of-dir-tree? xs)
  (andmap (lambda (x) (dir-tree? x)) xs)
  )

(module+ test

  (define fd-xs-1 '())
  (define fd-xs-2 (list tf-1 tf-2))
  (define dt-xs-1 '())
  (define dt-xs-2 (list (dir-tree "test" '() '())))

  (run-tests
   (test-suite "testing list-of-file-def?"
               (test-case "fd-xs-1 true" (check-true (list-of-file-def? fd-xs-1)))
               (test-case "fd-xs-2 true" (check-true (list-of-file-def? fd-xs-2)))
               (test-case "l1 not true" (check-true (not (list-of-file-def? l1))))
               )
   )

  (run-tests
   (test-suite "testing list-of-dir-tree?"
               (test-case "dt-xs-1 true" (check-true (list-of-dir-tree? dt-xs-1)))
               (test-case "dt-xs-2 true" (check-true (list-of-dir-tree? dt-xs-2)))
               (test-case "l1 not true" (check-true (not (list-of-dir-tree? l1))))
               )
   )
  )

(define (make-dir-tree name files children)
  (define legal-name? (string? name))
  (define legal-files? (list-of-file-def? files))
  (define legal-children? (list-of-dir-tree? children))
  (cond
    [(and legal-name? legal-files? legal-children?) (dir-tree name files children)]
    [(not legal-name?) (raise-arguments-error 'invalid-name-make-dir-tree
                                              "make-dir-tree requires name to be a string"
                                              "name" name)]
    [(not legal-files?) (raise-arguments-error 'invalid-files-make-dir-tree
                                               "make-dir-tree requires files to be a list of file-def"
                                               "files" files)]
    [(not legal-children?) (raise-arguments-error 'invalid-children-make-dir-tree
                                                  "make-dir-tree requires children to be a list of dir-tree"
                                                  "children" children)]
    )
  )

(module+ test

  (define dt-1 (dir-tree "test1" '() '()))
  (define dt-2 (dir-tree "test2" '() (list dt-1)))
  (define dt-3 (dir-tree "test3" (list tf-1) (list dt-1)))

  (run-tests
   (test-suite "make-dir-tree tests"
               (test-case "string name, empty files, empty children"
                          (check-equal? (make-dir-tree "test1" '() '()) dt-1)
                          )
               (test-case "string name, empty files, non-empty children"
                          (check-equal? (make-dir-tree "test2" '() (list dt-1)) dt-2)
                          )
               (test-case "string name, non-empty files, non-empty children"
                          (check-equal? (make-dir-tree "test3" (list tf-1) (list dt-1)) dt-3)
                          )
               (test-case "error: 'dir-tree-non-string-name"
                          (check-exn
                           exn:fail?
                           (lambda () (make-dir-tree 1 '() '()))
                           )
                          )
               (test-case "error: 'dir-tree-bad-files"
                          (check-exn
                           exn:fail?
                           (lambda () (make-dir-tree "test" (list 1) '()))
                           )
                          )
               (test-case "error: 'dir-tree-bad-children"
                          check-exn
                          exn:fail?
                          (lambda () (make-dir-tree "test" '() (list 1)))
                          )
               )
   )
  )

(define (verify-dir-by-tree dir-tree [base-dir (interpret-path "~")] [strict? #f])
  #t
  )

(module+ test

  (run-tests
   (test-suite "verify-dir-by-tree tests"
               (test-case "verify-dir-by-tree empty files, empty children 'success" #t)
               (test-case "verify-dir-by-tree empty files, non-empty children 'success" #t)
               (test-case "verify-dir-by-tree non-empty files, non-empty children 'success" #t)
               (test-case "verify-dir-by-tree multiply-nested 'success" #t)
               (test-case "verify-dir-by-tree 'does-not-exist" #t)
               (test-case "verify-dir-by-tree 'exists-but-different" #t)
               )
   )
  )

(define (dir-from-tree dir-tree [base-dir (interpret-path "~")])
  (define dir (build-path base-dir (dir-tree-name dir-tree)))
  (cond
    [(and (directory-exists? dir))
     'dir-already-exists]
    [(not (directory-exists? dir))
     (begin
       (make-directory dir)
       (for-each (lambda (f) (file-from-def f)) (dir-tree-files dir))
       (for-each (lambda (c) (dir-from-tree c)) (dir-tree-children dir))
       'success
       )]
    )
  )

(module+ test

  (run-tests
   (test-suite "dir-from-tree tests"
               (test-case "dir-from-tree empty files, empty children test" #t)
               )
   )
  )

(define (delete-dir-by-tree dir-tree [base-dir (interpret-path "~")] [strict? #f])
  #t
  )

(module+ test

  (run-tests
   (test-suite "delete-dir-by-tree tests"
               (test-case "delete-dir-by-tree 'success" #t)
               )
   )
  )



#|
function: dir-is-compliant?
1. Checks whether >:base-dir:< exists
  1a. if it does exist, check to see if it contains all and only the items in >:legal-files:<
    i. if it does exist but contains no files, return symbol 'exists-but-empty
    ii. if it exists and contains all of the files, and only those files, in >:legal-files:<, return 'compliant
    iii. if it exists and contains only files in the list, but not all of them, return 'subset
    iv. if it exists and contains all of the files in the list, and more, return 'superset
    v. if it exists and contains some, but not all of the files in the list, and some not on the list, return 'mixed-bag
    vi. if it contains files but none of the files on the list, return 'not-compliant
  1b. if >:base-dir:< does not exist, return 'does-not-exist
|#

(define (dir-is-compliant? dir-tree [base-dir (interpret-path "~")] [acc '()])
  (define dir (build-path base-dir (dir-tree-name dir-tree)))
  (if
   (directory-exists? dir)
   (
    (let* (
           [file-names (dir-tree-files dir-tree)]
           [legal-files (for/list ([fp file-names]) (build-path dir fp))]
           [actual-files (directory-list dir)]
           )
      (append acc (list (qualify-lists legal-files actual-files)))
      (for-each (lambda (d) (dir-is-compliant? d dir acc) (dir-tree-children dir-tree)))
      )
    )
   'does-not-exist
   )
  )

(define (interpret-compliance cmplnc)
  (cond
    [(andmap (lambda (x) (equal? 'identical) cmplnc)) 'compliant]
    [else 'non-compliant])
  )
  
(define fallback-home-env "~/.anther")
(define anther-home-env (getenv "anther-home"))
(define anther-top-config-rel (string->path ".anther.toml"))