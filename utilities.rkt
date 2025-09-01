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
               (test-case "test that l1 is not a subset of correct-slash-2"
                          (check-not-strict-subset? l1 correct-slash-2)
                          )
               )
   )

  (define mb-test '(1 2 7 8))
  
  (run-tests
   (test-suite "mixed-bag tests"
               (test-case "test that correct-delta is a mixed-bag with l1"
                          (check-mixed-bag? mb-test l1)
                          )
               (test-case "same but commutativity"
                          (check-mixed-bag? l1 mb-test)
                          )
               (test-case "test that null is not a mixed bag with l1"
                          (check-not-mixed-bag? '() l1)
                          )
               (test-case "test that l1 is not a mixed bag of l1"
                          (check-not-mixed-bag? l1 l1)
                          )
               (test-case "test that l1 is not a mixed bag of correct-slash-2"
                          (check-not-mixed-bag? l1 correct-slash-2)
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

|#

(define (list-of-symbols? xs)
  (andmap (lambda (x) (symbol? x)) xs)
  )

(module+ test

  (let*
      (
       [test-list-1 '()]
       [test-list-2 '(test1 test2 test3)]
       [test-list-3 (list 'test1 1)]
       [test-list-4 (list 1 2)]
       )
    (run-tests
     (test-suite "list-of-symbols? tests"
                 (test-case "empty #t"
                            (check-equal? (list-of-symbols? test-list-1) #t)
                            )
                 (test-case "test-list-2 #t"
                            (check-equal? (list-of-symbols? test-list-2) #t)
                            )
                 (test-case "test-list-3 #f"
                            (check-equal? (list-of-symbols? test-list-3) #f)
                            )
                 (test-case "test-list-4 #f"
                            (check-equal? (list-of-symbols? test-list-4) #f)
                            )
                 )
     )
    )
  )

(define (summarize-list-of-symbols xs)
  (if (list-of-symbols? xs)
      (let* (
             [bkt (make-hash)]
             )
        (for-each (lambda (x)
                    (let*
                        (
                         [key-exists? (hash-ref bkt x #f)]
                         [value (if key-exists? (+ key-exists? 1) 1)]
                         )
                      (hash-set! bkt x value)
                      )
                    )
                  xs
                  )
        bkt
        )
      'not-list-of-symbols
      )
  )


(module+ test

  (let*
      (
       [test-list-1 '()]

       [test-list-2 '(apple banana apple banana orange peach tree)]
       [test-verification-2 (make-hash '([banana . 2] [apple . 2] [orange . 1] [peach . 1] [tree . 1]))]

       [test-list-3 '(apple 1)]
       )
    (run-tests
     (test-suite "summarize-list-of-symbols tests"
                 (test-case "test-list-1 success"
                            (check-equal? (make-hash) (summarize-list-of-symbols test-list-1))
                            )
                 (test-case "test-list-2 success"
                            (check-equal? test-verification-2 (summarize-list-of-symbols test-list-2))
                            )
                 (test-case "test-list-3"
                            (check-equal? 'not-list-of-symbols (summarize-list-of-symbols test-list-3))
                            )
                 )
     )
    )
  )

(define (ensure-symbols xs legal-symbols)
  (define summary (summarize-list-of-symbols xs))
  (if (equal? summary 'not-list-of-symbols)
      'not-list-of-symbols
      (let*
          (
           [summary-keys (hash-keys summary)]
           [quality (cond
                      [(and (empty? xs) (empty? legal-symbols)) 'empty]
                      [(empty? xs) 'empty-xs]
                      [(empty? legal-symbols) 'empty-legal-symbols]
                      [else (qualify-lists summary-keys legal-symbols)]
                      )]
           )
        (match quality
          ['identical 'success]
          ['strict-subset 'some-legal-symbols-absent]
          ['strict-superset 'non-legal-symbols-also-present]
          ['mixed-bag 'some-symbols-legal-some-not]
          ['disjoint 'no-legal-symbols-present]
          [else quality]
          )
        )
      )
  )

(module+ test
  
  (let*
      (
       [test-list-1 '(apple banana)]
       [test-list-2 '(apple banana apple banana banana banana apple banana apple)]
       [test-list-3 '(apple apple apple apple)]
       [test-list-4 '(apple banana orange)]
       [test-list-5 '(apple orange)]
       [test-list-6 '(pizza pasta pepperoni)]
       )
    (run-tests
     (test-suite "ensure-symbols tests"
                 (test-case "empty xs, empty legal-symbols, 'empty"
                            (check-equal? (ensure-symbols '() '()) 'empty)
                            )
                 (test-case "empty xs, non-empty legal-symbols, 'empty-xs"
                            (check-equal? (ensure-symbols '() test-list-1) 'empty-xs)
                            )
                 (test-case "non-empty xs, empty legal-symbols, 'empty-legal-symbols"
                            (check-equal? (ensure-symbols test-list-1 '()) 'empty-legal-symbols)
                            )
                 
                 (test-case "non-empty xs, non-empty legal-symbols, 'success"
                            (check-equal? (ensure-symbols test-list-2 test-list-1) 'success)
                            )
                 (test-case "same but commutative"
                            (check-equal? (ensure-symbols test-list-1 test-list-2) 'success)
                            )
                 
                 (test-case "non-empty xs, non-empty legal-symbols, 'some-legal-symbols-absent"
                            (check-equal? (ensure-symbols test-list-3 test-list-1) 'some-legal-symbols-absent)
                            )
                 (test-case "same but anticommutative"
                            (check-equal? (ensure-symbols test-list-1 test-list-3) 'non-legal-symbols-also-present)
                            )
                 
                 (test-case "non-empty xs, non-empty legal-symbols, 'non-legal-symbols-also-present"
                            (check-equal? (ensure-symbols test-list-4 test-list-1) 'non-legal-symbols-also-present)
                            )
                 (test-case "same but anticommutative"
                            (check-equal? (ensure-symbols test-list-1 test-list-4) 'some-legal-symbols-absent)
                            )
                 
                 (test-case "non-empty xs, non-empty legal-symbols, 'some-symbols-legal-some-not"
                            (check-equal? (ensure-symbols test-list-1 test-list-5) 'some-symbols-legal-some-not)
                            )
                 (test-case "same but commutative"
                            (check-equal? (ensure-symbols test-list-5 test-list-1) 'some-symbols-legal-some-not)
                            )
                 
                 (test-case "non-empty xs, non-empty legal-symbols, 'no-legal-symbols-present"
                            (check-equal? (ensure-symbols test-list-6 test-list-1) 'no-legal-symbols-present)
                            )
                 (test-case "same but commutative"
                            (check-equal? (ensure-symbols test-list-1 test-list-6) 'no-legal-symbols-present)
                            )
                 )
     )
    )
  )

(define (symbols-ensured? xs legal-symbols)
  (define result (ensure-symbols xs legal-symbols))
  (match result
    ['success #t]
    [else #f]
    )
  )

(module+ test

  (let* (
         [test-list-1 '(apple banana)]
         [test-list-2 '(apple banana apple banana banana apple apple banana)]
         [test-list-3 '(pasta pizza pepperoni)]
         )
    (run-tests
     (test-suite "symbols-ensured? tests"
                 (test-case "two empties #f"
                            (check-false (symbols-ensured? '() '()))
                            )
                 (test-case "test-list-1 and -2 #t"
                            (check-true (symbols-ensured? test-list-1 test-list-2))
                            )
                 (test-case "same but commutative"
                            (check-true (symbols-ensured? test-list-2 test-list-1))
                            )
                 )
     )
    )
  )

(define (ensure-only-one-symbol xs x)
  (when (not (list-of-symbols? xs)) (raise-argument-error 'ensure-only-one-symbol
                                                          "xs must be list of symbols!"
                                                          xs)
                                                          )
  (when (not (symbol? x)) (raise-argument-error 'ensure-only-one-symbol
                                                "x must be symbol!"
                                                x)
                                                )
  (ensure-symbols xs (list x))
  )

(module+ test

  (let*
      (
       [test-list-1 '(apple apple apple apple)]
       [test-symbol-1 'apple]

       [test-list-2 '(apple banana orange)]
       [test-list-3 '(banana orange peach)]
       )
    (run-tests
     (test-suite "ensure-only-one-symbol tests"
                 (test-case "'success"
                            (check-equal? (ensure-only-one-symbol test-list-1 test-symbol-1) 'success)
                            )
                 (test-case "'non-legal-symbols-also-present"
                            (check-equal? (ensure-only-one-symbol test-list-2 test-symbol-1)
                                          'non-legal-symbols-also-present)
                            )
                 (test-case "'no-legal-symbols-present"
                            (check-equal? (ensure-only-one-symbol test-list-3 test-symbol-1)
                                          'no-legal-symbols-present)
                            )
                 )
     )
    )
  )

(define (single-symbol-ensured? xs x)
  (define result (ensure-only-one-symbol xs x))
  (match result
    ['success #t]
    [else #f]
    )
  )

(module+ test

  (let* (
         [test-symbol-1 'apple]
         [test-list-1 '(apple apple apple apple apple)]
         [test-list-2 '(apple banana)]
         [test-list-3 '(pizza pasta pepperoni)]
         )
    (run-tests
     (test-suite "single-symbol-ensured? tests"
                 (test-case "test-list-1 & test-symbol-1 #t"
                            (check-true (single-symbol-ensured? test-list-1 test-symbol-1)
                                        ))
                 (test-case "test-list-2 & test-symbol-1 #f"
                            (check-false (single-symbol-ensured? test-list-2 test-symbol-1)
                                         ))
                 (test-case "test-list-3 & test-symbol-1 #f"
                            (check-false (single-symbol-ensured? test-list-3 test-symbol-1)
                                         ))
                 )
     )
    )
  )

(define (curry-symbol-into-single-symbol-ensured symbol)
  (lambda (xs) (single-symbol-ensured? xs symbol)))

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

(define (list-of-file-def? xs)
  (andmap (lambda (x) (file-def? x)) xs)
  )

(module+ test

  (define fd-xs-1 '())
  (define fd-xs-2 (list tf-1 tf-2))

  (run-tests
   (test-suite "testing list-of-file-def?"
               (test-case "fd-xs-1 true" (check-true (list-of-file-def? fd-xs-1)))
               (test-case "fd-xs-2 true" (check-true (list-of-file-def? fd-xs-2)))
               (test-case "l1 not true" (check-true (not (list-of-file-def? l1))))
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

(define (all-success? xs)
  (single-symbol-ensured? xs 'success)
  )

(define (some-exist? xs)
  (equal? (ensure-only-one-symbol xs 'does-not-exist) 'non-legal-symbols-also-present)
  )

(define (none-exist? xs)
  (single-symbol-ensured? xs 'does-not-exist)
  )

(define (all-exist-but-some-different? xs)
  (equal? (ensure-only-one-symbol xs 'exists-but-different) 'non-legal-symbols-also-present)
  )

(define (all-exist-but-all-different? xs)
  (single-symbol-ensured? xs 'exists-but-different)
  )

(define (verify-files-by-def-list xs [base-dir (interpret-path "~")] #:strict? [strict? #t])
  (define results (map (lambda (def) (verify-file-by-def def base-dir #:strict? strict?)) xs))
  (cond
    [(empty? xs) 'empty]
    [(all-success? results) 'success]
    [(some-exist? results) 'some-exist]
    [(none-exist? results) 'none-exist]
    [(all-exist-but-some-different? results) 'all-exist-but-some-different]
    [(all-exist-but-all-different? results) 'all-exist-but-all-different]
    )
  )

(define (path-from-def def [base-dir (interpret-path "~")])
  (build-path base-dir (file-def-name def))
  )

(module+ test

  (let*
      (       
       [test-def-1 (make-file-def "test-file-1" "test1")]
       [test-path-1 (path-from-def test-def-1)]
       
       [test-def-1-fakey (make-file-def "test-file-1" "test1!?")]
       [test-path-1-fakey (path-from-def test-def-1-fakey)]

       [test-def-2 (make-file-def "test-file-2" "test2")]
       [test-path-2 (path-from-def test-def-2)]

       [test-def-2-fakey (make-file-def "test-file-2" "test2!?")]
       [test-path-2-fakey (path-from-def test-def-2-fakey)]

       [test-def-3 (make-file-def "test-file-3" "test3")]
       [test-path-3 (path-from-def test-def-3)]

       [defs-list-1 '()]
       
       [defs-list-2 (list test-def-1 test-def-2)]
       [create-defs-list-2 (lambda () (for-each (lambda (def) (file-from-def def)) defs-list-2))]

       [defs-list-2-fakey (list test-def-1-fakey test-def-2-fakey)]
       [create-defs-list-2-fakey (lambda () (for-each (lambda (def) (file-from-def def)) defs-list-2-fakey))]

       [defs-list-2-pseudofakey (list test-def-1 test-def-2-fakey)]

       [all-paths (list test-path-1 test-path-2 test-path-3)]
       [clean-paths (lambda () (for-each (lambda (fp) (when (file-exists? fp) (delete-file fp))) all-paths))]
       [create-test-def-1-only (lambda () (file-from-def test-def-1))]
       )
    (run-tests
     (test-suite "verify-files-by-def-list tests"
                 (test-case "fd-xs-1 'empty"
                            (check-equal? (verify-files-by-def-list defs-list-1) 'empty)
                            )
                 (test-case "fd-xs-2 'success"
                            (create-defs-list-2)
                            (check-equal? (verify-files-by-def-list defs-list-2) 'success)
                            (clean-paths)
                            )
                 (test-case "fd-xs-3 'some-exist"
                            (create-test-def-1-only)
                            (check-equal? (verify-files-by-def-list defs-list-2) 'some-exist)
                            (clean-paths)
                            )
                 (test-case "fd-xs-2 'none-exist"
                            (check-equal? (verify-files-by-def-list defs-list-2) 'none-exist)
                            )
                 (test-case "fd-xs-4 'all-exist-but-all-different"
                            (create-defs-list-2)
                            (check-equal? (verify-files-by-def-list defs-list-2-fakey #:strict? #t)
                                          'all-exist-but-all-different)
                            (clean-paths)
                            )
                 (test-case "fd-xs-5 'all-exist-but-some-different"
                            (create-defs-list-2)
                            (check-equal? (verify-files-by-def-list defs-list-2-pseudofakey)
                                          'all-exist-but-some-different)
                            (clean-paths)
                            )
                 )
     )
    )
  )

#|
file-from-def outputs three symbols: 'file-already-exists, 'successfully-overwritten, 'succesfully-created
files-from-def-list should output these symbols:
- 'all-successfully-created
- 'all-successfully-overwritten
- 'all-exist-already
- 'some-created-and-some-existed
- 'some-created-and-some-overwritten
|#

(define (all-successfully-created? xs)
  (single-symbol-ensured? xs 'successfully-created)
  )

(define (all-successfully-overwritten? xs)
  (single-symbol-ensured? xs 'successfully-overwritten)
  )

(define (all-existed-already? xs)
  (single-symbol-ensured? xs 'file-already-exists)
  )

(define (some-created-some-existed? xs)
  (symbols-ensured? xs '(successfully-created file-already-exists))
  )

(define (some-created-some-overwritten? xs)
  (symbols-ensured? xs '(successfully-created successfully-overwritten))
  )

(define (files-from-def-list xs [base-dir (interpret-path "~")] #:overwrite? [overwrite? #f])
  (define results (map (lambda (def) (file-from-def def base-dir overwrite?)) xs))
  (cond
    [(empty? xs) 'empty]
    [(all-successfully-created? results) 'all-successfully-created]
    [(all-successfully-overwritten? results) 'all-successfully-overwritten]
    [(all-existed-already? results) 'all-exist-already]
    [(some-created-some-existed? results) 'some-created-some-existed]
    [(some-created-some-overwritten? results) 'some-created-some-overwritten]
    )
  )

(define (ffdl-symbol-verifier setup-targets make-targets verify-targets overwrite? expected-symbol verify-symbol do-cleanup)
  (when setup-targets (files-from-def-list setup-targets))
  (define result (files-from-def-list make-targets #:overwrite? overwrite?))
  (define success? (equal? result expected-symbol))
  (define verify? (equal?
                   (verify-files-by-def-list verify-targets #:strict? overwrite?)
                   verify-symbol))
  (do-cleanup)
  (display (string-append (format "\nresult symbol: ~a" result) (format " success? ~a" success?) (format " verify? ~a \n" verify?)))
  (and success? verify?)
  )

(module+ test

  (let*
      (
       [test-def-1 (make-file-def "test-file-1" "test1")]
       [test-path-1 (path-from-def test-def-1)]
       
       [test-def-2 (make-file-def "test-file-2" "test2")]
       [test-path-2 (path-from-def test-def-2)]

       [test-def-2-fakey (make-file-def "test-file-2" "test-2!?")]

       [test-def-3 (make-file-def "test-file-3" "test3")]
       [test-path-3 (path-from-def test-def-3)]

       [test-defs-list-1 '()]

       [all-paths (list test-path-1 test-path-2 test-path-3)]
       [clean-paths (lambda () (for-each (lambda (fp) (when (file-exists? fp) (delete-file fp))) all-paths))]


       [test-defs-list-2 (list test-def-1 test-def-2)]
       [test-all-successfully-created? (lambda (make-targets verify-targets)
                                          (ffdl-symbol-verifier #f make-targets verify-targets #f 'all-successfully-created
                                                           'success clean-paths))
                                        ]

       [test-defs-list-2-fakey (list test-def-1 test-def-2-fakey)]
       [test-all-successfully-overwritten? (lambda (setup-targets make-targets verify-targets)
                                              (ffdl-symbol-verifier setup-targets make-targets verify-targets #t
                                                               'all-successfully-overwritten 'success clean-paths))]
       
       [test-all-already-exist? (lambda (setup-targets make-targets verify-targets)
                                   (ffdl-symbol-verifier setup-targets make-targets verify-targets #f
                                                    'all-exist-already 'success clean-paths))]

       [test-defs-list-3 (list test-def-1 test-def-3)]
       [test-some-created-some-existed? (lambda (setup-targets make-targets verify-targets)
                                           (ffdl-symbol-verifier setup-targets make-targets verify-targets #f
                                                            'some-created-some-existed 'success clean-paths))]

       [test-some-created-some-overwritten? (lambda (setup-targets make-targets verify-targets)
                                               (ffdl-symbol-verifier setup-targets make-targets verify-targets #t
                                                                'some-created-some-overwritten 'success clean-paths))]
       )
    (run-tests
     (test-suite "files-from-def-list"
                 (test-case "test-defs-list-1 'empty"
                            (check-equal? (files-from-def-list test-defs-list-1)
                                          'empty)
                            )
                 (test-case "test-defs-list-2 'all-successfully-created"
                            (check-true (test-all-successfully-created? test-defs-list-2 test-defs-list-2)) 
                            )
                 (test-case "test-defs-list-2 'all-successfully-overwritten"
                            (check-true (test-all-successfully-overwritten?
                             test-defs-list-2 test-defs-list-2-fakey test-defs-list-2-fakey))
                            )
                 (test-case "test-defs-list-2 'all-already-exist"
                            (check-true (test-all-already-exist?
                             test-defs-list-2 test-defs-list-2-fakey test-defs-list-2))
                            )
                 (test-case "test-defs-list-3 'some-created-some-existed"
                            (check-true (test-some-created-some-existed?
                             test-defs-list-2 test-defs-list-3 test-defs-list-3))
                            )
                 (test-case "test-defs-list-3 'some-created-some-overwritten"
                            (check-true (test-some-created-some-overwritten?
                             test-defs-list-2 test-defs-list-3 test-defs-list-3))
                            )
                 )))
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

(define (delete-files-by-def-list xs [base-dir (interpret-path "~")] [strict? #f] [strict-all? #f])
  #t
  )

(module+ test

  (run-tests
   (test-suite "delete-files-by-def-list test"
               (test-case "fd-xs-1 'empty" #t)
               (test-case "fd-xs-2 'success" #t)
               (test-case "fd-xs-3 'success #:strict? true" #t)
               (test-case "fd-xs-4 'success-some #:strict? true" #t)
               (test-case "fd-xs-5 'all-exist-but-some-different #:strict-all? true" #t)
               (test-case "fd-xs-6 'all-exist-but-all-different #:strict-all? true" #t)
               )
   )
  )
       

#|
dir-tree utilities
|#

(struct dir-tree (name files children) #:transparent)

(define (list-of-dir-tree? xs)
  (ensure-symbols '(hello test1 hello test1 test2 hello test1 hello test1 test2 test2) '(hello test1 test2))(andmap (lambda (x) (dir-tree? x)) xs)
  )

(module+ test
  
  (define dt-xs-1 '())
  (define dt-xs-2 (list (dir-tree "test" '() '())))

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

  (define std-test-dirpath (interpret-path "~/test-dir"))

  (define test-files-1 (list tf-1 tf-2))
  
  (define test-files-2 (list
                        (make-file-def "test-file-3" "test3")
                        (make-file-def "test-file-4" "test4")
                        (make-file-def "test-file-5" "test5")
                        )
    )

  (define test-children-1 (list
                           (make-dir-tree "test-child-1" '() '())
                           (make-dir-tree "test-child-2" '() '())
                           )
    )
                                     

  (define test-children-2 (list
                           (make-dir-tree "test-child-3" '()
                                          (make-dir-tree "test-child-4" test-files-2 '()
                                                         )
                                          )
                           (make-dir-tree "test-child-4" test-files-2 '()
                                          )
                           )
    )

  (define test-children-3 (list
                           (make-dir-tree "test-child-5" test-files-1 test-children-2)
                           (make-dir-tree "test-child-6" '() '())
                           (make-dir-tree "test-child-7" '() test-children-1)
                           (make-dir-tree "test-child-8" test-files-2 test-children-1)
                           )
    )

  (define dirtest-1 (make-dir-tree "test-dir" '() '()))

  (define create-dirtest-1 (lambda ()
                             (unless (directory-exists? std-test-dirpath)
                               (make-directory std-test-dirpath)
                               )
                             )
    )
  
  (define dirtest-2 (make-dir-tree "test-dir" '() test-children-1))

  (define create-dirtest-2 (lambda ()
                             (unless (directory-exists? std-test-dirpath)
                               (make-directory std-test-dirpath)
                               (let (
                                     [tc-1-path (build-path std-test-dirpath "test-child-1")]
                                     [tc-2-path (build-path std-test-dirpath "test-child-2")]
                                     )
                                 (make-directory tc-1-path)
                                 (make-directory tc-2-path)
                                 )
                               )
                             )
    )
  
  (define dirtest-3 (make-dir-tree "test-dir" test-files-1 test-children-1))
  (define create-dirtest-3 #t)
  (define dirtest-4 (make-dir-tree "test-dir" test-files-2 test-children-3))
  (define create-dirtest-4 #t)
  (define dirtest-5 (make-dir-tree "test-dir" test-files-2 test-children-1))
  (define create-dirtest-5 #t)
  (define dirtest-6 (make-dir-tree "test-dir" test-files-1 test-children-2))
  (define create-dirtest-6 #t)
  (define dirtest-7 (make-dir-tree "test-dir" test-files-1 test-children-3))
  (define create-dirtest-7 #t)

  (run-tests
   (test-suite "verify-dir-by-tree tests"
               #:after (
                        (lambda ()
                          (when (directory-exists? std-test-dirpath) (delete-directory/files std-test-dirpath))
                          )
                        )
               (test-case "test-1: verify-dir-by-tree empty files, empty children 'success"
                          (create-dirtest-1)
                          (check-equal? 'success
                                        (verify-dir-by-tree dirtest-1))
                          )
               (test-case "test-2: verify-dir-by-tree empty files, non-empty children 'success"
                          (create-dirtest-2)
                          (check-equal? 'success
                                        (verify-dir-by-tree dirtest-2))
                          )
               (test-case "test-3: verify-dir-by-tree non-empty files, non-empty children 'success"
                          (create-dirtest-3)
                          (check-equal? 'success
                                        (verify-dir-by-tree dirtest-3))
                          )
               (test-case "test-4: verify-dir-by-tree multiply-nested 'success"
                          (create-dirtest-4)
                          (check-equal? 'success
                                        (verify-dir-by-tree dirtest-4))
                          )
               (test-case "test-5: verify-dir-by-tree 'does-not-exist"
                          (check-equal? 'does-not-exist
                                        (verify-dir-by-tree dirtest-1))
                          )
               (test-case "test-6: verify-dir-by-tree 'exists-but-different-file-names for files"
                          (create-dirtest-5)
                          (check-equal? 'exists-but-different-file-names
                                        (verify-dir-by-tree dirtest-5))
                          )
               (test-case "test-7: verify-dir-by-tree 'exists-but-different-file-contents for files, #:strict? true"
                          (create-dirtest-6)
                          (check-equal? 'exists-but-different-file-contents
                                        (verify-dir-by-tree dirtest-6))
                          )
               (test-case "test-8: verify-dir-by-tree 'exists-but-different-children for children"
                          (create-dirtest-7)
                          (check-equal? 'exists-but-different-children
                                        (verify-dir-by-tree dirtest-7))
                          )
               )
   )
  )

(define (verify-dirs-by-tree-list xs)
  #t
  )

(module+ test

  (run-tests
   (test-suite "verify-dir-by-tree-list"
               (test-case "bunk" #t)
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

(define (dirs-from-tree-list xs)
  #t
  )

(module+ test

  (run-tests
   (test-suite "dirs-from-tree-list tests"
               (test-case "bunk" #t)
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