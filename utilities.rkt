#lang racket

(provide (all-defined-out))
(require racket/hash racket/format racket/generic racket/contract racket/exn)

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
regex-matches?
|#

(define (regex-matches? regex text)
  (if (regexp-match-positions regex text) #t #f))

(module+ test
  (let* ([blank #rx""]
         [correct-1 #rx"s over t"])
    (run-tests
     (test-suite "regex-matches? tests"
                 (test-case "true on blank regex"
                            (check-true (regex-matches? blank "test")))

                 (test-case "true on correct"
                            (check-true (regex-matches? correct-1 "the quick brown fox jumps over the lazy dog")))))))

#|
throws-exn?
|#

(define (throws-particular-exn? thunk exn-pred [exn-msg #rx""])
  (with-handlers ([exn-pred (lambda (v) (regex-matches? exn-msg (exn->string v)))]
                  [(lambda (v) #t) (lambda (v) #f)])
    (thunk)))

(module+ test
  (let* ([thunk-1 (lambda () (#t))]
         [thunk-2 (lambda () (raise-argument-error 'test "test" 'a))])
    (run-tests
     (test-suite "throws-exn? tests"
                 (test-case "returns #f on no exception"
                            (throws-particular-exn? thunk-1 exn?))
                 (test-case "returns #f on wrong exception"
                            (throws-particular-exn? thunk-2 exn:fail:read?))
                 (test-case "returns #t on right exception"
                            (throws-particular-exn? thunk-2 exn:fail:contract?))))))


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
list-of-symbols?
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

#|
summarize-list-of-symbols
|#

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

#|
ensure-symbol-absent
|#

(define (ensure-symbol-absent xs illegal-symbol)
  (define summary (summarize-list-of-symbols xs))
  (if (member illegal-symbol xs)
      'present
      'absent))

(module+ test
  
  (run-tests
   (test-suite "ensure-symbol-absent tests"
               (test-case "ensure 'present"
                          (check-equal? 'present
                                        (ensure-symbol-absent '(a b c d) 'a)))
               (test-case "ensure 'absent"
                          (check-equal? 'absent
                                        (ensure-symbol-absent '(b c d) 'a))))))

#|
symbol-absent?
|#

(define (symbol-absent? xs illegal-symbol)
  (match (ensure-symbol-absent xs illegal-symbol)
    ['present #f]
    ['absent #t]))

(module+ test
  (run-tests
   (test-suite "symbol-absent? tests"
              (test-case "ensure #t"
                         (check-true (symbol-absent? '(b c d) 'a)))
              (test-case "ensure #f"
                         (check-false (symbol-absent? '(a b c d) 'a))))))

#|
ensure-symbols
|#

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

#|
symbols-ensured?
|#

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

#|
ensure-only-one-symbol
|#

(define (ensure-only-one-symbol xs x)
  (unless (list-of-symbols? xs) (raise-argument-error 'ensure-only-one-symbol "xs must be list of symbols!" xs))
  (unless (symbol? x) (raise-argument-error 'ensure-only-one-symbol "x must be symbol!" x))
  (ensure-symbols xs (list x)))

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
file-def
|#


(struct file-def (name content) #:transparent
  #:guard (lambda (name content name-str)
            (unless (string? name)
              (raise-argument-error 'file-def "name string?" name))
            (unless (string? content)
              (raise-argument-error 'file-def "content string?" content))
            (values name content)
            ))

(define file-def/c (struct/c file-def string? string?))

(module+ test

  (let* (
         [t 1]
         )
    (run-tests
     (test-suite "file-def tests"
                 (test-case "ensure error on non-string name"
                            (check-exn exn:fail:contract? (lambda () (file-def 1 "name"))))
                 (test-case "ensure error on non-string content"
                            (check-exn exn:fail:contract? (lambda () (file-def "name" 1))))
                 (test-case "ensure success"
                            (check-not-exn (lambda () (file-def "name" "name"))))
                 (test-case "ensure equality"
                            (check-equal? (file-def "name" "name") (file-def "name" "name")))
                 (test-case "ensure name inequality"
                            (check-not-eq? (file-def "name1" "name") (file-def "name" "name")))
                 (test-case "ensure content inequality"
                            (check-not-eq? (file-def "name" "name1") (file-def "name" "name1")))
                 )))
  )

#|
path-from-def
|#

(define (path-from-def def [base-dir (interpret-path "~")])
  (build-path base-dir (file-def-name def))
  )

#|
file-def->bytes
|#

(define (file-def->bytes def) (string->bytes/utf-8 (file-def-content def)))

(module+ test

  (run-tests
   (test-suite "file-def->bytes test"
               (test-case "ensure file-def works correctly for string content"
                          (check-equal? (string->bytes/utf-8 "test") (file-def->bytes (file-def "test-file-1" "test")))
                          )
               )
   )
  )

#|
list-of-file-def?
|#

(define (list-of-file-def? xs)
  (cond
    [(and (list? xs) (non-empty? xs) (andmap (lambda (x) (file-def? x)) xs)) #t]
    [else #f]
    ))

(module+ test

  (let* (
         [xs-0 '()]

         [xs-1 '(1 2 3 4)]
         
         [file-def-1 (file-def "1" "")]
         [file-def-2 (file-def "2" "")]
         [xs-2 (list file-def-1 file-def-2)]
         )

    (run-tests
     (test-suite "testing list-of-file-def?"
                 (test-case "empty list false"
                            (check-false (list-of-file-def? xs-0)))
                 (test-case "non-file-def list false"
                            (check-false (list-of-file-def? xs-1)))
                 (test-case "fd-xs-2 true" (check-true (list-of-file-def? xs-2)))
                 ))))

#|
file-def-with-status
|#

(define verify-status '(compliant
                        non-compliant
                        file-absent
                        directory-absent))

(define create-status '(created
                        overwritten
                        file-already-exists
                        directory-absent))

(define delete-status '(deleted
                        file-absent
                        non-compliant
                        directory-absent))

(define legal-status (append verify-status create-status delete-status))

(struct file-def-with-status (def symbol) #:transparent
  #:guard (lambda (def symbol name)
            (unless (file-def? def)
            (raise-argument-error 'file-def-with-status
                                  "Def must be a file-def"
                                  def))
            (unless (member symbol legal-status)
              (raise-argument-error 'file-def-with-status
                                    "Symbol must be a legal status"
                                    symbol))
            (values def symbol)))

(module+ test

  (let* (
         [file-def-fakey 0]
         [file-def-1 (file-def "test-file-1" "")]
         [file-def-2 (file-def "test-file-2" "")]
         [legal-symbol-1 (car legal-status)]
         [legal-symbol-2 (cadr legal-status)]
         )
    (run-tests
     (test-suite "file-def-with-status tests"
                 (test-case "ensure exception on non-file-def def"
                            (check-exn exn:fail:contract?
                                       (lambda () (file-def-with-status file-def-fakey 'compliant))))
                 (test-case "ensure exception on non-symbol symbol"
                            (check-exn exn:fail:contract?
                                       (lambda () (file-def-with-status file-def-1 1))))
                 (test-case "ensure exception on non-legal symbol"
                            (check-exn exn:fail:contract?
                                       (lambda () (file-def-with-status file-def-1 'fakey))))
                 (test-case "ensure correct creation"
                            (check-not-exn (lambda () (file-def-with-status file-def-1 legal-symbol-1))))
                 (test-case "ensure equality"
                            (check-equal?
                             (file-def-with-status file-def-1 legal-symbol-1)
                             (file-def-with-status file-def-1 legal-symbol-1)))
                 (test-case "ensure inequality on file-def"
                            (check-not-eq?
                             (file-def-with-status file-def-1 legal-symbol-1)
                             (file-def-with-status file-def-2 legal-symbol-1)))
                 (test-case "ensure inequality on symbol"
                            (check-not-eq?
                             (file-def-with-status file-def-1 legal-symbol-1)
                             (file-def-with-status file-def-1 legal-symbol-2)))))))

#|
list-of-file-def-with-status?
|#

(define (list-of-file-def-with-status? xs)
  (cond
    [(and (list? xs) (non-empty? xs) (andmap (lambda (x) (file-def-with-status? x)) xs)) #t]
    [else #f]))


(module+ test

  (let* (
         [test-def-1 (make-file-def "test-file-1")]
         [defv-1 (file-def-with-status test-def-1 'compliant)]
         
         [test-def-2 (make-file-def "test-file-2")]
         [defv-2 (file-def-with-status test-def-2 'compliant)]
         
         [test-def-3 (make-file-def "test-file-3")]
         [defv-3 (file-def-with-status test-def-3 'file-absent)]
         
         [list-of-only-defs (list test-def-1 test-def-2 test-def-3)]
         
         [mixed-list (list test-def-1 defv-2 test-def-3)]
         
         [correct-list (list defv-1 defv-2 defv-3)]
         )
    (run-tests
     (test-suite "list-of-file-def-with-status? tests"
                 (test-case "#f on single symbol"
                            (check-false (list-of-file-def-with-status? 'a)))
                 (test-case "#f on empty list"
                            (check-false (list-of-file-def-with-status? '())))
                 (test-case "#f on list of symbols"
                            (check-false (list-of-file-def-with-status? '(a b c d))))
                 (test-case "#f on list of file-def"
                            (check-false (list-of-file-def-with-status? list-of-only-defs)))
                 (test-case "#f on mixed list of file-def-with-status"
                            (check-false (list-of-file-def-with-status? mixed-list)))
                 (test-case "#t on correct-list"
                            (check-true (list-of-file-def-with-status? correct-list)))
                 )))
  )

#|
- file-def-with-status-list->hash
|#

(define (verification-bucketer bkt fdws)
  (define def (file-def-with-status-def fdws))
  (define symb (file-def-with-status-symbol fdws))
  (if (hash-has-key? bkt symb)
      (hash-set! bkt symb (append (hash-ref bkt symb) (list def)))
      (hash-set! bkt symb (list def))
      ))

(define (status-hash->single-status-ensured? bkt status)
  (single-symbol-ensured? (hash->list bkt) status))

(define (file-def-with-status-list->status-hash xs)
  (unless (list-of-file-def-with-status? xs)
    (raise-argument-error 'file-def-with-status-list->status-hash "list of file-def-with-status" xs))
  (define bkt (make-hash))
  (for-each (lambda (x) (verification-bucketer bkt x)) xs)
  bkt
  )

(module+ test

  (let* (
         [test-def-1 (make-file-def "test-file-1" "test1")]
         [defv-1 (file-def-with-status test-def-1 'compliant)]
         
         [test-def-2 (make-file-def "test-file-2" "test2")]
         [defv-2 (file-def-with-status test-def-2 'file-absent)]

         [test-def-3 (make-file-def "test-file-3" "test3")]
         [defv-3 (file-def-with-status test-def-3 'compliant)]

         [test-def-4 (make-file-def "test-file-4" "test4")]
         [defv-4 (file-def-with-status test-def-4 'non-compliant)]

         [test-def-5 (make-file-def "test-file-5" "test5")]
         [defv-5 (file-def-with-status test-def-5 'compliant)]

         [test-def-6 (make-file-def "test-file-6" "test6")]
         [defv-6 (file-def-with-status test-def-6 'non-compliant)]

         [fdvl-1 (list defv-1 defv-2 defv-3 defv-4 defv-5 defv-6)]
         [fdv-bkt-1 (make-hash (list
                          (cons 'compliant (list test-def-1 test-def-3 test-def-5))
                          (cons 'file-absent (list test-def-2))
                          (cons 'non-compliant (list test-def-4 test-def-6))))]
         )
    (run-tests
     (test-suite "file-def-with-status-list->status-hash tests"
                 (test-case "check exception on symbol"
                            (check-exn exn:fail? (lambda () (file-def-with-status-list->status-hash 'a))))
                 (test-case "check exception on null list"
                            (check-exn exn:fail? (lambda () (file-def-with-status-list->status-hash '()))))
                 (test-case "check exception on non-compliant list"
                            (check-exn exn:fail? (lambda () (file-def-with-status-list->status-hash '(1 2 3 4)))))
                 (test-case "check exception on list of non-compliant pairs"
                            (check-exn exn:fail? (lambda () (file-def-with-status-list->status-hash '((a b) (c d))))))
                 (test-case "check exception on list of pairs with one compliant"
                            (check-exn exn:fail? (lambda ()
                                                   (file-def-with-status-list->status-hash (list '(a b) defv-3)))))
                 (test-case "check correct"
                            (check-equal? (file-def-with-status-list->status-hash fdvl-1) fdv-bkt-1))
                 )))
  )

#|
identical-status-hashes?
|#

(define (identical-status-hashes? bkt1 bkt2)
  (cond
    [(and
      (identical-lists? (hash-keys bkt1) (hash-keys bkt2))
      (andmap (lambda (x) (identical-lists? (hash-ref bkt1 x) (hash-ref bkt2 x))) (hash-keys bkt1)))
     #t]
    [else #f]))

(module+ test

  (define-simple-check (check-identical-status-hashes? bkt1 bkt2)
    (identical-status-hashes? bkt1 bkt2))

  (let* (
         [file-def-1 (make-file-def "test")]
         [dws-1 (file-def-with-status file-def-1 'compliant)]
         [dws-2 (file-def-with-status file-def-1 'non-compliant)]
         
         [dws-list-1 (list dws-1 dws-1 dws-2 dws-2)]
         [dws-bkt-1 (file-def-with-status-list->status-hash dws-list-1)]
         
         [dws-list-2 (list dws-1 dws-1 dws-2 dws-2)]
         [dws-bkt-2 (file-def-with-status-list->status-hash dws-list-2)]
         )
    (run-tests
     (test-suite "identical-status-hashes? tests"
                 (test-case "ensure it works"
                            (check-identical-status-hashes? dws-bkt-1 dws-bkt-2))
                 )))
  )

#|
make-file-def
|#

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

#|
create-file-by-def
|#

(define (create-file-by-def def #:base-dir [base-dir (interpret-path "~")] #:overwrite? [overwrite? #f])
  (unless (file-def? def)
    (raise-argument-error 'create-file-by-def "def file-def?" def))
  (unless (path? base-dir)
    (raise-argument-error 'create-file-by-def "base-dir path?" base-dir))
  (unless (boolean? overwrite?)
    (raise-argument-error 'create-file-by-def "overwrite? boolean?" overwrite?))
  (define fp (build-path base-dir (file-def-name def)))
  (cond
    [(not(directory-exists? base-dir)) (file-def-with-status def 'directory-absent)]
    [(and (file-exists? fp) (not overwrite?)) (file-def-with-status def 'file-already-exists)]
    [else
     (let ([result (if (file-exists? fp)
                       (file-def-with-status def 'overwritten)
                       (file-def-with-status def 'created))])
       (call-with-atomic-output-file fp (lambda (out tf) (display (file-def-content def) out)))
       result)]))

(define (cfbd-symbol-verifier dir-type overwrite? setup-targets create-target expected-symbol)
  (define temp-dir (match dir-type
      ['fakey (interpret-path "fakey")]
      ['temp (make-temporary-directory)]))
  (unless (or (equal? dir-type 'fakey) (empty? setup-targets))
    (for-each (lambda (x) (create-file-by-def x #:base-dir temp-dir)) setup-targets))
  (define actual-symbol (create-file-by-def create-target #:base-dir temp-dir #:overwrite? overwrite?))
  (unless (equal? dir-type 'fakey) (delete-directory/files temp-dir))
  (equal? actual-symbol expected-symbol)
  )

(module+ test

  (define-simple-check (cfbd-works? dir-type do-setup create-target overwrite? expected-create-symbol)
    (check-true (cfbd-symbol-verifier dir-type overwrite? do-setup create-target expected-create-symbol))
    )
  
  (let* (
         [test-def-1 (make-file-def "test-file-1" "test1")]
         [test-def-1-fakey (make-file-def "test-file-1" "test1!?")]
         
         [setup-0 '()]
         [setup-1 (list test-def-1)]
         )
    (run-tests
     (test-suite "create-file-by-def tests"
                 (test-case "outputs 'directory-absent"
                            (cfbd-works? 'fakey setup-0 test-def-1 #f
                                         (file-def-with-status test-def-1 'directory-absent)))
                 (test-case "outputs 'created"
                            (cfbd-works? 'temp setup-0 test-def-1 #f
                                         (file-def-with-status test-def-1 'created)))
                 (test-case "outputs 'file-already-exists"
                            (cfbd-works? 'temp setup-1 test-def-1 #f
                                         (file-def-with-status test-def-1 'file-already-exists)))
                 (test-case "outputs 'overwritten"
                            (cfbd-works? 'temp setup-1 test-def-1-fakey #t
                                         (file-def-with-status test-def-1-fakey 'overwritten)))
                 )))
  )

#|
verify-file-by-def
|#

(define (verify-file-by-def def #:base-dir [base-dir (interpret-path "~")] #:strict? [strict? #f])
  (unless (file-def? def)
    (raise-argument-error 'verify-file-by-def "def file-def?" def))
  (unless (path? base-dir)
    (raise-argument-error 'verify-file-by-def "base-dir path?" base-dir))
  (unless (boolean? strict?)
    (raise-argument-error 'verify-file-by-def "strict? boolean?" strict?))
  
  (define fp (build-path base-dir (file-def-name def)))
  (cond
    [(or
     (and (file-exists? fp) (not strict?))
     (and (file-exists? fp) strict? (equal? (file->bytes fp) (file-def->bytes def))))
     (file-def-with-status def 'compliant)]
    [(and (file-exists? fp) strict? (not (equal? (file->bytes fp) (file-def->bytes def))))
     (file-def-with-status def 'non-compliant)]
    [(not (directory-exists? base-dir)) (file-def-with-status def 'directory-absent)]
    [(not (file-exists? fp))
     (file-def-with-status def 'file-absent)]
    )
  )

(define (vfbd-symbol-verifier dir-type setup-targets verify-target strict? expected-symbol)
  (define temp-dir (match dir-type
                     ['fakey (interpret-path "fakey")]
                     ['temp (make-temporary-directory)]))
  (unless (or (equal? dir-type 'fakey) (empty? setup-targets))
    (for-each (lambda (x) (create-file-by-def x #:base-dir temp-dir)) setup-targets))
  (define actual-symbol (verify-file-by-def verify-target #:base-dir temp-dir #:strict? strict?))
  (unless (equal? dir-type 'fakey) (delete-directory/files temp-dir))
  (equal? actual-symbol expected-symbol)
  )

(module+ test

  (define-simple-check (vfbd-works? dir-type setup-targets verify-target strict? expected-symbol)
    (check-true (vfbd-symbol-verifier dir-type setup-targets verify-target strict? expected-symbol)))

  (let* (
         [test-def-1 (make-file-def "test-file-1" "test1")]
         [test-def-1-fakey (make-file-def "test-file-1" "test1!?")]
         
         [setup-0 '()]
         [setup-1 (list test-def-1)]
         )
    (run-tests
     (test-suite "verify-file-by-def tests"
                 (test-case "outputs 'directory-absent"
                            (vfbd-works? 'fakey setup-0 test-def-1
                                         #f (file-def-with-status test-def-1 'directory-absent)))
                 (test-case "outputs 'file-absent"
                            (vfbd-works? 'temp setup-0 test-def-1
                                         #f (file-def-with-status test-def-1 'file-absent)))
                 (test-case "outputs 'compliant, strict? #f, same file"
                            (vfbd-works? 'temp setup-1 test-def-1
                                         #f (file-def-with-status test-def-1 'compliant)))
                 (test-case "outputs 'compliant, strict? #t, same file"
                            (vfbd-works? 'temp setup-1 test-def-1
                                         #t (file-def-with-status test-def-1 'compliant)))
                 (test-case "outputs 'compliant, strict? #f, different content"
                            (vfbd-works? 'temp setup-1 test-def-1-fakey
                                         #f (file-def-with-status test-def-1-fakey 'compliant)))
                 (test-case "outputs 'non-compliant"
                            (vfbd-works? 'temp setup-1 test-def-1-fakey
                                         #t (file-def-with-status test-def-1-fakey 'non-compliant)))
                 )))
  )

#|
- file-def-list->def-verification-list
verify-file-by-def outputs these symbols:
- compliant
- non-compliant
- file-absent
- directory-absent
|#

(define (file-def-list->def-verification-list defs #:base-dir [base-dir (interpret-path "~")] #:strict? [strict? #f])
  (unless (list-of-file-def? defs)
    (raise-argument-error 'file-def-list->def-verification-list "list of file-def" defs))
  (when (empty? defs)
    (raise-argument-error 'file-def-list->def-verification-list "populated list of file-def" defs))
  (map (lambda (def) (verify-file-by-def def #:base-dir base-dir #:strict? strict?)) defs)
  )

(define (fdldvl-symbol-verifier dir-type setup-targets verify-targets strict? expected-outcome)
  (define temp-dir (match dir-type
                     ['fakey (interpret-path "fakey")]
                     ['temp (make-temporary-directory)]))
  (unless (or (equal? dir-type 'fakey) (empty? setup-targets))
    (for-each (lambda (x) (create-file-by-def x #:base-dir temp-dir)) setup-targets))
  (define actual-outcome (file-def-list->def-verification-list verify-targets #:base-dir temp-dir #:strict? strict?))
  (unless (equal? dir-type 'fakey) (delete-directory/files temp-dir))
  (identical-lists? actual-outcome expected-outcome)
  )

(module+ test

  (define-simple-check (fdldvl-works? dir-type setup-targets verify-targets strict? expected-outcome)
    (check-true (fdldvl-symbol-verifier dir-type setup-targets verify-targets strict? expected-outcome)))

  (let* (
         [test-def-1 (make-file-def "test-file-1" "test1")]
         [test-def-1-fakey (make-file-def "test-file-1" "test1!?")]
         
         [test-def-2 (make-file-def "test-file-2" "test2")]
         [test-def-2-fakey (make-file-def "test-file-2" "test2!?")]
         
         [verify-1 (list test-def-1 test-def-2)]
         [verify-2 (list test-def-2 test-def-1)]

         [setup-0 '()]
         [setup-1 (list test-def-1 test-def-2)]
         [setup-2 (list test-def-1-fakey test-def-2)]
         [setup-3 (list test-def-1-fakey test-def-2-fakey)]
         [setup-4 (list test-def-1)]
         [setup-5 (list test-def-2-fakey)]

         [outcome-1 (list (file-def-with-status test-def-1 'directory-absent)
                          (file-def-with-status test-def-2 'directory-absent))]
         
         [outcome-2 (list (file-def-with-status test-def-1 'file-absent)
                          (file-def-with-status test-def-2 'file-absent))]
         
         [outcome-3 (list (file-def-with-status test-def-1 'compliant)
                          (file-def-with-status test-def-2 'compliant))]
         
         [outcome-4 (list (file-def-with-status test-def-1 'non-compliant)
                          (file-def-with-status test-def-2 'compliant))]
         
         [outcome-5 (list (file-def-with-status test-def-1 'non-compliant)
                          (file-def-with-status test-def-2 'non-compliant))]
         
         [outcome-6 (list (file-def-with-status test-def-1 'compliant)
                          (file-def-with-status test-def-2 'file-absent))]
         
         [outcome-7 (list (file-def-with-status test-def-1 'file-absent)
                          (file-def-with-status test-def-2 'non-compliant))]
         )
    (run-tests
     (test-suite "file-def-list->def-verification-list tests"
                 (test-case "exception on symbol"
                            (check-exn exn:fail? (lambda () (file-def-list->def-verification-list 'a))))
                 (test-case "exception on null list"
                            (check-exn exn:fail? (lambda () (file-def-list->def-verification-list '()))))
                 (test-case "exception on list of symbols"
                            (check-exn exn:fail? (lambda () (file-def-list->def-verification-list '(a b c)))))
                 (test-case "exception on list of file-def and symbol"
                            (check-exn exn:fail? (lambda ()
                                                   (file-def-list->def-verification-list (list 'a test-def-1)))))
                 (test-case "all 'directory-absent"
                            (fdldvl-works? 'fakey setup-0 verify-1 #f outcome-1))
                 (test-case "all 'file-absent"
                            (fdldvl-works? 'temp setup-0 verify-1 #f outcome-2))
                 (test-case "all 'compliant"
                            (fdldvl-works? 'temp setup-1 verify-1 #f outcome-3))
                 (test-case "same but ensure commutativity"
                            (fdldvl-works? 'temp setup-1 verify-2 #f outcome-3))
                 (test-case "some compliant"
                            (fdldvl-works? 'temp setup-2 verify-1 #t outcome-4))
                 (test-case "all non-compliant"
                            (fdldvl-works? 'temp setup-3 verify-1 #t outcome-5))
                 (test-case "some compliant, some absent"
                            (fdldvl-works? 'temp setup-4 verify-1 #t outcome-6))
                 (test-case "some non-compliant, some absent"
                            (fdldvl-works? 'temp setup-5 verify-1 #t outcome-7))
     )
    )
  )
  )

#|
verify-procedure-arity
|#

(define (list-of-keywords? xs)
  (andmap (lambda (x) (keyword? x)) xs))

(define (verify-procedure-arity proc by-pos required-keywords optional-keywords)
  (unless (list-of-keywords? required-keywords)
    (raise-argument-error 'verify-procedure-arity
                          "required-keywords list-of-keywords?"
                          required-keywords))

  (unless (list-of-keywords? optional-keywords)
    (raise-argument-error 'verify-procedure-arity
                          "optional-keywords list-of-keywords?"
                          optional-keywords))
  
  (unless (or
           (strict-superset? optional-keywords required-keywords)
           (identical-lists? optional-keywords required-keywords))
    (raise-argument-error 'verify-procedure-arity
                          "Optional keywords must be a strict superset of or identical to required-keywords"
                          (list
                           (cons 'required-keywords required-keywords)
                           (cons 'optional-keywords optional-keywords))))
  (let-values ([(req-kws opt-kws) (procedure-keywords proc)])
    (and
     (procedure-arity-includes? proc by-pos #t)
     (identical-lists? required-keywords req-kws)
     (identical-lists? optional-keywords opt-kws))))

(module+ test
  (let* (
         [null-proc (lambda () '())]
         [correct-proc-1 (lambda (x) x)]
         [correct-proc-2 (lambda (x #:y y) (values x y))]
         [correct-proc-3 (lambda (x #:y [y 1]) (values x y))]
         [correct-proc-4 (lambda (x y) (values x y))]
         [correct-proc-5 (lambda (x y #:z z) (values x y z))]
         [correct-proc-6 (lambda (w x #:y y #:z [z 1]) (values w x y z))]
         )
    (run-tests
     (test-suite "verify-procedure-arity tests"
                 (test-case "ensure exception on non-list-of-keywords required-keywords"
                            (check-exn exn:fail:contract?
                                       (lambda () (verify-procedure-arity null-proc 0 '(a b c) '(#:a #:b #:c)))))

                 (test-case "ensure exception on non-list-of-keywords optional-keywords"
                            (check-exn exn:fail:contract?
                                       (lambda () (verify-procedure-arity null-proc 0 '(#:a #:b #:c) '(a b c)))))
                 
                 (test-case "ensure exception on required-keywords greater than optional-keywords"
                            (check-exn exn:fail:contract?
                                       (lambda () (verify-procedure-arity null-proc 0 '(#:a #:b) '(#:a)))))
                 
                 (test-case "ensure exception on required-keywords mixed-bag with optional-keywords"
                            (check-exn exn:fail:contract?
                                       (lambda () (verify-procedure-arity null-proc 0 '(#:a #:b) '(#:a #:c)))))
                 
                 (test-case "ensure #t on 1-pos with 0 required keywords and 0 optional keywords"
                            (check-true (verify-procedure-arity correct-proc-1 1 '() '())))
                 
                 (test-case "ensure #t on 1-pos with 1 required keyword and 0 optional keywords"
                            (check-true (verify-procedure-arity correct-proc-2 1 '(#:y) '(#:y))))

                 (test-case "ensure #t on 1-pos with 0 required keywords and 1 optional keyword"
                            (check-true (verify-procedure-arity correct-proc-3 1 '() '(#:y))))

                 (test-case "ensure #t on 2-pos with 0 required keywords and 0 optional keywords"
                            (check-true (verify-procedure-arity correct-proc-4 2 '() '())))

                 (test-case "ensure #t on 2-pos with 1 required keywords and 0 optional keywords"
                            (check-true (verify-procedure-arity correct-proc-5 2 '(#:z) '(#:z))))

                 (test-case "ensure #t on 2-pos with 1 required keyword and 1 optional keyword"
                            (check-true (verify-procedure-arity correct-proc-6 2 '(#:y) '(#:y #:z))))))))

#|
file-def-list->def-creation-list
|#

; useful precondition predicates

(define (truthy xs #:base-dir base-dir)
  #t)

(define (none-may-exist xs #:base-dir base-dir)
  (single-symbol-ensured? (hash-keys
                           (file-def-with-status-list->status-hash
                            (file-def-list->def-verification-list xs #:base-dir base-dir #:strict? #f)) 'file-absent)))

(define (all-must-exist xs #:base-dir base-dir)
  (single-symbol-ensured? (hash-keys
                           (file-def-with-status-list->status-hash
                            (file-def-list->def-verification-list xs #:base-dir base-dir #:strict? #f)) 'compliant)))

(define (all-must-comply xs #:base-dir base-dir)
  (single-symbol-ensured? (hash-keys
                           (file-def-with-status-list->status-hash
                            (file-def-list->def-verification-list xs #:base-dir base-dir #:strict? #t)) 'compliant)))

; main function

(define (file-def-list->def-creation-list xs #:base-dir [base-dir (interpret-path "~")]
                                          #:overwrite? [overwrite? #f]
                                          #:precondition [precondition truthy])
  (unless (list-of-file-def? xs)
    (raise-argument-error 'file-def-list->def-creation-list "xs list-of-file-def?" xs))
  
  (unless (path? base-dir)
    (raise-argument-error 'file-def-list->def-creation-list "base-dir path?" base-dir))

  (unless (directory-exists? base-dir)
    (raise-argument-error 'file-def-list->def-creation-list "base-dir must exist" base-dir))
  
  (unless (boolean? overwrite?)
    (raise-argument-error 'file-def-list->def-creation-list "overwrite? boolean?" overwrite?))

  (unless (procedure? precondition)
    (raise-argument-error 'file-def-list->def-creation-list "precondition procedure?" precondition))

  (define required-keywords '(#:base-dir))
  (define legal-optional-keywords '(#:base-dir))
  
  (unless (verify-procedure-arity precondition 1 required-keywords legal-optional-keywords)
    (raise-argument-error 'file-def-list->def-creation-list
                          (string-append "precondition must take 1 required positional argument"
                                         " and 1 required keyword argument, which must be base-dir")
                          (let-values ([(req opt) (procedure-keywords precondition)])
                            (list (cons 'required-keywords req) (cons 'optional-keywords opt)))))

  (unless (precondition xs #:base-dir base-dir)
    (raise-argument-error 'file-def-list->def-creation-list
                          "precondition must return #t on xs with #:base-dir"
                          (list (cons 'precondition precondition) (cons 'xs xs) (cons 'base-dir base-dir))))
  
  (map (lambda (def) (create-file-by-def def #:base-dir base-dir #:overwrite? overwrite?)) xs))

; result verifier

(define (fdldcl-verifier dir-type setup-targets create-targets overwrite? precon expected-outcome)
  (define temp-dir (match dir-type
                     ['fakey (interpret-path "fakey")]
                     ['temp (make-temporary-directory)]))
  (unless (or (equal? dir-type 'fakey) (empty? setup-targets))
    (for-each (lambda (def) (create-file-by-def def #:base-dir temp-dir)) setup-targets))
  (define actual-outcome
    (file-def-list->def-creation-list create-targets #:base-dir temp-dir #:overwrite? overwrite? #:precondition precon))
  (unless (equal? dir-type 'fakey) (delete-directory/files temp-dir))
  (and (identical-lists? expected-outcome actual-outcome)))

; exception verifier

(define (fdldcl-exn-verifier dir-type setup-targets create-targets overwrite? precon exn-pred)
  
  (define temp-dir (match dir-type
                     ['non-path 'a]
                     ['fakey (interpret-path "fakey")]
                     ['temp (make-temporary-directory)]))
  
  (unless (or (equal? dir-type 'fakey) (empty? setup-targets))
    (for-each (lambda (def) (create-file-by-def def #:base-dir temp-dir)) setup-targets))
  
  (define result
    (throws-particular-exn? (lambda () (file-def-list->def-creation-list create-targets
                                                                         #:base-dir temp-dir
                                                                         #:overwrite? overwrite?
                                                                         #:precondition precon)) exn-pred))
  
  (when (equal? dir-type 'temp) (delete-directory/files temp-dir))
  result)
  
(module+ test

  (define-simple-check (fdldcl-works? dir-type setup-targets create-targets overwrite? precon expected-outcome)
    (check-true (fdldcl-verifier dir-type setup-targets create-targets overwrite? precon expected-outcome)))

  (define-simple-check (fdldcl-exn? dir-type setup-targets create-targets overwrite? precon exn-pred exn-msg)
    (check-true (fdldcl-exn-verifier dir-type setup-targets create-targets overwrite? precon exn-pred)))

  (let* (
         [blank-msg #rx""]
         [list-of-file-def-msg #rx"list-of-file-def?"]
         [base-dir-path?-msg #rx"path?"]
         [base-dir-exists?-msg #rx"must exist"]
         [non-bool-msg #rx"boolean?"]
         [non-proc-msg #rx"procedure?"]
         [precon-arity-msg #rx"must take"]
         [precon-return-msg #rx"must return"]
         
         [thunk-pred (lambda () #t)]
         [two-by-pos-pred (lambda (x y #:base-dir base-dir) #t)]
         [no-base-dir-pred (lambda (x) #t)]
         [fakey-pred (lambda (x #:base-dir base-dir #:fakey fakey) #t)]
         [falsey-pred (lambda (x #:base-dir base-dir) #f)]
         
         [file-def-1 (make-file-def "test-file-1")]
         [file-def-2 (make-file-def "test-file-2")]
         [file-def-3 (make-file-def "test-file-3")]

         [setup-0 '()]
         [create-1 (list file-def-1 file-def-2 file-def-3)]
         [defv-1 (file-def-with-status file-def-1 'directory-absent)]
         [defv-2 (file-def-with-status file-def-2 'directory-absent)]
         [defv-3 (file-def-with-status file-def-3 'directory-absent)]
         [fdws-list-1 (list defv-1 defv-2 defv-3)]

         [defv-4 (file-def-with-status file-def-1 'created)]
         [defv-5 (file-def-with-status file-def-2 'created)]
         [defv-6 (file-def-with-status file-def-3 'created)]
         [fdws-list-2 (list defv-4 defv-5 defv-6)]

         [setup-1 (list file-def-1 file-def-2 file-def-3)]
         [defv-7 (file-def-with-status file-def-1 'overwritten)]
         [defv-8 (file-def-with-status file-def-2 'overwritten)]
         [defv-9 (file-def-with-status file-def-3 'overwritten)]
         [fdws-list-3 (list defv-7 defv-8 defv-9)]

         [setup-2 (list file-def-1)]
         [defv-10 (file-def-with-status file-def-1 'overwritten)]
         [defv-11 (file-def-with-status file-def-2 'created)]
         [defv-12 (file-def-with-status file-def-3 'created)]
         [fdws-list-4 (list defv-10 defv-11 defv-12)]

         [setup-3 (list file-def-2)]
         [defv-13 (file-def-with-status file-def-1 'created)]
         [defv-14 (file-def-with-status file-def-2 'file-already-exists)]
         [defv-15 (file-def-with-status file-def-3 'created)]
         [fdws-list-5 (list defv-13 defv-14 defv-15)])
    
    (run-tests
     (test-suite "file-def-list->def-creation-list tests"
                 (test-case "ensure exception on non-list-of-file-def"
                            (fdldcl-exn? 'temp setup-0 '(a b c d) #f truthy exn:fail:contract? list-of-file-def-msg))
                 
                 (test-case "ensure exception on non-path base-dir"
                            (fdldcl-exn? 'non-path setup-0 create-1 #f truthy exn:fail:contract? base-dir-path?-msg))
                 
                 (test-case "ensure exception on non-bool overwrite?"
                            (fdldcl-exn? 'temp setup-0 create-1 'a truthy exn:fail:contract? non-bool-msg))
                 
                 (test-case "ensure exception on non-procedure precondition"
                            (fdldcl-exn? 'temp setup-0 create-1 #f 'a exn:fail:contract? non-proc-msg))
                 
                 (test-case "ensure exception on thunk precondition"
                            (fdldcl-exn? 'temp setup-0 create-1 #f thunk-pred exn:fail:contract? precon-arity-msg))
                 
                 (test-case "ensure exception on precondition with two by-position arguments"
                            (fdldcl-exn?
                             'temp setup-0 create-1 #f two-by-pos-pred exn:fail:contract? precon-arity-msg))
                 
                 (test-case "ensure exception on precondition with no base-dir keyword"
                            (fdldcl-exn?
                             'temp setup-0 create-1 #f no-base-dir-pred exn:fail:contract? precon-arity-msg))
                 
                 (test-case "ensure exception on precondition with fakey keyword"
                            (fdldcl-exn?
                             'temp setup-0 create-1 #f fakey-pred exn:fail:contract? precon-arity-msg))
                 
                 (test-case "ensure exception on precondition that returns #f"
                            (fdldcl-exn? 'temp setup-0 create-1 #f falsey-pred exn:fail:contract? precon-return-msg))
                 
                 (test-case "ensure exception on directory that doesn't exist"
                            (fdldcl-exn? 'fakey setup-0 create-1 #t truthy exn:fail:contract? base-dir-exists?-msg))
                 
                 (test-case "ensure 'created"
                            (fdldcl-works? 'temp setup-0 create-1 #t truthy fdws-list-2))
                 
                 (test-case "ensure 'overwritten"
                            (fdldcl-works? 'temp setup-1 create-1 #t truthy fdws-list-3))
                 
                 (test-case "ensure some 'overwritten and some 'created"
                            (fdldcl-works? 'temp setup-2 create-1 #t truthy fdws-list-4))
                 
                 (test-case "ensure some 'created and some 'file-already-exists"
                            (fdldcl-works? 'temp setup-3 create-1 #f truthy fdws-list-5))))))

#|
delete-file-by-def
|#

(define (delete-file-by-def file-def #:base-dir [base-dir (interpret-path "~")] #:strict? [strict? #f])
  (define fp (build-path base-dir (file-def-name file-def)))
  (define verification (verify-file-by-def file-def #:base-dir base-dir #:strict? strict?))
  (define verification-symbol (file-def-with-status-symbol verification))
  (when (equal? verification-symbol 'compliant) (delete-file fp))
  (match verification-symbol
    ['directory-absent verification]
    ['compliant (file-def-with-status 'deleted)]
    ['non-compliant verification]
    ['file-absent verification]))

(define (dfbd-verifier dir-type setup-target delete-target strict? expected-outcome)
  (define temp-dir (match dir-type
    ['fakey (interpret-path "fakey")]
    ['temp (make-temporary-directory)]))
  (when (and (equal? dir-type 'temp) (file-def? setup-target)) (create-file-by-def setup-target #:base-dir temp-dir))
  (define actual-outcome (delete-file-by-def delete-target #:base-dir temp-dir #:strict? strict?))
  (when (equal? dir-type 'fakey) (delete-directory/files temp-dir))
  (equal? expected-outcome actual-outcome))

(module+ test

  (define-simple-check (dfbd-works? dir-type setup-target delete-target strict? expected-outcome)
    (check-true (dfbd-verifier dir-type setup-target delete-target strict? expected-outcome)))

  (let* ([file-def-1 (make-file-def "test-file-1")])
    (run-tests
     (test-suite "delete-file-by-def tests"
                 (test-case "delete-file-from-def 'success not-strict" #f)
                 (test-case "delete-file-from-def 'success strict" #f)
                 (test-case "delete-file-from-def 'exists-but-different" #f)
                 (test-case "delete-file-from-def 'does-not-exist" #f)
                 ))))


#|
- file-def-list->def-deletion-list 
|#

(define (file-def-list->def-deletion-list xs
                                          #:base-dir [base-dir (interpret-path "~")]
                                          #:strict? [strict? #f]
                                          #:precondition truthy)
  (unless (list-of-file-def? xs)
    (raise-argument-error 'file-def-list->def-deletion-list "xs list-of-file-def?" xs))

  (unless (path? base-dir) (raise-argument-error 'file-def-list->def-deletion-list
                                                 "base-dir must be a path" base-dir))
  #f
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
               (test-case "l1 not true" (check-false (list-of-dir-tree? l1))))
               (test-case "dt-xs-2 true" (check-true (list-of-dir-tree? dt-xs-2)))
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
       (for-each (lambda (f) (create-file-by-def f)) (dir-tree-files dir))
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