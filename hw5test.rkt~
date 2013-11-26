#lang racket
;; Programming Languages Homework5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
;;(require "HOMEWORK_FILE")

(require "hw5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "1a-1")
   (check-equal? (racketlist->mupllist null) (aunit) "1a-2")
   (check-equal? (racketlist->mupllist (list (aunit))) (apair (aunit) (aunit)) "1a-3")

   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "1b-1") 
   (check-equal? (mupllist->racketlist (aunit)) null "1b - 3")
   (check-equal? (mupllist->racketlist (apair (aunit) (aunit))) (list (aunit)) "1b-2")
   
   ; Am I supposed to use apair or cons for the env?
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (int 3)) (int 3) "2-1: simple int")
   (check-equal? (eval-exp (add (int 3) (int 4))) (int 7) "2-2: simple add")
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "2-3: ifgreater false case")
   (check-equal? (eval-exp (add (int 3) (ifgreater (int 4) (int 3) (int 3) (int 2)))) (int 6) "2-4: add of ifgreater true case")
   (check-equal? (eval-exp (apair (add (int 4) (int 7)) (int 5))) (apair (int 11) (int 5)) "2-5: simple apair")
   (check-equal? (eval-exp (apair (add (int 4) (int 7)) (apair (add (int 9) (int 5)) (int 1)))) (apair (int 11) (apair (int 14) (int 1))) "2-6: nested apair")
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "2-7: simple fst")
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "2-8: simple snd")
   (check-equal? (eval-exp (aunit)) (aunit) "2-9: simple aunit")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "2-10: simple isaunit true") 
   (check-equal? (eval-exp (isaunit (int 5))) (int 0) "2-11: simple isaunit false")
   (check-equal? (void? (check-exn #rx"bad MUPL expression" (lambda () (eval-exp (isaunit aunit))))) #t "2-12: simple isaunit false syntax exn")
   (check-equal? (void? (check-exn #rx"unbound variable during evaluation" (lambda () (eval-exp (var "ok"))))) #t "2-13: simple unbound var exn")
   (check-equal? (eval-exp (fun #f "x" (var "x"))) (closure '() (fun #f "x" (var "x"))) "2-14: simple fun") 
   (check-equal? (eval-exp (closure (apair (apair "y" (int 7)) (aunit)) (fun #f "x" (var "x"))))  
                           (closure (apair (apair "y" (int 7)) (aunit)) (fun #f "x" (var "x"))) "2-15: simple closure")      
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "2-16: simple mlet")
   ;(check-equal? (void? (check-exn #rx"MUPL call applied to non-closure" (lambda () (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1)))))) #t "2-17: bad call")
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "2-18: call empty closure")
   (check-equal? (eval-exp (call (closure (list (cons "y" (int 7))) (fun #f "x" (add (var "y") (int 7)))) (int 1))) (int 14) "2-19: call closure with var in env")
   (check-equal? (eval-exp (call (closure (list (cons "x" (int 7))) (fun #f "x" (add (var "x") (int 2)))) (int 1))) (int 3) "2-20: call closure with shadowed var")
   (check-equal? (eval-exp (mlet "sum-to" (fun "sum-to" "n" (ifgreater (var "n") (int 1) (add (var "n") (call (var "sum-to") (add (var "n") (int -1)))) (int 1))) (call (var "sum-to") (int 100)))) (int 5050) "2-21: complex test")

   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "3a-1: simple false")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "3a-2: simple true")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "3b-1: simple mlet*")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30) "3b-2: mlet* 2 vars")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (var "x"))) (var "y"))) (int 10) "3b-3: mlet* access local")
   
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "3c-1: simple ifeq false")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "3c-2: simple ifeq true")
   (check-equal? (eval-exp (ifeq (int 5) (add (int 2) (int 3)) (int 3) (int 4))) (int 3) "3c-3: true with add")
   
    #|
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "4a-1: ")
   
   
   
     
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   

   
   
|#
   ))
(require rackunit/text-ui)
;; runs the test
(run-tests tests)
