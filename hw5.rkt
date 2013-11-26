;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; We were not allowed to change the architecture of the structs and eval functions below.
;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
;; a recursive(?) 1-argument function. fun-name arg-name fun-body
(struct fun  (nameopt formal body) #:transparent)
;; function call. Arguments are fun (fun's arg-value)
(struct call (funexp actual)       #:transparent) 
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is aunit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1.a: Convert Racket list to MUPL list
(define (racketlist->mupllist rs)
  (if (null? rs) 
      (aunit)
      (apair (car rs) (racketlist->mupllist (cdr rs)))))

;; Problem 1.b
(define (mupllist->racketlist ms)
  (if (aunit? ms)
       null
      (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))))

;; Looks up a variable str in an "environment" env
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
;; This function evaluates MUPL expressions as closures. 
;; Takes an expression e and an environment env
; option for append without list?
(define (eval-under-env e env)
  (cond [(int? e) e] ;; The first four conditions are like base cases/units of MUPL.
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(var? e) (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) 
                           (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) 
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) 
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e) 
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(mlet? e) 
         (eval-under-env (mlet-body e) ;; Like let, evaluates the body in updated env
                         (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e) ;; Calls and evaluates a closure
         (let ([clos (eval-under-env (call-funexp e) env)])
           (if (not (closure? clos)) 
               (error "MUPL call applied to non-closure")
               (let* ([arg-val (eval-under-env (call-actual e) env)]
                      [func (closure-fun clos)]
                      [fun-name (fun-nameopt func)]
                      [arg (fun-formal func)])
                 (eval-under-env (fun-body func)
                 ;; Updates function's env with arg and fun-name if not anonymous fun
                                 (append (list (cons arg arg-val))
                                         (if fun-name ;; anonymous?
                                            (list (cons fun-name clos))
                                             null)
                                         (closure-env clos))))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; This function evaluates MUPL expressions as closures. 
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; 3.a ifaunit macro: if e1 is aunit then e2 else e3
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; 3.b mlet* like let* for MUPL expressions
(define (mlet* lstlst e2)
  (if (null? lstlst)
       e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

;; 3.c if e1 == e2 then e3 else e4
(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1 
        (mlet "_y" e2 
              (ifgreater (var "_x") (var "_y") 
                         e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))

;; 4.a map for MUPL
(define mupl-map ;; Curried, returns function that takes a list
  (fun #f "fun-arg"
       (fun "mupl-rec" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "fun-arg") (fst (var "lst"))) 
                            (call (var "mupl-rec") (snd (var "lst"))))))))

;; 4.b adds i to each element of list given to curried fun returned from map
(define mupl-mapAddN 
  (mlet "map" mupl-map
       (fun #f "i"
            (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))