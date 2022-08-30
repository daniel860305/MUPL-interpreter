#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs using struct inside Racket
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Convert list in Racket to MUPL list
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

;; Convert MUPL list to list in Racket
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; a function for looking up a variable in an environment; used in eval-under-env function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))


;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
;; The function takes an expression and evaluate it under an environment.
;; Depending on the structure type defined above, the function works accordingly.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun? e) (closure env e)] ; create a closure
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)] ; evaluate e1
               [v2 (eval-under-env (ifgreater-e2 e) env)]); evaluate e2
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)]); evaluate e
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]; evaluate body with extended env
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([val (eval-under-env (fst-e e) env)])
                    (if (apair? val)
                        (apair-e1 val)
                        (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let ([val (eval-under-env (snd-e e) env)])
                    (if (apair? val)
                        (apair-e2 val)
                        (error "MUPL fst applied to non-pair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(call? e)
         (let ([clos (eval-under-env (call-funexp e) env)]) 
           (if (closure? clos)
               (let* ([f (closure-fun clos)] ; function
                      [arg (eval-under-env (call-actual e) env)] ; evaluate the argument value
                      [arg-bind (cons (fun-formal f) arg)]) 
                 (if (fun-nameopt f) ; check name of the function
                     (eval-under-env (fun-body f) (cons (cons (fun-nameopt f) clos) (cons arg-bind (closure-env clos))))
                     (eval-under-env (fun-body f) (cons arg-bind (closure-env clos)))))
               (error "MUPL call applied to non-function")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; This function is called in the interpreter-test to initialize a null environment.
(define (eval-exp e)
  (eval-under-env e null))
        
;; ifaunit, mlet*, ifeq are Racket functions that act like MUPL macros to expand the MUPL

;; ifaunit evaluate e2 if e1 is MUPL's aunit or it evaluate e3.
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; mlet* takes a Racket list of Racket pairs (si, ei) and a MUPL expression en+1
;; each si is a variable bound to the result of evaluating ei
;; mlet* works similarly as the original let expression in Racket
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

;; ifeq takes four MUPL expressions e1, e2, e3 and e4.
;; It returns e3 if e1 and e2 evaluate to equal integers.
;; Otherwise, it reutrns e4.
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; mupl-map acts like map function used in Racket
;; it takes a MUPL function and return a function that takes a MUPL list
;; and applies the function to every element in the list and return a MUPL list
(define mupl-map
  (fun #f "f" ; takes a function "f" and return a function
       (fun "map" "xs" ; takes a MUPL list "xs" and return a MUPL list
            (ifaunit (var "xs") (var "xs")
                     (apair (call (var "f") (fst (var "xs"))) (call (var "map") (snd (var "xs"))))))))

;; mupl-mapAddN is a MUPL function that takes an MUPL integer i
;; and returns a MUPL function that takes a MUPL list of MUPL integers
;; and returns a MUPL list with each MUPL integers added by i
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "f" "int" ; a function that takes MUPL int and return a function
             (fun #f "xs" ; a function that takes MUPL list and call mupl-map
                  (call (call (var "map")
                              (fun #f "element" (add (var "int") (var "element"))))
                        (var "xs"))))))

