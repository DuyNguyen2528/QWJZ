#lang typed/racket
;FULLLY FINISH
(require typed/rackunit)

(define-type primitiveOp (U '+ '- '* '/ ))
(define-type booleanOp (U '< '> '<= '>= 'equal? 'true 'false))
(define prim-operation-table (hash '+ + '- - '* * '/ / ))
(define comparitive-table (hash '> > '< < '>= >= '<= <= 'equal? equal? ))


;(define-type ArithC (U NumC BinC))
(struct NumC ([n : Real]) #:transparent)
(struct PrimOp ([op : primitiveOp] [l : ExprC] [r : ExprC]) #:transparent)
(struct BoolOp ([op : booleanOp]  [l : ExprC] [r : ExprC]) #:transparent)


(struct IdC ([s : Symbol])#:transparent)
(struct AppC ([proc : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct FunDefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct ifleq0? ([test : ExprC] [then :  ExprC] [else : ExprC]) #:transparent)
(struct equalC? ([first :  ExprC] [second : ExprC]) #:transparent)

(struct declareC ([c : ClauseC] [body : ExprC])#:transparent)
(struct ClauseC ([id : IdC] [body : ExprC])#:transparent)
(struct ProcC ([arg : (Listof IdC)] [body : ExprC])#:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct BooleanC ([v : Boolean])#:transparent)
(struct LamC ([arg : (Listof Symbol)] [body : ExprC])#:transparent)
(struct ClosV ([arg : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct NumV ([n : Number]))
(struct BindingC ([name : Symbol] [val : Value])#:transparent)
(struct Env ([env : (Listof BindingC)])#:transparent)
(struct PrintC ([s : String]) #:transparent)
(struct StringC ([s : String]) #:transparent)

(define-type ExprC (U
                    IdC
                    AppC
                    NumC
                    PrimOp
                    BoolOp
                    ifleq0?
                    ifC
                    String
                    booleanOp
                    equalC?
                    ClosV
                    LamC
                    PrintC
                    StringC
                    BindingC))

(define-type Value (U primitiveOp Number Boolean String Procedure ExprC NumV ClosV))

(define (validId? [v : Sexp]) : Boolean
  (match v
    ['if #f]
    ['proc #f]
    ['println #f]
    ['ifleq0? #f]

    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]

    [other #t]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top–Level Enviroment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define top-env

  (Env (list
        (BindingC 'true #t)  ; true will be treated as 1 (boolean-like behavior)
        (BindingC 'false #f) ; false will be treated as 0
        (BindingC '+ "#<primop>")
        (BindingC '- "#<primop>")
        (BindingC '* "#<primop>")
        (BindingC '/ "#<primop>")

        )))

;----------------------------------
; DESUGAR
;; purpose statement:
;; input: <Sexp>
;; output: <Sexp>
;;  
;; convert declare {...} in {...} format
;;    into proc (...) (...)
;----------------------------------
(define (desugar [s : Sexp]) : Sexp
  (match s
    [(list 'declare (list (list vars vals) ...) 'in body)
     (cast (cons (list 'proc vars body) vals) Sexp)] 
    [other (error 'desugar "QWJZ4 failed: invalid Sexp for desugar")]))

;----------------------------------
; DESUGAR TEST
;----------------------------------
(check-equal? (desugar '{declare
                         {{z {+ 9 14}}
                          {y 98}}
                         in
                         {+ z y}}) '{{proc (z y) {+ z y}}
                                     {+ 9 14}
                                     98})
#;(desugar '{declare {[a {proc (x) (+ 1 x)}]}
                  in
                  {{a} 1}})

#;(desugar '{declare
             {{z {+ 9 14}}
              {y 98}}
             in
             {+ z y}})
;;(desugar '(declare ((z (+ 9 14)) (y 98)) in (+ z y)))

(check-exn (regexp (regexp-quote "QWJZ4 failed: invalid Sexp for desugar"))
           (lambda () (desugar (cast "this will fail" Sexp))))

;;----------------------------------
;; PARSE
;; Purpose Statement: parse concrete syntax
;;                    into abstract synstax
;; input: <Sexp>
;; output: <ExprC>
;; 
;;----------------------------------

(define (parse [s : Sexp]) : ExprC
  (match s
    [(list 'declare (list define ...) in (list apply ...)) (parse (desugar s))]
    ;;[(list 'println (? string? s)) (PrintC s)]
    
    [(? real? n) (NumC n)]
    #;[(? symbol? sym) (IdC sym)]
    [(? symbol? sym)
     (if (validId? sym)
         (IdC sym)
         (error "QWJZ4 failed: Invalid argument"))]
    #;[(list (? validId? sym)) (cond
                               [(symbol? sym) (IdC sym)]
                               [else (parse sym)]
                               )]
    [(? string? s) (StringC s)]
    
    [(list 'seq exprs ...) (AppC (IdC 'seq) (map parse exprs))]

    [(list 'proc (list args ...) body)
     (match (map (lambda (x) (if (symbol? x) x (error "QWJZ5 failed: Parameters must be symbols"))) args)
       [(list (? symbol? items) ...)  (cond
                            [(duplicates? (cast args (Listof Symbol)))
                             (error 'parse "QWJZ4 failed: repeated parameter in proc definition")]
                            [else (LamC (cast args (Listof Symbol)) (parse body))])])]

    [(list farg num ... ) (AppC (parse-op farg) (map parse num))]
    
    ;;[(list program ...) (parse (first program))]
    
   
    ;;[other (error 'parse "QWJZ4 failed: Invalid expression")]
    ))
;;----------------------------------
;; PARSE-OP
;; Purpose Statement: check to see concrete syntax
;;                    into abstract synstax
;; input: <Sexp>
;; output: <ExprC>
;; 
;;----------------------------------
(define (parse-op [s : Sexp]) : ExprC
  (match s
    [(? symbol? sym) (IdC sym)]
    [other (parse other)]))

(define (duplicates? [lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(member (first lst) (rest lst)) #t]
    [else (duplicates? (rest lst))]))

;;-------------------------------------
;; PARSE TEST
;;-------------------------------------
;;-----PROC TEST-------
#;(check-exn (regexp (regexp-quote "QWJZ4:"))
           (λ () (parse '((pro a (* a 1 4)) 4))) )
;; Test 1
(check-equal? (parse '((proc (z y) (+ z y)) (+ 9 14) 98))
              (AppC (LamC '(z y) (AppC (IdC '+)
                                       (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+) (list (NumC 9) (NumC 14))) (NumC 98))))
;;------IF TEST-------
;; Test 1
(check-equal? (parse '(if true + -))
              (AppC (IdC 'if) (list (IdC 'true) (IdC '+) (IdC '-))))
;; Test 2
(check-equal? (parse '(if true 1 2)) (AppC (IdC 'if)
                                           (list (IdC 'true) (NumC 1) (NumC 2))))
;; Test 3
(check-equal? (parse '(ifleq0? true 1 2)) (AppC (IdC 'ifleq0?)
                                           (list (IdC 'true) (NumC 1) (NumC 2))))
(check-equal? (parse '(equal? true 1 2)) (AppC (IdC 'equal?)
                                           (list (IdC 'true) (NumC 1) (NumC 2))))
;; Test 4---(g) 10
(check-equal? (parse '((g) 10))
              (AppC (AppC (IdC 'g) '()) (list (NumC 10))))

;;-------PRINTLN TEST-------
(check-equal? (parse '(println "s"))
              (AppC (IdC 'println) (list (StringC "s"))))
;; (check-exn (regexp (regexp-quote "QWJZ4 failed: Invalid expression")) (lambda () (parse '(+ if 3))))
;; (check-exn (regexp (regexp-quote "QWJZ4 failed: Invalid expression")) (lambda () (parse '(+ + 1))))
;; 
;; 
;; 
;; 
;; ;;(check-equal? (parse '((proc (x) (x)) 42)) (AppC (LamC '(x) (IdC 'x)) (list (NumC 42))))
 
(check-equal? (parse  '{declare
                                  {{z {+ 9  14}}
                                   {y    98   }}
                                  in
                                  {+ z y}})
                (parse '((proc (z y) (+ z y)) (+ 9 14) 98)))
;; 
#;(desugar '{declare
         {{z {+ 9  14}}
          {y    98   }}
         in
         {+ z y}})

;; 
;; 


;;(check-exn (regexp (regexp-quote "QWJZ3 failed: Invalid expression")) (lambda () (parse '(+ +))))


;This function takes in the bindings and forms the Sexp we need for parse
;;-----------------------------
;; SERIALIZE-NUM

;;-----------------------------
(define (serialize-num [val : Value]) : Number
  "Return a string serialization of the QWJZ4 value val."
  (match val
    [(? number? val) val]
    [(NumC val) val]
    ;;[(boolean? val) (if val 1 0)]
    [_ (error "QWJZ4: cannot serialize value" (format "~v" val))]))
(check-exn (regexp (regexp-quote "QWJZ4"))
           (lambda () (serialize-num (AppC "this should fail" (list (NumC 2))))))
(check-equal? (serialize-num (NumC 1)) 1)

;;-----------------------------
;; SERIALIZE-REAL
;;-----------------------------
(define (serialize-real [val : Value]) : Real
  "Return a string serialization of the QWJZ4 value val."
  (match val
    [(? real? val) val]
    [(NumC n) n]
    [(? boolean? val) (if val 1 0)]
    [else (error "QWJZ4: cannot serialize value" (format "~v" val))]))
(check-exn (regexp (regexp-quote "QWJZ4"))
           (lambda () (serialize-real (AppC "fail" (list (NumC 2))))))
;;-----------------------------
;; SERIALIZE
;;-----------------------------
(define (serialize [val : Value]) : String
  "Return a string serialization of the QWJZ4 value val."
  (match val
    [(? number? n) (~v n)]
    [(NumC n) (number->string n)]
    [(? boolean? b) (if val "true" "false")]
    [(IdC s) (symbol->string s)]
    [(? string? s) s]
    [(ClosV _ _ _) "#<procedure>"]
    [(AppC _ _) "#<AppC>"]
    [(LamC _ _) "#<LamC>"]
    ;;[(PrimOp _ _ _) "#<primop>"]
    [other (error "QWJZ4: fail cannot serialize" val)]))

(check-equal? (serialize (ClosV '() (NumC 1) (Env '()))) "#<procedure>") 

(check-equal? (serialize (ClosV '() (NumC 1) (Env '()))) "#<procedure>") 
(check-equal? (serialize (IdC 'x)) "x")
(check-equal? (serialize (AppC (IdC '+) (list (IdC 'x) (NumC 2))) ) "#<AppC>" ) 
(check-equal? (serialize (LamC '(x y) (NumC 2)) ) "#<LamC>" )
#;(check-exn (regexp (regexp-quote "QWJZ4: failed"))
           (lambda ()(serialize  (PrimOp '+ (NumC 1) (NumC 2)))) )
(check-exn (regexp (regexp-quote "QWJZ4:"))
           (lambda () (serialize  (PrimOp '+ (NumC 1) (NumC 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment and Value Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (lookup [env : Env] [x : Symbol]) : Value
  
  ;; Extract the list from the Env struct
  (define env-list (Env-env env))  

  (match env-list
    ['() 
     (error "QWJZ4: unbound identifier user-error" x )]
    [(list (BindingC name val) rest-bindings ...)
     (if (equal? name x)
         val
         (lookup (Env rest-bindings) x))]))

(check-exn (regexp (regexp-quote "QWJZ4:"))
           (lambda () (lookup top-env 'x)))
;----------------------------------
; EXTENT-EVN
;----------------------------------
;; extent-env helper
(define (bind [params : (Listof Symbol)] [args : (Listof Value)]) : (Listof BindingC)
  (cond
    [(and (empty? params) (empty? params)) '()]
    [(> (length params) (length args)) (error 'QWJZ4 ": Binding error ~v ~v" params args)]   
    [else (cons (BindingC (first params) (first args)) (bind (rest params) (rest args)))])
  )
(check-equal? (bind '(x y) '(1 2)) (list (BindingC 'x 1) (BindingC 'y 2)))
(check-equal? (bind '(x) '(1 2)) (list (BindingC 'x 1) ))
 (check-exn (regexp (regexp-quote "QWJZ4:"))
           (lambda () (bind  '(x y) (list (NumC 1))) ))


(define (extend-env [new-bindings : (Listof BindingC)]
                    [old-env : Env]) : Env
  (Env (append new-bindings (Env-env old-env))))
;----------------------------------
; EXTENT-EVN TEST
;----------------------------------
(define env-test1 (Env (list (BindingC 'true #f))))
#;(check-equal? (extend-env (list (BindingC 'x 2)) env-test1)
                (Env (list (BindingC 'true #f) (BindingC 'x 2))))
#;(check-equal? (extend-env (list (BindingC 'x 2) (BindingC 'y 2)) env-test1)
                (Env (list (BindingC 'true #f) (BindingC 'x 2) (BindingC 'y 2)))
                )
;----------------------------------
; ISARITH? 
;----------------------------------
(define (isArith? [exp : ExprC]) : Boolean
  (match exp
    [(IdC '+) #t]
    [(IdC '-) #t]
    [(IdC '/) #t]
    [(IdC '*) #t]
    
    [other #f]))
;----------------------------------
; ISCOMPARE? 
;----------------------------------
(define (isCompare? [exp : ExprC]) : Boolean
  (match exp
    [(IdC '>) #t]
    [(IdC '<) #t]
    [(IdC '>=) #t]
    [(IdC '<=) #t]
    [other #f]))

;----------------------------------
; ISCONDITIONAL? 
;----------------------------------
(define (isConditional? [exp : ExprC]) : Boolean
  (match exp
    [(IdC 'if) #t]
    [(IdC 'equal?) #t]
    
    
    [other #f]))



;----------------------------------
; EVALUATE
;----------------------------------
(define (evaluate [exp : ExprC] [env : Env]) : Value
  
  (match exp
    [(NumC n) n]

    [(AppC (IdC 'error) (list message ...)) (if (= (length message) 1)
                                                (string-append "user-error" (serialize(interp (first message) env)))
                                    (error "QWJZ4: error" "take exactly 1 arg ")) ]

    [(AppC (IdC '/) (list operand ...)) (if (and (= (length operand) 2))
                                            ;;then
                                            (if (not (= (serialize-num(interp (second operand) env)) 0))

                                                (/ (serialize-num(interp (first operand) env))
                                                 (serialize-num(interp (second operand) env)))
                                                (error "QWJZ4: divide by 0" ))
                                            ;else
                                            (error "QWJZ4: "'/ "take exactly 2 arg")) ]
    [(AppC (? isConditional? (IdC op)) (list test then else)) 
     (let ([test-val (interp test env)])  ; Evaluate the condition expression
       (if (boolean? test-val)  ; Check if the result is a boolean
           (if test-val  ; If true, evaluate the 'then' branch
               (interp then env)
               (interp else env))
           (error "QWJZ5 failed: condition of if must be a boolean")))]

    [(AppC (? isArith? (IdC op)) (list operand ...)) (if (= (length operand) 2)
                                    ((hash-ref prim-operation-table op)
                                              (serialize-num(interp (first operand) env))
                                       (serialize-num(interp (second operand) env)))
                                    (error "QWJZ4: "'+ "take exactly 2 arg ")) ]
    
    [(AppC (? isCompare? (IdC op)) (list operand ...)) (if (= (length operand) 2)
                                    ((hash-ref comparitive-table op)
                                              (serialize-real(interp (first operand) env))
                                              (serialize-real(interp (second operand) env)))
                                    (error "QWJZ4: "'+ "take exactly 2 arg ")) ]
    
    
    [(AppC (IdC 'read-num) '()) (match (read)
                                  [(? real? n) (read-line) n]
                                  [other (error "QWJZ4: input not real number")])
                                ]
    [(AppC (IdC 'read-str) '()) (match (read-line)
                                  [(? string? s)  s]
                                  )
                                ]
    

    ;; New clause for function applications:
    [(AppC f args)
     (let ([fun (interp f env)]
           [argvals (map (lambda ([a : ExprC]) (interp a env)) args)])
       (match fun
         [(ClosV params body f-env)
          (if (= (length params) (length argvals))
              (interp body (extend-env (bind params argvals) f-env))
              (error "QWJZ4: argument mismatch, expected" (length params) "got" (length argvals)))]
         [other (error "QWJZ4: attempted to call a non-closure" fun)]))]

     [(AppC (IdC sym) '()) (lookup env sym)]
                           
    [(AppC (IdC '++) (list exp ...)) (string-join (map serialize (map
                                                                  (lambda ([l : ExprC]) (interp l env))
                                                                  exp)))]
    [(AppC (IdC 'println) (list strs ...)) (cond
                                             [(= (length strs) 1) 
                                              (match (interp (first strs) env)
                                                [(? string? s) (println s) #t]  
                                                [_ (error "QWJZ5 failed: println expects a string")])]
                                             [else (error "QWJZ5 failed: println expects only one argument")])]
    [other (error "QWJZ4: no matching clause")]))
;----------------------------------
; EVALUATE TEST
;----------------------------------
;;(evaluate (AppC (IdC '+) (list (NumC 1) (NumC 2)) ) top-env)
;;(check-equal? (evaluate(AppC (IdC '+) (list (NumC 1) (NumC 3))) top-env) 4)
#;(check-equal? (evaluate(AppC (LamC '(x) (AppC (IdC '+)
                                              (list (IdC 'x) (NumC 2))))
                             (list (NumC 2))) top-env) 4)
#;(check-exn (regexp (regexp-quote "QWJZ4:"))
           (lambda () (evaluate(AppC (IdC '+) (list (IdC 'x) (NumC 3))) top-env)))


;----------------------------------
; EVALARITH 
;----------------------------------

;----------------------------------
; INTERP
;----------------------------------
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumC n) n]
    [(AppC (IdC 'equal?) (list first second)) (if (equal? (serialize (interp first env))
                                                                (serialize (interp second env)))
                                                  #t
                                                  #f)]

    [(AppC (IdC '++) (list strs ...)) 
   (if (empty? strs)
       (error "QWJZ4: ++ expects at least one argument")
       (string-join 
        (map (lambda (l) 
               (serialize (interp (cast l ExprC) env))) strs) ""))
]


    [(AppC (IdC 'println) (list strs ...)) 
     (cond
       [(= (length strs) 1) 
        (match (interp (first strs) env)
          [(? string? s) (println s) #t]  
          [_ (error "QWJZ5 failed: println expects a string")])]
       [else (error "QWJZ5 failed: println expects only one argument")])]

    [(IdC n) (lookup env n)]

    [(StringC s) s]

    [(ClosV parameters body e) (interp body e)]

    [(LamC (list args ...) body) (ClosV args body env)]

    [(AppC exprc (list args ...)) (match exprc
                                    [(NumC s) (error "QWJZ4: invalid expresion" exp)]
                                    [(LamC (list parameters ...) body)
                                     (if
                                      (not (= (length parameters) (length args)))

                                      (error "QWJZ4: no parameter to pass in")
                                      (interp (ClosV parameters
                                                     body
                                                     (extend-env (bind parameters
                                                                       (map (lambda ([la : ExprC]) (interp la env))
                                                                            args))
                                                                 env))
                                              env))]
                                    [(IdC s) (evaluate exp env)]
                                    [other (error "QWJZ4")]

                                    )]


    [other (error "QWJZ4: cant interp" exp)]
    ))
;----------------------------------
; INTERP TEST
;----------------------------------
;;(desugar '(declare { [a {read-num}]} in {a}))
;;(parse '(declare { [a {read-num}]} in {a})) 
(check-equal? (interp (LamC '(a) (IdC 'a)) top-env) (ClosV '(a) (IdC 'a) top-env))
;;(parse '((proc (z y) (+ z y)) (+ 9 14) 98))
;;(interp (parse '((proc (z y) (+ z y)) (+ 9 14) 98)) top-env)
;;(serialize(interp (parse '((proc (z y) (+ z y)) (+ 9 14) 98)) top-env))


;----------------------------------
; SUBSTITUTION
;----------------------------------
#;(define (subst [lam-parameter : (Listof Symbol)] [value : (Listof Expr)] [exp : Expr ]) : ExprC
    (match exp
      [(IdC (? valid? sym)) ()]))

#;(define (subst-LamC [lam : LamC] [args : (Listof ExprC)]) : LamC
    (match lam
      [(= (empty? args) 0) lam]
      [(LamC (list sym ...) body) (subst sym args body)])
    )
;----------------------------------
; INTERP-TEST
;----------------------------------
;;(check-equal? (interp (LamC '() (PrimOp '+ (NumC 1) (NumC 2))) top-env) 3)
;;(check-equal? (interp (LamC '() (PrimOp '+ (NumC 2) (NumC 2))) top-env) 4)
;;(check-equal? (interp (LamC '() (PrimOp '+ (IdC 'x) (NumC 2))) top-env) 3)
#;(check-exn (regexp (regexp-quote "QWJZ4:"))
             (lambda () (interp (LamC '() (PrimOp '+ (IdC 'x) (NumC 2))) top-env)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-INTERP
;; Top–Level Interpreter
;;
;; top-interp : Sexp -> String
;;
;; Parses the S-expression s, evaluates it in the top–level environment, and
;; returns its serialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (top-interp [s : Sexp]) : String 
  "Parse s (an Sexp), interpret it in the top–level environment, and return its serialization."
  
  (serialize (interp (parse s) top-env)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-INTERP TEST 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(parse '((proc (y) (y)) 9))
;;(parse '((p) 10))
;;(top-interp '(g))
;;(top-interp '((proc (a) (a)) 1))

(check-equal? (top-interp '((proc (a) (+ a 1)) 1)) "2")
(check-equal? (top-interp '((proc (a) (- a 1)) 1)) "0")

(check-equal? (top-interp '((proc (a) (/ a 1)) 2)) "2")
(check-equal? (top-interp '((proc (a) (* a 1)) 4)) "4")
;; 

(check-exn (regexp (regexp-quote "QWJZ4:"))
           (λ () (top-interp '((proc (a) (* a 1 4)) 4))) )

(check-equal? (top-interp '((proc (a) (> a 1)) 1)) "false")
(check-equal? (top-interp '((proc (a) (< a 1)) 1)) "false")
(check-equal? (top-interp '((proc (a) (>= a 1)) 2)) "true")
(check-equal? (top-interp '((proc (a) (<= a 1)) 4)) "false")
(check-exn (regexp (regexp-quote "QWJZ4:"))
           (λ () (top-interp '((proc (a) (> a 1 4)) 4))) )
;; 

(check-exn (regexp (regexp-quote "QWJZ4:"))
           (λ () (top-interp '((proc (a) (* a 1 4)) 4))) )
(check-exn (regexp (regexp-quote "QWJZ4:"))
           (λ () (top-interp 'aklsdj)) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRINT TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(check-equal? (top-interp '(println "hello")) "true" )
#;(check-equal? (top-interp '(println {++ "hello" "Duy"})) "true" )
#;(check-exn (regexp (regexp-quote "QWJZ5 failed: println expects only one argument"))
           (lambda () (top-interp '(println "hello" "world"))))
(check-exn (regexp (regexp-quote "QWJZ5 fail"))
           (lambda () (top-interp '(println 123))))

#;(check-equal? (top-interp '(declare {[g 10] [z 11]} in (println {++ "g" g "z" z}))) "true" )


;; Basic literals

;(check-equal? (top-interp 34) "34")

 ;(check-equal? (top-interp 'true) "true") ; 'true is looked up in top-env
;; ;; 
;; ;; ;; if–expressions
;; 
;(check-equal? (top-interp '(if true false txqrue)) "false")
;(check-equal? (top-interp '(if false false true)) "true")
;(check-equal? (top-interp '(if 1 2 3)) "2")
;; ;; 
;; ;; ;; Primitive arithmetic operators
;(check-equal? (top-interp '(+ 1 2)) "3")
;(check-equal? (top-interp '(- 10 3)) "7")
;(check-equal? (top-interp '(* 3 4)) "12")


#;(top-interp '{declare {[your-number {read-num}]
                       [s {read-str}]
                       }
          in
          {println {++ "Interesting, you picked " your-number s ". Bold choice!"}}})


;; 
#;(check-equal? (top-interp '(declare ((z (+ 9 14)) (y 98)) in (+ z y))) "121")



#;(top-interp '{seq
              
              {println "Enter a number: "}
              {declare {[n {read-num}]
                        [s {read-str}]}
                       in
                       {println {++ "You enter" n}}}})
              
#;(check-exn (regexp (regexp-quote "QWJZ4"))
           (lambda () (top-interp '(println 123))))

;(top-interp (quote ((proc (minus) (minus 8 5)) (proc (a b) (+ a (* -1 b))))))

;(top-interp '((declare ((y 9)) in (f 3))))

;(top-interp '(+ 4 (error "1234")))

;(top-interp '((proc (e) (e e)) error))



(top-interp `{declare
 {[empty 15]}
 in
 {declare
  {[empty? {proc (x) {equal? x empty}}]
   [cons {proc (f r)
               {proc (key)
                     {if {equal? key 0}
                         f
                         r}}}]
   [first {proc (pair) {pair 0}}]
   [rest  {proc (pair) {pair 1}}]}
  in
  {declare
   {[sum-list {proc (l self)
                    {if {empty? l}
                        0
                        {+ {first l}
                           {self {rest l} self}}}}]
    [my-list {cons 3 {cons 24 {cons 8 empty}}}]}
   in
   {println {++ "The sum of the list is " {sum-list my-list sum-list} "."}}}}})






(define example-program
  '{seq {println "Please enter a number and I will guess it"}
        {declare {[user-number {read-num}]} in
        {seq {println "Finding possible options..."}
             {println "Analyzing brainwaves..."}
             {println "Running equations..."}
             {println {++ "Your number is" user-number}}}}})

;(top-interp example-program)


;;THIS IS WHAT OUR EXAMPLE PROGRAM OUTPUTS
#;"Please enter a number and I will guess it"
#;342
;"Finding possible options..."
;"Analyzing brainwaves..."
;"Running equations..."
;"Your number is 342"

