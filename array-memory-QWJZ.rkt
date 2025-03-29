#lang typed/racket
;; Full project implemented.

(require typed/rackunit)
(require racket/list)
(require typed/racket)

;; =============================================================================
;; Data Types and Helpers
;; =============================================================================

;; Sexp represents an S‐expression.


;; ExprC represents the abstract syntax tree for QWJZ4 expressions.
(define-type ExprC
  (U NumC IfC LamC AppC IdC StringC AssignC))

(define-type TypeT
  (U NumT BoolT StringT FuncT))
;;Abstract Syntax
(struct NumC ([n : Number]) #:transparent)
(struct IdC ([s : Symbol])#:transparent)
(struct IfC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([arg : (Listof Symbol)] [body : ExprC])#:transparent)
(struct StringC ([s : String]) #:transparent)
(struct AssignC ([id : Symbol] [exp : ExprC]) #:transparent)
;;Value 
(struct ArrayV ([pointer : Natural] [size : Natural]) #:transparent)
(struct Store ([mem : (Vectorof Value)])#:transparent)
(struct NullV ([s : Symbol]) #:transparent)
(struct NumV ([v : Number])#:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
;; Type
(struct NumT ([n : Number]) #:transparent)
(struct BoolT ([b : Boolean]) #:transparent)
(struct StringT ([s : String]) #:transparent)
(struct IdT ([s : Symbol]) #:tramsparent)
(struct FuncT ([argsT :( Listof TypeT)] [return : TypeT]))


;; An environment is a list of bindings mapping identifiers to values.
(define-type Env (Listof (Pairof Symbol Value )))

;; Value represents the run-time values in QWJZ4.
(define-type Value
  (U NumV
     BoolV
     String
     Closure
     PrimOp
     StringC
     ArrayV
     NullV
     ))

;; A Closure stores a function’s parameter list, its body, and the defining env.
(struct Closure ([params : (Listof Symbol)]
                 [body   : ExprC]
                 [env    : Env]) #:transparent)

;; A PrimOp represents a primitive operator.
(struct PrimOp ([fn    : (-> (Listof Value) Store Value)]
                
                [name  : String]) #:transparent)

;-------------DESUGAR-----------------
(define (desugar [s : Sexp]) : Sexp
  (match s
    [(list 'declare (list (list vars vals) ...) 'in body)
     (cast (cons (list 'proc vars body) vals) Sexp)] 
    [other (error "QWJZ4 failed: invalid Sexp for desugar")]))
;-------------Dubplicates?-----------------
(define (duplicates? [lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(member (first lst) (rest lst)) #t]
    [else (duplicates? (rest lst))]))
;; =============================================================================
;; Serialization
;; =============================================================================

;; serialize : Value -> String
;; Returns a string representation of a QWJZ4 value.
(define (serialize [v : Value]) : String
  (match v
    [(NumV v) (number->string v)]
    [(BoolV v) (if v "true" "false")]
    [(? string?) (format "\"~a\"" v)]
    [(NullV n) (symbol->string n)]
    [(Closure _ _ _) "#<procedure>"]
    [(PrimOp f n) "#<primop>"]
    [(StringC str) (format "\"~a\"" str)]
    ;;[else (error 'serialize  (format "QWJZ: Unknown value type: ~v" v))]
    ))
;; =============================================================================
;; Primitive Operators
;; =============================================================================

;; Each primop checks its argument types and signals errors with "QWJZ" in the message.
;; -----------------------------------------------------------------------------
;;  PRIM-Plus
;; -----------------------------------------------------------------------------
(define (prim-plus args sto)
  (match args
    [(list (NumV a) (NumV b)) (NumV (+ a b))]
    [_ (error (format "QWJZ: Incorrect number of arguments or not NumV for +: ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM-minus
;; -----------------------------------------------------------------------------
(define (prim-minus args sto)
  (match args
    [(list (NumV a) (NumV b)) (NumV ( - a b))]
    [_ (error (format "QWJZ: Incorrect number of arguments or not NumV for -: ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM-mult
;; -----------------------------------------------------------------------------
(define (prim-mult args sto)
  (match args
    [(list (NumV a) (NumV b)) (NumV ( * a b))]
    [_ (error (format "QWJZ: Incorrect number of arguments or not NumV for *: ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM-div
;; -----------------------------------------------------------------------------
(define (prim-div args sto)
  (match args
    [(list (NumV a) (NumV b))
     (if (= b 0)
         (error 'prim-div "QWJZ: Division by zero")
         (NumV (/ a b)))]
    [_ (error (format "QWJZ: Incorrect number of arguments or not NumV for /: ~a" args))]))
;; -----------------------------------------------------------------------------
;;  PRIM-<=
;; -----------------------------------------------------------------------------
(define (prim-<= args sto)
  (match args
    [(list (NumV a) (NumV b)) (BoolV (<= (cast a Real) (cast b Real)))]
    [_ (error (format "QWJZ: Incorrect number of arguments or not NumV for <=: ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM-<
;; -----------------------------------------------------------------------------
(define (prim-< args sto)
  (match args
    [(list (NumV a) (NumV b)) (BoolV(< (cast a Real) (cast b Real)))]
    [_ (error 'prim-<= "QWJZ: Incorrect number of arguments for <")]))

;; -----------------------------------------------------------------------------
;;  PRIM->
;; -----------------------------------------------------------------------------
(define (prim-> args sto)
  (match args
    [(list (NumV a) (NumV b)) (BoolV (> (cast a Real) (cast b Real)))]
    [_ (error (format "QWJZ: prim-> Incorrect number of arguments for: ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM->=
;; -----------------------------------------------------------------------------
(define (prim->= args sto)
  (match args
    [(list (NumV a) (NumV b)) (BoolV (>= (cast a Real) (cast b Real)))
         ]
    [_ (error (format "QWJZ: Incorrect number of arguments for >= ~a" args))]))

#;(define (prim-if [args : (Listof Value)]) : Value
  (match args
    [(list (? boolean? a) b c)
     (if a
         b
         c)]
     [other (error "QWJZ: Wrong type or wromg number of arguments of if" args)]))
;; -----------------------------------------------------------------------------
;;  PRIM-SUBSTRING
;; -----------------------------------------------------------------------------
(define (prim-substring args sto)
  (match args
    [(list s (NumV startv) (NumV stopv))
     (let ([start (cast startv Real)]
           [stop (cast stopv Real)])
      (if (not (string? s))
         (error "QWJZ: First argument must be a string in substring" s)
         (let ([len (string-length s)])
           (if (or (< start 0) (> start len)
                   (< stop 0) (> stop len)
                   (< stop start))
               (error
                (format "QWJZ: Indices out of range or stop before start in substring:
                                         start=~a, stop=~a, string=~a"
                        start stop s))
               (substring s (cast start Integer) (cast stop Integer))))))]
    [_ (error "QWJZ: Incorrect number of arguments or incorect type for substring" args)]))

;; -----------------------------------------------------------------------------
;;  PRIM-STRLEN
;; -----------------------------------------------------------------------------
(define (prim-strlen args sto)
  (match args
    [(list s)
     (if (string? s)
         (NumV (string-length s))
         (error "QWJZ: Argument must be a string in strlen" s))]
    [_ (error 'prim-strlen "QWJZ: Incorrect number of arguments for strlen")]))

;; -----------------------------------------------------------------------------
;;  PRIM-EQUAL?
;; -----------------------------------------------------------------------------
(define (prim-equal? args sto) : BoolV
  (match args
    [(list a b)
     (cond
       [(and (NumV? a) (NumV? b)) (BoolV (= (NumV-v a) (NumV-v b)))]
       [(and (BoolV? a) (BoolV? b)) (BoolV (equal? a b))]
       [(and (string? a) (string? b)) (BoolV (string=? a b))]
       [(and (ArrayV? a) (ArrayV? b)) (BoolV (equal? (ArrayV-pointer a) (ArrayV-pointer b)))]
       [(and (NullV? a) (NullV? b)) (BoolV (equal? a b))]
       [else (BoolV #f)])]
    [_ (error 'prim-strlen "QWJZ: Incorrect number of arguments for equal?")]))

;; -----------------------------------------------------------------------------
;;  PRIM-ERROR
;; -----------------------------------------------------------------------------    
(define (prim-error args sto)
  (match args
    [(list v)
     (error  (format "QWJZ: user-error ~a" v))]
    [_ (error 'prim-strlen "QWJZ: Incorrect number of arguments for error")]))

;; -----------------------------------------------------------------------------
;;  PRIM-PRINLN
;; -----------------------------------------------------------------------------
(define (prim-println [args : (Listof Value)] [sto : Store]) : Value
  (if (= (length args) 1)
      (match (first args)
        [(? string? s) (println s) (BoolV #t)] 
        [(NumV s) (println s) (BoolV #t)]
        
        [(BoolV s) (if s
                       (println "true")
                       (println "false"))
                   (BoolV s)]
        [other (error (format "QWJZ: Incorrect type ~a" args))]
        )
      (error (format "QWJZ: Incorrect number of arguments ~a" args)))
  )
;; -----------------------------------------------------------------------------
;;  PRIM-SEQ
;; -----------------------------------------------------------------------------
(define (prim-seq [statements : (Listof Value)] [sto : Store]) : Value
  (last statements))
;; -----------------------------------------------------------------------------
;;  ALLOCATE
;; allocate : Store amount -> location
;; allocate takes in a Store and amount and allocates that amount, returning the new base location
;; -----------------------------------------------------------------------------
(define (allocate [store : Store] [slots : Real]) : Number
  (match (vector-ref (Store-mem store) 0)
    [(NumV v)
     (if (> (cast (+ v slots) Real) (vector-length (Store-mem store)))
         (error "QWJZ out of memory: not enough space in store")
         (vector-set! (Store-mem store) 0 (NumV (+ v slots))))
     (- (NumV-v (cast (vector-ref (Store-mem store) 0) NumV)) slots)]))

;; -----------------------------------------------------------------------------
;;  PRIM-MAKE-ARRAY
;; -----------------------------------------------------------------------------
(define (prim-make-array [args : (Listof Value)] [store : Store]) : Value
  (match args
    [(list (NumV n) init-val)
     (if (< (cast n Real) 1)
         (error "QWJZ6 failed: Array size must be at least 1")
         (let ([base (allocate store (+ (cast n Natural) 1))])
           (vector-set! (Store-mem store) (cast base Integer) (ArrayV (cast (+ base 1) Natural) (cast n Natural)))
           (for ([i (in-range (cast n Real))])
             (vector-set! (Store-mem store) (cast (+ base 1 i) Integer) init-val))
           (ArrayV (cast (+ base 1) Natural) (cast n Natural))))]
    [_ (error "QWJZ make-array expects exactly 2 arguments: a number and an initial value")]))

;; -----------------------------------------------------------------------------
;;  PRIM-ARRAY
;; -----------------------------------------------------------------------------
(define (prim-array [args : (Listof Value)] [store : Store] ) : Value
  (match args
    ['() (error "QWJZ6 failed: array expects one or more arguments")]
    [(list vals ...) 
     (let* ([n (+ (length vals) 1)]
            [base (allocate store (+ n 1))])
       (vector-set! (Store-mem store) (cast base Integer) (ArrayV (+ (cast base Natural) 1) n))
       (for ([i (in-range (- n 1))])
         (vector-set! (Store-mem store) (+ (cast base Integer) 1 i)  (list-ref vals i)))
       (ArrayV (cast (+ base 1) Natural) n))]))

;; -----------------------------------------------------------------------------
;;  PRIM-AREF
;; -----------------------------------------------------------------------------
(define (prim-aref  [args : (Listof Value)] [store : Store]) : Value
  (match args
    [(list (ArrayV pointer size) (NumV index))
     (if (or (< (cast index Real) 0) (>= (cast index Real) size))
         (error "QWJZ6 failed: Index out of bounds")
         (vector-ref (Store-mem store) (cast (+ pointer index) Integer)))]
    [_ (error (format "QWJZ6 failed: aref -> Provided value is not an array ~a" args))]))

;; -----------------------------------------------------------------------------
;;  PRIM-ASET!
;; -----------------------------------------------------------------------------
(define (prim-aset! [args : (Listof Value)] [store : Store] ) : Value
  (match args
    [(list (ArrayV pointer size) (NumV index)  new-value)
     (if (or (< (cast index Real) 0) (>= (cast index Real) size))
         (error (format "QWJZ6 failed: Index out of bounds: ~a" index))
         (begin
           (vector-set! (Store-mem store) (cast (+ pointer index) Integer) new-value)
           (NullV 'null)))]
    [_ (error "QWJZ6 failed: Provided value is not an array or incorrect number of arguments")]))

;; =============================================================================
;; LOOK UP
;; =============================================================================

;; lookup : Symbol Env -> Value
;; Looks up an identifier in the environment to return NumV position refer to
;; actual value in the store
;; With that NumV position: Look up in the position of the store
;; return the actual Value that binded with the symbol
;; or signals an error if unbound.

(define (lookup [id : Symbol] [env : Env] [sto : Store]) : Value
  (cond
    [(empty? env)
     (error 'lookup (format "QWJZ: Unbound identifier ~a" id))]
    [else
     (let ([binding (first env)])
       (if (eq? (car binding) id)
           (match (cdr binding)
             [(NumV v) (vector-ref (Store-mem sto) (cast v Integer))]
             ;;[other (error (format "~a is not NumV" (cdr binding)))]
             )
           (lookup id (rest env) sto)))]))
;; =============================================================================
;; LOOK UP POSITION
;; base on NumV [v : Number] lookup the position [v] in Store
;; return the Value is store in Store
;; =============================================================================
(define (lookup-pos [id : Symbol] [env : Env] [sto : Store]) : Value
  (cond
    [(empty? env)
     (error (format "QWJZ: Unbound identifier ~a" id))]
    [else
     (let ([binding (first env)])
       (if (eq? (car binding) id)
           (match (cdr binding)
             [(NumV v) (NumV v)]
             ;;[other (error (format "~a is not NumV" (cdr binding)))]
             )
           (lookup-pos id (rest env) sto)))]))

;; =============================================================================
;; notValid?
;; =============================================================================
(define (notValid? [s : (Listof Any)]) : Boolean
  (if (empty? s) #f
      (match (first s)
        
        ['if #t]
        ['declare #t]
        ['proc #t]
        ['in #t]
        
        [other (notValid? (rest s))])))

;; =============================================================================
;; numOp?
;; =============================================================================
(define (numOp? [op : Sexp]) : Boolean
  (match op
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['<= #t]
    [_ #f]))
;; =============================================================================
;; PARSE-TYPE
;; =============================================================================
(define ([parse-type] ([ exp : Sexp])) : TypeT
  (match s
    ;; 'Declare
    [(list 'declare (list define ...) 'in body ...) (parse (desugar s))]
    ;; ------------
    ;;[(list sym ...)]
    [(? string? s) (StringT s)]
    [(? symbol? sym) (IdT sym)]
    [(? real? n) (NumT n)]
    ;; Assignment form: (:= l exp)
    #;[(list id ':= exp)
     (if (symbol? id)
         (AssignC id (parse exp))
         (error "QWJZ: Left-hand side of := must be an identifier"))]
    ;; If
    [(list 'if test then else) (if (notValid? (list test)) (error "QWJZ4 failed: conditon for if is invalid")
                                   (IfC (parse test) (parse then) (parse else)))]
    ;; LamC
    [(list 'proc (list args ...) body)
     (match (map (lambda (x) (if (symbol? x) x (error "QWJZ4 failed: Parameters must be symbols"))) args)
       [(list (? symbol? items) ...)  (cond
                                        [(duplicates? (cast args (Listof Symbol)))
                                         (error 'parse "QWJZ4 failed: repeated parameter in proc definition")]
                                        [(notValid? items)
                                         (error 'parse "QWJZ4 failed: argument is reserved keyword") ]
                                        [else (LamC (cast args (Listof Symbol)) (parse body))])])]
    ;;[(list  exp (list args ...) ) (AppC (parse exp) (map parse args))]
    
    [(list exp arg ...) (cond
                          [(equal? exp 'proc) (error "QWJZ4 failed: invalid syntax for proc")]
                          [(equal? exp 'if) (error "QWJZ4 failed: invalid syntax for if")]
                          [(and (numOp? exp) (notValid? arg)) (error (format "QWJZ: ~a not valid" arg))]
                          [else (AppC (parse exp) (map parse arg))])]    
   
    ;[other (error 'parse "QWJZ4 failed: Invalid expression" s)]
    ))
;; =============================================================================
;; Parser (with Desugaring for 'declare')
;; =============================================================================

;; parse : Sexp -> ExprC
;; Parses an S-expression into an AST for QWJZ4.
(define (parse [s : Sexp]) : ExprC
  (match s
    ;; 'Declare
    [(list 'declare (list define ...) 'in body ...) (parse (desugar s))]
    ;; ------------
    ;;[(list sym ...)]
    [(? string? s) (StringC s)]
    [(? symbol? sym) (IdC sym)]
    [(? real? n) (NumC n)]
    ;; Assignment form: (:= l exp)
    [(list id ':= exp)
     (if (symbol? id)
         (AssignC id (parse exp))
         (error "QWJZ: Left-hand side of := must be an identifier"))]
    ;; If
    [(list 'if test then else) (if (notValid? (list test)) (error "QWJZ4 failed: conditon for if is invalid")
                                   (IfC (parse test) (parse then) (parse else)))]
    ;; LamC
    [(list 'proc (list args ...) body)
     (match (map (lambda (x) (if (symbol? x) x (error "QWJZ4 failed: Parameters must be symbols"))) args)
       [(list (? symbol? items) ...)  (cond
                                        [(duplicates? (cast args (Listof Symbol)))
                                         (error 'parse "QWJZ4 failed: repeated parameter in proc definition")]
                                        [(notValid? items)
                                         (error 'parse "QWJZ4 failed: argument is reserved keyword") ]
                                        [else (LamC (cast args (Listof Symbol)) (parse body))])])]
    ;;[(list  exp (list args ...) ) (AppC (parse exp) (map parse args))]
    
    [(list exp arg ...) (cond
                          [(equal? exp 'proc) (error "QWJZ4 failed: invalid syntax for proc")]
                          [(equal? exp 'if) (error "QWJZ4 failed: invalid syntax for if")]
                          [(and (numOp? exp) (notValid? arg)) (error (format "QWJZ: ~a not valid" arg))]
                          [else (AppC (parse exp) (map parse arg))])]    
   
    ;[other (error 'parse "QWJZ4 failed: Invalid expression" s)]
    ))
;; =============================================================================
;; Top-Level Interface
;; =============================================================================
;; =============================================================================
;; Top-Level Environment
;; =============================================================================

;; top-env binds the QWJZ4 primitives as well as the literal booleans.
(define top-env : Env
  (list
   (cons '+ (PrimOp prim-plus "+"))
   (cons '- (PrimOp prim-minus "-"))
   (cons '* (PrimOp prim-mult "*"))
   (cons '/ (PrimOp prim-div "/"))
   (cons '<= (PrimOp prim-<= "<="))
   (cons '< (PrimOp prim-< "<"))
   (cons '> (PrimOp prim-> ">"))
   (cons '>= (PrimOp prim->= ">="))
   
   (cons 'substring (PrimOp prim-substring "substring"))
   (cons 'strlen (PrimOp prim-strlen "strlen"))
   (cons 'equal? (PrimOp prim-equal? "equal?"))
   (cons 'error (PrimOp prim-error "error"))
   (cons 'make-array (PrimOp prim-make-array "make-array"))
   (cons 'aref (PrimOp prim-aref "aref"))
   (cons 'aset! (PrimOp prim-aset! "aset~"))
   (cons 'array (PrimOp prim-array "array"))
   
   (cons 'println (PrimOp prim-println "println"))
   (cons 'seq (PrimOp prim-seq "seq"))
   ;;(cons '++ (PrimOp prim-++ "++"))
   (cons 'true (BoolV #t))
   (cons 'false (BoolV #f))
   
   ))

;; =============================================================================
;; MUTATE
;; =============================================================================
(define (mutate [var : Symbol] [val : Value] [env : Env] [store : Store]) : NullV
  (match (lookup-pos var env store)
    [(NumV pos) (begin
                  ;;(println (vector-length (Store-mem store)))
                  (vector-set! (Store-mem store) (cast pos Integer) val)
                  (vector-set! (Store-mem store) 0 (NumV (+ pos 1)))
                ;;(println (vector-ref (Store-mem store)  (cast pos Integer)))
                  (NullV 'null))])
  
  )

;; =============================================================================
;; INTERP
;; =============================================================================
;; interp : ExprC Env -> Value
;; Evaluates an expression in the given environment.
(define (interp [e : ExprC][env : Env] [sto : Store]) : Value
  (match e
    [(NumC n) (NumV n)]
    
    [(IdC s) (lookup s env sto)]
    [(StringC s) s]
    [(IfC test then else)
     
     (let ([cond-val (interp test env sto)])
       (cond
         [(BoolV? cond-val) (if (BoolV-b cond-val)
                        (interp (IfC-then e) env sto)
                        (interp (IfC-else e) env sto))]
         [else (error (format "QWJZ: condition is not BoolV: ~a" cond-val))]
         ))]
    [(AssignC s expresC) (begin
                            (mutate s (interp expresC env sto) env sto)
                            ;;(println env)
                            ;;(println sto)
                            (NullV 'null))]
    [(LamC arg exp)
     (Closure arg exp env)]
    ;;[(list exp ...) (interp )]
    [(AppC fun args)
     (cond
       
       #;[(equal? fun (IdC 'if))
         (let ([test (interp (first args) env)])
            (cond
              [(equal? test #t) (interp (second args) env)]
              [(equal? test #f) (interp (third args) env)]
              [else (error (format "QWJZ: condition is not valid ~a" test))]
                
                ))]
       #;[(equal? fun (IdC '++)) (string-join (map serialize (map
                                                            (lambda ([l : ExprC]) (interp l env))
                                                            args)))]
      
       [else (let (
                   [fun-val (interp fun env sto)]
                   [arg-vals (map (λ ([arg : ExprC]) (interp arg env sto)) args)])
           (cond
             
             [(Closure? fun-val)
              (let ([params (Closure-params fun-val)]
                    [body   (Closure-body fun-val)]
                    [closure-env (Closure-env fun-val)])
                (if (not (= (length params) (length arg-vals)))
                    (error (format "QWJZ: Wrong number of arguments. Expected ~a, got ~a in expression: ~v"
                                   (length params) (length arg-vals) fun))
                    (let ([new-bindings (map (λ ([p : Symbol] [a : Value]) (cons p (alloc sto a)))
                                         params arg-vals)])
                      (let
                          ([new-e (append new-bindings closure-env)])
                        ;;(println new-e)
                        ;;(println sto)
                        (interp body new-e sto)
                        ))))]
             
             [(PrimOp? fun-val)
                  ;;caling helper on arg-vals
                  ;(println arg-vals)
                  ((PrimOp-fn fun-val) arg-vals sto)]
             
             [else
              (error (format "QWJZ: Attempted to call a non-function value: ~v" fun-val))]))])]
    ;;[other (error (format "QWJZ: Unknown expression type: ~v" e))]
    ))

;; -----------------------------------------------------------------------------
;;  ALLOC
;; alloc : (Vectorof Value) Value -> Loc
;; Allocates a new cell in the store for val and returns its index.
;; -----------------------------------------------------------------------------

(define (alloc [sto : Store] [val : Value]) : Value
  (let ([mem (Store-mem sto)]
        [next (vector-ref (Store-mem sto) 0)])
    (match next
      [(NumV n)(if (>= (cast n Real) (vector-length mem))
                          (error 'alloc "QWJZ: Out of memory")
                          (begin
                            
                            (vector-set! (Store-mem sto) (cast n Integer) val)
                            (vector-set! (Store-mem sto) 0 (NumV (+ n 1)))
                            next
                            ))]
      
    )))

;; -----------------------------------------------------------------------------
;;  MAKE-INITIAL-STORE
;; The store is a mutable vector that holds Value.
;; We reserve index 0 to hold the next free index.
;; -----------------------------------------------------------------------------
(define (make-initial-store [memsize : Natural]) : Store
  (let: ([sto : (Vectorof Value) (make-vector memsize (NullV 'null))])
    (vector-set! sto 0 (NumV 1))
    (Store sto)))
;;---------------------------------------------------------------------
;;  MAKE-INITIAL-ENV
;; initial-env (top-env) : Env
;; Builds the top-level environment by allocating each binding in the store.
;; -----------------------------------------------------------------------------
(define empty-env : Env '())

;;(: initial-env ((Vectorof Value) -> Env))
(define (make-initial-env [sto : Store]) : Env
  (foldl (lambda ([binding : (Pairof Symbol Value)]
                  [env : Env]) : Env 
           (match binding
             [(cons sym val)
              ;; Alloc returns a Loc
              (cons (cons sym (alloc sto val)) env)]))
         '()
         top-env))
;; -----------------------------------------------------------------------------
;;  TOP-INTERP 
;; -----------------------------------------------------------------------------
(define (top-interp [s : Sexp] [memsize : Natural]) : String
  (define initial-mem (make-initial-store memsize))
  (define initial-env (make-initial-env initial-mem))
  ;;(println initial-env)
  ;;(println initial-mem)
  (serialize (interp (parse s) initial-env initial-mem)))


;; -----------------------------------------------------------------------------
;;  PARSE TEST
;; -----------------------------------------------------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () (parse '(proc (i) "Hello" 31/7 +))))

(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (parse '(if "" 3))))

;;=============================================================================
;; ------------------------------TEST-------------------------------------------
;; =============================================================================


;; =============================================================================
;; DEFINE memory-size
;; =============================================================================
(define memory-size 100)

;; =============================================================================
;; Test Cases (using check-equal?)
;; =============================================================================
;; 
;; Numbers and strings
(check-equal? (top-interp "hello" memory-size) "\"hello\"")

;;Booleans from the top-level env
 (check-equal? (top-interp 'true memory-size) "true")
 (check-equal? (top-interp 'false memory-size) "false")

;;if-expressions
(parse '(if true 1 2))
(check-equal? (top-interp '(if true 1 2) memory-size) "1")
(check-equal? (top-interp '(if false 1 2) memory-size) "2")
(check-exn (regexp (regexp-quote "QWJZ4"))
           (lambda () (parse '(if in 0 1))))

;; Procedures and application
(check-equal? (top-interp '((proc (x) (+ 1 1)) 5) memory-size) "2")


;;declare (desugared to a proc application)

(check-equal? (top-interp '(declare ([x 10]) in (+ x 5)) 100) "15")
(check-equal? (top-interp '(declare ([x 2] [y 3]) in (+ x y)) 100) "5")

;; Verify closures serialize properly.
(define inc (parse '(proc (x) (+ x 1))))
(define initial-mem (make-initial-store memory-size))
(define initial-env (make-initial-env initial-mem))
(define inc-closure (interp inc initial-env initial-mem))
(check-equal? (serialize inc-closure) "#<procedure>")

;; =============================================================================
;; ERROR TEST 
;; =============================================================================
;; parse error test
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp  '(parse '(if in 0 1)) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp  '(parse '(proc (i) "Hello" 31/7 +)) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp  '(parse '(if "" 3)) memory-size)))
;; error test
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp  '(top-interp '(+ 4 (error "1234"))) memory-size)))
;; 
;; 
;; declare test
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare (y 3) in (+ x y)) memory-size)))
(check-equal? (top-interp (quote (declare ((z (proc () 3)) (q 9)) in (+ (z) q))) memory-size) "12")



;; =============================================================================
;; PRIMOP TEST 
;; =============================================================================

(check-equal? (top-interp '(+ 3 4) memory-size) "7")
(check-equal? (top-interp '(- 1 4) memory-size) "-3")
(check-equal? (top-interp '(/ 4 4) memory-size) "1")
(check-equal? (top-interp '(* 3 4) memory-size) "12")
(check-equal? (top-interp '(<= 3 4) memory-size) "true")
(check-equal? (top-interp '(>= 3 4) memory-size) "false")
(check-equal? (top-interp '(> 3 4) memory-size) "false")
(check-equal? (top-interp '(< 3 4) memory-size) "true")
(check-equal? (top-interp '(> 3 4) memory-size) "false")
(check-equal? (top-interp '(equal? "hello" 4) memory-size) "false")
(check-equal? (top-interp '(if  (<= 0 0) 13 (/ 3 0)) memory-size) "13")
(check-equal? (top-interp '(if  (<= 0 2) 13 (/ 3 0)) memory-size) "13")

;;--------PRIMOP FUNCTION TEST---------

(check-equal? (top-interp '(substring "hello" 1 4) memory-size) "\"ell\"")
(check-equal? (top-interp '(strlen "hello") memory-size) "5")

(check-equal? (top-interp '(equal? 5 5) memory-size) "true")
(check-equal? (top-interp '(equal? "a" "a") memory-size) "true")
(check-equal? (top-interp '(equal? true false) memory-size) "false")
(check-equal? (top-interp '(equal? false false) memory-size) "true")


 
;;--------PRIMOP FUNC ERROR TEST---------
;; substring
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(substring "hello" 1 4 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(substring "hello" 4 1 ) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(substring "hello" 4.1 1.2 ) memory-size)))
;; strlen
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(strlen "hello" 2) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(equal? 5 5 4) memory-size)))

;; -----------------------------------------------------------------------------
;;  PRIMITIVE OP ERROR TEST
;; -----------------------------------------------------------------------------
;;---primop error invalid argument--
;; primop error (arithmatic)
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(+ 1 2 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(- 1 2 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(/ 1 2 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(/ 1 0) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(* 1 2 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(+ "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(- "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(* "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(/ "hello" 3) memory-size)))
;; primop error comparision
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(> "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(< "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ:"))
 (lambda () (top-interp '(>= "hello" 3) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(<= "hello" 3) memory-size)))

(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(> "hello" 3 2) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(< "hello" 3 2) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(>= "hello" 3 2) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(<= "hello" 3 2) memory-size)))

(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(if  (<= 1 0) 13 (/ 3 0)) memory-size)))

;; 1. Using + with a non-number argument.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(+ "a" 3) memory-size)))

;; 2. Using + with the wrong number of arguments.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(+ 3) memory-size)))

;; 3. Division by zero.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(/ 8 0) memory-size)))

;; 4. Using <= with a non-number argument.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(<= 3 "a") memory-size)))

;; 5. Using substring with a non-string first argument.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(substring 123 0 3) memory-size)))

;; 6. Using substring with indices out of range.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(substring "hello" 1 10) memory-size)))

;; 7. primop invalid expresion
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(+ if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(- if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(* if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(/ if 4) memory-size)))

(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(> if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(< if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(>= if 4) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(<= if 4) memory-size)))



;; -----------------------------------------------------------------------------
;;  PRIM-STRLEN TEST
;; -----------------------------------------------------------------------------

;; Valid case: strlen returns the length of a string.
(check-equal? (top-interp '(strlen "hello") memory-size) "5")

;; Error case: strlen applied to a non-string argument.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(strlen 42) memory-size)))

;; -----------------------------------------------------------------------------
;;  PRIM_EQUAL? TEST
;; -----------------------------------------------------------------------------

;; Valid cases:
(check-equal? (top-interp '(equal? 5 5) memory-size) "true")
(check-equal? (top-interp '(equal? "a" "a") memory-size) "true")
(check-equal? (top-interp '(equal? true false) memory-size) "false")

;; Error case: wrong number of arguments for equal?
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(equal? 5) memory-size)))
(check-equal? (top-interp '(declare ([a {make-array 3 0}] [b {make-array 3 0}])
               in (equal? a b)) 100) "false")
(check-equal? (top-interp '(declare ([a {make-array 3 0}] [b {make-array 3 0}])
               in (equal? a b)) 100) "false")
(check-equal? (top-interp 
  '{declare {[a (make-array 2 19)]
             }
            in
            {equal? {aset! a 1 3} {aset! a 1 3}}} 100) "true")

;; -----------------------------------------------------------------------------
;; PRIM_ERROR TEST
;; -----------------------------------------------------------------------------

;; Valid behavior: prim-error should halt and signal an error with a message
;; containing "QWJZ: user-error" (so we check that an error is raised).
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(error "oops") memory-size)))

;; Error case: calling error with too many arguments.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(error "oops" "extra") memory-size)))

;; -----------------------------------------------------------------------------
;; PRIM-SUBSTRING TEST
;; -----------------------------------------------------------------------------

;; Valid case: substring returns the correct substring.
(check-equal? (top-interp '(substring "hello" 1 4) memory-size) "\"ell\"")

;; Error cases:
;; 1. Non-string first argument.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(substring 123 0 2)memory-size)))

;; 2. Indices out of range.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(substring "hello" 1 10)memory-size)))

;; 3. Non-integer indices.
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(substring "hello" 1.5 3.5) memory-size)))
 
;; -----------------------------------------------------------------------------
;; PRINTLN TEST
;; -----------------------------------------------------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
  (lambda () (top-interp '(println "heloo" "1as") memory-size)))


(check-equal? (top-interp '(println true) memory-size) "true")
(check-equal? (top-interp '(println 1) memory-size) "true")
(check-equal? (top-interp '(println (+ 1 2)) memory-size) "true")
(check-equal? (top-interp '(println "hello") memory-size) "true")

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () (top-interp '(println {array 1 2 3}) memory-size)))
(check-equal? (top-interp '(println false) memory-size) "false")

;; ;; -----------------------------------------------------------------------------
;; ;;  SERIALIZE TEST
;; ;; -----------------------------------------------------------------------------

;; Test that numbers serialize correctly.
(check-equal? (serialize (NumV 42)) "42")
(check-equal? (serialize (NumV 3.14)) "3.14")

;; Test that booleans serialize correctly.
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")

;; Test that strings serialize correctly (wrapped in double quotes).
(check-equal? (serialize (StringC "hello")) "\"hello\"")

;; Test that closures serialize as "#<procedure>".
;; Create a dummy closure: a proc that takes one argument and returns that argument.
(define dummy-closure (Closure (list 'x) (IdC 'x) empty))
(check-equal? (serialize dummy-closure) "#<procedure>")

;; Test that primitive operators serialize as "#<primop>".
;; Create a dummy primop with a dummy function.
(define dummy-primop (PrimOp prim-plus "dummy"))
(check-equal? (serialize dummy-primop) "#<primop>")

;; Test that strings serialize correctly (wrapped in double quotes).
(check-equal? (serialize (StringC "hello")) "\"hello\"")


;; Define a dummy structure that is not part of the Value union.
(struct Dummy ([x : Number]) #:transparent)

;; Force a Dummy value into type Value using unsafe-coerce.



;; Test case: serialize should error when given an unknown value type.
;; =============================================================================
;; ERROR TEST
;; =============================================================================

;; 1. Unbound Variable
;; Using a variable that is not in the environment should trigger an error.
(check-exn (regexp (regexp-quote "QWJZ: Unbound identifier"))
 (lambda () (top-interp 'x memory-size))
 )

;; 2. if Condition Not Boolean
;; The condition in an if expression is not a boolean.

(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(if 42 1 2) memory-size)))

;; 3. Wrong Number of Arguments for a Primitive
;; Calling + with only one argument should trigger an error.
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(+ 1) memory-size)))
;;-----------
;; 4. Wrong Number of Arguments for a Procedure
;; Define a procedure that expects one argument and call it with none.
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '((proc (x) (+ x 1))) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '((proc (1) (+ x 1))) memory-size)))

;; 5. Substring with Non-string Argument
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(substring 123 0 1) memory-size)))

;; 6. Substring with Out-of-Range Indices
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(substring "hello" 4 10) memory-size)))

;; 7. Duplicate Identifier in Declaration
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([x 10] [x 20]) in (+ x 5)) memory-size)) )

;; 8. Invalid Identifier in Declaration (using reserved keyword)
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([if 10]) in (+ if 5)) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([a 10]) in (+ if 5)) memory-size)) )
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([declare 10]) in (+ if 5)) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([proc 10]) in (+ if 5)) memory-size)))
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(declare ([in 10]) in (+ if 5)) memory-size)))

;; 9. Parameter List Not a List in Proc
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(proc if 1) memory-size)))

;; 10. Calling nonfunction
(check-exn (regexp (regexp-quote "QWJZ"))
 (lambda () (top-interp '(1 2 3) memory-size)))

;; -----------------------------------------------------------------------------
;;  INVALID RESVERED KEYWORD TEST
;; -----------------------------------------------------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () (top-interp '(declare ([if 10]) in (+ if 5)) memory-size)))


;; -------------------------------
;; Array Primitive Test Cases
;; -------------------------------
;; 
;; 1. Using make-array: Create an array of 3 cells all containing 0,
;; ;;    then check that aref returns 0 for a valid index.
(check-equal? 
 (top-interp 
  '{declare {[a (make-array 2 19)]
             }
            in
            {aref a 0}} 100)
 "19")
(check-equal? 
 (top-interp 
  '{declare {[a (array 3 2 19 4)]
             }
            in
            {seq {println {aref a 0}}
                 {println {aref a 1}}
                 {aset! a 2 -1}
                 {println {aref a 2}}
                 {println {aref a 3}}
                 }} 100)
 "true")
;; 2. Using array: Create an array with several elements and check an element.
(check-equal? 
 (top-interp 
  '(declare ([a {array 3 14 false 5}])
            in (aref a 1)) 100)
 "14")
;; ;; 
;; ;; ;; 3. Error: Creating an array with fewer than one cell (size 0) should error.
(check-exn (regexp (regexp-quote "QWJZ")) (lambda () (top-interp 
                                                      '{declare {[a (make-array 0 19)]
                                                                 }
                                                                in
                                                                {aref a 0}} 100)))


;; ;; ;; 4. Error: Using aref with an index out of range should signal an error.
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () 
             (top-interp 
              '{declare {[a (make-array 2 19)]
                         }
                        in
                        {aref a 50}} 100)))

;; ;; ;; 5. Error: Calling prim array without arguments will error
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (array)]
                         }
                        in
                        {aset! a 50 0}} 100)))
;; -------------------------------
;; ARRAY ERROR TEST 
;; -------------------------------

;; ;; ;; 6. Error: Calling aset! with an index out of range should error.

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (make-array 2 19)]
                         }
                        in
                        {aset! a 50 0}} 100)))
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (make-array 2 19)]
                         }
                        in
                        {aset! a 50 0 3}} 100)))

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (make-array 200 19)]
                         }
                        in
                        {aset! a 50 0}} 100)))

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (make-array 2 19 0 0432)]
                         }
                        in
                        {aset! a 50 0}} 100)))

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a (make-array 2 19)]
                         }
                        in
                        {aref a 50 0}} 100)))
;; -------------------------------
;; ASSIGN TEST 
;; -------------------------------
(check-equal? 
 (top-interp 
  '{declare {[a 10]
             }
            in
            {a := 0}} 100)
 "null")

(check-equal? 
 (top-interp 
  '{declare {[a 10]
             }
            in
            {seq {a := 1}
                 a
                 }} 100)
 "1")

(check-equal? 
 (top-interp 
  '{declare {[a 10]
             [b 20]
             ;;[c 30]
             ;;[d 40]
             ;;[c 100]
             }
            in
            {seq {a := 3}
                 {b := 2}
                 ;;{c := 300}
                 ;;{println b}
                 ;;{println b}
                 ;;{println c}
                 }} 100)
 "null")
;; -------------------------------
;; ASSIGN ERROR TEST 
;; -------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a 10]
                         }
                        in
                        {seq 
                             {b := 2}
                             }} 100)))

(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a 10]
                         }
                        in
                        {seq {3 := 3}
                             
                             }} 100)))
;; -------------------------------
;; OUT OF MEMORY ERROR TEST 
;; -------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda ()
             (top-interp 
              '{declare {[a 10]
                         }
                        in
                        {seq {a := 3}
                             {b := 2}
                             }} 11)))

(define while '{declare {[fact "bogus"]} in
                        {seq {fact := {proc (x) {if {equal? x 0}
                                                    1
                                                    {* x {fact {- x 1}}}}}}
                             {fact 10}}})


(define not-in-order '{declare {[in-order "bogus"]
                                [dummy {array 1}]
                                [arr (array 3 10 2 3)]
                                [size 4]
                                [start 0]
                                [next 1]}
                               in
                               {seq {in-order := {proc (a ) {if {>= next size}
                                                                true
                                                                {if (> (aref a start) (aref a next))
                                                                    false
                                                                    (seq {start := (+ start 1)}
                                                                         {next := (+ next 1)}
                                                                         {in-order a})}}}}
                                    {in-order arr}}})
(define in-order '{declare {[in-order "bogus"]
                            [arr (array 1 2 3 5)]
                            [size 4]
                            [dummy {array 1}]
                            [start 0]
                            [next 1]}
                           in
                           {seq {in-order := {proc (a ) {if {>= next size}
                                                            true
                                                            {if (> (aref a start) (aref a next))
                                                                false
                                                                (seq {start := (+ start 1)}
                                                                     {next := (+ next 1)}
                                                                     {in-order a})}}}}
                                {in-order arr}}})

(check-equal? (top-interp while 100000) "3628800")
(check-equal? (top-interp not-in-order 10000000) "false")
(check-equal? (top-interp in-order 10000000) "true")

(define while1 '{declare {[mutate-in-x "bogus"]
                          [dummy {array 1}]
                          [arr (array 3 10 2 3)]
                          
                          [var 100]
                          }
                         in
                         {seq {mutate-in-x := {proc (a x start size) { if ( < start size)
                                                                          {seq {println start}
                                                                               {seq
                                                                                {aset! a start x}
                                                                                {mutate-in-x a x (+ start 1) size}}}
                                                                          {seq {arr := a}}}}}
                              {mutate-in-x arr var 0 4}
                              {aref arr 2}}})

(top-interp while1 1000)
