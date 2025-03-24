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
  (U NumC IfC LamC AppC IdC StringC AssignC RecC))

(define-type Ty
  (U NumT BoolT StringT AppT AnyT))
;;Abstract Syntax
(struct NumC ([n : Number]) #:transparent)
(struct IdC ([s : Symbol])#:transparent)
(struct IfC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([params : (Listof (Pairof Symbol Ty))] [body : ExprC]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct AssignC ([id : Symbol] [exp : ExprC]) #:transparent)
(struct RecC ([name : Symbol] [val : ExprC] [ty : Ty] [body : ExprC]) #:transparent)
;;Value 
(struct ArrayV ([pointer : Natural] [size : Natural]) #:transparent)
(struct Store ([mem : (Vectorof Value)])#:transparent)
(struct NullV ([s : Symbol]) #:transparent)
(struct NumV ([v : Number])#:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
;; Type
(struct AnyT ())
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StringT () #:transparent)
(struct IdT ([s : Symbol]) #:transparent)
;;(struct LamT ([argsT : (Listof Ty)] [body : Ty] [return : Ty]) #:transparent )
(struct AppT ([argsT :( Listof Ty)] [return : Ty]) #:transparent)


;; An environment is a list of bindings mapping identifiers to values.
(define-type Env (Listof (Pairof Symbol Value )))
(define-type TEnv (Listof (Pairof Symbol Ty )))

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
(struct Closure ([params : (Listof (Pairof Symbol Ty))]
                 [body   : ExprC]
                 [env    : Env]) #:transparent)

;; A PrimOp represents a primitive operator.
(struct PrimOp ([fn    : (-> (Listof Value) Store Value)]
                [name  : String]) #:transparent)
(struct PrimOpT ([func-type : AppT]
                 [name : String]) #:transparent)


;-------------Dubplicates?-----------------
(define (duplicates? [lst : (Listof Sexp)]) : Boolean
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
     #;(fprintf (current-output-port)
                  "env in lookup: ~a ~n ~n ~n"
                  env
                  )
     (error 'lookup (format "QWJZ: Unbound identifier ~a" id))]
    [else
     (let ([binding (first env)])
       
       #;(if (eq? (car binding) 'square-helper)
           
           #(fprintf (current-output-port)
                    "env in lookup: comparing ~a in ~n ~a ~n ~n ~n"
                    id
                    env)
           #;(fprintf (current-output-port)
                    "env in lookup: id ~a in ~n ~n ~n ~n"
                    id
                    )
                  )
       (if (eq? (car binding) id)
           (match (cdr binding)
             [(NumV v) (vector-ref (Store-mem sto) (cast v Integer))]
             [other (error (format "~a is not NumV" (cdr binding)))]
             )
           (lookup id (rest env) sto)))]))
;; =============================================================================
;; LOOK UP
;; =============================================================================

;; lookup : Symbol Env -> Value
;; Looks up an identifier in the environment to return NumV position refer to
;; actual value in the store
;; With that NumV position: Look up in the position of the store
;; return the actual Value that binded with the symbol
;; or signals an error if unbound.

(define (lookup-type [id : Symbol] [env : TEnv]) : Ty
  (cond
    [(empty? env)
     (error 'lookup (format "QWJZ: Unbound identifier ~a" id))]
    [else
     (let ([binding (first env)])
       (if (eq? (car binding) id)
           (cdr binding)
           (lookup-type id (rest env))))]))
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
;; Valid?
;; =============================================================================
(define (Valid? [s : Sexp]) : Boolean
  
  (match  s
        
    ['if #f]
    ['declare #f]
    ['proc #f]
    ['in #f]
        
    [other #t]))

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
;; Desugar
;; =============================================================================
(define (desugar [s : Sexp]) : Sexp
  (match s
    [(list 'declare (list (list vars vals ': tys) ...) 'in body)
     (cast (cons (list 'proc (map (lambda (var ty) (list var ': ty)) vars tys) body) vals) Sexp)]
    [_ (error "QWJZ4 failed: invalid Sexp for desugar")]))

;; =============================================================================
;; PARSE-TYPE
;; =============================================================================
(define (parse-type [ exp : Sexp]) : Ty
  (match exp
    ['num (NumT )]
    ['bool (BoolT )]
    ['str (StringT )] 
    [(list args-ty ... -> return-ty) (AppT (map parse-type (cast args-ty (Listof Sexp))) (parse-type return-ty))]
    
    [_ (error (format "QWJZ: Invalid type syntax: ~a" exp))]))

;; =============================================================================
;; Parser (with Desugaring for 'declare')
;; =============================================================================

;; parse : Sexp -> ExprC
;; Parses an S-expression into an AST for QWJZ4.
(define (parse [s : Sexp]) : ExprC
  (match s
    ;; 'Declare
    [(list 'declare (list define ...) 'in body ...) (parse (desugar s))]
    [(? string? s) (StringC s)]
    [(? symbol? sym) (IdC sym)]
    [(? real? n) (NumC n)]
    ;; If
    [(list 'if test then else) (if (notValid? (list test)) (error "QWJZ4 failed: conditon for if is invalid")
                                   (IfC (parse test) (parse then) (parse else)))]
    [(list 'reclare  (list (? symbol? id) val : ty) 'in body)
     (let ([a (RecC id (parse val) (parse-type ty) (parse body))])
       
       a)
     ]
    ;; LamC
    [(list 'proc (list args ...) body)
     
     (let ([list-of-args-w-type (map (lambda (x) (match x
                                                   [(list (? symbol? s) : ty) (cons s (parse-type (cast ty Sexp)))]
                                                   [other (error (format "QWJZ failed: Invalid expresion ~a" x) )]))
                                     args)])
       (match list-of-args-w-type
         [(list (cons (? symbol? items) ty) ...)
          (cond
            [(duplicates? args)
             (error 'parse "QWJZ4 failed: repeated parameter in proc definition")]
            [(notValid? items)
             (error 'parse "QWJZ4 failed: argument is reserved keyword") ]
            [else (LamC list-of-args-w-type (parse body))])]))]


    
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
(define top-tenv : TEnv
  (list
   (cons '+ (AppT (list (NumT) (NumT)) (NumT)))
   (cons '- (AppT (list (NumT) (NumT)) (NumT)))
   (cons '* (AppT (list (NumT) (NumT)) (NumT)))
   (cons '/ (AppT (list (NumT) (NumT)) (NumT)))
   (cons '<= (AppT (list (NumT) (NumT)) (BoolT)))
   (cons '< (AppT (list (NumT) (NumT)) (BoolT)))
   (cons '> (AppT (list (NumT) (NumT)) (BoolT)))
   (cons '>= (AppT (list (NumT) (NumT)) (BoolT)))
   
   ;;(cons 'substring (PrimOp prim-substring "substring"))
   ;;(cons 'strlen (PrimOp prim-strlen "strlen"))
   ;;(cons 'equal? (PrimOp prim-equal? "equal?"))
   ;;(cons 'error (PrimOp prim-error "error"))
   
   
   ;;(cons 'println (PrimOp prim-println "println"))
   ;;(cons 'seq (PrimOp prim-seq "seq"))
   ;;(cons '++ (PrimOp prim-++ "++"))
   (cons 'true (BoolT))
   (cons 'false (BoolT))
   
   ))
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
(define (type-equal? [t1 : Ty] [t2 : Ty]) : Boolean
  (cond
    [(and (NumT? t1) (NumT? t2)) #t]
    [(and (BoolT? t1) (BoolT? t2)) #t]
    [(and (StringT? t1) (StringT? t2)) #t]
    [else #f]))
;; =============================================================================
;; EXTEND-TENV
;; =============================================================================
(define (extend-tenv [list-of-pair : (Listof (Pairof Symbol Ty))] [tenv : TEnv]) : TEnv
  (let ([new-tenv (append list-of-pair tenv)])
    ;;(println new-tenv)
    new-tenv))
;; =============================================================================
;; TYPE-CHECK
;; =============================================================================
(define (type-check [exp : ExprC] [tenv : TEnv]) : Ty
  (match exp
    [(NumC n) (NumT)]
    
    [(StringC s) (StringT)]
    [(IdC s) (lookup-type s tenv)]
    
    [(IfC test then else) (let ([test-type (type-check test tenv)])
                            (match test-type
                              [(BoolT) (if (type-equal? (type-check then tenv) (type-check else tenv))
                                           (type-check then tenv)
                                           (error (format "QWJZ: two clause is not same type ~a ~a" then else)))]
                              
                              (other (error (format "QWJZ: condition ~a is not BoolT ~a" test (type-check test tenv) )))))]
    [(LamC args body) 
                      ;;(println (extend-tenv args tenv))
                      #;(println (cdr (first args)))
                      (AppT (map (inst cdr Symbol Ty) args) (type-check body (extend-tenv args tenv)))]
   
    [(AppC fun args) (let* ([fun-type (type-check fun tenv)]
                           [args-type (map (λ ([arg : ExprC]) (type-check arg tenv)) args)])
                           (match fun-type
                             [(AppT list-type return)
                              #;(fprintf (current-output-port)
                                       "AppC fun: ~a ~n ~n ~n AppT fun: ~a ~n ~n~n"
                                       fun fun-type
                                       )
                                                      (if (equal? list-type args-type )
                                                          return
                                                          (error (format "QWJZ: type ~a does not match the type of ~a "
                                                                         list-type
                                                                         args-type)))]
                             [other (error (format "QWJZ: AppC: ~a AppT: ~a fun-type is not an function type" fun fun-type))]))]
    [(RecC sym helper-body helper-type body)  ;; put placeholder
                                             (let ([new-tenv (append (list (cons sym helper-type)) tenv)])
                                               (let ([helper-body-type (type-check helper-body new-tenv)] )
                                                 (if (equal? helper-body-type helper-type)
                                                     helper-body-type
                                                     (error (format "QWJZ: type ~a in ~a does not match type ~a you delcare"
                                                                    helper-body-type
                                                                    helper-body
                                                                    helper-type)))
                                               (type-check body new-tenv)))
                                             
                                             ]
    
    
    ))
(check-equal? (type-check (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-tenv) (NumT))
;;(type-check (LamC (list (cons 'x (NumT))) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) top-tenv)


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
    ;; LamC case
    [(LamC arg exp)
     
     (let ([clo (Closure arg exp env)])
       #;(fprintf (current-output-port)
                  "LamC: ~a ~n ~n ~n"
                  e
                  )
       #;(fprintf (current-output-port)
                  "Closure: ~a ~n ~n ~n"
                  clo
                  )
       clo)]
    
    ;; RecC case
    [(RecC name val ty body)
     #;(fprintf (current-output-port)
                  "RecC (val): ~a ~n ~n ~n"
                  val
                  )
     #;(fprintf (current-output-port)
                  "interp val: ~a ~n ~n ~n"
                  (interp val env sto)
                  )
     (let* ([placeholder-location (alloc sto (NumV -99))]
           [new-binding (list (cons name placeholder-location))])
       #;(fprintf (current-output-port)
                  "new binding: ~a ~n ~n ~n"
                  new-binding
                  )
       (let ([new-env (append new-binding env)])
         #;(fprintf (current-output-port)
                  "new env: ~a ~n ~n ~n sto: ~a"
                  new-env
                  sto)
         (match placeholder-location
           [(NumV v) (vector-set! (Store-mem sto) (cast v Integer) (interp val new-env sto))]
           [_ (error "Type Error: Expected NumV for placeholder-location but got" placeholder-location)])
         (interp body new-env sto)))]

    ;; AppC case
    [(AppC fun args)
     (cond
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
                        (let ([new-bindings (map (λ ([p : (Pairof Symbol Ty)] [a : Value]) (cons (car p) (alloc sto a)))
                                                 params
                                                 arg-vals)])
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
    [other (error (format "QWJZ: Unknown expression type: ~v" e))]
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
  (type-check (parse s) top-tenv)
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

;; =============================================================================
;; ERROR TEST 
;; =============================================================================




;; =============================================================================
;; PRIMOP TEST 
;; =============================================================================



;;--------PRIMOP FUNCTION TEST---------



 
;;--------PRIMOP FUNC ERROR TEST---------


;; -----------------------------------------------------------------------------
;;  PRIMITIVE OP ERROR TEST
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;  PRIM-STRLEN TEST
;; -----------------------------------------------------------------------------



;; Error case: strlen applied to a non-string argument.
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () (top-interp '(strlen 42) memory-size)))

;; -----------------------------------------------------------------------------
;;  PRIM_EQUAL? TEST
;; -----------------------------------------------------------------------------


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


;; -----------------------------------------------------------------------------
;; PRINTLN TEST
;; -----------------------------------------------------------------------------


;; ;; -----------------------------------------------------------------------------
;; ;;  SERIALIZE TEST
;; ;; -----------------------------------------------------------------------------



;; =============================================================================
;; ERROR TEST
;; =============================================================================



;; -----------------------------------------------------------------------------
;;  INVALID RESVERED KEYWORD TEST
;; -----------------------------------------------------------------------------
(check-exn (regexp (regexp-quote "QWJZ"))
           (lambda () (top-interp '(declare ([if 10]) in (+ if 5)) memory-size)))


;
(parse '(declare ([x 5 : num] [b 3 : num])
                                     in (+ x b)))
(check-equal? (top-interp '(declare ([x 5 : num] [b 3 : num])
                                     in (+ x b)) 100) "8")
(check-equal? (top-interp '(declare ([x (proc ([b : num]) (+ b 1)) : (num -> num)])
                                     in (x 1)) 100) "2")


(check-equal? (top-interp '{reclare {square-helper
                                       {proc ([n : num])
                                             {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}
                                       : {num -> num}}
                                    in
                                    {declare {[square {proc ([n : num])
                                                            {square-helper {- {* 2 n} 1}}}
                                                      : {num -> num}]}
                                             in
                                             {square 10}}} 100) "100")
#;(type-check (LamC (list (cons 'x (NumT))) (LamC (list (cons 'y (NumT)))
                                                (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))) top-tenv)