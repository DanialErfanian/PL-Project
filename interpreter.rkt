#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ((:+ (char-range #\0 #\9)) (token-int (string->number lexeme)))
   ((:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))) (token-float (string->number lexeme)))
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-mult))
   ("/" (token-div))
   ("**" (token-power-sign))
   (";" (token-semicolon))
   ("if" (token-if))
   ("=" (token-assign))
   ("return" (token-return))
   ("def" (token-def))
   ("():" (token-no-arguments))
   ("(" (token-open-paranthesis))
   (")" (token-close-paranthesis))
   (":" (token-colon))
   ("," (token-comma))
   ("for" (token-for))
   ("in" (token-in))
   ("or" (token-or))
   ("and" (token-and))
   ("not" (token-not))
   ("==" (token-equals))
   ("<" (token-less-than))
   (">" (token-greater-than))
   ("[" (token-open-bracket))
   ("]" (token-close-bracket))
   ("()" (token-open-close-paranthesis))
   ("[]" (token-open-close-brackets))
   ("->" (token-arrow))
   ("checked" (token-checked))
   ("pass" (token-pass))
   ("else" (token-else))
   ("None" (token-none))
   ((:or "int" "float" "bool" "list" "None") (token-type lexeme))
   ((:or "True" "False") (token-bool lexeme))
   ((:+ (:: (:or (char-range #\a #\z)
                 (char-range #\A #\Z))))
        (token-ID lexeme))
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens non-empties (int float bool type ID))
(define-empty-tokens empties (EOF plus minus semicolon mult div power-sign if assign return def no-arguments
                                 open-paranthesis close-paranthesis colon comma for in or and not equals less-than greater-than
                                 open-bracket close-bracket open-close-paranthesis open-close-brackets arrow checked pass else none))

(define parse
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens non-empties empties)
   (grammar
    (program
     ((statements) (append $1 #f))
     ((checked statements) (append $2 #t)))
    (statements
     ((statement semicolon) (list 'statement $1))
     ((statements statement semicolon) (list 'statements (list 'statements $1) (list 'statement $2))))
    (statement
     ((compound-stmt) $1)
     ((simple-stmt) $1))
    (simple-stmt
     ((assignment) $1)
     ((return-stmt) $1)
     ((pass) (list 'pass)))
    (compound-stmt
     ((function-def) $1)
     ((if-stmt) $1)
     ((for-stmt) $1))
    (assignment
     ((assignment-lhs assign expression) (list 'assignment $1 $3)))
    (return-stmt
     ((return) (list 'return))
     ((return expression) (list 'return-stmt $2)))
    (function-def
     ((def ID open-paranthesis params close-paranthesis return-type statements) (list 'def-stmt $2 $4 $6 $7))
     ((def ID open-close-paranthesis return-type statements) (list 'def-with-no-param-stmt $2 $4 $5)))
    (params
     ((param-with-default) (list 'param-with-default $1))
     ((params comma param-with-default) (list 'params (list 'params $1) (list 'param-with-default $3))))
    (param-with-default
     ((assignment-lhs assign expression) (list 'param-assignment-lhs $1 $3)))
    (if-stmt
     ((if expression colon statements else-block) (list 'if-stmt $2 $4 $5)))
    (else-block
     ((else colon statements) $3))
    (for-stmt
     ((for ID in expression colon statements) (list 'for-stmt $2 $4 $6)))
    (expression
     ((disjunction) $1)
     ((sum) $1))
    (disjunction
     ((conjunction) $1)
     ((disjunction or conjunction) (list 'dis-or-con $1 $3)))
    (conjunction
     ((inversion) $1)
     ((conjunction and inversion) (list 'con-and-inv $1 $3)))
    (inversion
     ((not inversion) (list 'not-inv $2))
     ((comparison) (list 'comparison $1)))
    (comparison
     ((eq-sum) (list 'eq-sum $1))
     ((lt-sum) (list 'lt-sum $1))
     ((gt-sum) (list 'gt-sum $1)))
    (eq-sum
     ((sum equals sum) (list 'equals $1 $3)))
    (lt-sum
     ((sum less-than sum) (list 'less-than $1 $3)))
    (gt-sum
     ((sum greater-than sum) (list 'greater-than $1 $3)))
    (sum
     ((sum plus term) (list 'plusnumbers $1 $3))
     ((sum minus term) (list 'minusnumbers $1 $3))
     ((term) (list 'term $1)))
    (term
     ((term mult factor) (list 'multnumbers $1 $3))
     ((term div factor) (list 'divnumbers $1 $3))
     ((factor) $1))
    (factor
     ((plus factor) (list 'plus-factor $2))
     ((minus factor) (list 'negate-factor $2))
     ((power) $1))
    (power
     ((atom power-sign factor) (list 'powernumbers $1 $3))
     ((primary) $1))
    (primary
     ((atom) $1)
     ((primary open-bracket expression close-bracket) (list 'primary-exp $1 $3))
     ((primary open-close-paranthesis) (list 'primary-no-arg $1))
     ((primary open-paranthesis arguments close-paranthesis) (list 'primary-with-args $1 $3)))
    (arguments
     ((expression) $1)
     ((arguments comma expression) (list 'arguments (list 'expression $3) (list 'arguments $1))))
    (atom
     ((ID) (list 'id $1))
     ((bool) (list 'bool-value $1))
     ((none) (list 'none))
     ((number) $1)
     ((lst) $1))
    (number
     ((int) (list 'int-value $1))
     ((float) (list 'float-value $1)))
    (lst
     ((open-bracket expressions close-bracket) (list 'lst $2))
     ((open-bracket close-bracket) (list 'lst `())))
    (expressions
     ((expressions comma expression) (list 'expressions (list 'expressions $1) (list 'expressions $3))))
    (assignment-lhs
     ((ID) (list 'id-value $1 "undefined"))
     ((ID colon type) (list 'id-value $1 $3)))
    (return-type
     ((colon) (list 'no-retrun-type))
     ((arrow type colon) (list 'return-type $2)))
    )))

(define
  empty-env
  (lambda ()
    `()))

(define extend-env
  (lambda (var val type env)
    (cons (list var val type) env)))

(define apply-env
 (lambda (var env)
   (if (equal? var (caar env))
       (car env)
       (apply-env var (cdr env)))))


(define value-of
  (lambda (tree env check)
           (let ([first (car tree)])
           (cond
             [(equal? first 'statements) (let ([v1 (value-of (cadr tree) env check)])
                                          (if (equal? (car v1) 'return-value)
                                             v1
                                             (value-of (caddr tree) (cadr v1) check)))]
             [(equal? first 'statement) (value-of (cadr tree) env check)]
             [(equal? first 'assignment) (let ([left-value (cadr tree)]
                                               [right-expression (caddr tree)])
                                           (if (equal? (car left-value 'id-with-type))
                                                       (list 'assignment-value 'none (extend-env (cadr left-value) right-expression (caddr left-value) env))
                                                       (list 'assignment-value 'none (extend-env (cadr left-value) right-expression "undefined" env))))]
             [(equal? first 'return) ('return-value 'none "None" env)]
             [(equal? first 'return-stmt ('return-value (cadr tree)))]
             [(equal? first 'def-stmt) (list 'def-value 'none
                                             (extend-env (cadr tree) (list (cadr (cdddr tree))
                                                                      (value-of (caddr tree) env check)) (value-of (cadddr tree) env check) env check))]
             [(equal? first 'def-with-no-param-stmt (list 'def-value 'none (extend-env (cadr tree) (list (cadddr tree) `())
                                                                                       (value-of (caddr tree) env check))))]
            [(equal? first 'param-with-default) (extend-env (value-of (cadr tree) env check) (empty-env))]
             [(equal? first 'params) (extend-env (value-of (cadr tree) env check) (value-of (caddr tree) env check))]
             [(equal? first 'param-assignment-lhs) (let ([left-value (cadr tree)]
                                                         [right-exp (caddr tree)])
                                                     (list (cadr left-value) right-exp (caddr left-value)))]
             [(equal? first 'if-stmt) (let ([v1 (value-of (cadr tree) env check)])
                                       (if (equal? (car v1) 'bool-value)
                                           (if (equal? (cadr v1) "True")
                                               (value-of (caddr tree) env check)
                                               (value-of (cadddr tree) env check))
                                           (error "error: condition should be a bool value")))]
             [(equal? first 'lst) tree]
             [(equal? first 'for-stmt) (let ([v1 (value-of (cadr tree) env check)])
                                         (if (null? (cdr v1))
                                             (value-of (cadddr tree) env check)
                                             (value-of (list 'for-stmt (cadr tree) (append (list 'lst) (cdr v1))
                                                             (caddr (value-of (cadddr tree) env check))))))]
             [(equal? first 'dis-or-con) (let ([v1 (value-of (cadr tree) env check)]
                                               [v2 (value-of (caddr tree) env check)])
                                           (if (and (equal? (car v1) 'bool-value)
                                                    (equal? (car v2) 'bool-value))
                                               (if (or (equal? (cadr v1) "True")
                                                        (equal? (cadr v2) "True"))
                                                        (list 'bool-value "True")
                                                        (list 'bool-value "False"))
                                               (error "error: values of or operator should be bool values!")))]
             [(equal? first 'con-and-inv) (let ([v1 (value-of (cadr tree) env check)]
                                               [v2 (value-of (caddr tree) env check)])
                                           (if (and (equal? (car v1) 'bool-value)
                                                    (equal? (car v2) 'bool-value))
                                               (if (and (equal? (cadr v1) "True")
                                                        (equal? (cadr v2) "True"))
                                                        (list 'bool-value "True")
                                                        (list 'bool-value "False"))
                                               (error "error: values of or operator should be bool values!")))]
             
                                             
         ;    [(equal? first 'id) (cadr tree)]
          ;   [(equal? first 'bool-value) tree]
           ;  [(equal? first 'none) tree]
           ;  [(equal? first 'int-value) tree]
            ; [(equal? first 'float-value) tree]
            ; [(equal? first 'primary-no-arg) (value-of (cadr tree) env)]
            ; [(equal? first 'primary-with-args (value-of (cadr tree)))]
             ))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "if True: a = b + 4; else: b = 2;;")))
(let ((parser-res (parse my-lexer))) parser-res)

