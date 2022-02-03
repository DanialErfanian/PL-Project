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
   ("print" (token-print))
   ("None" (token-none))
   ((:or "int" "float" "bool" "list" "None") (token-type lexeme))
   ((:or "True" "False") (token-bool lexeme))
   ((:+ (:: (:or (char-range #\a #\z)
                 (char-range #\A #\Z)
                 #\_)))
        (token-ID lexeme))
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens non-empties (int float bool type ID))
(define-empty-tokens empties (EOF plus minus semicolon mult div power-sign if assign return def
                                 open-paranthesis close-paranthesis colon comma for in or and not equals less-than greater-than
                                 open-bracket close-bracket open-close-paranthesis open-close-brackets arrow checked pass else none print))

(define parse
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens non-empties empties)
   (grammar
    (program
     ((statements) (list $1 #f))
     ((checked statements) (list $2 #t)))
    (statements
     ((statement semicolon) $1)
     ((statements statement semicolon) (list 'statements $1 $2)))
    (statement
     ((compound-stmt) $1)
     ((simple-stmt) $1)
     ((print open-paranthesis expression close-paranthesis) (list 'print $3)))
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
     ((def ID open-paranthesis params close-paranthesis return-type statements) (list 'def-stmt $2 $4 $6 (list 'statements $7 (list 'return))))
     ((def ID open-close-paranthesis return-type statements) (list 'def-with-no-param-stmt $2 $4 (list 'statements $5 (list 'return)))))
    (params
     ((param-with-default) $1)
     ((params comma param-with-default) (list 'params $1 $3)))
    (param-with-default
     ((assignment-lhs assign expression) (list 'param-assignment $1 $3)))
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
     ((comparison) $1))
    (comparison
     ((eq-sum) $1)
     ((lt-sum) $1)
     ((gt-sum) $1))
    (eq-sum
     ((sum equals sum) (list 'equals $1 $3)))
    (lt-sum
     ((sum less-than sum) (list 'less-than $1 $3)))
    (gt-sum
     ((sum greater-than sum) (list 'greater-than $1 $3)))
    (sum
     ((sum plus term) (list 'plus $1 $3))
     ((sum minus term) (list 'minus $1 $3))
     ((term) $1))
    (term
     ((term mult factor) (list 'multnumbers $1 $3))
     ((term div factor) (list 'divnumbers $1 $3))
     ((factor) $1))
    (factor
     ((plus factor) (list 'plus-factor $2))
     ((minus factor) (list 'negate-factor $2))
     ((power) $1))
    (power
     ((atom power-sign factor) (list 'power-numbers $1 $3))
     ((primary) $1))
    (primary
     ((atom) $1)
     ((primary open-bracket expression close-bracket) (list 'primary-expression $1 $3))
     ((primary open-close-paranthesis) (list 'primary-no-arg $1))
     ((primary open-paranthesis arguments close-paranthesis) (list 'primary-with-args $1 $3)))
    (arguments
     ((expression) (list 'argument $1))
     ((arguments comma expression) (list 'arguments $1 (list 'argument $3))))
    (atom
     ((ID) (list 'id $1))
     ((bool) (list 'value $1 "bool"))
     ((none) (list 'value "None" "None"))
     ((number) $1)
     ((lst) $1))
    (number
     ((int) (list 'value $1 "int"))
     ((float) (list 'value $1 "float")))
    (lst
     ((open-bracket expressions close-bracket) $2)
     ((open-bracket close-bracket) (list 'empty-list `())))
    (expressions
     ((expressions comma expression) (list 'expressions $1  (list 'expression $3)))
     ((expression) (list 'expression $1)))
    (assignment-lhs
     ((ID) (list 'id-value $1 "undefined"))
     ((ID colon type) (list 'id-value $1 $3))
     ((ID colon none) (list 'id-value $1 "None")))
    (return-type
     ((colon) "undefined")
     ((arrow type colon) $2))
    )))

(define
  empty-env
  (lambda ()
    `()))

(struct function (output-type) #:transparent)

(define extend-env
  (lambda (var val type env)
    (cons (list var val type) env)))

(define change-value
  (lambda (new-val old-val)
    (list (car old-val) new-val (caddr old-val))))


(define get-elem
  (lambda (var env)
    (if (equal? var (caar env))
        (car env)
        (get-elem var (cdr env)))))

(define resize
  (lambda (defaults new-args)
    (let ([l1 (length new-args)]
          [l2 (length defaults)])
    (if (= l1 l2)
        new-args
        (resize defaults (append new-args (list (cadr (list-ref defaults l1)))))))))


(define apply-env
  (lambda (var env check [args `()])
    (begin ;(display env) (display "\n")
    (if (equal? var (caar env))
        (let ([elem (car env)])
          (let ([type (caddr elem)])
            (if (function? type)
                    (let ([new-env (map change-value (resize (cadadr elem) (if (null? args)
                                                                               args
                                                                               (cadr args))) (cadadr elem))])
                      (let ([value (value-of (caadr elem) (append new-env env) check)])
                        (if check
                            (if (equal? (function-output-type type) "undefined")
                                value
                                (if (equal? (function-output-type type) (caddr value))
                                    value
                                    (error (string-append "error: type of " var " must be " (function-output-type type) "."))))
                            value)))
                (let [(value (value-of (cadr elem) (cdr env) check))]
                  (if check
                      (begin; (display elem) (display var) (display "\n")
                      (if (equal? type "undefined")
                          value
                          (if (equal? type (caddr value))
                              value
                              (error (string-append "error: type of " var " must be " type ".")))))
                      value)))))
        (apply-env var (cdr env) check args)))))
                


(define value-of
  (lambda (tree env check)
           (let ([first (car tree)])
           (cond
             [(equal? first 'statements) (let ([v1 (value-of (cadr tree) env check)]) 
                                          (if (equal? (car v1) 'value)
                                             v1
                                             (value-of (caddr tree) (cadr v1) check)))] 
             [(equal? first 'assignment) (let ([left-value (cadr tree)]
                                               [right-expression (caddr tree)])
                                           (if (equal? 'id (car right-expression))
                                               (let ([right-elem (get-elem (cadr right-expression) env)])
                                               (begin
                                               (list 'env (extend-env (cadr left-value) (cadr right-elem)
                                                                      (if (function? (caddr right-elem))
                                                                          (function (caddr left-value))
                                                                          (caddr left-value)) env))))
                                                       (list 'env (extend-env (cadr left-value) right-expression (caddr left-value) env))))]
             [(equal? first 'return) (list 'value "None" "None")]
             [(equal? first 'return-stmt) (value-of (cadr tree) env check)]
                                            
             [(equal? first 'pass) (list 'env env)]
             [(equal? first 'def-stmt) (list 'env
                                             (extend-env (cadr tree) (list (cadr (cdddr tree))
                                                                      (value-of (caddr tree) env check)) (function (cadddr tree)) env))]
             [(equal? first 'def-with-no-param-stmt) (list 'env (extend-env (cadr tree) (list (cadddr tree) (empty-env))
                                                                                        (function (caddr tree)) env))]
             [(equal? first 'params) (append (value-of (cadr tree) env check) (value-of (caddr tree) env check))]
             [(equal? first 'param-assignment) (let ([left-value (cadr tree)]
                                                         [right-exp (caddr tree)])
                                                     (extend-env (cadr left-value) right-exp (caddr left-value) (empty-env)))]
             [(equal? first 'if-stmt) (let ([v1 (value-of (cadr tree) env check)])
                                       (if (equal? (caddr v1) "bool")
                                           (if (equal? (cadr v1) "True")
                                               (value-of (caddr tree) env check)
                                               (value-of (cadddr tree) env check))
                                           (error "error: condition should be a bool value")))]
             [(equal? first 'for-stmt) (let ([v1 (value-of (caddr tree) env check)])
                                         (if (null? (cadr v1))
                                             (list 'env env)
                                             (let ([new-env (extend-env (cadr tree) (caadr v1) (caddr v1) env)])
                                               (let ([v2 (value-of (cadddr tree) new-env check)])
                                                 (begin; (display v1)
                                             (if (equal? (car v2) 'value)
                                                 v2
                                             (value-of (list 'for-stmt (cadr tree) (list 'value (cdadr v1) "list") (cadddr tree))
                                                             env check)))))))]
             [(equal? first 'dis-or-con) (let ([v1 (value-of (cadr tree) env check)]
                                               [v2 (value-of (caddr tree) env check)])
                                           (if (and (equal? (caddr v1) "bool")
                                                    (equal? (caddr v2) "bool"))
                                               (if (or (equal? (cadr v1) "True")
                                                        (equal? (cadr v2) "True"))
                                                        (list 'value "True" "bool")
                                                        (list 'value "False" "bool"))
                                               (error "error: arguments of or operator should be bool values!")))]
             [(equal? first 'con-and-inv) (let ([v1 (value-of (cadr tree) env check)]
                                               [v2 (value-of (caddr tree) env check)])
                                           (if (and (equal? (caddr v1) "bool")
                                                    (equal? (caddr v2) "bool"))
                                               (if (and (equal? (cadr v1) "True")
                                                        (equal? (cadr v2) "True"))
                                                        (list 'value "True" "bool")
                                                        (list 'value "False" "bool"))
                                               (error "error: arguments of and operator should be bool values!")))]
             [(equal? first 'not-inv) (let ([v1 (value-of (cadr tree) env check)])
                                       (if (equal? (caddr v1) "bool")
                                           (if (equal? (cadr v1) "True")
                                               (list 'value "False" "bool")
                                               (list 'value "True" "bool"))
                                           (error "error: argument of not operator should be a bool value!")))] 
             [(equal? first 'equals) (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                       (if (and
                                            (or (equal? (caddr v1) "int") (equal? (caddr v1) "float"))
                                            (or (equal? (caddr v2) "int") (equal? (caddr v2) "float")))
                                       (if (= (cadr v1) (cadr v2))
                                           (list 'value "True" "bool")
                                           (list 'value "False" "bool"))
                                       (error "error: arguments of == are not the same type or not numeric")))]
             [(equal? first 'less-than) (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                       (if (and
                                            (or (equal? (caddr v1) "int") (equal? (caddr v1) "float"))
                                            (or (equal? (caddr v2) "int") (equal? (caddr v2) "float")))
                                       (if (< (cadr v1) (cadr v2))
                                           (list 'value "True" "bool")
                                           (list 'value "False" "bool"))
                                       (error "error: arguments of < are not the same type or not numeric")))]
             [(equal? first 'greater-than) (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                       (if (and
                                            (or (equal? (caddr v1) "int") (equal? (caddr v1) "float"))
                                            (or (equal? (caddr v2) "int") (equal? (caddr v2) "float")))
                                       (if (> (cadr v1) (cadr v2))
                                           (list 'value "True" "bool")
                                           (list 'value "False" "bool"))
                                       (error "error: arguments of == are not the same type or not numeric")))]
             [(equal? first 'plus) (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                    (if (and (equal? (caddr v1) "int") (equal? (caddr v2) "int"))
                                        (list 'value (+ (cadr v1) (cadr v2)) "int")
                                        (if (and (equal? (caddr v1) "float") (equal? (caddr v2) "float"))
                                            (list 'value (+ (cadr v1) (cadr v2)) "float")
                                            (if (and (equal? (caddr v1) "list") (equal? (caddr v2) "list"))
                                                (list 'value (append (cadr v1) (cadr v2)) "list")
                                                (error "error: not appropariate type for plus operator")))))]
             [(equal? first 'minus)  (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                    (if (and (equal? (caddr v1) "int") (equal? (caddr v2) "int"))
                                        (list 'value (- (cadr v1) (cadr v2)) "int")
                                        (if (and (equal? (caddr v1) "float") (equal? (caddr v2) "float"))
                                            (list 'value (- (cadr v1) (cadr v2)) "float")
                                            (error "error: not appropariate type for minus operator"))))]
             [(equal? first 'multnumbers) (let ([v1 (value-of (cadr tree) env check)])
                                            (if (or (equal? (caddr v1) "int") (equal? (caddr v1) "float"))
                                                (if (= (cadr v1) 0)
                                                v1
                                                (let ([v2 (value-of (caddr tree) env check)])
                                                  (if (equal? (caddr v1) (caddr v2))
                                                      (list 'value (* (cadr v1) (cadr v2)) (caddr v1))
                                                      (error "error: operands for * shoud be the same type"))))
                                                (error "error:  not appropariate type for multiply operator")))]

             [(equal? first 'divnumbers) (let ([v1 (value-of (cadr tree) env check)]
                                               [v2 (value-of (caddr tree) env check)])
                                           (if (and (or
                                                     (equal? (caddr v1 "int"))
                                                     (equal? (caddr v1 "float")))
                                                    (or
                                                     (equal? (caddr v2 "int"))
                                                     (equal? (caddr v2 "float"))))
                                               (if (equal? (caddr v1) (caddr v2))
                                                   (if (= (cadr v2) 0)
                                                       (error "error: division by zero")
                                                       (list 'value (/ (cadr v1) (cadr v2)) (caddr v1)))
                                                   (error "error: operands for / should be the same type"))
                                               (error "error: not numberic operands for / operator")))]
             [(equal? first 'plus-factor) (let ([v1 (value-of (cadr tree) env check)])
                                           (if (or (equal? (caddr v1) "int")
                                                   (equal? (caddr v1) "float"))
                                               v1
                                               (error "operand for + factor should be numeric")))]

             [(equal? first 'minus-factor) (let ([v1 (value-of (cadr tree) env check)])
                                           (if (or (equal? (caddr v1) "int")
                                                   (equal? (caddr v1) "float"))
                                               (list 'value (- 0 (cadr v1)) (caddr v1))
                                               (error "operand for - factor should be numeric")))]

             [(equal? first 'power-numbers)  (let ([v1 (value-of (cadr tree) env check)]
                                           [v2 (value-of (caddr tree) env check)])
                                    (if (and (equal? (caddr v1) "int") (equal? (caddr v2) "int"))
                                        (if (>= (cadr v2) 0)
                                        (list 'value (expt (cadr v1) (cadr v2)) "int")
                                        (list 'value (expt (cadr v1) (cadr v2)) "float"))
                                        (if (and (equal? (caddr v1) "float") (equal? (caddr v2) "float"))
                                            (list 'value (- (cadr v1) (cadr v2)) "float")
                                            (error "error: not appropariate type for minus operator"))))]

             [(equal? first 'primary-expression) (let ([l1 (value-of (cadr tree) env check)]
                                                       [v1 (value-of (caddr tree) env check)])
                                                   (if (and
                                                        (equal? (caddr v1) "int")
                                                        (equal? (caddr l1) "list"))
                                                       (list-ref (cadr l1) (cadr v1))
                                                       (error "index of primary expression must be integer")))]

             

             [(equal? first 'primary-no-arg) (apply-env (cadadr tree) env check)]
             [(equal? first 'primary-with-args) (apply-env (cadadr tree) env check (value-of (caddr tree) env check))]
                                                  

             [(equal? first 'argument) (list 'value (list (value-of (cadr tree) env check)) "list")]
             [(equal? first 'arguments) (let ([v1 (value-of (cadr tree) env check)]
                                              [v2 (value-of (caddr tree) env check)])
                                          (list 'value (append (cadr v1) (cadr v2)) "list"))]

             [(equal? first 'id) (apply-env (cadr tree) env check)]

             [(equal? first 'empty-list) (list 'value (list) "list")]

             [(equal? first 'expression) (list 'value (list (value-of (cadr tree) env check)) "list")]
             [(equal? first 'expressions) (let ([v1 (value-of (cadr tree) env check)]
                                                             [v2 (value-of (caddr tree) env check)])
                                                         (list 'value (append (cadr v1) (cadr v2)) "list"))]        

             [(equal? first 'value) tree]
             [(equal? first 'print) (begin
                                     (display (cadr(value-of (cadr tree) env check))) (display "\n")
                                     (list 'env env))]
             ))))

;test
(define evaluate (lambda (code_address)
                   (let* ([code_input (with-input-from-file code_address (lambda () (read-string 1000)))]
                         [lex-this (lambda (lexer input) (lambda () (lexer input)))]
                         [my-lexer (lex-this simple-math-lexer (open-input-string code_input))]
                         [parser-res (parse my-lexer)]
                         [tree (car parser-res)]
                         [check (cadr parser-res)])
                     (begin
                       ; (display tree)
                       (value-of tree (empty-env) check)
                       (display ""))))
  )
(define code_address (vector-ref (current-command-line-arguments) 0))
(evaluate code_address)
;(evaluate "tests/1.in")