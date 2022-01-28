#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
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

(define-tokens non-empties (NUM bool type ID))
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
     ((statements) (list 'program $1 #f))
     ((checked statements) (list 'program $2 #t)))
    (statements
     ((statement semicolon) (list 'statement $1))
     ((statements statement semicolon) (list 'statements (list 'statement $2) (list 'statement $1))))
    (statement
     ((compound-stmt) (list 'compund-stmt $1))
     ((simple-stmt) (list 'simple-stmt $1)))
    (simple-stmt
     ((assignment) (list 'assignment $1))
     ((return-stmt) (list 'return-stmt $1))
     ((pass) (list 'pass)))
    (compound-stmt
     ((function-def) (list 'function-def $1))
     ((if-stmt) (list 'if-stmt $1))
     ((for-stmt) (list 'for-stmt $1)))
    (assignment
     ((assignment-lhs assign expression) (list 'assignment-lhs $1 $3)))
    (return-stmt
     ((return) (list 'return))
     ((return expression) (list 'return-exp $2)))
    (function-def
     ((def ID open-paranthesis params close-paranthesis return-type statements) (list 'def $2 $4 $6 $7))
     ((def ID open-close-paranthesis return-type statements) (list 'def-with-no-param $2 $4 $5)))
    (params
     ((param-with-default) (list 'param-with-default $1))
     ((params comma param-with-default) (list 'params (list 'param-with-default $3) (list 'params $1))))
    (param-with-default
     ((assignment-lhs assign expression) (list 'param-assignment-lhs $1 $3)))
    (if-stmt
     ((if expression colon statements else-block) (list 'if $2 $4 $5)))
    (else-block
     ((else colon statements) (list 'else $3)))
    (for-stmt
     ((for ID in expression colon statements) (list 'for $2 $4 $6)))
    (expression
     ((disjunction) (list 'disjunction $1)))
    (disjunction
     ((conjunction) (list 'conjunction $1))
     ((disjunction or conjunction) (list 'dis-or-con $1 $3)))
    (conjunction
     ((inversion) (list 'inversion $1))
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
     ((sum plus term) (list 'plusnumbes $1 $3))
     ((sum minus term) (list 'minusnumbers $1 $3))
     ((term) (list 'term $1)))
    (term
     ((term mult factor) (list 'multnumbers $1 $3))
     ((term div factor) (list 'divnumbers $1 $3))
     ((factor) (list 'factor $1)))
    (factor
     ((plus factor) (list 'plus-factor $2))
     ((minus factor) (list 'negate-factor $2))
     ((power) (list 'power $1)))
    (power
     ((atom power-sign factor) (list 'powernumbers $1 $3))
     ((primary) (list 'primary $1)))
    (primary
     ((atom) (list 'atom $1))
     ((primary open-bracket expression close-bracket) (list 'primary-exp $1 $3))
     ((primary open-close-paranthesis) (list 'primary-no-arg $1))
     ((primary open-paranthesis arguments close-paranthesis) (list 'primary-with-args $1 $3)))
    (arguments
     ((expression) (list 'expression $1))
     ((arguments comma expression) (list 'arguments (list 'expression $3) (list 'arguments $1))))
    (atom
     ((ID) (list 'id $1))
     ((bool) (list 'bool $1))
     ((none) (list 'none))
     ((NUM) (list 'num $1))
     ((lst) (list 'list ($1))))
    (lst
     ((open-bracket expressions close-bracket) (list 'lst $2))
     ((open-bracket close-bracket) (list 'lst `())))
    (expressions
     ((expressions comma expression) (list 'expressions (list 'expressions $1) (list 'expressions $3))))
    (assignment-lhs
     ((ID) (list 'id $1))
     ((ID colon type) (list 'id-with-type $1 $3)))
    (return-type
     ((colon) (list 'no-retrun-type))
     ((arrow type colon) (list 'return-type $2)))
    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "b = 234.23 > a;")))
(let ((parser-res (parse my-lexer))) parser-res)

