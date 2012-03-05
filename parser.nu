; parser.nu 
; Parser Generator in the Nu Programming Language

;Utility
(function foreach (f l) (if l (cons (f (car l)) (foreach f (cdr l)))))
(function reduce (f i l) (while l (set i (f i (car l))) (set l (cdr l))) i)

;Expression
(class PExpr is NSObject
  (- children is @children)
  (- name is @name)
  (- initWithName:name children:children is
    (set @name name)
    (set @children children)
    self)
  (- description is 
     (set childrenStr (foreach (do (x) (+ " " (x description))) @children))
     (+ "(" (@name stringValue) (reduce + "" childrenStr) ")"))
   (- stringValue is
    (reduce + "" (foreach (do (x) (x stringValue)) @children)))
  (+ exprWithName:name children:children is
    ((PExpr alloc) initWithName:name children:children)))

;Token
(class PToken is NSObject
  (- description is (+ "(PToken " @name " \"" @content "\")"))
  (- stringValue is @content)
  
  (- initWithName:name content:content is
    (set self (super init))
    (set @name name)
    (set @content content) self)
  (+ tokenWithName:name content:content is
    ((PToken alloc) initWithName:name content:content)))

;Test if target matches beginning of source
;return number of characters matched
(function consume (target source)
  (if ((target class) isSubclassOfClass:NSString)
    (if (and (>= (source length) (target length))
             (== target (source substringToIndex:(target length))))
      (target length))
  (else if ((target class)  isSubclassOfClass:NSRegularExpression)
    (set range ((target firstMatchInString:source options:0 
                        range:`(0 ,(source length))) range))
    (if (and range (== 0 (car range))) (car (cdr range))))))

;See bottom of source file for usage example
(macro parser (name *rules)
  (function replace (expr)
    (if (atom expr) expr
    (else 
      (cons 
        (case (car expr)
          ('| 'any-of)
          ('+ 'one-or-more-of)
          ('* 'zero-or-more-of)
          ('? 'optional-of)
          ('& 'sequence-of)
          (else (car expr)))
        (foreach replace (cdr expr))))))

  (macro any-of (*any)
    (set error (reduce + "expected any of" (foreach (do (f) (+ " " f)) *any)))
    (function any (arg) 
      (set result `(PExpr exprWithName:"|" 
                          children:(list (__expect ,(car arg)))))
      (if (not (cdr arg)) result
      (else `(try ,result (catch (o) ,(any (cdr arg)))))))
    `(do () (try ,(any *any) (catch (object) (throw ,error)))))

  (macro zero-or-more-of (*many)
    `(do () 
        (set __children '())
        (while ,(*many length)
          (try ,@(foreach (do (m) 
            `(set __children (append __children (list (__expect ,m))))) *many)
          (catch (o) (break))))
        (PExpr exprWithName:"*" children:__children)))
 
  (macro one-or-more-of (*many)
    `(do () 
        (set __children (quote ,(foreach (do (m) (__expect m)) *many)))
        (while ,(*many length)
          (try ,@(foreach (do (m) 
            `(set __children (append __children (list (__expect ,m))))) *many)
          (catch (o) (break))))
        (PExpr exprWithName:"+" children:__children)))

  (macro optional-of (*many)
    `(do () 
      (PExpr exprWithName:"?" children:
          (quote ,(try (foreach (do (m) (__expect m)) *many)
                  (catch (o) '()))))))

  (macro sequence-of (*many)
    `(do ()
      (PExpr exprWithName:"&" 
             children:(quote ,(foreach (do (m) (__expect m)) *many)))))

  `(function ,name (__src)
    (set __index 0)
    (function __advance (i) 
      (set __src (__src substringFromIndex:i))
      (set __index (+ __index i)))
    (function __expect (str)
      (if (or ((send str class) isSubclassOfClass:NSString)
              ((send str class) isSubclassOfClass:NSRegularExpression))
        (set __name (str description))
        (if ((send str class) isSubclassOfClass:NSRegularExpression)
          (set __name (+ "/" (str pattern) "/")))
        (let (i (consume str __src))
          (if i (let ((token (PToken tokenWithName:__name
                        content:(__src substringToIndex:i))))
                  (__advance i) token)
          (else (throw (+ "expected " (str description) " at " __index)))))
      (else 
        ;treat as rule
        (str))))

    ,@(foreach (do (__rule)
        `(function ,(car __rule) ()
            (PExpr exprWithName:,(+ (car __rule)) 
                   children:(list ,@(foreach (do (__f) 
                                                `(__expect ,(replace __f))) 
                                             (cdr __rule)))))) *rules)
    (set tree (,(car (car *rules))))
    (if (__src length) (throw (+ "unexpected remainder: " __src)))
    tree))

;Silent if test is successful
;x- is abbreviation for "expected-"
(macro test (name expr x-result x-error)
  (if (!= x-result 'nil)
    `(try (assert (== (,expr description) ,(x-result description))) 
    (catch (o) (puts (+ ,(+ name) " test failed: " 
      (try (,expr description) (catch (p) o))))))
  (else
    `(try ,expr (catch (o) (if (!= (o description) ,(x-error description)) 
      (puts (+ ,(+ name " test failed: ") (o description)))))))))

(test simple ((parser __p (a "a")) "") 
  nil "expected a at 0")
(test simple ((parser __p (a "aa" "b")) "aa") 
  nil "expected b at 2")
(test two-deep ((parser __p (a b) (b c) (c d) (d "d")) "d") 
  (a (b (c (d (PToken d "d"))))) nil)
(test regex ((parser __p (a /a+/)) "aaaa") 
  "(a (PToken /a+/ \"aaaa\"))" nil)
(test any-of ((parser __p (a (| "a" "b" "c"))) "a") 
  (a (| (PToken a "a"))) nil)
(test any-of ((parser __p (a (| "a" "b" "c"))) "d") 
  nil "expected any of a b c")
(test any-of ((parser __p (a (| "a" "b" "c"))) "a") 
  (a (| (PToken a "a"))) nil)
(test any-of-regex ((parser __p (a (| "a" "b" /c/))) "c") 
  "(a (| (PToken /c/ \"c\")))" nil)
(test any-of-sequence ((parser __p (a (| (& "a" "b") "c"))) "ab") 
  (a (| (& (PToken a "a") (PToken b "b")))) nil)
(test zero-or-more-of ((parser __p (a (*))) "") 
  (a (*)) nil)
(test zero-or-more-of ((parser __p (a (* "a" "b" "c"))) "abc") 
  (a (* (PToken a "a") (PToken b "b") (PToken c "c"))) nil)
(test one-or-more-of ((parser __p (a (+ "a" "b" "c"))) "abc") 
  (a (+ (PToken a "a") (PToken b "b") (PToken c "c"))) nil)
(test one-or-more-of ((parser __p (a (+ "a" "b" "c"))) "abcabcabc") 
  (a (+ (PToken a "a") (PToken b "b") (PToken c "c")
        (PToken a "a") (PToken b "b") (PToken c "c")
        (PToken a "a") (PToken b "b") (PToken c "c"))) nil)
(test optional-of ((parser __p (a "b" (? "a" "a") "b")) "bb") 
  (a (PToken b "b") (?) (PToken b "b")) nil)
(test optional-of ((parser __p (a "b" (? "a" "a") "b")) "baab") 
  (a (PToken b "b") (? (PToken a "a") (PToken a "a")) (PToken b "b")) nil)
(test unicode ((parser __p (wsp /(\u0020|\u0009|\u000D|\u000A)*/)) "     ") 
  "(wsp (PToken /(\\u0020|\\u0009|\\u000D|\\u000A)*/ \"     \"))" nil)
