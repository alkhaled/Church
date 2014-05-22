{-# LANGUAGE QuasiQuotes #-}

module Tests where 

import Data.SExp
import Parser 
import Syntax  

-- finishes with the value `unit` and the string "oh hai"
e1 = parse [sexp|
((lambda (z)
  (output " hai"))
 (output "oh"))

|]

-- finishes with the value `unit` and the string "oh hai"
e2 = parse [sexp| 

((lambda (fconcat)
 (output ((((lambda (x) x) 
            fconcat) 
           "oh") 
          "hai")))
 (lambda (x) 
  (lambda (y) 
   ((lambda (z) 
     (concat x y))
    unit))))

|]


e3 = parse [sexp| (output ( (lambda (x) (concat x "hai")) "oh"  )) |]


e4 = parse [sexp| 
((lambda (x)
  (output "hai"))
 (output "oh"))
|]

 -- Should Fail : No Val composed of two strings

e5 = parse [sexp| (output ( concat (lambda (x) ( x "hai")) "oh"  )) |]

e6 = parse [sexp| (output ( concat ((lambda (x)  x) "oh") ((lambda (x) x) "hai"))) |]

e7 = parse [sexp|
(output (concat ((lambda (x) (concat "1" x )) "2") ((lambda (x) "3") (output "0"))))
|]

-- Env test, should evaluate to "1+2 =3!"
e8 = parse [sexp|
((lambda (y) (output (concat y "!")))
 ((lambda (x)
  ((lambda (y) 
   (concat 
    ((lambda (y)
      (concat 
        ((lambda (y)
           (concat 
            ((lambda (y)  (concat y x )) "1")
             y ))
        "2")
      y)) 
   "=")
  y)) 
 "3"))
"+")) 
|]

-- Invalid output, should throw error then catch and output it
e9 = parse [sexp|
(try (output (lambda (x) x))
   catch (s) "->" (output s ))
|]

-- Intentional error, should be caught and outputed
-- Expression inside throw should evaluate
e10 = parse [sexp|
(try (throw ((lambda (x) x) "Throwing an error"))
   catch (s) "->" (output s ))
|]

-- If an error is thrown in an application don't ignore it and continue computation
e11 = parse [sexp|
((lambda (x) (concat "s" "t")) (throw "Throwing an error"))

|]

-- Catch should be able to continue application if error is ignored
e12 = parse [sexp|
((try (output (lambda (x) x))
   catch (s) "->" (lambda (x) (output x ))) "Testing App ")
|]

-- Successful evaluation of try should continue program
e13 = parse [sexp|
((try (lambda (x) x)
   catch (s) "->" (lambda (x) (output x ))) "Testing App ")
|]


-- Uncaught exception should behave like a Fail, as well as output the uncaught exception
e14 = parse [sexp|
((lambda (x) x) (concat "" (output "2")))
|]

e15 = parse [sexp|
(div ((lambda (x) x) 3) ((lambda (x) x) 3))
|]

e16 = parse [sexp|
(let x "=" 3 in ((lambda (f) (f x)) (lambda (x) (add x 1))))
|]

e17 = parse [sexp|
(if (eq 4 5) then true else false )
|]

e18 = parse [sexp|
(output 3)
|]

--Factorial
e19 = parse [sexp|
(letrec f "=" 
        (lambda (x) (if (eq x 0) then 1 else (mult x (f (sub x 1) ) ) ) )
   in (f 10))
|]

e20 = parse [sexp|
(((lambda (z)
((lambda (y)
    (lambda (x) (concat y x))) "oh")) "no") "hai")
|]

e21 = parse [sexp|
 (eq "4" (lambda (x) x))
|]

e30 = parse [sexp|
(lambda (x) (x x))
|]

e31 = parse [sexp|
(let x "=" (lambda (x) x) in 
(x 7) )
|]

e32 = parse [sexp|
(lambda (x) (add x x))
|]

e33 = parse [sexp|
(let x "=" 0 in 
(let f "=" (lambda (x)  (add x 35)) in 
(f 7)))
|]

e34 = parse [sexp|
(let y "=" 7 in 
(let f "=" (lambda (x)  (add y 35)) in 
(let y "=" 100 in 
(f 7))))
|]

-- should fail since non-poly x is bound to int and bool
e325 = parse [sexp|
((lambda (x)
   (let b "=" (lambda(y) (x y)) in
   (let c "=" (b true) in
   (b 12)))) (lambda (x) x))
|]

-- Let polymorphism test, should type to Bool.
e35 = parse [sexp|
(let a "=" (lambda (x) x) in 
(let b "=" (a 1) in 
  (a false)))
|]

e36 = parse [sexp|
(3 "," true)
|]


e37 = parse [sexp|
(snd (3 "," true))
|]