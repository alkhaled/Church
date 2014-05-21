{-# LANGUAGE QuasiQuotes #-}

module Parser where

import Data.SExp
import Syntax


--Temporary Parser, Uses S expressions to parse language
parse :: SExp -> Exp
parse   [sexp| unit |] = UnitLit
parse s@[sexp| lambda |] = error $ "bad 'lambda' expression: " ++ show s
parse s@[sexp| concat |] = error $ "bad 'concat' expression: " ++ show s
parse s@[sexp| output |] = error $ "bad 'output' expression: " ++ show s
parse   [sexp| @str:s |] = StringLit s
parse   [sexp| @int:n |] = IntLit n

parse    [sexp| true  |] = BoolLit True
parse    [sexp| false |] = BoolLit False

parse   [sexp| @sym:x |] = Var x

parse   [sexp| (try  @:e1  catch (@sym:s) "->"  @:e2) |] = Try (parse e1) s (parse e2)
parse   [sexp| (throw @:e) |] = Throw (parse e)

parse   [sexp| (lambda (@sym:x) @:e) |] = Lambda x (parse e)
parse s@[sexp| (lambda . @:_) |] = error $ "bad 'lambda' expression: " ++ show s

parse   [sexp| (concat @:e1 @:e2) |] = Concat (parse e1) (parse e2)
parse s@[sexp| (concat . @:_) =|] = error $ "bad 'concat' expression: " ++ show s

parse   [sexp| (add @:e1 @:e2) |] = Plus (parse e1) (parse e2)
parse s@[sexp| (add . @:_) =|] = error $ "bad 'plus' expression: " ++ show s

parse   [sexp| (sub @:e1 @:e2) |] = Minus (parse e1) (parse e2)
parse s@[sexp| (sub . @:_) =|] = error $ "bad 'minus' expression: " ++ show s

parse   [sexp| (mult @:e1 @:e2) |] = Mult (parse e1) (parse e2)
parse s@[sexp| (mult . @:_) =|] = error $ "bad 'mult' expression: " ++ show s

parse   [sexp| (div @:e1 @:e2) |] = Div (parse e1) (parse e2)
parse s@[sexp| (div . @:_) =|] = error $ "bad 'div' expression: " ++ show s

parse   [sexp| (eq @:e1 @:e2) |] = Eq (parse e1) (parse e2)
parse s@[sexp| (eq . @:_) =|] = error $ "bad 'eq' expression: " ++ show s

parse   [sexp| (output @:e) |] = Output (parse e)
parse s@[sexp| (output . @:_) |] = error $ "bad 'output' expression: " ++ show s

parse   [sexp| (fst @:e) |] = Fst (parse e)
parse s@[sexp| (fst . @:_) =|] = error $ "bad 'fst' expression: " ++ show s

parse   [sexp| (snd @:e) |] = Snd (parse e) 
parse s@[sexp| (snd . @:_) =|] = error $ "bad 'snd' expression: " ++ show s

parse   [sexp| (@:e1 @:e2) |] = App (parse e1) (parse e2)

parse   [sexp| (@:e1 "," @:e2) |] = Pair (parse e1) (parse e2)

parse   [sexp| (let @sym:x "="  @:e1  in @:e2)|] = Let x (parse e1) (parse e2)
parse   [sexp| (letrec @sym:x "="  @:e1  in @:e2)|] = LetRec x (parse e1) (parse e2)
parse   [sexp| (if  @:e1  then @:e2 else @:e3 )|] = If (parse e1) (parse e2) (parse e3)
parse _ = error "could not parse s-expression into Exp"
---------------------------------------------------