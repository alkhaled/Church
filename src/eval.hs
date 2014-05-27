{-# LANGUAGE RecursiveDo #-}

module Eval (eval) where 

import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map
import Data.SExp
import Language.Haskell.TH.Quote
import Control.Monad.Fix

import Syntax


-- ### BEGIN MaybeWriter Monad Definition ###

data MaybeWriter w a = Exc String | Fail | Writer (a,w)
  deriving Show
  
instance (Monoid w) => Monad (MaybeWriter w) where
  return = mwReturn
  (>>=) = mwBind

mwReturn :: (Monoid w) => a -> MaybeWriter w a
mwReturn a = Writer (a, mempty) 

mwBind :: (Monoid w) => MaybeWriter w a -> (a -> MaybeWriter w b) -> MaybeWriter w b
mwBind Fail _ = mwFailure 
mwBind (Exc s) _ = mwThrow s 
mwBind (Writer (x, w1)) f =  case f x of 
                               (Writer (y, w2)) -> Writer (y, mappend w1 w2)
                               Fail -> mwFailure
                               Exc s -> mwThrow s 

mwFailure :: (Monoid w) => MaybeWriter w a
mwFailure = Fail

mwOutput :: (Monoid w) => w -> MaybeWriter w ()
mwOutput w = Writer ((), w)

mwThrow :: (Monoid w) => String -> MaybeWriter w a 
mwThrow s =  Exc s

mwTry :: (Monoid w) => MaybeWriter w a -> (String -> MaybeWriter w a) -> MaybeWriter w a 
mwTry mA f = case mA of 
                Exc s -> f s
                otherwise -> mA -- No error to be caught, return result or fail 

-- MonadFix instance allows us to define letrec in monadic style
-- It allows us to do value recursion on the environment we recieve
-- from the nound expression so that the resulting closure
-- can reference itself in its own environment
instance (Monoid w) => MonadFix (MaybeWriter w) where
    mfix f = let a = f (getVal a) in a
             where getVal (Writer (x,w)) = x
                   getVal _  = error "mfix applied to non-Writer"

-- ### END MaybeWriter Monad Definition ###

-- ### BEGIN helpers ###

coerceClosure :: Val -> MaybeWriter String (String, Exp, Env)
coerceClosure (Closure x e env) = mwReturn (x, e, env)
coerceClosure v = mwThrow ("Expected closure but recieved: " ++ show v)

coerceInt :: Val -> MaybeWriter String Integer
coerceInt (IntVal n) = mwReturn n
coerceInt v = mwThrow ("Expected Int but recieved: " ++ show v)

coerceString :: Val -> MaybeWriter String String
coerceString (StringVal s) = mwReturn s
coerceString v = mwThrow ("Expected String but recieved: " ++ show v)

coerceBool :: Val -> MaybeWriter String Bool
coerceBool (BoolVal b) = mwReturn b
coerceBool v = mwThrow ("Expected Bool but recieved: " ++ show v)

coercePair :: Val -> MaybeWriter String Val
coercePair (PairVal v1 v2) = mwReturn (PairVal v1 v2)
coercePair v = mwThrow ("Expected Pair but recieved: " ++ show v)

evalBool op  x  y = mwReturn (BoolVal (op x y)) 
evalBool op v1 v2 = mwThrow("Exception: Cannot perform equality expression: ("++ 
                                     ") on values: (" ++ show v1 ++") (" ++ show v2 ++" )") 
-- ### END helpers ###

-- ### BEGIN Evaluator ###

-- (evalM e env) evaluates 'e' in the environment 'env' to a final value inside
-- the MaybeWriter monad.  The monoid which the monad manipulates is a String,
-- which is the output that accumulates during evaluation.
evalM :: Exp -> Env -> MaybeWriter String Val                      
evalM  UnitLit       env = return  UnitVal
evalM (IntLit n)     env = return (IntVal n)
evalM (StringLit s)  env = return (StringVal s) 
evalM (BoolLit b)    env = return (BoolVal b)
evalM (Var x)        env = let v = Map.lookup x env in 
                           case v of 
                             Nothing -> mwThrow ("Exception: Unbound Variable: (" ++
                                                                       show x ++ ")") 
                             Just s -> return s 

evalM (Concat e1 e2) env = do x <- coerceString =<< evalM e1 env
                              y <- coerceString =<< evalM e2 env
                              return (StringVal (mappend x  y)) 
                              
evalM (Output e)     env = do v <- evalM e env 
                              case v of 
                                StringVal s -> mwOutput s >>= (\y -> return UnitVal) 
                                IntVal i -> mwOutput (show i) >>= (\y -> return UnitVal) 
                                BoolVal b -> mwOutput (show b) >>= (\y -> return UnitVal) 
                                otherwise -> mwThrow ("Exception: Output Value: " ++  
                                                      show e ++ " cannot be outputed")

evalM (Eq e1 e2)    env = do b1 <- evalM e1 env 
                             b2 <- evalM e2 env 
                             evalBool (==) b1 b2 

evalM (Plus e1 e2)  env = do x <- coerceInt =<< evalM e1 env
                             y <- coerceInt =<< evalM e2 env 
                             return (IntVal ((+) x y)) 

evalM (Minus e1 e2) env = do x <- coerceInt =<< evalM e1 env
                             y <- coerceInt =<< evalM e2 env 
                             return (IntVal ((-) x y)) 
-- inegral division
evalM (Div e1 e2)   env = do x <- coerceInt =<< evalM e1 env
                             y <- coerceInt =<< evalM e2 env 
                             if (y == 0) 
                              then mwThrow "Error: Division by 0"
                              else return (IntVal ((quot) x y))
                             
evalM (Mult e1 e2)  env = do x <- coerceInt =<< evalM e1 env
                             y <- coerceInt =<< evalM e2 env 
                             return (IntVal ((*) x y))                                                                            
                             
evalM (App e1 e2)   env = do (x, e, env1) <- coerceClosure =<< evalM e1 env
                             res2 <- evalM e2 env
                             evalM e (Map.insert x res2 env1) -- env1 will contain all values from env plus those created in the evaluation of e1

evalM (Lambda x e)  env = return (Closure x e env)

evalM (Pair e1 e2 ) env = do v1 <- evalM e1 env
                             v2 <- evalM e2 env 
                             return (PairVal v1 v2)

evalM (Fst e)       env = do (PairVal v1 v2) <- coercePair =<< evalM e env
                             return(v1)

evalM (Snd e)       env = do (PairVal v1 v2) <- coercePair =<< evalM e env
                             return(v2)

evalM (Let f e1 e2) env = do res1 <- evalM e1 env
                             evalM e2 (Map.insert f res1 env)


evalM (If condExp thenExp elseExp) env = do b <- coerceBool =<< evalM condExp env
                                            case b of 
                                              True  -> evalM thenExp env
                                              False -> evalM elseExp env 
                                              
-- SHOULD WE COERCE EVALM E1 TO BE A CLOSURE?
evalM (LetRec f e1 e2) env = do rec v <- evalM e1 (Map.insert f v env)
                                evalM e2 (Map.insert f v env)

evalM (Try e s onFail) env = let aM = evalM e env in
                              mwTry aM (\err -> evalM onFail (Map.insert s (StringVal err) env)) 

evalM (Throw e) env = do s <- coerceString =<< evalM e env
                         mwThrow s
       
-- (eval e env) evaluates 'e' in the environment 'env' to a final pure result.
--
-- The result is either Nothing, indicating failure, or Just (v, out)
-- indicating a result value 'v' with global output 'out'.
eval :: Exp -> Maybe (Val, String)
eval e = case (evalM e Map.empty) of 
               Fail -> Nothing
               Exc s -> Just (UnitVal ,"Uncaught Exception: " ++ s) 
               Writer(v,out) -> Just (v,out)

-- ### END Evaluator ###
