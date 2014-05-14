module TypeChecker where

import Data.Map
import qualified Data.Map as Map
import qualified Data.List as List 
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import Syntax 
import Tests
import Substitution

-- Mapping from TypeVariables to Types
type Context = Map Var Type 

type TcMonad a = 
      WriterT [Constraint]   -- gathers constraints        
      (StateT  TypeVar       -- generates next FreshVar
      (Either  String))      -- error messages for Free Variables
      a

-- [typeCheck e] is the main function that typechecks the expression e and returns its type
-- or an error if it does not typecheck
typeCheck :: Exp -> Either String Type  
typeCheck e = do 
              (t,cons) <- getConstraints e 
              unified <- unify cons
              return (applySubst unified t)
              
getConstraints :: Exp -> Either String (Type,[Constraint])
getConstraints = writerState . (\x -> check x Map.empty)

-- initial state of the State monad
writerState m = evalStateT (runWriterT m) "a"


--[unify c] solves the set of constraints c if possible, and returns the most general substitution
unify :: [Constraint] -> Either String Subst 
unify [] =  return Map.empty
unify ((Equal t1 t2): xs) = if (t1 == t2) 
                            then unify xs
                            else case (Equal t1 t2) of 
                                    Equal (TVar x) t2 -> replace x t2 xs
                                    Equal t1 (TVar x) -> replace x t1 xs 
                                    Equal (TArrow t1 t2) (TArrow t1' t2') -> (unify ((Equal t1 t1') : (Equal t2 t2') : xs))
                                    otherwise -> throwError ("Unification failed for: " ++ show t1 ++ " = " ++show t2)


-- Assigns var x to t and replaces occurences of x with t in constraints 
replace :: Var -> Type -> [Constraint] -> Either String Subst
replace x t constraints = if not (Set.member x (freeTypeVars t)) 
                            then do 
                                   sub <- unify (substConstr (TVar x) t constraints) 
                                   return (Map.insert x t sub)
                            else throwError ("Circular Type: " ++ show x ++ " = " ++ show t)

-- [check e env] typechecks e in the context env and generates a type and a set of constraints
check :: Exp -> Context -> TcMonad Type 
check (IntLit x) _    = return TInt
check (BoolLit b) _   = return TBool 
check (StringLit s) _ = return TString
check (Var x) context = typeLookup x context 

check (Plus n1 n2)  context = arithCheck n1 n2 context
check (Minus n1 n2) context = arithCheck n1 n2 context
check (Mult n1 n2)  context = arithCheck n1 n2 context
check (Div n1 n2)   context = arithCheck n1 n2 context
check (Eq n1 n2) context    = bExpCheck n1 n2 context

check (Let f e1 e2) context = do 
                                t1 <- check e1 context
                                t2 <- check e2 (Map.insert f t1 context)
                                return t2

check (App e1 e2) context   = do
                                tvar <- freshVar
                                t1 <- check e1 context
                                t2 <- check e2 context
                                tell [Equal t1 (TArrow t2 (TVar tvar))]
                                return (TVar tvar)

check (Lambda x e) context  = do 
                                t1 <- freshVar 
                                t2 <- check e (Map.insert x (TVar t1) context)
                                return(TArrow (TVar t1) t2)  

check (If cond thenBranch elseBranch) context = do
                                                    tCond <- check cond context
                                                    tThen <- check thenBranch context
                                                    tElse <- check elseBranch context
                                                    tell [Equal tCond TBool] 
                                                    tell [Equal tThen tElse]
                                                    return tElse



--- Check Helper Functions ----------------------------
freshVar :: TcMonad TypeVar
freshVar = do
  s <- get
  put $  s ++ "1"
  return s      

arithCheck :: Exp  -> Exp -> Context -> TcMonad Type
arithCheck n1 n2 context = do 
                                t1 <- check n1 context
                                t2 <- check n1 context
                                tell [Equal t1 TInt]
                                tell [Equal t2 TInt]
                                return TInt

bExpCheck :: Exp  -> Exp -> Context -> TcMonad Type
bExpCheck n1 n2 context = do 
                                t1 <- check n1 context
                                t2 <- check n1 context
                                tell [Equal t1 TInt]
                                tell [Equal t2 TInt]
                                return TBool

typeLookup x context = case (Map.lookup x context) of 
                        Nothing -> throwError ("Free Variable: " ++ show x)
                        Just a -> do
                                    tell [Equal a a]
                                    return a                                 
--- END Check Helper Functions ----------------------- 

                                   

                           
