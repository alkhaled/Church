module Substitution where 

import Data.Map
import qualified Data.Map as Map
import qualified Data.List as List 
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import Syntax 

type TypeVar = Var 

type VarSet =  Set.Set TypeVar

data TypeScheme = Scheme Type VarSet

newScheme tvar = Scheme (TVar tvar ) Set.empty

emptyScheme t = Scheme t Set.empty

-- Final Substitution for each variable
type Subst = Map Var Type 

-- Type equality constraints
data Constraint = Equal Type Type

type Context = Map Var TypeScheme 


-- SHOULD THIS BE A FOLD?       
substConstr :: Type -> Type -> [Constraint]-> [Constraint]
substConstr var replacementType constraints = List.map (\(Equal t1 t2) -> (Equal (substitute var replacementType t1)
                                                                                (substitute var replacementType t2)))  constraints
substContext :: Subst -> Context -> Context
substContext sub pContext = let subst_mapping v (Scheme t varSet) cons = Map.insert v (Scheme (applySubst sub t) varSet) cons in
                                Map.foldrWithKey subst_mapping Map.empty pContext 


-- Substitutes var with replacement in t
substitute :: Type -> Type -> Type -> Type 
substitute var replacement t =  case t of 
                                      TInt -> t  
                                      TBool -> t
                                      TString -> t
                                      TVar x ->  if t == var then replacement else t
                                      TArrow x y -> TArrow (substitute var replacement x) (substitute var replacement y)  

-- Applies substitution sub to type t
applySubst :: Subst -> Type -> Type
applySubst sub t = case t of 
                      TInt -> t  
                      TBool -> t
                      TString -> t
                      TVar x ->  case Map.lookup x sub of 
                                        Nothing -> t
                                        Just t1 -> applySubst sub t1
                      TArrow x y -> TArrow (applySubst sub x) (applySubst sub y)  

-- Returns all free type variables in t
freeTypeVars :: Type -> Set.Set TypeVar
freeTypeVars t = case t of 
                  TInt -> Set.empty
                  TBool -> Set.empty
                  TVar x -> Set.singleton x 
                  TArrow t1 t2 -> Set.union (freeTypeVars t1) (freeTypeVars t2)

freeVarsPcontext :: Context -> VarSet
freeVarsPcontext pContext=
    Map.fold (\ (Scheme t vs) freeVarSet -> Set.union (freeTypeVars t) freeVarSet) Set.empty pContext 

   

