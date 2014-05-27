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

-- Map from a type to a set of vars, this is used to instantiate variables for polymorphism
data TypeScheme = Scheme Type VarSet

-- Construct a type scheme for a variable
newScheme tvar = Scheme (TVar tvar ) Set.empty

-- Bind a type to an empty scheme
emptyScheme t = Scheme t Set.empty

-- Final Substitution for each variable
type Subst = Map Var Type 

-- Type equality constraints
data Constraint = Equal Type Type

-- Map from program variables to TypeSchemes
type Context = Map Var TypeScheme 


-- [subConstr v r c] substitutes type r for type v in constraints c   
substConstr :: Type -> Type -> [Constraint]-> [Constraint]
substConstr var replacementType constraints = List.map (\(Equal t1 t2) -> (Equal (substitute var replacementType t1)
                                                                                (substitute var replacementType t2)))  constraints
-- [subContext s c] applies substitution r to the context c
substContext :: Subst -> Context -> Context
substContext sub pContext = let subst_mapping v (Scheme t varSet) cons = Map.insert v (Scheme (applySubst sub t) varSet) cons in
                                Map.foldrWithKey subst_mapping Map.empty pContext 


-- [substitute v r t] substitutes type v for type r in t
substitute :: Type -> Type -> Type -> Type 
substitute var replacement t =  case t of 
                                      TUnit -> t
                                      TInt -> t  
                                      TBool -> t
                                      TString -> t
                                      TVar x ->  if t == var then replacement else t
                                      TPair x y -> TPair (substitute var replacement x) (substitute var replacement y)  
                                      TArrow x y -> TArrow (substitute var replacement x) (substitute var replacement y)  

-- [applySub s t] applies substitution r to the type t
applySubst :: Subst -> Type -> Type
applySubst sub t = case t of 
                      TUnit -> t 
                      TInt -> t  
                      TBool -> t
                      TString -> t
                      TVar x ->  case Map.lookup x sub of 
                                        Nothing -> t
                                        Just t1 -> applySubst sub t1
                      TPair x y -> TPair (applySubst sub x) (applySubst sub y)                                          
                      TArrow x y -> TArrow (applySubst sub x) (applySubst sub y)  
                      
-- The following two functions are used to find the variables that need to be instantiated for polymorphism
-- [freeTypeVars t] returns all free type variables in t
freeTypeVars :: Type -> Set.Set TypeVar
freeTypeVars t = case t of 
                  TUnit -> Set.empty
                  TInt -> Set.empty
                  TBool -> Set.empty
                  TString -> Set.empty
                  TVar x -> Set.singleton x
                  TPair t1 t2 -> Set.union (freeTypeVars t1) (freeTypeVars t2)
                  TArrow t1 t2 -> Set.union (freeTypeVars t1) (freeTypeVars t2)

-- [freeVarsPontext c] returns all free variables in context c
freeVarsContext :: Context -> VarSet
freeVarsContext pContext=
    Map.fold (\ (Scheme t vs) freeVarSet -> Set.union (freeTypeVars t) freeVarSet) Set.empty pContext 
