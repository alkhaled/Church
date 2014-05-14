module Syntax where

import Data.Map (Map)  


-- The abstract syntax of a lambda language with Integers, let expressions, continuations and printing
data Exp =
    Var String            -- x
  | Lambda String Exp     -- (lambda (x) e)
  | App Exp Exp           -- (e1 e2)
  | StringLit String      -- "foo"
  | IntLit Integer        -- 3 
  | BoolLit Bool          -- True
  | Concat Exp Exp        -- (concat e1 e2)
  | Output Exp            -- (output e)
  | UnitLit               -- unit
  | Plus Exp Exp          -- plus e1 e2  or + e1 e2
  | Minus Exp Exp         -- minus e1 e2
  | Mult Exp Exp          -- times e1 e2 or * e1 e2
  | Div Exp Exp           -- div e1 e2   or / e1 e2 
  | Eq Exp Exp            -- eq e1 e2
  | If Exp Exp Exp        -- If e1 then e2 else e3  #################
  | Let String Exp Exp    -- let f = e1 in e2
  | LetRec String Exp Exp -- letrec f = e1 in e2 ( where f can appear in e2)
  | Try Exp String Exp    -- try e1 catch s -> e2
  | Throw Exp             -- throw e
  deriving (Eq, Ord, Show)

-- Values that expressions evaluate to
data Val =
    Closure String Exp Env
  | StringVal String
  | IntVal Integer
  | BoolVal Bool 
  | UnitVal
  deriving (Eq, Ord, Show)


-- Types
data Type = 
    TInt                   -- int 
  | TBool                  -- bool 
  | TString                -- String
  | TVar  Var              -- X 
  | TPair Type Type        -- t1 * t2 
  | TArrow Type Type       -- t1 -> t2  
  deriving (Eq, Ord, Show)

type Var = String   

-- The variable environment, mapping variables to values
type Env = Map String Val
 