import Data.SExp
import Syntax 
import qualified Eval
import qualified TypeChecker as Checker
import Tests
import Parser

-- eval e evaluates an expression e to a value and output (e is already parsed)
eval s = putStrLn $ show $ Eval.eval s

-- evalD [sexp|e|] evaluates sexpression e dynamically without Static typechecking
evalD expr =  eval $ Parser.parse expr

-- evalS [sexp|e|] typechecks expression e then evaluates it
evalS expr =  case Checker.typeCheck (Parser.parse expr) of 
				Left s -> putStrLn s
				Right _ -> evalD expr