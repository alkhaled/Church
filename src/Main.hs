import Syntax 
import qualified Eval
import Tests
import Parser

main = putStrLn $ show $ Eval.eval e19 

eval s = putStrLn $ show $ Eval.eval s