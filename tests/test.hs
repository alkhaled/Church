import  Data.Map 
 
type Context = Map 


get2 x = (x, x)



main = putStrLn  $ show $  (get2  "2")