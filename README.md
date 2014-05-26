Church
======

Church is a lazy, strongly typed, interpreted, polymorphic programming language based on the lambda calculus. A Church program evaluates to a value, but also provides a global output stream. Other than output effects, the language is purely functional (for now).
While Church is strongly typed and uses type inference, any program can also be run dynamically. 

##Usage 
Start ghci with quasiquotes and then load Church.

''''
ghci -XQuasiQuotes
:load Church'''

Then, to evaluate a program at the command line, run evalD or evalS on an s-expression. 
'''
evalS [sexp| ((lambda (x) (add x x)) 2) |]
'''
evalD dynamically evaluates the program without running the type checker, and throws an error whenever a type mismatch occurs.
evalS runs the type checker first, and then evaluates the expression if it is well typed. Note: expressions don't need type annotations since we use type inference.

To enter a longer program or save one save it as an sexp in a file of its own, then load that file in with Church and run eval. To see an example of this look at test.hs to find saved test programs. The test programs are also a good introduction to the Chruch's syntax, since the test cases cover all language primitives.

##Features

###Syntax
Currently the language supports the basic lambda calculus extended with pairs, arithmetic and boolean expressions, let and letrec bindings, try/catch error handling, string concatenation, and a global output stream.Other than output effects, the language is purefly functional (for now). For more details look at src/Syntax.hs.

###Type Safety
Church programs are statically typed when evaluated with evalS. Typechecker.hs infers the types in the program and then typechecks it. You also have the option
to run a program without typechecking. In that case the program does not have to typecheck, and type errors will be handled at runtime. Since there are no type annotations, the same program can be run statically or dynamically.



###Polymorphism
Church implements parametric polyorphism. This allows functions to be written generically, yet stil maintain static type safety. Only named functions (bound with a let) are polymorphic.

### Laziness
Lazyness is achieved for free because the language is interpreted in haskell.


## TO DO:
1.Write a parser to get rid of s-expressions
2.Implement a module system
3.Add a pretty printer
4.Add parallelism to the language.
5.Add algebraic types.
6.Add Native continuation function (call/cc)
