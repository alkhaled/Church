Church
======

Church is a lazy, strongly typed, interpreted, polymorphic programming language based on the lambda calculus. A Church program evaluates to a value, but also provides a global output stream. Other than output effects, the language is purely functional (for now).
While Church is strongly typed and uses type inference, any program can also be run dynamically. 

##Usage 
Start ghci with quasiquotes and then load Church.

```
ghci -XQuasiQuotes
:load Church
```

Then, to evaluate a program at the command line, run evalD or evalS on an s-expression. 
```
evalS [sexp| ((lambda (x) (add x x)) 2) |]
```
evalD dynamically evaluates the program without running the type checker, and throws an error whenever a type mismatch occurs.
evalS runs the type checker first, and then evaluates the expression if it is well typed. Note: expressions don't need type annotations since we use type inference.

To enter a longer program or save one, enter it as an sexp in a file of its own, then load that file in with Church and run eval. To see an example of this look at test.hs to find saved test programs. The test programs are also a good introduction to the Chruch's syntax, since the test cases cover all language primitives.

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

### Error handling
Any thrown error will halt execution and execution will resume at the nearest enclosing try block's catch statement. If no enclosing try exists, program execution will halt and return the error. Code inside a try block will behave as if the try does not exist, and  code inside the catch will do the same (only the error string from the try will be preserved). Because of this the try and catch statements interact with the rest of the program as if the try/catch statement didn't exist and must have the same type unless the error is propogated.
When running dynamically all type mismatches will throw errors that the user can catch and handle. 
Since errors are named, catch statements can handle each error differently.

## TO DO:
1. Write a parser to get rid of s-expressions
2. Implement a module system
3. Add a pretty printer
4. Add parallelism to the language.
5. Add algebraic types.
6. Add Native continuation function (call/cc)
