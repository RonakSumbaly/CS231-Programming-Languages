exception ImplementMe

(* Problem 6 *)  

(* As you saw in Problem 5, implementing the lambda calculus, and any
other language that has variables, is annoying, because you have to
manually implement the proper scoping rules, in our case via a
substitution function.

An alternative style, known as higher-order abstract syntax (HOAS), is
a clever way to use variables from the host language (in our case,
OCaml) to represent variables in the language being implemented (in
our case, the lambda calculus).  That way, we get substitution for
free!  You will explore this style below...
*)
   
  
(* Type t represents abstract syntax trees for the lambda calculus.
We are only representing the subset of *closed* terms in the lambda
calculus -- HOAS cannot handle terms with free variables in them.
Note that we no longer have a term for variables below.  Instead,
variables are represented implicitly via the fact that a lambda
calculus function is represented as an OCaml function from terms to
terms (hence the notion of higher-order syntax -- the syntax tree
contains functions).
   
Example: the term ((function x -> x x) (function x -> x)) would be represented as follows:

   FunCall(Function(function x -> FunCall(x, x)), Function(function x -> x))

*)

type t = True | Function of (t -> t) | FunCall of t * t
	
  
(* Implement the step function, which takes a term of type t above and
   produces a new term that results from taking one step of
   computation on t.  If t is a normal form, the step function should
   raise the NormalForm exception. *)

exception NormalForm  
    
let rec step t = 
    match t with
        True -> raise NormalForm
    |   Function (_) -> raise NormalForm
    |   FunCall(func, arg) ->
            match func with 
                True -> raise NormalForm
            |   FunCall(_,_) ->  
                   let func2 = step func in
                   FunCall(func2, arg)
            |   Function(fn) -> 
                   (match arg with
                        True -> fn(True)
                    |   Function(_) -> raise NormalForm
                    |   FunCall(_,_) -> 
                         let arg2 = step arg in FunCall(func, arg2))
                  
                  
(* step ( FunCall(Function(function x -> FunCall(x, x)),FunCall(Function(function x ->x),True)));;
 *)