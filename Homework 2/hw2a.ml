exception ImplementMe

(* Problem 5 *)  
  
(* Type t represents abstract syntax trees for the lambda calculus.  A
variable name is represented as an OCaml string. We include the value
True so that you have a simple value to use for testing purposes.

Example: the term ((function x -> x x) (function x -> x)) would be represented as follows:

   FunCall(Function("x", FunCall(Var "x", Var "x")), Function("x", Var "x"))

*)

type t = True | Var of string | Function of string * t | FunCall of t * t

	
(* 3a: Implement the subst function below, which substitutes a given
   value v for all free occurrences of the variable named x in term t,
   returning the resulting term. You may assume that v has no free
   variables. *)

let rec subst (x:string) (v:t) (t:t) =
    match t with 
        True -> True
    |   Var x1 -> 
            if x1 = x then v 
            else Var x1
    |   Function (arg, body) ->
            if arg = x then body
            else subst (x)(v)(body)
    |   FunCall (t1, t2) -> 
            let a = subst (x)(v)(t1) and
             b = subst (x)(v)(t2)
            in FunCall (a, b)
                 
(* 3b: Implement the step function, which takes a term of type t above
and produces a new term that results from taking one step of
computation on t.  If t is a normal form, the step function should
raise the NormalForm exception declared below. *)

exception NormalForm  

let rec step t = 
    match t with 
        True -> raise NormalForm
    |   Var (_) -> raise NormalForm
    |   Function (_,_) -> raise NormalForm
    |   FunCall (t1, t2) ->
            match t1 with
                True | Var (_) -> raise NormalForm
            |   FunCall(_,_) -> 
                    let t1p = step t1 in (FunCall(t1p, t2))
            |   Function(arg, body) ->
                (match t2 with 
                    True | Var(_) -> subst (arg)(t2)(body)
                 |   Function(_,_) -> raise NormalForm
                 |   FunCall(_,_) -> 
                        let t3 = step t2 in (FunCall(t1,t3)))