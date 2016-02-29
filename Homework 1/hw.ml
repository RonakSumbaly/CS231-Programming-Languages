exception ImplementMe
exception NormalForm

type t = True | False | If of t * t * t | Int of int | Plus of t * t | GT of t * t

let isval t =
  match t with
      True|False -> true
    | Int _ -> true
    | _ -> false

(* Problem 1a.  *)

let rec step t =
  match t with
      True|False|Int _-> raise NormalForm
    | If(True,t2,t3) -> t2
    | If(False,t2,t3) -> t3
    | If(t1,t2,t3) -> let t1' = step t1 in If(t1',t2,t3)
    | Plus(Int(t1),Int(t2)) -> Int(t1+t2)
    | GT(Int(t1),Int(t2)) -> (if t1 > t2 then True else False)
    | Plus(t1,t2) -> 
     (match (t1,t2) with 
      (t1,t2) ->
        if not(isval(t1)) then 
           let t1' = step t1 in Plus(t1',t2)
        else if isval(t1) then
           let t2' = step t2 in Plus(t1,t2')
        else raise NormalForm)
    | GT(t1,t2) ->
     (match (t1,t2) with 
      (t1,t2) ->
        if not(isval(t1)) then 
           let t1' = step t1 in GT(t1',t2)
        else if isval(t1) then
           let t2' = step t2 in GT(t1,t2')
        else
            raise NormalForm)
  
(* Problem 1b. *)

let rec eval t = try eval(step(t)) with NormalForm -> t
 
 