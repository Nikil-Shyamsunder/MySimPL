open Ast

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

(** [subst e v x] is [e{v/x}]. *)
let subst _ _ _ = failwith "See next section"

(* let rec step : expr -> expr = function | Int _ -> failwith "Cannot step from
   Int" | Bool _ -> failwith "Cannot step from Int" | Var _ -> failwith "Unbound
   variable" | Binop (b, x, y) -> if is_value x && is_value y then step_bop
   (Binop (b, x, y)) else if is_value x then step (Binop (b, x, step y)) else if
   is_value y then step (Binop (b, step x, y)) else step (Binop (b, step x, step
   y)) | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x | Let (x, e1, e2) ->
   Let (x, step e1, e2) | If (Bool true, e2, _) -> e2 | If (Bool false, _, e3)
   -> e3 | If (Int _, _, _) -> failwith "Guard of if must have type bool" | If
   (e1, e2, e3) -> If (step e1, e2, e3) *)

let rec eval_big = function
  | Int x -> Int x
  | Bool b -> Bool b
  | Var v -> Var v
  | Binop (b, x, y) -> step_bop (Binop (b, eval_big x, eval_big y))
  | Let (x, e1, e2) -> subst (eval_big e2) (eval_big e1) x
  | If (e1, e2, e3) -> eval_if e1 e2 e3

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if e1 e2 e3 =
  match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | _ -> failwith "Guard of if must have type bool"

and step_bop = function
  | Binop (Add, Int x, Int y) -> Int (x + y)
  | Binop (Mult, Int x, Int y) -> Int (x * y)
  | Binop (Leq, Int x, Int y) -> Bool (x <= y)
  | _ -> failwith "Not a Bop"
