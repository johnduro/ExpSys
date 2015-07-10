module type VAL =
	sig
		type t
		val add : t -> t -> t
		val mul : t -> t -> t
end

module type EVALEXPR =
	sig
		type t
		type expr = Value of t | Add of (expr * expr) | Mul of (expr * expr)
		val eval : expr -> t
	end

module type MAKEEVALEXPR =
	functor (Val : VAL) -> EVALEXPR with type t = Val.t

module MakeEvalExpr : MAKEEVALEXPR =
	functor (Val : VAL) ->
	struct
		type t = Val.t
		type expr = Value of t | Add of (expr * expr) | Mul of (expr * expr)
		let rec eval e =
			match e with
			| Add (e1, e2) -> Val.add (eval e1) (eval e2)
 			| Mul (e1, e2) -> Val.mul (eval e1) (eval e2)
			| Value vl -> vl
	end

module IntVal : (VAL with type t = int) =
	struct
		type t = int
		let add = ( + )
		let mul = ( * )
	end

module FloatVal : (VAL with type t = float) =
	struct
		type t = float
		let add = ( +. )
		let mul = ( *. )
	end

module StringVal : (VAL with type t = string) =
	struct
		type t = string
		let add s1 s2 = if (String.length s1) > (String.length s2) then s1 else s2
		let mul = ( ^ )
	end

module IntEvalExpr : (EVALEXPR with type t := IntVal.t) = MakeEvalExpr (IntVal)
module FloatEvalExpr : (EVALEXPR with type t := FloatVal.t) = MakeEvalExpr (FloatVal)
module StringEvalExpr : (EVALEXPR with type t := StringVal.t) = MakeEvalExpr (StringVal)

let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
let ie2 = IntEvalExpr.Mul (IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2),IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2))
let ie3 = IntEvalExpr.Add (IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2),IntEvalExpr.Add (IntEvalExpr.Value ~-40, IntEvalExpr.Value ~-2))
(* let ie3 = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2) *)
(* let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2) *)
(* let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2) *)
let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)
let fe2 = FloatEvalExpr.Mul (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.1)
let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ",
(StringEvalExpr.Add (StringEvalExpr.Value "very long",
StringEvalExpr.Value "short")))
let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe)
let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se)
let () = Printf.printf "((40 + 2) * (40 + 2)) Res = %d\n" (IntEvalExpr.eval ie2)
let () = Printf.printf "(41.5 / 10) = (41.5 * 0.1) Res = %f\n" (FloatEvalExpr.eval fe2)
let () = Printf.printf "(40 + 2) + (-40 + -2) = ((40 + 2) - (40 + 2)) Res = %d\n" (IntEvalExpr.eval ie3)

