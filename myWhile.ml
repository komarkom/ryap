open Printf

type variable = Num of int
	| Bool of bool

let sprint_variable c = 
	let rec sprint = function 
		| Num n -> sprintf "%d" n
		| Bool b -> sprintf "%b" b
	in sprintf "%s" (sprint c)

type expr = Val of variable
	| Var of string
    | BinOp of expr * expr * ((int -> int -> int) * string)
	| BoolIntBinOp of expr * expr * ((int -> int -> bool) * string) 
    | BoolBinOp of expr * expr * ((bool -> bool -> bool) * string)

type operator = Assign of (string * expr)
		  | Skip
		  | Read of string
		  | Write of expr
		  | Seq of operator * operator
		  | Myif of expr * operator * operator
		  | Mywhile of expr * operator
		  
let sprint_expr e = 
	let rec sprint = function
		| Val c -> sprintf "%s" (sprint_variable c)
		| Var c -> sprintf "%s" c
		| BinOp (e1, e2, (fn, sign)) -> sprintf "(%s %s %s)" (sprint e1) sign (sprint e2)
		| BoolIntBinOp (e1, e2, (fn, sign)) -> sprintf "(%s %s %s)" (sprint e1) sign (sprint e2)
		| BoolBinOp (e1, e2, (fn, sign)) -> sprintf "(%s %s %s)" (sprint e1) sign (sprint e2)
	in sprintf "%s" (sprint e)	
  
let sprint_prog x = 
	let rec sprint = function
		| Skip -> sprintf "skip"
		| Assign (s, expr) -> sprintf "%s := %s" s (sprint_expr expr) 
		| Write expr -> sprintf "write %s" (sprint_expr expr)
		| Read s -> sprintf "read %s" s
		| Seq (op1, op2) -> sprintf "%s;\n%s;" (sprint op1) (sprint op2)
		| Mywhile (expr, op) -> sprintf "while %s {\n%s\n}" (sprint_expr expr) (sprint op)
		| Myif (expr, op1, op2) -> sprintf "if %s then {\n%s\n} else {\n%s\n}" (sprint_expr expr) (sprint op1) (sprint op2)
	in sprintf "%s" (sprint x)
	
let eval_expr env e = 
	let rec eval env = function
		| Val c -> c
		| Var v -> 
			(try
    	    	List.assoc v env
			with Not_found -> 
	    		let error_msg = sprintf "Variable %s is not defined" v in
	    		raise (Failure error_msg);
			);
					| BinOp (x, y, (fn, _)) -> 
			(
				let x1 = eval env x in
					match x1 with
					| Num n -> 
						(
							let y1 = eval env y in
							match y1 with
							| Num m -> Num (fn n m)
							| _ -> (raise (Failure "Wrong y type"););
						)
					| _ -> (raise (Failure "Wrong x type"););
			)
		| BoolBinOp (x, y, (fn, _)) -> 
			(
				let x1 = eval env x in
					match x1 with
					| Bool n -> 
						(
							let y1 = eval env y in
							match y1 with
							| Bool m -> Bool (fn n m)
							| _ -> (raise (Failure "Wrong y type"););
						)
					| _ -> (raise (Failure "Wrong x type"););
			)
		| BoolIntBinOp (x, y, (fn, _)) -> 
			(
				let x1 = eval env x in
					match x1 with
					| Num n -> 
						(
							let y1 = eval env y in
							match y1 with
							| Num m -> Bool (fn n m)
							| _ -> (raise (Failure "Wrong y type"););
						)
					| _ -> (raise (Failure "Wrong x type"););
			)
		| _ -> (raise (Failure "WTF ERROR"););
	in eval env e

let eval_operator e = 
	let rec eval env = function
	| Skip -> printf "skip\n"; env
	| Assign (s, expr) -> (s, eval_expr env expr) :: env
	| Write s -> printf "%s\n" (sprint_variable (eval_expr env s)); env
	| Read s -> printf "Please enter value for %s:\n" s; env
	| Seq (op1, op2) -> let env' = eval env op1 in eval env' op2 
	| Myif (expr, op1, op2) -> 
		(
			let v = eval_expr env expr in
			match v with
			| Num n -> if n != 0 then eval env op1 else eval env op2;
			| Bool b -> if b then eval env op1 else eval env op2;
		)
	| Mywhile (expr, op) -> 
		(
			let rec cycle env expr operator = 
				(
					let v = eval_expr env expr in
					match v with
					| Num n -> if n != 0 then let env' = eval env op in cycle env' expr op else env;
					| Bool b -> if b then let env' = eval env op in cycle env' expr op else env;
				)
			in cycle env expr op
		)
	| _ -> env
	in eval [] e

let mul = ((fun x y -> x * y), "*")
let div = ((fun x y -> x / y), "/")
let sub = ((fun x y -> x - y), "-")
let add = ((fun x y -> x + y), "+")

let less = ((fun x y -> x < y), "<")

let eql = ((fun x y -> x == y), "==")
let not_eql = ((fun x y -> x != y), "!=")

let _ =
	let yAssign = Assign("y", Val (Num 0)) in
	let xAssign = Assign("x", Val (Num 1)) in
	let pp = BinOp (Var "y", Val (Num 1), add) in
	let ee = BoolIntBinOp (pp, Var "x", eql) in
	let ii = Myif (ee, Write (Var "y"), Skip) in
	let prog = Seq (xAssign, Seq (yAssign, Seq (Read "x", ii))) in
	let pr = sprint_prog prog in
	printf "program:\n\n\n%s\n\n\n" pr;

	eval_operator prog;
	
	let iInc = BinOp(Var "i", Val (Num 1), add) in
	let printIAndInc = Seq (Write (Var "i"), Assign("i", iInc)) in
	let whileCycle = Mywhile (BoolIntBinOp(Var "i", Val (Num 5), less), printIAndInc)  in
	let prog = Seq (Assign("i", Val (Num 3)), whileCycle) in
	let pr = sprint_prog prog in
	printf "program:\n\n\n%s\n\n\n" pr;

	eval_operator prog;
	printf "prog end\n"

