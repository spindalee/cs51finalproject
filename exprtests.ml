(*
													CS51 Final Project
											Unit Testing for Expressions
*)

open Expr ;;

let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
	else Printf.printf "%s FAILED\n" msg ;;
	
let free_vars_test () =
	unit_test (same_vars (vars_of_list []) (free_vars (Num 3))) "free_vars num";
	unit_test (same_vars (vars_of_list ["x"]) (free_vars (Var "x"))) "free_vars var x";
	unit_test (same_vars (vars_of_list ["x"; "y"]) (free_vars (Binop (Plus, Var "x", Var "y")))) "free_vars two vars";
	unit_test (same_vars (vars_of_list []) (free_vars (Let ("x", Num 5, Var "x")))) "free_vars let";
	unit_test (same_vars (vars_of_list ["x"; "y"; "z"]) (free_vars (Let ("x", Binop (Plus, Var "x", Var "y"), Binop (Times, Var "z", Num 3))))) "free_vars let vars";
	unit_test (same_vars (vars_of_list []) (free_vars (Let ("x", Num 5, Let ("y", Var "x", Binop (Plus, Var "x", Var "y")))))) "free_vars buried let";
	unit_test (same_vars (vars_of_list ["z"]) (free_vars (Let ("x", Num 5, Let ("y", Var "x", Binop (Plus, Var "x", Var "z")))))) "free_vars two lets new var";
	unit_test (same_vars (vars_of_list []) (free_vars (Unop (Negate, Num 3)))) "free_vars unop";
	unit_test (same_vars (vars_of_list ["x"; "y"]) (free_vars (Conditional (Binop (Equals, Var "x", Var "y"), Binop (Plus, Var "x", Num 3), Var "y")))) "free_vars conditional";
	unit_test (same_vars (vars_of_list []) (free_vars (Fun ("x", Binop (Plus, Var "x", Num 3))))) "free_vars fun";
	unit_test (same_vars (vars_of_list ["y"]) (free_vars (Letrec ("x", Num 3, Var "y")))) "free_vars letrec";
	unit_test (same_vars (vars_of_list ["y"]) (free_vars (App ((Fun ("x", Binop (Plus, Var "x", Num 3))), Var "y")))) "free_vars app" ;;

let new_varname_test () =
	unit_test ("x0" = new_varname ()) "new_varname first try";
	unit_test ("x1" = new_varname ()) "new_varname second try";
	let _ = new_varname () in
	unit_test ("x3" = new_varname ()) "new_varname double" ;;

let subst_test () =
	unit_test (subst "x" (Num 5) (Num 3) = Num 3) "subst num";
	unit_test (subst "x" (Num 2) (Var "x") = Num 2) "subst var";
	unit_test (subst "x" (Num 2) (Var "y") = Var "y") "subst free var";
	unit_test (subst "x" (Num 2) (Bool true) = Bool true) "subst bool";
	unit_test (subst "x" (Num 2) (Fun ("x", Binop (Plus, Var "x", Num 3))) 
						= Fun ("x", Binop (Plus, Var "x", Num 3))) "subst fun case 1";
	unit_test (subst "x" (Binop (Plus, Var "y", Num 2)) (Fun ("y", Binop (Plus, Var "x", Num 3))) 
						= Fun ("x4", Binop (Plus, Binop (Plus, Var "y", Num 2), Num 3))) "subst fun case 3";
	unit_test (subst "x" (Num 2) (Fun ("y", Binop (Plus, Var "x", Num 3))) = Fun ("y", Binop (Plus, Num 2, Num 3))) "subst fun case 2";
	unit_test (subst "x" (Num 2) (Let ("x", Binop (Plus, Var "x", Var "y"), Binop (Times, Var "z", Var "x"))) 
						= Let ("x", Binop (Plus, Num 2, Var "y"), Binop (Times, Var "z", Var "x"))) "subst let x";
	unit_test (subst "y" (Num 2) (Let ("x", Binop (Plus, Var "x", Var "y"), Binop (Times, Var "z", Var "x"))) 
						= Let ("x", Binop (Plus, Var "x", Num 2), Binop (Times, Var "z", Var "x"))) "subst let y";
	unit_test (subst "y" (Binop (Plus, Var "x", Num 2)) (Let ("x", Binop (Plus, Var "x", Var "y"), Binop (Times, Var "z", Var "x"))) 
						= Let ("x5", Binop (Plus, Var "x", Binop (Plus, Var "x", Num 2)), Binop (Times, Var "z", Var "x5"))) "subst let case 3";
	unit_test (subst "f" (Num 2) (Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num (0)), Var "x", 
	           App (Var "f", Binop (Minus, Var "x", Num (1))))), App (Var "f", Num (2)))) 
						= Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Var "x", App (Var "f", Binop (Minus, Var "x", Num 1)))), App (Var "f", Num 2))) "subst letrec";
	unit_test (subst "x" (Num 2) (Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num (0)), Var "x", 
	           App (Var "f", Binop (Minus, Var "x", Num (1))))), App (Var "f", Num (2)))) 
						= Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Var "x", App (Var "f", Binop (Minus, Var "x", Num 1)))), App (Var "f", Num 2))) "subst letrec case 2";
	unit_test (subst "x" (Binop (Plus, Var "f", Num 2)) (Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num (0)), 
	           Var "x", App (Var "f", Binop (Minus, Var "x", Num (1))))), App (Var "f", Num (2)))) 
						= Letrec ("x6", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Var "x", App (Var "x6", Binop (Minus, Var "x", Num 1)))), App (Var "x6", Num 2))) "subst letrec case 3" ;;

let exp_to_abstract_string_test () =
	unit_test (exp_to_abstract_string (Num 0) = "Num(0)") "exp_to_abstract_string num";
	unit_test (exp_to_abstract_string (Var "x") = "Var(x)") "exp_to_abstract_string var";
	unit_test (exp_to_abstract_string (Bool true) = "Bool(true)") "exp_to_abstract_string bool";
	unit_test (exp_to_abstract_string (Unop (Negate, Num 3)) = "Unop(Negate, Num(3))") "exp_to_abstract_string unop";
	unit_test (exp_to_abstract_string (Binop (Equals, Var "x", Var "y")) = "Binop(Equals, Var(x), Var(y))") "exp_to_abstract_string binop";
	unit_test (exp_to_abstract_string (Conditional (Bool true, Var "x", Binop (Plus, Num 3, Var "x"))) 
						= "Conditional(Bool(true), Var(x), Binop(Plus, Num(3), Var(x)))") "exp_to_abstract_string conditional";
	unit_test (exp_to_abstract_string (Fun ("x", Binop (Plus, Var "x", Num 2)))
						= "Fun(x, Binop(Plus, Var(x), Num(2)))") "exp_to_abstract_string fun";
	unit_test (exp_to_abstract_string (Let ("x", Num 3, Var "x")) = "Let(x, Num(3), Var(x))") "exp_to_abstract_string let";
	unit_test (exp_to_abstract_string (Letrec ("x", Num 3, Var "x")) = "Letrec(x, Num(3), Var(x))") "exp_to_abstract_string letrec";
	unit_test (exp_to_abstract_string Raise = "Raise(EvalException)") "exp_to_abstract_string raise";
	unit_test (exp_to_abstract_string RaiseExn = "RaiseExn(ImpossibleCase") "exp_to_abstract_string raiseexn";
	unit_test (exp_to_abstract_string (App (Fun ("f", Var "x"), Num 3)) = "App(Fun(f, Var(x)), Num(3))") "exp_to_abstract_string app" ;;

let test_all () =
	free_vars_test ();
	new_varname_test ();
	subst_test ();
	exp_to_abstract_string_test () ;;

let _ = test_all () ;;