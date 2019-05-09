(*
													CS51 Final Project
											Unit Testing for Evaluations
*)

open Expr ;;
open Evaluation ;;

let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else Printf.printf "%s FAILED\n" msg ;;

let env = Env.create () ;;
let close_test () =
  unit_test (Env.close (Num 3) env = Closure (Num 3, env)) "close num";
  unit_test (Env.close (Var "x") env = Closure (Var "x", env)) "close var";
  unit_test (Env.close (Bool true) env = Closure (Bool true, env)) "close bool";
  unit_test (Env.close (Unop (Negate, Var "x")) env = Closure (Unop (Negate, Var "x"), env)) "close unop";
  unit_test (Env.close (Binop (Plus, Var "x", Num 3)) env = Closure (Binop (Plus, Var "x", Num 3), env)) "close binop";
  unit_test (Env.close (Conditional (Binop (Equals, Var "x", Num 5), Var "x", Var "y")) env 
             = Closure (Conditional (Binop (Equals, Var "x", Num 5), Var "x", Var "y"), env)) "close binop";
  unit_test (Env.close (Fun ("x", Binop (Plus, Var "x", Num 3))) env 
             = Closure (Fun ("x", Binop (Plus, Var "x", Num 3)), env)) "close fun";
  unit_test (Env.close (Let ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5))) env 
             = Closure (Let ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5)), env)) "close let";
  unit_test (Env.close (Letrec ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5))) env 
             = Closure (Letrec ("x", Binop (Plus, Var "y", Num 3), Binop (Plus, Var "x", Num 5)), env)) "close letrec";
  unit_test (Env.close Raise env = Closure (Raise, env)) "close raise";
  unit_test (Env.close RaiseExn env = Closure (RaiseExn, env)) "close raiseexn";
  unit_test (Env.close (App (Fun ("x", Binop (Plus, Var "x", Num 3)), Num 5)) env
             = Closure (App (Fun ("x", Binop (Plus, Var "x", Num 3)), Num 5), env)) "close app" ;;

let lookup_test () =
 let new_env = Env.extend env "x" (ref (Env.Val (Num 5))) in
 unit_test (Env.lookup new_env "x" = Env.Val (Num 5)) "lookup num";;

let env = Env.create () ;;
let eval_s_test () =
  unit_test (eval_s (Num 0) env = Env.Val (Num 0)) "eval_s num";
  unit_test (eval_s (Bool true) env = Env.Val (Bool true)) "eval_s bool";
  unit_test (eval_s (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_s unop num";
  unit_test (eval_s (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_s binop plus";
  unit_test (eval_s (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_s binop lessthan";
  unit_test (eval_s (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_s conditional true";
  unit_test (eval_s (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_s conditional false";
  unit_test (eval_s (Let ("x", Num 5, Binop(Times, Var "x", Num 3))) env = Env.Val (Num 15)) "eval_s let";
  unit_test (eval_s (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_s let fun app";
  unit_test (eval_s (Letrec ("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0), Num 1, Binop(Times, Var "x", App(Var "f", 
                     Binop(Minus, Var "x", Num 1))))), App(Var "f", Num 4))) env = Env.Val (Num 24)) "eval_s letrec";
  unit_test (eval_s (Fun ("x", Var "x")) env = Env.Val (Fun ("x", Var "x"))) "eval_s fun" ;;

let env2 = Env.extend env "x" (ref (Env.Val (Num 5))) ;;
let eval_d_test () =
  unit_test (eval_d (Var "x") env2 = Env.Val (Num 5)) "eval_d Var";
  unit_test (eval_d (Num 0) env2 = Env.Val (Num 0)) "eval_d Num";
  unit_test (eval_d (Bool true) env = Env.Val (Bool true)) "eval_d bool";
  unit_test (eval_d (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_d unop num";
  unit_test (eval_d (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_d Binop plus";
  unit_test (eval_d (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_d Binop LessThan";
  unit_test (eval_d (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_d Conditional True";
  unit_test (eval_d (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_d Conditional False";
  unit_test (eval_d (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_d Let,Fun,App";
  unit_test (eval_d (Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Num 1, Binop (Times, Var "x", App (Var "f", 
                     Binop (Minus, Var "x", Num 1))))), App (Var "f", Num 4))) env = Env.Val (Num 24)) "eval_d Letrec";
  unit_test (eval_d (Let ("x", Num 1, Let ("f", Fun ("y", Binop (Plus, Var"x", Var"y")), Let ("x", Num 2, App (Var"f", Num 3))))) env
                    = Env.Val (Num 5)) "eval_d Let";
  unit_test (eval_d (Fun ("x", Var "x")) env = Env.Val (Fun ("x", Var "x"))) "eval_d Fun";
  unit_test (eval_d (Fun ("x", Binop (Plus, Var "x", Num 2))) env2 
                     = Env.Val (Fun ("x", Binop (Plus, Var "x", Num 2)))) "eval_d Fun extended env";
  unit_test (eval_d (Let ("x", Num 2, Let ("f", Fun ("y", Binop (Plus, Var"x", Var"y")), Let ("x", Num 8, App (Var"f", Var"x"))))) env
                     = Env.Val (Num 16)) "eval_d let diff" ;;
let eval_l_test () =
  unit_test (eval_l (Num 0) env = Env.Val (Num 0)) "eval_l num";
  unit_test (eval_l (Bool true) env = Env.Val (Bool true)) "eval_l bool";
  let env1 = Env.extend env "x" (ref (Env.Val (Num 2))) in
  unit_test (eval_l (Var "x") env1 = Env.Val (Num 2)) "eval_l var";
  unit_test (eval_l (Unop (Negate, Num 5)) env = Env.Val (Num (~-5))) "eval_l unop num";
  unit_test (eval_l (Binop (Plus, Num 2, Num 3)) env = Env.Val (Num 5)) "eval_l binop plus";
  unit_test (eval_l (Binop (LessThan, Num 1, Num 2)) env = Env.Val (Bool true)) "eval_l binop lessthan";
  unit_test (eval_l (Conditional (Bool true, Num 0, Num 1)) env = Env.Val (Num 0)) "eval_l conditional true";
  unit_test (eval_l (Conditional (Bool false, Num 0, Num 1)) env = Env.Val (Num 1)) "eval_l conditional false";
  unit_test (eval_l (Let ("x", Num 5, Binop(Times, Var "x", Num 3))) env = Env.Val (Num 15)) "eval_l let";
  unit_test (eval_l (Let ("x", Num 1, Let ("f", Fun ("y", Binop (Plus, Var "x", Var "y")), Let ("x", Num 2, App (Var "f", Num 3))))) env
                     = Env.Val (Num 4)) "eval_l let defined";
  unit_test (eval_l (Let ("f", Fun("x", Var "x"), App(Var "f", App(Var "f", Num 3)))) env = Env.Val (Num 3)) "eval_l let fun app";
  unit_test (eval_l (Letrec ("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0), Num 1, Binop(Times, Var "x", App(Var "f", 
                     Binop(Minus, Var "x", Num 1))))), App(Var "f", Num 4))) env = Env.Val (Num 24)) "eval_l letrec";
  unit_test (eval_l (Fun ("x", Var "x")) env = Env.Closure (Fun ("x", Var "x"), env)) "eval_l fun";
  unit_test (eval_l (Let ("x", Num 3, Let ("y", Var "x", Let ("x", Num 2, Binop (Plus, Var "y", Num 3))))) env = Env.Val (Num 6)) "eval_l let twice";
  unit_test (eval_l (Let ("x", Num 2, Let ("f", Fun ("y", Binop (Plus, Var "x", Var "y")), 
                     Let ("x", Num 8, App (Var "f", Var "x"))))) env = Env.Val (Num 10)) "eval_l let diff";
  unit_test (eval_l (Fun ("x", Binop (Plus, Var "x", Num 2))) env1 
                     = Env.Closure (Fun ("x", Binop (Plus, Var "x", Num 2)), env1)) "eval_l fun extended env" ;;

let test_all () =
  close_test ();
  lookup_test ();
  eval_s_test ();
  eval_d_test ();
  eval_l_test () ;;

let _ = test_all () ;;