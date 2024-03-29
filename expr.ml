(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | RaiseExn
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Bool _ | Raise | RaiseExn | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> 
      SS.union (free_vars e1) (SS.union (free_vars e2) (free_vars e3))
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) | Letrec (v, e1, e2) ->
      SS.union (SS.remove v (free_vars e2)) (free_vars e1) ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname =
  let ctr = ref ~-1 in
  fun () -> ctr := !ctr + 1;
            "x" ^ string_of_int !ctr ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec sub (ex : expr) : expr =
    match ex with
    | Var x -> if x = var_name then repl else ex
    | Num _ | Bool _ | Raise | RaiseExn | Unassigned -> ex
    | Unop (n, e) -> Unop (n, sub e)
    | Binop (b, e1, e2) -> Binop (b, sub e1, sub e2) 
    | Conditional (e1, e2, e3) -> Conditional (sub e1, sub e2, sub e3)
    | Fun (v, e) -> 
        if v = var_name then ex
        else if SS.mem v (free_vars repl) then 
          let z = new_varname () in
          Fun (z, sub (subst v (Var z) e))
        else Fun (v, sub e)
    | Let (v, d, b) -> 
        if v = var_name then Let (v, sub d, b)
        else if SS.mem v (free_vars repl) then
          let z = new_varname () in
          Let (z, sub d, sub (subst v (Var z) b))
        else Let (v, sub d, sub b)
    | Letrec (v, d, b) -> 
        if v = var_name then ex
        else if SS.mem v (free_vars repl) then
          let z = new_varname () in
          Letrec (z, sub (subst v (Var z) d), sub (subst v (Var z) b))
        else Letrec (v, sub d, sub b)
    | App (f, d) -> App (sub f, sub d)
  in sub exp ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> "x"
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (_, e) -> "~- " ^ exp_to_concrete_string e
  | Binop (b, e1, e2) ->
      let binop_to_string (b : binop) : string =
        match b with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Equals -> "="
        | LessThan -> "<"
      in
      exp_to_concrete_string e1 ^ " " ^ binop_to_string b ^ " " ^
      exp_to_concrete_string e2
  | Conditional (e1, e2, e3) -> 
      "if " ^ exp_to_concrete_string e1 ^ " then " ^ exp_to_concrete_string e2
      ^ " else " ^ exp_to_concrete_string e3
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | Let (v, e1, e2) -> 
      "let " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " 
      ^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) ->
      "let rec " ^ v ^ " = " ^ exp_to_concrete_string e1
      ^ exp_to_concrete_string e2
  | Raise -> "raise Evalexception"
  | RaiseExn -> "raise ImpossibleCase"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> 
      exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2 ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (n, e) -> "Unop(Negate, " ^ exp_to_abstract_string e ^ ")"
  | Binop (b, e1, e2) ->
      let binop_to_string (b : binop) : string =
        match b with
        | Plus -> "Plus"
        | Minus -> "Minus"
        | Times -> "Times"
        | Equals -> "Equals"
        | LessThan -> "LessThan"
      in
      "Binop(" ^ binop_to_string b ^ ", " ^ exp_to_abstract_string e1 ^ ", "
      ^ exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) -> 
      "Conditional(" ^ exp_to_abstract_string e1 ^ ", " 
      ^ exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (v, e1, e2) -> 
      "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " 
      ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (v, e1, e2) ->
      "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", "
      ^ exp_to_abstract_string e2 ^ ")"
  | Raise -> "Raise(EvalException)"
  | RaiseExn -> "RaiseExn(ImpossibleCase)"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> 
      "App(" ^ exp_to_abstract_string e1 ^ ", " 
      ^ exp_to_abstract_string e2 ^ ")" ;;
