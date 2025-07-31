open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval t1
  | _ -> false

let rec isval t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval t -> true
  | _ -> false

let rec term_eq t1 t2 =
  match (t1, t2) with
  | TmTrue _, TmTrue _ -> true
  | TmFalse _, TmFalse _ -> true
  | TmIf (_, t1, t2, t3), TmIf (_, t1', t2', t3') ->
      term_eq t1 t1' && term_eq t2 t2' && term_eq t3 t3'
  | TmZero _, TmZero _ -> true
  | TmSucc (_, t1), TmSucc (_, t1') -> term_eq t1 t1'
  | TmPred (_, t1), TmPred (_, t1') -> term_eq t1 t1'
  | TmIsZero (_, t1), TmIsZero (_, t1') -> term_eq t1 t1'
  | _ -> false

let rec eval1 t = match t with
  |   TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies

(* big step *)
let rec eval2 t =
  match t with
  | TmTrue _ -> TmTrue dummyinfo
  | TmFalse _ -> TmFalse dummyinfo
  | TmZero _ -> TmZero dummyinfo
  | TmIf (_, t1, t2, t3) when term_eq (TmTrue dummyinfo) t1 -> eval2 t2
  | TmIf (_, t1, t2, t3) when term_eq (TmFalse dummyinfo) t1 -> eval2 t3
  | TmSucc (fi, t1)->
      let nv1 = eval2 t1 in
      TmSucc (fi, nv1)
  | TmPred (_, t1) -> (
      let nv1 = eval2 t1 in
      match nv1 with
      | TmZero _ -> TmZero dummyinfo
      | TmSucc (_, nv1') -> nv1'
      | _ -> raise NoRuleApplies)
  | TmIsZero (_, t1) -> (
      let nv1 = eval2 t1 in
      match nv1 with
      | TmZero _ -> TmTrue dummyinfo
      | TmSucc _ -> TmFalse dummyinfo
      | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies

let rec eval t =
  try
    eval2 t
  with NoRuleApplies -> 
    print_string "error: ";
    print_flush ();
    t
(* let rec eval t =
  try
    let t' = eval1 t in
    eval t'
  with NoRuleApplies ->
    t *)