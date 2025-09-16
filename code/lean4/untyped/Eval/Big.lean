import Parser.Syntax
import Eval.Subst
import Std.Data.HashMap
open Syntax
open Subst
open Std

namespace Big
-- 大步语义
private partial def eval' : Term Unit -> Option (Term Unit)
| t@(Term.Lam _ _ _) => some t
| Term.App _ t1 t2 => match eval' t1 with
                      | some (Term.Lam _ x t12) =>
                          match eval' t2 with
                          | some v2 => let t3 := subst x v2 t12
                                       t3
                                      --  eval' t3
                          | none => none
                      | _ => none
| _ => none

def eval : Term Unit -> List (Term Unit)
| t => [(eval' t).getD Syntax.Err]

def testEval (s : String) : List (Term Unit) :=
  match (Syntax.parse ()).run s with
  | Except.error _=> [Syntax.Err]
  | Except.ok (t, _) => eval t

#eval testEval "x"
#eval testEval "(lambda x . x)"
#eval testEval "((lambda x . x) (lambda y . y))"
#eval testEval "((lambda x . x) ((lambda y . y) (lambda z . z)))"
#eval testEval "((lambda x . (x x)) ((lambda y . y) (lambda z . z)))"
#eval testEval "((lambda x . (x x)) ((lambda y . y) (lambda z . z)))"
end Big
