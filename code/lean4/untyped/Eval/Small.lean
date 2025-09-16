import Parser.Syntax
import Eval.Subst
import Std.Data.HashMap
open Syntax
open Subst
open Std

namespace Small

def isValue : Term α -> Bool
| Term.Lam _ _ _ => true
| _ => false

private def eval' : Term Unit -> Option (Term Unit)
| Term.App _ (Term.Lam _ ident1 body1) v2@(Term.Lam _ _ _) => some <| subst ident1 v2 body1
| Term.App _ v1@(Term.Lam _ _ _) t2 => let t2' := eval' t2
                                       (Term.App () v1 .) <*> t2'
| Term.App _ t1 t2 => let t1' := eval' t1
                      (Term.App () . t2) <*> t1'
| _ => none

partial def eval (t : Term Unit) : List (Term Unit) :=
  let rec go t acc :=
    match eval' t with
    | none => (t :: acc).reverse
    | some t' => t :: eval t'
  go t []

def testEval (s : String) : List (Term Unit) :=
  match (Syntax.parse ()).run s with
  | Except.error _=> [Syntax.Err]
  | Except.ok (t, _) => Small.eval t

def testSubst (ident : Tag) (s : String) (t : String) : Term Unit :=
  subst ident (parseD s) (parseD t)

def testRename := rename ∘ parseD

#eval testRename "x"
#eval testRename "(lambda x . x)"
#eval testRename "(lambda x . y)"
#eval testRename "(lambda x . (lambda x .  (x y)))"
#eval testRename "(lambda x . (x (lambda x . (x y))))"
#eval testRename "(lambda x . (x (lambda x . (lambda y . (y x)))))"
#eval testRename "(lambda x . (x (lambda x. (lambda y . (y (z x))))))"

#eval testSubst "x" "s" "x"
#eval testSubst "x" "s" "y"
#eval testSubst "x" "s" "(lambda x . x)"
#eval testSubst "x" "z" "(lambda z . x)"
#eval testSubst "x" "(y z)" "(lambda y . (x y))"
#eval testSubst "x" "(y z)" "(lambda w . (x w))"

#eval testEval "x"
#eval testEval "(lambda x . x)"
#eval testEval "((lambda x . x) (lambda y . y))"
#eval testEval "((lambda x . x) ((lambda y . y) (lambda z . z)))"
#eval testEval "((lambda x . (x x)) ((lambda y . y) (lambda z . z)))"
end Small
