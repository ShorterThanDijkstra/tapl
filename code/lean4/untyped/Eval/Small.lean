import Parser.Syntax
import Std.Data.HashMap
open Syntax
open Std

namespace Small

def isValue : Term α -> Bool
| Term.Lam _ _ _ => true
| _ => false

private def frees' (bounds : List Tag) (t : Term α) : List Tag :=
  match t with
  | Term.Var _ ident => if bounds.elem ident then [] else [ident]
  | Term.Lam _ ident body => frees' (ident :: bounds) body
  | Term.App _ t1 t2 => frees' bounds t1 ++ frees' bounds t2

private def frees  (t : Term α) : List Tag := frees' [] t

private def rename' (depth : Nat) (ctx : HashMap Tag Tag) (t : Term Unit) : Term Unit :=
  match t with
  | Term.Var _ x => let x' := ctx.getD x x
                    Term.Var () x'
  | Term.Lam _ x body => let x' := s!"{x}{depth}"
                         Term.Lam () x' <| rename' (depth + 1) (ctx.insert x x') body
  | Term.App _ t1 t2 => Term.App () (rename' depth ctx t1) (rename' depth ctx t2)

private def rename (t : Term Unit) : Term Unit := rename' 0 (HashMap.emptyWithCapacity 64) t

private partial def subst (ident : Tag) (s : Term Unit) (t : Term Unit) : Term Unit :=
  match t with
  | Term.Var _ x => if x == ident then s else t
  | Term.Lam _ y body => if ident != y && !(frees s).elem y
                         then Term.Lam () y <| subst ident s body
                         else subst ident s (rename t)
  | Term.App _ t1 t2 => Term.App () (subst ident s t1) (subst ident s t2)

private def eval' : Term Unit -> Option (Term Unit)
| Term.App _ (Term.Lam _ ident1 body1) v2@(Term.Lam _ _ _) => some <| subst ident1 v2 body1
| Term.App _ v1@(Term.Lam _ _ _) t2 => let t2' := eval' t2
                                       (Term.App () v1 .) <*> t2'
| Term.App _ t1 t2 => let t1' := eval' t1
                      (Term.App () . t2) <*> t1'
| _ => none

partial def eval (t : Term Unit) : Term Unit :=
  dbg_trace s!"{t}\n"
  match eval' t with
  | none => t
  | some t' => eval t'

end Small
