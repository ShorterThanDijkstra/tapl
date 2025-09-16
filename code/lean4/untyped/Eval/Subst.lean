import Parser.Syntax
import Std.Data.HashMap
open Syntax
open Std

namespace Subst
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

def rename (t : Term Unit) : Term Unit := rename' 0 (HashMap.emptyWithCapacity 64) t

partial def subst (ident : Tag) (s : Term Unit) (t : Term Unit) : Term Unit :=
  match t with
  | Term.Var _ x => if x == ident then s else t
  | Term.Lam _ y body => if ident != y && !(frees s).elem y
                         then Term.Lam () y <| subst ident s body
                        --  else t
                         else subst ident s (rename t)
  | Term.App _ t1 t2 => Term.App () (subst ident s t1) (subst ident s t2)

end Subst
