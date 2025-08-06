import Parser.Syntax
import Eval.Small
open Syntax
open Small

def test (s : String) : Term Unit :=
  match (Syntax.parse ()).run s with
  | none => Syntax.Err
  | some (t, _) => Small.eval t

#eval test "((lambda x . x x) (lambda y . y))"
def main : IO Unit :=
  IO.println s!"Hello"
