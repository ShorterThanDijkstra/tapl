import Parser.ParserC

open ParserC

namespace Syntax

abbrev Tag := String

inductive Term (α : Type) where
   | Var : α -> Tag -> Term α
   | Lam : α -> Tag -> Term α -> Term α
   | App : α -> Term α -> Term α -> Term α
deriving Nonempty

instance : ToString (Term α) where
  toString := fun t =>
    let rec go (t : Term α) : String :=
      match t with
      | Term.Var _ tag => s!"{tag}"
      | Term.Lam _ tag body =>
          s!"(λ{tag}. {go body})"
      | Term.App _ f arg =>
          s!"({go f} {go arg})"
    s! "\"{go t}\""

def Err := Term.Var () "error"

def keywords := ["lambda", "λ"]

mutual

partial def termParser (a : α) : Parser (Term α) :=
  varParser a <|> lamParser a <|> appParser a

partial def varParser (a : α) : Parser (Term α) := do
  _ <- whitespace
  let iden <- identifier keywords
  pure <| Term.Var a iden

partial def lamParser (a : α) : Parser (Term α) := do
  _ <- whitespace
  _ <- char '('
  _ <- whitespace
  _ <- (string "lambda" <|> string "λ")
  _ <- whitespace
  let bind <- identifier keywords
  _ <- whitespace
  _ <- char '.'
  _ <- whitespace
  let body <- termParser a
  _ <- char ')'
  pure <| Term.Lam a bind body

-- how does lexme do?
/- partial def lamParser (a : α) : Parser (Term α) := do
  _ <- whitespace
  let lp <- option (char '(')
  _ <- whitespace
  _ <- (string "lambda" <|> string "λ")
  _ <- whitespace
  let bind <- identifier keywords
  _ <- whitespace
  _ <- char '.'
  _ <- whitespace
  let body <- termParser a
  if lp then do
        _ <- char ')'
        pure <| Term.Lam a bind body
  else do
       pure <| Term.Lam a bind body
 -/
partial def appParser (a : α) : Parser (Term α) := do
  _ <- whitespace
  _ <- char '('
  let t1 <- termParser a
  _ <- whitespace
  let t2 <- termParser a
  _ <- char ')'
  pure <| Term.App a t1 t2
end

partial def parse (a : α) : Parser (Term α) := do
  _ <- whitespace
  let t <- termParser a
  _ <- eof
  pure t

partial def parseD(s : String) : Term Unit :=
  match (Syntax.parse ()).run s with
  | Except.error _ => Syntax.Err
  | Except.ok (t, _) => t


#eval (Syntax.parse ()).run "x"

#eval (Syntax.parse ()).run "(lambda x.x)"

#eval (Syntax.parse ()).run "((lambda x.x) (lambda x.x))"

#eval ((Syntax.parse ()).run "((lambda x.x) (lambda x.x))")

#eval (Syntax.parse ()).run  "((lambda x . (x x)) (lambda y . y))"

#eval (Syntax.parse ()).run "((x y) z)"

#eval (Syntax.appParser ()).run "(x (y z))"

#eval (Syntax.appParser ()).run "  (y z))"

end Syntax
