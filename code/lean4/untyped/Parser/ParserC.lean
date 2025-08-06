namespace ParserC
structure Parser (α : Type) where
  run : String → Option (α × String)

def Parser.pure {α : Type} (a : α) : Parser α :=
  ⟨fun input => some (a, input)⟩

def Parser.bind {α β : Type} (p : Parser α) (f : α → Parser β) : Parser β :=
  ⟨fun input =>
    match p.run input with
    | some (a, rest) => (f a).run rest
    | none => none⟩

instance : Monad Parser where
  pure := Parser.pure
  bind := Parser.bind

def Parser.map {α β} (f : α → β) (p : Parser α) : Parser β := do
  let a <- p
  pure <| f a

instance : Functor Parser where
  map := Parser.map

def Parser.orElse  {α : Type}  (p1 : Parser α)  (p2 : Unit-> Parser α) : Parser α :=
  ⟨fun input =>
    match p1.run input with
      | a@(some _) => a
      | none => (p2 ()).run input⟩

instance : Alternative Parser where
  failure := ⟨fun _ => none⟩
  orElse := Parser.orElse

def Parser.seq {α β : Type} (pf : Parser (α → β)) (pa : Unit -> Parser α) : Parser β := do
  let f <- pf
  let a <- pa ()
  pure <| f a

instance : Applicative Parser where
  seq := Parser.seq

def item : Parser Char :=
  ⟨fun input =>
    match input.data with
    | []      => none
    | c :: cs => some (c, String.mk cs)⟩

def sat (pred : Char → Bool) : Parser Char := do
  let c <- item
  if pred c
  then pure c
  else failure

def peek (ahead : Nat) : Parser String :=
  ⟨fun input => some (input.take ahead, input)⟩

def peakWith (f: String->Bool) : Parser Bool :=
  ⟨fun input => some (f input, input)⟩

def option {α : Type} (p : Parser α) : Parser Bool :=
  ⟨fun input => match p.run input with
                | none => some (false, input)
                | some (_, rest) => some (true, rest)⟩

def char (c : Char) : Parser Char :=
  sat (· == c)

partial def many {α : Type} (p : Parser α) : Parser (List α) :=
  (do
    let x ← p
    let xs ← many p
    pure (x :: xs)
  ) <|> pure []

partial def many1 {α : Type} (p : Parser α) : Parser (List α) := do
    let x ← p
    let xs ← many p
    pure (x :: xs)

partial def sepBy {α β : Type} (p : Parser α) (sep : Parser β) : Parser (List α) :=
  (do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs)
  ) <|> pure []

partial def sepBy1 {α β : Type} (p : Parser α) (sep : Parser β) : Parser (List α) := do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs)

def eof : Parser Unit :=
  ⟨fun input => if input.isEmpty
                then some ((), "")
                else none⟩

def digit : Parser Char := sat Char.isDigit

def number : Parser String :=
  many1 digit >>= (fun cs => pure (String.mk cs))

def isAlpha (c : Char) : Bool := c.isAlpha

def isAlphaNum (c : Char) : Bool :=
  c.isAlpha || c.isDigit

def string (pat : String) : Parser String :=
  ⟨fun input => if input.startsWith pat
                then some (pat, input.drop pat.length)
                else none⟩

def notFollowedBy (p : Parser α) : Parser Unit :=
  ⟨fun input =>
    match p.run input with
    | some _ => none
    | none   => some ((), input)⟩

def keyword (kw : String) : Parser String :=
  string kw <* notFollowedBy (sat isAlphaNum)

def whitespace : Parser Unit :=
  many (sat Char.isWhitespace) *> pure ()

def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c == '_'

def puncs := ['_', '?', '!', '@', '$']

def isIdentRest (c : Char) : Bool :=
  c.isAlpha || c.isDigit || puncs.elem c

def identifier (keywords : List String) : Parser String :=
  do
    let first ← sat isIdentStart
    let rest ← many (sat isIdentRest)
    let ident := (String.mk (first :: rest))
    if keywords.elem ident
    then failure
    else pure ident


end ParserC
