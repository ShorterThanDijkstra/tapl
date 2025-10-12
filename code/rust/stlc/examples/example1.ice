zero : (t -> t) -> t -> t
zero = \f => \x => x
one : (t -> t) -> t -> t
one = \f => \x => f x
two : (t -> t) -> t -> t
two = \f => \x => f (f x)
three : (t -> t) -> t -> t
three = \f => \x => f (f (f x))

three two 