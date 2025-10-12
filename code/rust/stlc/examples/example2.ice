zero : (t -> t) -> t -> t
zero = \f => \x => x

x : (t -> t) -> t -> t
x = id zero

id : t -> t
id = \x => x

x