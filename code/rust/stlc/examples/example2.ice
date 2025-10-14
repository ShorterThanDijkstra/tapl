def zero : (t -> t) -> t -> t= \f : t => \x : t => x

def x : (t -> t) -> t -> t= id zero

def id : t -> t = \x : t=> x

x