- 3.2.4\
  3 \
  $3 + 3 * 3 + 3 ^ 3$ = 39
  $3 + 3 * 39 + 39 ^ 3$ = 59439
- 3.2.5\
  (1) $S_0 subset.eq S_1$ \
  (2) if $S_(k-1) subset.eq \S_k$ then $S_k subset.eq S_(k+1)$ \
  $S_k = {"true", "false", "0"} union {"succ" t_1, "pred" t_1, "iszero" t_1 | t_1 in S_(k-1)} union {"if" t_1 "then" t_2 "else" t_3 | t_1 , t_2 , t_3 ∈ S_(k-1) }$ \
  $S_(k+1) = {"true", "false", "0"} union {"succ" t_1^', "pred" t_1^', "iszero" t_1^' | t_1^' in S_k} union {"if" t_1^' "then" t_2^' "else" t_3^' | t_1^' , t_2^' , t_3^' ∈ S_k }$ \
  prove that every element of $S_k$ is an element of $S_(k+1)$ \
  let $x$ be an element of $S_K$, $x in S_k$\
  if $x$ is a constant, trival.\
  if $x = "succ" t_1$, where $t_1 in S_(k-1)$,
  because $S_(k-1) in S_k$, so $t_1 in S_k$.
  by the definition of $S_(k+1)$ ($S_(k+1) = ... union {"succ" t_1^' ... | t_1^' in S_k} union ...$), we have $"succ" t_1 in S_(k+1)$ \
  same if $x = "pred" t$ or $x = "iszero t"$ \
  if $x = "if" t_1 "then" t_2 "else" t_3$where $ t_1 , t_2 , t_3 ∈ S_(k-1)$,
  because $S_(k-1) in S_k$, so $t_1, t_2, t_3 in S_k$.
  by the definition of $S_(k+1)$ ($S_(k+1) = ... union ... union {"if" t_1^' "then" t_2^' "else" t_3^' | t_1^' , t_2^' , t_3^' ∈ S_k }$), we have $"if" t_1 "then" t_2 "else" t_3 in S_(k+1)$ \