#import "@preview/curryst:0.5.1": prooftree, rule
- 5.3.8 \
  #let rule1 = rule(name: [B-Value], [$v arrow.b.double v$])
  #let rule2 = rule(name: [B-E-APPABS], 
                   [$t_1 space t_2 arrow.b.double t_3$],
                   [$t_1 arrow.b.double (lambda space x.t_12) space space 
                     t_2 arrow.b.double v_2 space space 
                    \[x arrow v_2\]t_12 space arrow.b.double t_3$]
                 )
  #prooftree(rule1)
  #prooftree(rule2)
