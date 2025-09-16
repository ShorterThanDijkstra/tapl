#import "@preview/curryst:0.5.1": prooftree, rule
#set text(size: 12pt)
- 9.2.2 \
  1: \
  #let rule1 = rule(
    name: [T-APP],
    $Gamma,"f":"Bool" arrow.r "Bool" tack "f" "(if false then true else false)" : "Bool"$,
    rule(
      name: [T-IF],
      $Gamma tack "if false then true else false" : "Bool"$,
      rule(name: [T-FALSE], $Gamma tack "false" : "Bool"$),
      rule(name: [T-TRUE], $Gamma tack "true" : "Bool"$),
      rule(name: [T-FALSE], $Gamma tack "false" : "Bool"$),
    ),
  )

  #prooftree(rule1)
  2: \
  #let rule2 = rule(
    name: [T-ABS],
    $Gamma,"f":"Bool" arrow.r "Bool" tack lambda x : "Bool" . space "f" "(if false then true else false)" : "Bool" arrow.r "Bool"$,
    rule(
      name: [T-APP],
      $Gamma,"f":"Bool" arrow.r "Bool",x : "Bool" tack f "(if false then true else false)": "Bool"$,
      rule(
        name: [T-IF],
        $Gamma tack "if false then true else false" : "Bool"$,
        rule(name: [T-FALSE], $Gamma tack "false" : "Bool"$),
        rule(name: [T-TRUE], $Gamma tack "true" : "Bool"$),
        rule(name: [T-FALSE], $Gamma tack "false" : "Bool"$),
      ),
    ),
  )

  #prooftree(rule2)
