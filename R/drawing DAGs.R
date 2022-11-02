library(DiagrammeR)

grViz("digraph {
  graph []
  node [shape = plaintext, fontname=Arial]
  A [label='Universal Credit Receipt']
  B [label='Income']
  C [label='Lagged pay']
  D [label='Sanctions']
  E [label='Mental Health']
  F [label='Employment']
  edge []
  A -> B -> E
  A -> C -> E
  A -> D -> E
  A -> F -> E
  {rank = same; A; B; E}
  {rank = max; C}
  {rank = min; D}
  {rang = same; F}
}")


grViz("digraph {
      graph [ranksep = 0.1]
      node[shape=plaintext, fontname=Arial]
      A [label='UC transition']
      B [label='UC receipt']
      C [label='Income']
      D [label='Conditionality']
      E [label='Lagged pay']
      F [label='Immediate\nMH impact']
      G [label='Ongoing MH\nstatus']
      H [label='Employment']
      edge [minlen=2]
      A -> B -> C -> G
      A -> E -> F -> G
      B -> D -> F
      D -> G
      C -> H -> G
      D -> H
      B -> H
      {rank = max; B; C; G}
      {rank = min; A; E; F}
      }")

grViz("digraph {
      graph []
      node[shape=plaintext, fontname=Arial]
      A [label='UC eligibility']
      B [label='Change in claim']
      C [label='Managed migration']
      D [label='UC switch\napprehension']
      E [label='UC transition']
      F [label='Legacy receipt']
      G [label='Change in circumstances']
      edge []
      A -> E
      B -> E
      C -> E
      D -> E
      F -> A
      F -> B
      F -> C
      F -> D
      G -> A
      }")

grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "Employment status change" -> Employment
    "Employment status change" -> UC
    "First payment" -> "Immediate MH"
    "First payment" -> "Ongoing MH"
    "First payment" -> Poverty
    "Potential Income" -> Employment
    "Potential Income" -> Income
    Conditionality -> "Immediate MH"
    Conditionality -> "Ongoing MH"
    "Benefit claim change" -> UC
    "Benefit claim change" -> "Immediate MH"
    "Benefit claim change" -> "Potential Income"
    Employment -> "Ongoing MH"
    Employment -> Income
    Income -> "First payment"
    Income -> "Ongoing MH"
    Income -> Poverty
    Poverty -> "Ongoing MH"
    UC -> "First payment"
    UC -> "Potential Income"
    UC -> Conditionality
  {rank = same; UC; "Potential Income"; "Income"; "Ongoing MH"}
  {rank = max; "Employment status change"; Employment; Poverty}
  {rank = min; "First payment"; "Conditionality"; "Immediate MH"}
}'
)

grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "Employment status change" -> Employment
    "Employment status change" -> UC
    "First payment" -> "Immediate MH"
    "First payment" -> "Ongoing MH"
    "First payment" -> Poverty
    "Potential Income" -> Employment
    "Potential Income" -> Income
    Conditionality -> "Immediate MH"
    Conditionality -> "Ongoing MH"
    "Benefit claim change" -> UC
    "Benefit claim change" -> "Immediate MH"
    "Benefit claim change" -> "Potential Income"
    Employment -> "Ongoing MH"
    Employment -> Income
    Income -> "First payment"
    Income -> "Ongoing MH"
    Income -> Poverty
    Poverty -> "Ongoing MH"
    UC -> "First payment"
    UC -> "Potential Income"
    UC -> Conditionality
  {rank = same; UC; "Potential Income"; "Income"; "Ongoing MH"}
  {rank = max; "Employment status change"; Employment; Poverty}
  {rank = min; "First payment"; "Conditionality"; "Immediate MH"}
}'
)


# Attempting a full DAG of 'normal' UC exposure ---------------------------

grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "Potential Income" -> Employment
    "Potential Income" -> Income
    Conditionality -> Employment
    Conditionality -> Income
    Conditionality -> MH
    Eligibility -> "Potential Income"
    Eligibility -> Employment
    Eligibility -> MH
    Eligibility -> Poverty
    Eligibility -> UC
    Employment -> MH
    Employment <-> Income
    Income -> MH
    Income -> Poverty
    Poverty -> MH
    UC -> "Potential Income"
    UC -> Conditionality
  }'
)

grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "Prior MH" -> "Mental Health"
    "Prior MH" -> Employment
    "UC rollout" -> "Mental Health" 
    "UC rollout" -> Employment
    "UC rollout" -> Income
    Employment -> "Mental Health"
    Employment -> Income
    Income -> "Mental Health"
  {rank =same; "UC rollout"; Employment; Income; "Mental Health"}
  {rank = max; "Prior MH"}
  }'
  
)
