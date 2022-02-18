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
