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


# DAGging analyses --------------------------------------------------------



grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    point1 [shape = point, style=invis width = 0]
  {
    rank = same
    "Universal Credit" -> point1 [dir = none minlen = 2]
    point1 -> "Mental Health" [minlen = 2]
  }
  "Employment\nParent\nDisabled\nStudent\nCaring responsibilities" -> point1
  }'
  
)

grViz(
'digraph {
graph[]
node[shape=plaintext, fontname=Arial]
edge[]
"UC rollout" -> "Mental Health" [minlen = 5]
"UC rollout" -> Employment
"UC rollout" -> "Benefit Income"
Employment -> "Mental Health"
Employment -> "Benefit Income"
"Benefit Income" -> "Mental Health"
{rank =same; "UC rollout"; "Mental Health"}
{rank = same; Employment; "Benefit Income"}
}'
)


grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    Employment -> "Mental Health"
    Employment -> "Earned Income"
    "Earned Income" -> "Benefit Income"
    "UC rollout" -> "Earned Income"
    "Earned Income" -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =same; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Earned Income"; "Benefit Income"}
  }'
  
)

grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    Employment -> "Mental Health"
    Employment -> "Hours Worked"
    "Hours Worked" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    "Hours Worked" -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =same; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Benefit Income"}
  }'
  
)


# Super all-mediators DAG -------------------------------------------------


grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    Employment -> "Mental Health"
    Employment -> "Hours Worked"
    "Hours Worked" -> "Mental Health"
    "Earned Income" -> "Benefit Income"
    "UC rollout" -> "Earned Income"
    "Hours Worked" -> "Earned Income"
    "Earned Income" -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =same; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Earned Income"; "Benefit Income"}
  }'
  
)


# Simpler, 3-mediator DAG -------------------------------------------------


grViz(
  'digraph {
    graph[]
    node[shape=plaintext, fontname=Arial]
    edge[]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    Employment -> "Mental Health"
    Employment -> "Hours Worked"
    "Hours Worked" -> "Benefit Income"
    "Hours Worked" -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =same; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Benefit Income"}
  }'
  
)

# Hernan DiD DAG --------------

grViz(
  '
      digraph {
      graph []
      node [shape = plaintext, fontname = Arial]
      edge []
      
      A [label = "Universal Credit\nrollout"]
      C [label = "Pre-UC mental health"]
      U [label = "Unmeasured\nConfounders"]
      Y [label = "Post-UC mental health"]
      I1 [style = invis]
      I2 [style = invis]
      
  {edge [style = invis]
    U -> I1
    I1 -> A
  }
  {edge [style = invis]
    C -> I2
    I2 -> A
  }
      
      U -> A -> Y
      U -> C -> Y
      U -> Y
  {rank = min; C; I2}
  {rank = max; U; I1}
  {rank = same; A; Y}
      }
      '
)


# mediated ----------------------------------------------------------------
library(manipulateWidget)

#gr1
gr1 <- grViz(
  'digraph {
    graph[label = "a)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    Employment -> "Mental Health"
    Employment -> "Hours Worked"
    "Hours Worked" -> "Benefit Income"
    "Hours Worked" -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Benefit Income"}
  }'
)


#gr2
gr2 <- grViz(
  'digraph {
    graph[label = "c)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = black, style = solid]
    "Hours Worked" [label = <<b>Hours Worked</b>>]
    "UC rollout", Employment [shape = box]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    Employment -> "Mental Health" 
    Employment -> "Hours Worked" 
    "Hours Worked" -> "Benefit Income" [style = dashed, color = red]
    "Hours Worked" -> "Mental Health" [color = red]
    "Benefit Income" -> "Mental Health" [style = dashed, color = red]
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Benefit Income"}
  }'
  
)


gr3 <- grViz(
  'digraph {
    graph[label = "b)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = black, style = solid]
    "Employment" [label = <<b>Employment</b>>]
    "UC rollout" [shape = box]
    "UC rollout" -> "Mental Health" [minlen = 10]
    "UC rollout" -> Employment
    "UC rollout" -> "Benefit Income"
    "UC rollout" -> "Hours Worked"
    Employment -> "Mental Health" [color = red]
    Employment -> "Hours Worked" [style = dashed, color = red]
    "Hours Worked" -> "Benefit Income" [style = dashed, color = red]
    "Hours Worked" -> "Mental Health" [style = dashed, color = red]
    "Benefit Income" -> "Mental Health" [style = dashed, color = red]
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Hours Worked"; "Benefit Income"}
  }'
  
)



combineWidgets(gr1, gr3, gr2, nrow = 3, ncol = 1)


gr2b <-
  grViz(
  'digraph {
    graph[label = "b)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> "Employment"
    "Employment" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  }'
)

gr3b <-
  grViz(
  'digraph {
    graph[label = "c)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> "Hours Worked"
    "Hours Worked" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  }'
)

combineWidgets(gr1, gr2b, gr3b, nrow = 3)  


grViz(
  'digraph {
    graph[label = "a)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> Employment
    Employment -> "Benefit Income"
    "UC rollout" -> "Benefit Income"
    Employment -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Benefit Income"}
  }'

)


# controlled --------------------------------------------------------------


gr1 <- grViz(
  'digraph {
    graph[label = "a)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> Employment
    Employment -> "Benefit Income"
    "UC rollout" -> "Benefit Income"
    Employment -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Benefit Income"}
  }'
  
)

gr2_unlabelled <- grViz(
  'digraph {
    graph[label = "b)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 10, style = solid]
    "UC rollout" -> Employment [color = white]
    Employment -> "Benefit Income" [color = white]
    "UC rollout" -> "Benefit Income" [color = white]
    Employment -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Benefit Income"}
  }'
  
)

combineWidgets(gr1, gr2_unlabelled, ncol = 1)

grViz(
  'digraph {
    graph[label = "b)", labelloc = t, labeljust = l]
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    "UC rollout" [label = <<b>UC rollout</b>>]
    "UC rollout" -> "Mental Health" [minlen = 8, style = solid, label = <&beta;<sub>0</sub>>]
    "UC rollout" -> Employment [color = white]
    Employment -> "Benefit Income" [color = white, label = <&#946;<sub>1</sub>+&beta;<sub>2</sub>>]
    "UC rollout" -> "Benefit Income" [color = white]
    Employment -> "Mental Health"
    "Benefit Income" -> "Mental Health"
  {rank =min; "UC rollout"; "Mental Health"}
  {rank = same; Employment; "Benefit Income"}
  }'
  
)


# simplified separate graphs ----------------------------------------------
# 730 x 280 px

grViz(
  'digraph {
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    UC [label = <<b>UC exposure</b>>]
    MH [label = "Mental Health"]
    E [label = Employment]
    BI [label = "Benefit Income"]
    UC -> MH [minlen = 10, style = solid]
    UC -> E 
    E -> BI 
    UC -> BI 
    E -> MH
    BI -> MH
  {rank =min; UC; MH}
  {rank = same; E; BI}
  }'
  
)

grViz(
  'digraph {
    node[shape=plaintext, fontname=Arial]
    edge[color = red, style = dashed]
    UC [label = <<b>UC exposure</b>>]
    MH [label = "Mental Health"]
    E [label = Employment]
    BI [label = "Benefit Income"]
    UC -> MH [minlen = 10, style = solid]
    UC -> E [color = white]
    E -> BI [color = white]
    UC -> BI [color = white]
    E -> MH
    BI -> MH
  {rank =min; UC; MH}
  {rank = same; E; BI}
  }'
  
)

