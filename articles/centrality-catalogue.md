# Centrality catalogue

``` r

library(cograph)
data(student_interactions)
```

A centrality is a **node-level** statistic: it assigns every node a
single number that summarises some aspect of its position in the network
— how many ties it has, how short its paths to others are, how often it
sits between other nodes, or how it participates in the network’s global
structure. Centralities describe **nodes**, not edges or whole graphs,
and they are the most widely used way to compare the relative prominence
of actors within a network.

`cograph` ships the **widest collection of node centrality measures
available in R — 99 in total** — spanning degree and strength, distance
and closeness, shortest-path brokerage, spectral and walk-based
influence, neighbourhood cohesion, directed prestige, and
community/group-based roles. Every measure is **equivalence-validated**:
its output is checked numerically against an established reference
implementation (`igraph`, `sna`, `centiserve`, `brainGraph`,
`influenceR`, `tidygraph`, and NetworkX via `reticulate`), so values
match the canonical definitions to within documented tolerances.

`student_interactions` is a built-in edge-list dataset of observed
student interactions. It can be passed to
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
directly. The single verb
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md) is
the recommended entry point: it returns a tidy one-row-per-node
`data.frame` and accepts an argument for every tuning knob (see the
[argument catalogue](#argument-catalogue) below).

``` r

centrality(student_interactions)
#>    node degree_all strength_all closeness_all betweenness  eigenvector
#> 1    Ac         33          129    0.01754386   26.342857 1.000000e+00
#> 2    Ad         20           36    0.01754386   42.541520 1.096110e-01
#> 3    Fi         24           51    0.01666667   35.721634 1.789565e-01
#> 4    Ik         14           24    0.01666667   25.844874 1.551369e-02
#> 5    Vx         26           43    0.01960784   90.717124 7.238902e-02
#> 6    Rt         20           37    0.01785714   63.135739 1.159931e-01
#> 7    Km         11           16    0.01639344   18.175108 2.804725e-02
#> 8    Gj         19           31    0.01818182  114.599049 3.265786e-02
#> 9    Bd         12           18    0.01612903   21.769264 9.607736e-03
#> 10   Ce         10           13    0.01612903   16.648629 4.473504e-03
#> 11   Oq         14           20    0.01754386   34.151726 2.293068e-02
#> 12   Ya         13           19    0.01612903   18.216122 1.758656e-02
#> 13   Mo         12           17    0.01587302   38.264502 1.003629e-01
#> 14   Hj         12           19    0.01754386   85.816522 2.013125e-02
#> 15   Tv         10           13    0.01666667   25.916306 1.320877e-02
#> 16   Eg         10           12    0.01639344   22.335171 5.783916e-03
#> 17   Pr         11           18    0.01666667   23.974060 7.602231e-02
#> 18   Qs         15           19    0.01785714   76.910851 1.511764e-02
#> 19   Xz         14           18    0.01639344   22.280159 8.533484e-03
#> 20   Np          8            8    0.01666667   12.044048 1.549052e-02
#> 21   Dg         13           13    0.01886792   29.240901 6.260099e-03
#> 22   Hk         16           25    0.01818182   72.176441 1.201845e-01
#> 23   Wy         11           16    0.01639344   34.014358 1.054705e-03
#> 24   Jl         15           18    0.01818182   67.359085 5.009801e-02
#> 25   Fh         21           55    0.01818182   78.588877 2.817489e-01
#> 26   Zb          7            8    0.01538462    9.583333 5.429715e-05
#> 27   Eh          7           13    0.01428571   34.325000 1.163185e-03
#> 28   Be         14           16    0.01851852  105.250898 2.203252e-03
#> 29   Df          8           10    0.01562500   11.026190 4.800876e-06
#> 30   Cf         12           15    0.01724138  119.109163 1.243207e-02
#> 31   Su          6            9    0.01369863   33.154401 1.028472e-04
#> 32   Ln          7            8    0.01408451    5.749708 1.376854e-02
#> 33   Gi          3            4    0.01351351    0.000000 1.130400e-17
#> 34   Uw          4            7    0.01250000    0.000000 8.831252e-20
#>       pagerank
#> 1  0.285861728
#> 2  0.052985644
#> 3  0.077591140
#> 4  0.024836857
#> 5  0.057364714
#> 6  0.042552472
#> 7  0.016655998
#> 8  0.025444498
#> 9  0.014321668
#> 10 0.010679742
#> 11 0.016087378
#> 12 0.016588876
#> 13 0.031180263
#> 14 0.019051413
#> 15 0.012644206
#> 16 0.010425091
#> 17 0.022794289
#> 18 0.019784870
#> 19 0.013229134
#> 20 0.008466679
#> 21 0.010345027
#> 22 0.040383192
#> 23 0.009067435
#> 24 0.020529375
#> 25 0.070538086
#> 26 0.005635780
#> 27 0.010080122
#> 28 0.009378924
#> 29 0.004957518
#> 30 0.017877547
#> 31 0.005136500
#> 32 0.007628879
#> 33 0.005483193
#> 34 0.004411765
```

Pass `type = "all"` to compute every measure at once,
`measures = c(...)` to pick a subset, `digits =` to round, and
`sort_by =` to order the result.

## How to read this catalogue

Every measure below is documented with four fields:

- **Concept** — what the measure captures, in words.
- **Definition** — the formula, faithful to what `cograph` actually
  computes (verified against the package source, which is occasionally
  normalised differently from a textbook statement).
- **Meaning** — how to read a high (or low) value.
- **Example** and **Equivalence reference** — a runnable call and the
  external implementation it is validated against.

### Notation

Throughout, $`G = (V, E)`$ is a graph with $`n = |V|`$ nodes and
adjacency matrix $`A = (a_{ij})`$; $`w_{ij}`$ are edge weights, $`N(v)`$
is the neighbour set of $`v`$, $`k_v = |N(v)|`$ its degree, and
$`s(v) = \sum_u w_{vu}`$ its strength. We write $`d(u, v)`$ for the
shortest-path distance, $`\Delta`$ for the graph diameter, and
$`\sigma_{st}`$ (resp. $`\sigma_{st}(v)`$) for the number of shortest
$`s`$–$`t`$ paths (resp. those passing through $`v`$). $`L = D - A`$ is
the graph Laplacian and $`\mathbf{1}`$ the all-ones vector.

## Catalogue index

**Degree, strength and local connectivity**

[Degree Centrality](#cent-degree) · [Indegree
Centrality](#cent-indegree) · [Outdegree Centrality](#cent-outdegree) ·
[Strength Centrality](#cent-strength) · [Instrength
Centrality](#cent-instrength) · [Outstrength
Centrality](#cent-outstrength) · [Expected Centrality](#cent-expected) ·
[Leverage Centrality](#cent-leverage) · [Lobby Centrality](#cent-lobby)
· [H-Index Strength Centrality](#cent-hindex_strength) · [Local H-Index
Centrality](#cent-local_hindex) · [Semi-Local
Centrality](#cent-semilocal) · [ClusterRank
Centrality](#cent-clusterrank) · [Collective Influence
Centrality](#cent-collective_influence) · [MNC Centrality](#cent-mnc) ·
[DMNC Centrality](#cent-dmnc) · [LAC Centrality](#cent-lac) · [Gravity
Centrality](#cent-gravity)

**Distance and closeness**

[Closeness Centrality](#cent-closeness) · [Incloseness
Centrality](#cent-incloseness) · [Outcloseness
Centrality](#cent-outcloseness) · [Harmonic Centrality](#cent-harmonic)
· [Inharmonic Centrality](#cent-inharmonic) · [Outharmonic
Centrality](#cent-outharmonic) · [Residual Closeness
Centrality](#cent-residual_closeness) · [Dangalchev Closeness
Centrality](#cent-dangalchev) · [Generalized Closeness
Centrality](#cent-generalized_closeness) · [Harary
Centrality](#cent-harary) · [Average Distance
Centrality](#cent-average_distance) · [Barycenter
Centrality](#cent-barycenter) · [Wiener Centrality](#cent-wiener) · [Lin
Centrality](#cent-lin) · [Decay Centrality](#cent-decay) · [Radiality
Centrality](#cent-radiality) · [Gil-Schmidt
Centrality](#cent-gilschmidt) · [Integration
Centrality](#cent-integration) · [Eccentricity
Centrality](#cent-eccentricity) · [Ineccentricity
Centrality](#cent-ineccentricity) · [Outeccentricity
Centrality](#cent-outeccentricity) · [Closeness
Vitality](#cent-closeness_vitality) · [Entropy
Centrality](#cent-entropy) · [Centroid Centrality](#cent-centroid)

**Shortest-path brokerage and flow**

[Betweenness Centrality](#cent-betweenness) · [Stress
Centrality](#cent-stress) · [Load Centrality](#cent-load) · [Bottleneck
Centrality](#cent-bottleneck) · [Bridging Centrality](#cent-bridging) ·
[Local Bridging Centrality](#cent-local_bridging) · [Percolation
Centrality](#cent-percolation) · [Flow Betweenness
Centrality](#cent-flow_betweenness) · [Current-Flow Betweenness
Centrality](#cent-current_flow_betweenness) · [Current-Flow Closeness
Centrality](#cent-current_flow_closeness)

**Spectral, walk and influence**

[Eigenvector Centrality](#cent-eigenvector) · [PageRank
Centrality](#cent-pagerank) · [Authority Centrality](#cent-authority) ·
[Hub Centrality](#cent-hub) · [SALSA Centrality](#cent-salsa) ·
[LeaderRank Centrality](#cent-leaderrank) · [Alpha
Centrality](#cent-alpha) · [Bonacich Power Centrality](#cent-power) ·
[Katz Centrality](#cent-katz) · [Hubbell Centrality](#cent-hubbell) ·
[Subgraph Centrality](#cent-subgraph) · [Laplacian
Centrality](#cent-laplacian) · [Communicability
Centrality](#cent-communicability) · [Communicability Betweenness
Centrality](#cent-communicability_betweenness) · [Random Walk
Centrality](#cent-random_walk) · [Markov Centrality](#cent-markov) ·
[Second-Order Centrality](#cent-second_order) · [Information
Centrality](#cent-information) · [Nonbacktracking
Centrality](#cent-nonbacktracking) · [Diffusion
Centrality](#cent-diffusion) · [Infection Centrality](#cent-infection) ·
[VoteRank Centrality](#cent-voterank) · [Expected Influence
1-Step](#cent-expected_influence_1) · [Expected Influence
2-Step](#cent-expected_influence_2) · [Spanning Tree
Centrality](#cent-spanning_tree)

**Neighbourhood structure and cohesion**

[Transitivity Centrality](#cent-transitivity) · [Constraint
Centrality](#cent-constraint) · [Effective Size
Centrality](#cent-effective_size) · [Topological Coefficient
Centrality](#cent-topological_coefficient) · [Diversity
Centrality](#cent-diversity) · [Cross-Clique
Centrality](#cent-cross_clique) · [Coreness Centrality](#cent-coreness)
· [Onion Centrality](#cent-onion) · [K-Reach Centrality](#cent-kreach)

**Directed prestige and hierarchy**

[Prestige Domain Centrality](#cent-prestige_domain) · [Prestige Domain
Proximity Centrality](#cent-prestige_domain_proximity) · [Local Reaching
Centrality](#cent-reaching_local) · [Pairwise Disconnectivity
Centrality](#cent-pairwisedis) · [Trophic Level
Centrality](#cent-trophic_level)

**Community and group-based**

[Participation Coefficient](#cent-participation) · [Within-Module Z
Centrality](#cent-within_module_z) · [Gateway Centrality](#cent-gateway)
· [Brokerage Coordinator Centrality](#cent-brokerage_coordinator) ·
[Brokerage Itinerant Centrality](#cent-brokerage_itinerant) · [Brokerage
Representative Centrality](#cent-brokerage_representative) · [Brokerage
Gatekeeper Centrality](#cent-brokerage_gatekeeper) · [Brokerage Liaison
Centrality](#cent-brokerage_liaison)

## Degree, strength and local connectivity

### Degree Centrality

Degree centrality counts the number of direct ties incident on a node.
In directed graphs, it can be separated into incoming and outgoing ties.

``` math
C_D(v) = \sum_{u \in V} a_{vu} = k_v
```

**Meaning.** A high degree value indicates many direct observed
connections. It is a local connectivity measure and does not imply
brokerage or global reach.

``` r

centrality_degree(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::degree()`](https://r.igraph.org/reference/degree.html) under
matching `mode` and loop settings.

### Indegree Centrality

Indegree counts ties pointing into a node under a directed
interpretation.

``` math
k_v^{\mathrm{in}} = \sum_{u \in V} a_{uv}
```

**Meaning.** A high indegree value indicates many incoming observed
ties. Its substantive meaning depends on what edge direction represents.

``` r

centrality_indegree(student_interactions)
```

**Equivalence reference.** Equivalent to `igraph::degree(mode = "in")`.

### Outdegree Centrality

Outdegree counts ties leaving a node under a directed interpretation.

``` math
k_v^{\mathrm{out}} = \sum_{u \in V} a_{vu}
```

**Meaning.** A high outdegree value indicates many outgoing observed
ties. In transition networks, this can mean many possible next states.

``` r

centrality_outdegree(student_interactions)
```

**Equivalence reference.** Equivalent to `igraph::degree(mode = "out")`.

### Strength Centrality

Strength centrality is weighted degree: it sums edge weights instead of
counting edges.

``` math
s(v) = \sum_{u \in V} w_{vu}
```

**Meaning.** A high strength value indicates high total incident edge
weight. This should be interpreted according to how weights were
defined.

``` r

centrality_strength(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::strength()`](https://r.igraph.org/reference/strength.html)
when weights are supplied.

### Instrength Centrality

Instrength sums incoming edge weights.

``` math
s^{\mathrm{in}}(v) = \sum_{u \in V} w_{uv}
```

**Meaning.** A high instrength value indicates high total incoming
weight. It is a weighted incoming-connectivity measure.

``` r

centrality_instrength(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::strength(mode = "in")`.

### Outstrength Centrality

Outstrength sums outgoing edge weights.

``` math
s^{\mathrm{out}}(v) = \sum_{u \in V} w_{vu}
```

**Meaning.** A high outstrength value indicates high total outgoing
weight. It does not show whether that weight is concentrated or
distributed.

``` r

centrality_outstrength(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::strength(mode = "out")`.

### Expected Centrality

Expected centrality sums the degrees of a node’s neighbours.

``` math
C_{\mathrm{exp}}(v) = \sum_{u \in N(v)} k_u
```

**Meaning.** A high value indicates adjacency to well-connected nodes.

``` r

centrality_expected(student_interactions)
```

**Equivalence reference.** Implemented as a native neighbourhood
centrality in `cograph`.

### Leverage Centrality

Leverage centrality compares a node’s degree with the degrees of its
neighbours.

``` math
\ell(v) = \frac{1}{k_v} \sum_{u \in N(v)} \frac{k_v - k_u}{k_v + k_u}
```

**Meaning.** A positive high value indicates that the node is more
connected than its neighbours under this degree comparison.

``` r

centrality_leverage(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::leverage()`](https://rdrr.io/pkg/centiserve/man/leverage.html).

### Lobby Centrality

Lobby centrality is the h-index of neighbour degrees.

``` math
h(v) = \max\bigl\{ h : |\{ u \in \{v\} \cup N(v) : k_u \ge h \}| \ge h \bigr\}
```

**Meaning.** A high value indicates many neighbours that themselves have
reasonably high degree.

``` r

centrality_lobby(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::lobby()`](https://rdrr.io/pkg/centiserve/man/lobby.html).

### H-Index Strength Centrality

H-index strength is a weighted h-index-style neighbourhood measure.

``` math
h_s(v) = \max\bigl\{ h : |\{ u \in \{v\} \cup N(v) : s(u) \ge h \}| \ge h \bigr\}
```

**Meaning.** A high value indicates strong ties to neighbours meeting a
weighted connectivity threshold.

``` r

centrality(student_interactions, measures = "hindex_strength")
```

**Equivalence reference.** Equivalent to lobby centrality on unweighted
graphs in package tests.

### Local H-Index Centrality

Local h-index centrality iteratively applies h-index logic to local
neighbourhoods.

``` math
h^{(t+1)}(v) = \mathcal{H}\bigl(\{ h^{(t)}(u) : u \in N(v) \}\bigr), \quad h^{(0)}(v) = k_v
```

**Meaning.** A high value indicates a locally robust neighbourhood
position under the h-index updating rule.

``` r

centrality(student_interactions, measures = "local_hindex")
```

**Equivalence reference.** Implemented following local h-index
centrality formulations and checked in package tests.

### Semi-Local Centrality

Semi-local centrality extends degree-like counting beyond immediate
neighbours.

``` math
C_{SL}(v) = \sum_{u \in N(v)} \sum_{w \in N(u)} \bigl| \{\, x : d(w, x) \le 2 \,\} \bigr|
```

**Meaning.** A high value indicates proximity to a locally
well-connected neighbourhood.

``` r

centrality_semilocal(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::semilocal()`](https://rdrr.io/pkg/centiserve/man/semilocal.html).

### ClusterRank Centrality

ClusterRank combines neighbour degree information with local clustering.

``` math
C_{CR}(v) = c(v) \sum_{u \in N(v)} \bigl( k_u + 1 \bigr), \quad c(v) = \text{local clustering coefficient}
```

**Meaning.** A high value indicates local connectedness adjusted for
clustering-related redundancy.

``` r

centrality_clusterrank(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::clusterrank()`](https://rdrr.io/pkg/centiserve/man/clusterrank.html).

### Collective Influence Centrality

Collective influence combines a node’s excess degree with excess degree
on a local boundary.

``` math
\mathrm{CI}_\ell(v) = (k_v - 1) \sum_{u \,\in\, \partial B(v, \ell)} (k_u - 1), \quad \ell = 2
```

**Meaning.** A high value indicates potential importance under the
collective influence model.

``` r

centrality(student_interactions, measures = "collective_influence")
```

**Equivalence reference.** Based on Morone and Makse’s collective
influence formulation.

### MNC Centrality

Maximum neighbourhood component centrality is the size of the largest
connected component in a node’s neighbourhood.

``` math
\mathrm{MNC}(v) = \max_{C \,\in\, \mathcal{C}(G[N(v)])} |C|
```

**Meaning.** A high value indicates that many neighbours belong to one
connected local component.

``` r

centrality_mnc(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::mnc()`](https://rdrr.io/pkg/centiserve/man/mnc.html).

### DMNC Centrality

DMNC is a density-adjusted version of maximum neighbourhood component
centrality.

``` math
\mathrm{DMNC}(v) = \frac{E_c}{N_c^{\,\varepsilon}}, \quad \varepsilon = 1.7
```

**Meaning.** A high value indicates a large and dense local neighbour
component under the chosen density exponent. Here $`E_c`$ and $`N_c`$
are the edges and nodes of the largest component of $`G[N(v)]`$.

``` r

centrality_dmnc(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::dmnc()`](https://rdrr.io/pkg/centiserve/man/dmnc.html).

### LAC Centrality

LAC, or local average connectivity, measures connectivity in a node’s
neighbourhood.

``` math
\mathrm{LAC}(v) = \frac{1}{k_v} \sum_{u \in N(v)} \deg_{G[N(v)]}(u)
```

**Meaning.** A high value indicates that the node’s neighbours form a
relatively connected local structure.

``` r

centrality_lac(student_interactions)
```

**Equivalence reference.** Corresponds to CytoHubba-style local average
connectivity.

### Gravity Centrality

Gravity centrality treats node degree or mass as attracting over graph
distance.

``` math
G(v) = \sum_{u \ne v,\; d(u,v) < \infty} \frac{k_u \cdot ks(u)}{d(u, v)^2}, \quad ks = \text{k-shell}
```

**Meaning.** A high value indicates proximity to high-degree nodes under
the gravity formula.

``` r

centrality(student_interactions, measures = "gravity")
```

**Equivalence reference.** Formula verified in package tests against the
degree-mass-over-distance definition.

## Distance and closeness

### Closeness Centrality

Closeness centrality summarises how short a node’s paths are to other
reachable nodes.

``` math
C_C(v) = \frac{1}{\sum_{u \ne v} d(v, u)}
```

**Meaning.** A high value indicates short graph distances to other
nodes. In disconnected graphs, harmonic centrality may be easier to
interpret.

``` r

centrality_closeness(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::closeness()`](https://r.igraph.org/reference/closeness.html)
under matching `mode` and weight settings.

### Incloseness Centrality

Incloseness computes closeness over incoming directed paths.

``` math
C_C^{\mathrm{in}}(v) = \frac{1}{\sum_{u \ne v} d(u, v)}
```

**Meaning.** A high value indicates that other nodes can reach the focal
node through short directed paths.

``` r

centrality_incloseness(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::closeness(mode = "in")`.

### Outcloseness Centrality

Outcloseness computes closeness over outgoing directed paths.

``` math
C_C^{\mathrm{out}}(v) = \frac{1}{\sum_{u \ne v} d(v, u)}
```

**Meaning.** A high value indicates that the focal node can reach other
nodes through short directed paths.

``` r

centrality_outcloseness(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::closeness(mode = "out")`.

### Harmonic Centrality

Harmonic centrality sums reciprocal shortest-path distances. Unreachable
nodes contribute zero.

``` math
C_H(v) = \sum_{u \ne v} \frac{1}{d(v, u)}
```

**Meaning.** A high value indicates broad reach through short paths
while handling disconnected components more gracefully than classical
closeness.

``` r

centrality_harmonic(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::harmonic_centrality()`](https://r.igraph.org/reference/harmonic_centrality.html)
under matching settings.

### Inharmonic Centrality

Inharmonic centrality computes harmonic centrality over incoming
directed paths.

``` math
C_H^{\mathrm{in}}(v) = \sum_{u \ne v} \frac{1}{d(u, v)}
```

**Meaning.** A high value indicates that the node is reachable from many
others through short directed paths.

``` r

centrality_inharmonic(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::harmonic_centrality(mode = "in")`.

### Outharmonic Centrality

Outharmonic centrality computes harmonic centrality over outgoing
directed paths.

``` math
C_H^{\mathrm{out}}(v) = \sum_{u \ne v} \frac{1}{d(v, u)}
```

**Meaning.** A high value indicates that many nodes can be reached from
the focal node through short directed paths.

``` r

centrality_outharmonic(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::harmonic_centrality(mode = "out")`.

### Residual Closeness Centrality

Residual closeness sums $`1 / 2^d`$ over shortest-path distances (the
focal node contributes $`1`$).

``` math
C_R(v) = \sum_{u \in V} 2^{-d(v, u)}
```

**Meaning.** A high value indicates many close reachable nodes with
rapid distance decay.

``` r

centrality_residual_closeness(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::closeness.residual()`](https://rdrr.io/pkg/centiserve/man/closeness.residual.html).

### Dangalchev Closeness Centrality

Dangalchev closeness is the residual-closeness distance-decay measure.

``` math
C_D(v) = \sum_{u \in V} 2^{-d(v, u)}
```

**Meaning.** A high value indicates short-distance reach with distant
nodes down-weighted.

``` r

centrality_dangalchev(student_interactions)
```

**Equivalence reference.** Implemented as the residual closeness
equivalent used in `cograph`.

### Generalized Closeness Centrality

Generalized closeness sums $`\alpha^d`$, where $`d`$ is the
shortest-path distance.

``` math
C_G(v) = \sum_{u \in V} \alpha^{\,d(v, u)}, \quad \alpha = 0.5
```

**Meaning.** A high value indicates reach under the chosen
distance-decay parameter.

``` r

centrality_generalized_closeness(student_interactions)
```

**Equivalence reference.** Corresponds to generalized distance-decay
closeness formulations.

### Harary Centrality

Harary centrality sums inverse squared shortest-path distances.

``` math
C_{\mathrm{Har}}(v) = \sum_{u \ne v} \frac{1}{d(v, u)^2}
```

**Meaning.** A high value indicates many nearby reachable nodes, with
distant nodes strongly down-weighted.

``` r

centrality_harary(student_interactions)
```

**Equivalence reference.** Based on the Harary distance family.

### Average Distance Centrality

Average distance centrality summarises mean shortest-path distance from
a node (the focal node’s $`d = 0`$ term is included; the centiserve
convention divides by $`n + 1`$).

``` math
\bar{d}(v) = \frac{\sum_{u \in V} d(v, u)}{n + 1}
```

**Meaning.** Lower values indicate shorter average graph distance. The
direction of interpretation should be checked because it is a distance
quantity.

``` r

centrality_average_distance(student_interactions)
```

**Equivalence reference.** Validated against the corresponding
`centiserve` average-distance measure.

### Barycenter Centrality

Barycenter centrality is the inverse of the sum of shortest-path
distances.

``` math
C_{\mathrm{Bary}}(v) = \frac{1}{\sum_{u \ne v} d(v, u)}
```

**Meaning.** A high value indicates a small total distance to other
reachable nodes.

``` r

centrality_barycenter(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::barycenter()`](https://rdrr.io/pkg/centiserve/man/barycenter.html).

### Wiener Centrality

Wiener centrality records total shortest-path distance from a node
(unreachable pairs contribute zero).

``` math
W(v) = \sum_{u \ne v} d(v, u)
```

**Meaning.** Lower values indicate shorter total distance. It is better
read as distance burden than influence.

``` r

centrality_wiener(student_interactions)
```

**Equivalence reference.** Based on the Wiener index distance
formulation.

### Lin Centrality

Lin centrality combines reachable nodes and total distance to those
nodes.

``` math
C_{\mathrm{Lin}}(v) = \frac{|R(v)|^2}{\sum_{u \in R(v)} d(v, u)}, \quad R(v) = \{ u \ne v : d(v,u) < \infty \}
```

**Meaning.** A high value indicates many reachable nodes through
relatively short paths.

``` r

centrality_lin(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::lincent()`](https://rdrr.io/pkg/centiserve/man/lincent.html).

### Decay Centrality

Decay centrality sums reach discounted by distance (the focal node
contributes $`1`$).

``` math
C_{\delta}(v) = \sum_{u \in V} \delta^{\,d(v, u)}, \quad \delta = 0.5
```

**Meaning.** A high value indicates access to many nodes, especially
nearby nodes. The value depends on the decay parameter.

``` r

centrality_decay(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::decay()`](https://rdrr.io/pkg/centiserve/man/decay.html).

### Radiality Centrality

Radiality compares node distances to the graph diameter.

``` math
\mathrm{Rad}(v) = \frac{\sum_{u \ne v} \bigl( \Delta + 1 - d(v, u) \bigr)}{n - 1}
```

**Meaning.** A high value indicates short distances to others relative
to overall graph diameter.

``` r

centrality_radiality(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::radiality()`](https://rdrr.io/pkg/centiserve/man/radiality.html).

### Gil-Schmidt Centrality

Gil-Schmidt centrality sums reciprocal distances and normalises by graph
size.

``` math
C_{GS}(v) = \frac{1}{n - 1} \sum_{u \ne v} \frac{1}{d(v, u)}
```

**Meaning.** A high value indicates broad reciprocal-distance access to
the network.

``` r

centrality_gilschmidt(student_interactions)
```

**Equivalence reference.** Validated against `sna` Gil-Schmidt style
centrality.

### Integration Centrality

Integration centrality is a distance-based measure of how integrated a
node is within the graph.

``` math
\mathrm{Int}(v) = \sum_{u \ne v} \left( 1 - \frac{d(v, u) - 1}{d_{\max}} \right), \quad d(v,u) = d_{\max} + 1 \text{ if unreachable}
```

**Meaning.** A high value indicates broad distance-based access to the
network.

``` r

centrality_integration(student_interactions)
```

**Equivalence reference.** Implemented as a native distance-based
centrality in `cograph`.

### Eccentricity Centrality

Eccentricity is the maximum shortest-path distance from a node to any
reachable node.

``` math
\varepsilon(v) = \max_{u \in V} d(v, u)
```

**Meaning.** Lower eccentricity generally indicates smaller worst-case
distance. A high value means at least one reachable node is far away.

``` r

centrality_eccentricity(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::eccentricity()`](https://r.igraph.org/reference/eccentricity.html).

### Ineccentricity Centrality

Ineccentricity computes eccentricity over incoming directed paths.

``` math
\varepsilon^{\mathrm{in}}(v) = \max_{u \in V} d(u, v)
```

**Meaning.** It summarises the largest directed distance from other
nodes into the focal node.

``` r

centrality_ineccentricity(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::eccentricity(mode = "in")`.

### Outeccentricity Centrality

Outeccentricity computes eccentricity over outgoing directed paths.

``` math
\varepsilon^{\mathrm{out}}(v) = \max_{u \in V} d(v, u)
```

**Meaning.** It summarises the largest directed distance from the focal
node to reachable others.

``` r

centrality_outeccentricity(student_interactions)
```

**Equivalence reference.** Equivalent to
`igraph::eccentricity(mode = "out")`.

### Closeness Vitality

Closeness vitality measures how total network distance changes when a
node is removed, using the Wiener index $`W(G) = \sum_{s, t} d(s, t)`$.

``` math
\mathrm{CV}(v) = W(G) - W(G \setminus v)
```

**Meaning.** A high value indicates that removing the node substantially
changes shortest-path structure.

``` r

centrality_closeness_vitality(student_interactions)
```

**Equivalence reference.** Corresponds to NetworkX closeness vitality
conventions.

### Entropy Centrality

Entropy centrality measures the change in graph entropy associated with
a node, from the distribution of finite shortest-path distances after
the node is removed.

``` math
H(v) = -\sum_{w} Y_w \log_2 Y_w, \quad Y_w = \frac{|\{ x : d(w, x) < \infty \}|}{\sum_{w'} |\{ x : d(w', x) < \infty \}|}
```

**Meaning.** A high value indicates strong contribution under the graph
entropy definition being used.

``` r

centrality_entropy(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::entropy()`](https://rdrr.io/pkg/centiserve/man/entropy.html).

### Centroid Centrality

Centroid centrality compares distance dominance between pairs of nodes.

``` math
f(v, u) = \gamma(v, u) - \gamma(u, v), \quad \gamma(v, u) = |\{ w : d(v, w) < d(u, w) \}|, \quad C_{\mathrm{cen}}(v) = \min_{u \ne v} f(v, u)
```

**Meaning.** A high value indicates a favourable position in pairwise
distance comparisons.

``` r

centrality_centroid(student_interactions)
```

**Equivalence reference.** Implemented natively because some reference
implementations have known edge-case issues.

## Shortest-path brokerage and flow

### Betweenness Centrality

Betweenness centrality measures how often a node lies on shortest paths
between other pairs of nodes.

``` math
C_B(v) = \sum_{s \ne v \ne t} \frac{\sigma_{st}(v)}{\sigma_{st}}
```

**Meaning.** A high value is consistent with a bridge or brokerage
position under the shortest-path model. It is not direct evidence of
intentional mediation.

``` r

centrality_betweenness(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::betweenness()`](https://r.igraph.org/reference/betweenness.html)
under matching directedness, weights, and normalization conventions.

### Stress Centrality

Stress centrality counts shortest paths passing through a node without
fractional normalization across tied paths.

``` math
C_S(v) = \sum_{s \ne v \ne t} \sigma_{st}(v)
```

**Meaning.** A high value indicates that many shortest paths include the
node.

``` r

centrality_stress(student_interactions)
```

**Equivalence reference.** Validated against
[`sna::stresscent()`](https://rdrr.io/pkg/sna/man/stresscent.html).

### Load Centrality

Load centrality distributes shortest-path load across alternative
shortest routes: at each branch point a unit packet is split evenly
among next hops on shortest paths.

``` math
C_L(v) = \sum_{s \ne v \ne t} \mathrm{load}_{st}(v)
```

**Meaning.** A high value indicates that a node carries a large share of
geodesic routing load.

``` r

centrality_load(student_interactions)
```

**Equivalence reference.** Validated against
[`sna::loadcent()`](https://rdrr.io/pkg/sna/man/loadcent.html).

### Bottleneck Centrality

Bottleneck centrality counts cases where a node is critical in
shortest-path tree structures.

``` math
\mathrm{BN}(v) = \sum_{s \in V} p_s(v), \quad p_s(v) = 1 \text{ if } v \text{ carries} > \tfrac{n}{4} \text{ of the shortest-path tree } T_s
```

**Meaning.** A high value indicates frequent bottleneck position in
local shortest-path trees.

``` r

centrality_bottleneck(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::bottleneck()`](https://rdrr.io/pkg/centiserve/man/bottleneck.html).

### Bridging Centrality

Bridging centrality combines betweenness with a bridging coefficient.

``` math
\mathrm{Br}(v) = C_B(v) \cdot \beta(v), \quad \beta(v) = \frac{1/k_v}{\sum_{u \in N(v)} 1/k_u}
```

**Meaning.** A high value indicates a possible bridge position between
locally distinct areas.

``` r

centrality_bridging(student_interactions)
```

**Equivalence reference.** Based on bridging centrality formulations by
Hwang and colleagues.

### Local Bridging Centrality

Local bridging centrality measures bridge-like position using local
neighbourhood information.

``` math
\mathrm{LBr}(v) = \frac{1}{k_v} \cdot \beta(v), \quad \beta(v) = \frac{1/k_v}{\sum_{u \in N(v)} 1/k_u}
```

**Meaning.** A high value indicates that the node connects neighbours
that are not strongly connected through local alternatives.

``` r

centrality_local_bridging(student_interactions)
```

**Equivalence reference.** Based on local bridging coefficient
formulations.

### Percolation Centrality

Percolation centrality weights shortest-path brokerage by node states in
a percolation process.

``` math
\mathrm{PC}(v) = \frac{1}{n - 2} \sum_{s \ne v \ne t} \frac{\sigma_{st}(v)}{\sigma_{st}} \cdot \frac{x_s}{\sum_{i \ne v} x_i}
```

**Meaning.** A high value indicates state-dependent path importance
under the supplied state vector $`x`$.

``` r

centrality_percolation(student_interactions)
```

**Equivalence reference.** Based on Piraveenan, Prokopenko, and
Hossain’s percolation centrality; equivalent to betweenness-like
behavior when states are equal.

### Flow Betweenness Centrality

Flow betweenness measures brokerage using maximum flow rather than only
shortest paths.

``` math
C_{FB}(v) = \sum_{s \ne v \ne t} f_{st}(v), \quad f_{st}(v) = \text{flow through } v \text{ in a max } s\text{-}t \text{ flow}
```

**Meaning.** A high value indicates importance for potential flow
capacity between other nodes under the graph model.

``` r

centrality_flow_betweenness(student_interactions)
```

**Equivalence reference.** Validated against
[`sna::flowbet()`](https://rdrr.io/pkg/sna/man/flowbet.html).

### Current-Flow Betweenness Centrality

Current-flow betweenness measures how much electrical current between
node pairs passes through a node.

``` math
C_{CFB}(v) = \frac{1}{(n - 1)(n - 2)} \sum_{s \ne t} \tau_{st}(v), \quad \tau_{st}(v) = \text{current through } v
```

**Meaning.** A high value indicates an intermediary position under an
all-path current-flow model.

``` r

centrality_current_flow_betweenness(student_interactions)
```

**Equivalence reference.** Corresponds to NetworkX current-flow
betweenness centrality.

### Current-Flow Closeness Centrality

Current-flow closeness uses electrical-network (effective-resistance)
distances rather than only shortest paths.

``` math
C_{CFC}(v) = \frac{n - 1}{\sum_{u \ne v} R_{vu}}, \quad R_{vu} = \text{effective resistance between } v \text{ and } u
```

**Meaning.** A high value indicates closeness under an all-path flow
model. It generally requires connected graphs.

``` r

centrality_current_flow_closeness(student_interactions)
```

**Equivalence reference.** Corresponds to NetworkX current-flow
closeness centrality.

## Spectral, walk and influence

### Eigenvector Centrality

Eigenvector centrality gives high scores to nodes connected to other
high-scored nodes.

``` math
A\,\mathbf{x} = \lambda_{\max}\,\mathbf{x}, \quad C_E(v) = x_v
```

**Meaning.** A high value indicates embeddedness in a central
neighbourhood. It should not be interpreted as influence unless the
relation supports that interpretation.

``` r

centrality_eigenvector(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::eigen_centrality()`](https://r.igraph.org/reference/eigen_centrality.html).

### PageRank Centrality

PageRank is a damped random-walk centrality. Nodes score highly when a
random walker reaches them often.

``` math
\mathrm{PR}(v) = \frac{1 - \alpha}{n} + \alpha \sum_{u \,:\, u \to v} \frac{\mathrm{PR}(u)}{k_u^{\mathrm{out}}}, \quad \alpha = 0.85
```

**Meaning.** A high value indicates random-walk prominence under the
chosen damping, direction, and weight conventions.

``` r

centrality_pagerank(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::page_rank()`](https://r.igraph.org/reference/page_rank.html)
under matching parameters; original method by Page, Brin, Motwani, and
Winograd.

### Authority Centrality

Authority centrality is part of HITS. Authorities are nodes pointed to
by good hubs.

``` math
\mathbf{a} = A^{\top} \mathbf{h}, \quad \mathbf{h} = A\,\mathbf{a} \;\Rightarrow\; \mathbf{a} = \text{principal eigenvector of } A^{\top} A
```

**Meaning.** A high value indicates incoming support from nodes that
point to good authorities.

``` r

centrality_authority(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::authority_score()`](https://r.igraph.org/reference/hub_score.html);
based on Kleinberg’s HITS algorithm.

### Hub Centrality

Hub centrality is the HITS counterpart to authority. Hubs point to good
authorities.

``` math
\mathbf{h} = \text{principal eigenvector of } A\,A^{\top}
```

**Meaning.** A high value indicates outgoing ties to high-authority
nodes.

``` r

centrality_hub(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::hub_score()`](https://r.igraph.org/reference/hub_score.html);
based on Kleinberg’s HITS algorithm.

### SALSA Centrality

SALSA is a stochastic link-analysis method related to HITS for directed
graphs.

``` math
\mathbf{a} = \text{principal eigenvector of } A_c^{\top} A_r, \quad A_r, A_c = \text{row- and column-normalised } A
```

**Meaning.** A high value indicates directed authority under the SALSA
random-walk model.

``` r

centrality_salsa(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::salsa()`](https://rdrr.io/pkg/centiserve/man/salsa.html)
where applicable.

### LeaderRank Centrality

LeaderRank is a PageRank-like directed ranking method that adds a ground
node $`g`$ linked both ways to every node; the stationary random-walk
mass on $`g`$ is then redistributed equally.

``` math
\mathbf{s}^{*} = \text{stationary distribution of the walk on } G \cup \{g\}, \quad C(v) = s^{*}_v + \tfrac{1}{n} s^{*}_g
```

**Meaning.** A high value indicates directed prestige under the
LeaderRank model.

``` r

centrality_leaderrank(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::leaderrank()`](https://rdrr.io/pkg/centiserve/man/leaderrank.html)
where applicable.

### Alpha Centrality

Alpha centrality is an eigenvector-like measure that includes exogenous
input.

``` math
\mathbf{x} = (I - \alpha A^{\top})^{-1} \mathbf{e}
```

**Meaning.** A high value indicates recursive prominence under the
chosen attenuation and exogenous assumptions.

``` r

centrality_alpha(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::alpha_centrality()`](https://r.igraph.org/reference/alpha_centrality.html)
under matching parameters.

### Bonacich Power Centrality

Bonacich power centrality scores nodes using the centrality of their
neighbours and a parameter $`\beta`$ controlling dependence.

``` math
\mathbf{c}(\alpha, \beta) = \alpha (I - \beta A)^{-1} A\,\mathbf{1}
```

**Meaning.** A high value indicates a favourable recursive position
under the selected Bonacich parameterization.

``` r

centrality_power(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::power_centrality()`](https://r.igraph.org/reference/power_centrality.html)
under matching parameters.

### Katz Centrality

Katz centrality counts walks from all nodes to a focal node, attenuating
longer walks.

``` math
\mathbf{x} = (I - \alpha A^{\top})^{-1} \mathbf{1}, \quad \alpha = 0.1
```

**Meaning.** A high value indicates that many short and longer walks
reach the node. The attenuation parameter must be valid for the graph.

``` r

centrality_katz(student_interactions)
```

**Equivalence reference.** Validated against
`igraph::alpha_centrality(exo = 1)` and NetworkX Katz centrality
conventions.

### Hubbell Centrality

Hubbell centrality is an input-output centrality where status is
recursively reinforced through ties.

``` math
\mathbf{x} = (I - w W)^{-1} \mathbf{1}, \quad w = 0.5,\; W = \text{weighted adjacency}
```

**Meaning.** A high value indicates recursive prominence under the
chosen weight factor. Some parameter settings are not solvable.

``` r

centrality_hubbell(student_interactions)
```

**Equivalence reference.** Based on Hubbell’s input-output centrality.

### Subgraph Centrality

Subgraph centrality measures participation in closed walks, with shorter
closed walks weighted more strongly.

``` math
\mathrm{SC}(v) = \sum_{k=0}^{\infty} \frac{(A^k)_{vv}}{k!} = (e^{A})_{vv}
```

**Meaning.** A high value indicates embeddedness in many closed walk
structures.

``` r

centrality_subgraph(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::subgraph_centrality()`](https://r.igraph.org/reference/subgraph_centrality.html);
based on Estrada’s formulation.

### Laplacian Centrality

Laplacian centrality measures a node’s contribution to the graph’s
Laplacian energy.

``` math
C_L(v) = \frac{E_L(G) - E_L(G \setminus v)}{E_L(G)}, \quad E_L(G) = \sum_i \mu_i^2 \;\; (\mu_i = \text{Laplacian eigenvalues})
```

**Meaning.** A high value indicates a large local structural
contribution under the Laplacian energy definition.

``` r

centrality_laplacian(student_interactions)
```

**Equivalence reference.** Matches NetworkX and
[`centiserve::laplacian()`](https://rdrr.io/pkg/centiserve/man/laplacian.html)
conventions.

### Communicability Centrality

Communicability centrality uses the matrix exponential to summarise
walk-based communication potential.

``` math
C_{\mathrm{Comm}}(v) = \sum_{u \in V} (e^{A})_{vu}
```

**Meaning.** A high value indicates many walk-based routes to other
nodes, with shorter walks weighted more strongly.

``` r

centrality_communicability(student_interactions)
```

**Equivalence reference.** Based on Estrada communicability.

### Communicability Betweenness Centrality

Communicability betweenness measures walk-based communicability between
other pairs through a node.

``` math
C_{CB}(v) = \frac{1}{(n-1)(n-2)} \sum_{s \ne t \ne v} \frac{G_{st} - G_{st}^{(v)}}{G_{st}}, \quad G = e^{A}
```

**Meaning.** A high value indicates walk-based intermediary position,
not limited to shortest paths. $`G^{(v)}`$ is computed with $`v`$
removed.

``` r

centrality_communicability_betweenness(student_interactions)
```

**Equivalence reference.** Based on Estrada, Higham, and Hatano’s
communicability betweenness.

### Random Walk Centrality

Random walk centrality uses expected random-walk access times rather
than shortest-path distances.

``` math
C_{RW}(v) = \frac{1}{\sum_{u \ne v} \tilde{m}_{vu}}, \quad \tilde{m}_{vu} = \tfrac{1}{2}\bigl( m_{vu} + m_{uv} \bigr)
```

**Meaning.** A high value indicates that the node is reached efficiently
under a random-walk process. $`m_{uv}`$ is the mean first-passage time.

``` r

centrality_random_walk(student_interactions)
```

**Equivalence reference.** Corresponds to random-walk centrality based
on mean first-passage or resistance-distance formulations.

### Markov Centrality

Markov centrality is based on mean first-passage times in a Markov
process on the graph.

``` math
C_{\mathrm{Mk}}(v) = \frac{1}{\frac{1}{n} \sum_{u} m_{uv}}, \quad m_{uv} = \text{mean first-passage time } u \to v
```

**Meaning.** A high value indicates that the node is reached quickly on
average under the Markov process.

``` r

centrality_markov(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::markovcent()`](https://rdrr.io/pkg/centiserve/man/markovcent.html).

### Second-Order Centrality

Second-order centrality summarises variability in random-walk return
times.

``` math
\mathrm{SO}(v) = \operatorname{sd}_{u}\bigl( m_{uv} \bigr)
```

**Meaning.** The statistic describes random-walk regularity rather than
direct connectivity (lower is more central).

``` r

centrality(student_interactions, measures = "second_order")
```

**Equivalence reference.** Rank behavior is checked against NetworkX
second-order centrality in package tests.

### Information Centrality

Information centrality measures centrality through resistance-distance
or information-flow ideas (Stephenson and Zelen).

``` math
C_I(v) = \left( C_{vv} + \frac{T - 2 R_v}{n} \right)^{-1}, \quad C = (D - A + J)^{-1},\; T = \operatorname{tr} C,\; R_v = \textstyle\sum_j C_{vj}
```

**Meaning.** A high value indicates a central position under the
information-flow model.

``` r

centrality_information(student_interactions)
```

**Equivalence reference.** Based on Stephenson and Zelen’s information
centrality.

### Nonbacktracking Centrality

Nonbacktracking centrality scores nodes using walks that do not
immediately return along the edge just traversed (the Hashimoto matrix
$`B`$).

``` math
B_{(i \to j),\,(k \to l)} = \delta_{jk}\,(1 - \delta_{il}), \quad C(v) \propto \text{aggregated leading eigenvector of } B
```

**Meaning.** A high value indicates walk-based prominence after reducing
immediate backtracking inflation.

``` r

centrality(student_interactions, measures = "nonbacktracking")
```

**Equivalence reference.** Validated against NetworkX-style
nonbacktracking behavior in package tests.

### Diffusion Centrality

Diffusion centrality estimates reach through a diffusion process over
the graph.

``` math
\mathbf{DC} = \sum_{t=1}^{T} (\lambda A)^{t}\,\mathbf{1}, \quad \lambda = 1
```

**Meaning.** A high value indicates a structurally favourable position
for spreading under the selected diffusion model.

``` r

centrality_diffusion(student_interactions)
```

**Equivalence reference.** Validated against `centiserve::diffusion()`
where applicable and TNA-style diffusion conventions for transition
networks.

### Infection Centrality

Infection centrality estimates spreading potential through
infection-style self-avoiding walks with attenuation.

``` math
\mathrm{Inf}(v) = \sum_{d=1}^{L} \beta^{\,d+1} (1 - \mu)^{d}\, w_d(v), \quad \beta = 0.8,\; \mu = 0,\; L = 6
```

**Meaning.** A high value indicates a favourable position under the
assumed infection process, not observed contagion. $`w_d(v)`$ counts
length-$`d`$ self-avoiding walks from $`v`$.

``` r

centrality(student_interactions, measures = "infection")
```

**Equivalence reference.** Implemented according to infection centrality
formulas used in the network centrality literature.

### VoteRank Centrality

VoteRank is an iterative voting algorithm for ranking spreader
candidates: each round the highest-voted node is selected, its voting
ability is zeroed, and its neighbours’ voting abilities are reduced.

**Meaning.** A high value indicates early selection by the VoteRank
rule. It is a heuristic model result, not observed spread.

``` r

centrality_voterank(student_interactions)
```

**Equivalence reference.** Corresponds to NetworkX VoteRank behavior.

### Expected Influence 1-Step

Expected influence 1-step sums signed edge weights adjacent to a node.

``` math
\mathrm{EI}_1(v) = \sum_{u \in V} W_{vu}, \quad W = \text{signed weight matrix}
```

**Meaning.** A high positive value indicates strong positive immediate
signed connectivity. This is mainly meaningful for signed networks.

``` r

centrality_expected_influence_1(student_interactions)
```

**Equivalence reference.** Based on Robinaugh, Millner, and McNally’s
expected influence measure.

### Expected Influence 2-Step

Expected influence 2-step extends signed influence to one- and two-step
paths.

``` math
\mathrm{EI}_2(v) = \mathrm{EI}_1(v) + \sum_{u \in V} W_{vu}\,\mathrm{EI}_1(u)
```

**Meaning.** A high positive value indicates positive signed
connectivity through direct and indirect paths. Interpretation requires
a signed network.

``` r

centrality_expected_influence_2(student_interactions)
```

**Equivalence reference.** Based on Robinaugh, Millner, and McNally’s
expected influence measure.

### Spanning Tree Centrality

Spanning tree centrality summarises a node’s contribution across
spanning-tree structures via the Laplacian pseudoinverse $`L^{+}`$.

``` math
\mathrm{ST}(v) = \frac{1}{L^{+}_{vv}}, \quad L^{+} = \text{Moore-Penrose pseudoinverse of } L
```

**Meaning.** A high value indicates structural participation across many
tree-like ways of connecting the graph.

``` r

centrality(student_interactions, measures = "spanning_tree")
```

**Equivalence reference.** Based on matrix-tree theorem centrality
formulations.

## Neighbourhood structure and cohesion

### Transitivity Centrality

Transitivity measures local clustering: whether a node’s neighbours are
connected to each other.

``` math
c(v) = \frac{2\, t_v}{k_v (k_v - 1)}, \quad t_v = \text{number of triangles through } v
```

**Meaning.** A high value indicates locally closed neighbourhoods. This
can represent cohesion or redundancy, depending on the relation.

``` r

centrality_transitivity(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::transitivity()`](https://r.igraph.org/reference/transitivity.html)
for standard types; `cograph` also supports weighted variants.

### Constraint Centrality

Constraint measures how redundant a node’s contacts are, following
Burt’s structural holes framework.

``` math
C(v) = \sum_{u \in N(v)} \left( p_{vu} + \sum_{q \ne v, u} p_{vq}\, p_{qu} \right)^2, \quad p_{vu} = \text{proportional tie strength}
```

**Meaning.** A high value indicates a locally constrained ego network.
Lower constraint may indicate structural holes, depending on theory and
relation type.

``` r

centrality_constraint(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::constraint()`](https://r.igraph.org/reference/constraint.html).

### Effective Size Centrality

Effective size estimates the number of nonredundant contacts in a node’s
ego network (Burt).

``` math
\mathrm{ES}(v) = k_v - \frac{1}{k_v} \sum_{u \in N(v)} |N(v) \cap N(u)|
```

**Meaning.** A high value indicates relatively nonoverlapping contacts.

``` r

centrality_effective_size(student_interactions)
```

**Equivalence reference.** Validated against
`influenceR::effective_size()`.

### Topological Coefficient Centrality

Topological coefficient centrality measures shared-neighbour overlap.

``` math
T(v) = \frac{\operatorname{avg}_{u}\, J(v, u)}{k_v}, \quad J(v, u) = \text{number of neighbours shared by } v \text{ and } u
```

**Meaning.** A high value indicates neighbourhood overlap or topological
similarity.

``` r

centrality_topological_coefficient(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::topocoefficient()`](https://rdrr.io/pkg/centiserve/man/topocoefficient.html).

### Diversity Centrality

Diversity centrality measures entropy in the distribution of edge
weights around a node.

``` math
\mathrm{Div}(v) = \frac{-\sum_{u \in N(v)} p_{vu} \log_2 p_{vu}}{\log_2 k_v}, \quad p_{vu} = \frac{w_{vu}}{\sum_{u'} w_{vu'}}
```

**Meaning.** A high value indicates that weighted ties are relatively
evenly distributed rather than concentrated.

``` r

centrality_diversity(student_interactions)
```

**Equivalence reference.** Validated against
[`igraph::diversity()`](https://r.igraph.org/reference/diversity.html).

### Cross-Clique Centrality

Cross-clique centrality counts how many cliques contain a node.

``` math
X(v) = |\{\, Q \in \mathcal{Q}(G) : v \in Q \,\}|, \quad \mathcal{Q}(G) = \text{set of cliques}
```

**Meaning.** A high value indicates participation in many fully
connected local groups.

``` r

centrality_cross_clique(student_interactions)
```

**Equivalence reference.** Validated against
[`centiserve::crossclique()`](https://rdrr.io/pkg/centiserve/man/crossclique.html).

### Coreness Centrality

Coreness assigns nodes to k-core shells.

``` math
C_{\mathrm{core}}(v) = \max \{\, k : v \in (k\text{-core of } G) \,\}
```

**Meaning.** A high value indicates membership in a dense core. It does
not imply brokerage or short paths to all nodes.

``` r

centrality_coreness(student_interactions)
```

**Equivalence reference.** Equivalent to
[`igraph::coreness()`](https://r.igraph.org/reference/coreness.html).

### Onion Centrality

Onion centrality assigns nodes to layers from onion decomposition, a
fine-grained extension of k-core decomposition.

``` math
\text{layer}(v) = \text{iteration index at which } v \text{ is peeled by the onion decomposition}
```

**Meaning.** A high layer indicates that the node remains until later
stages of the peeling process.

``` r

centrality(student_interactions, measures = "onion")
```

**Equivalence reference.** Validated against NetworkX onion layer
behavior in package tests.

### K-Reach Centrality

K-reach centrality counts nodes reachable within path length $`k`$.

``` math
C_{kR}(v) = |\{\, u : 0 < d(v, u) \le k \,\}|, \quad k = 3
```

**Meaning.** A high value indicates broad reach within a fixed local
radius. Results depend on the chosen $`k`$.

``` r

centrality_kreach(student_interactions)
```

**Equivalence reference.** Validated against k-path reach conventions.

## Directed prestige and hierarchy

### Prestige Domain Centrality

Prestige domain centrality counts how many nodes can reach a focal node
in a directed graph.

``` math
P(v) = |\{\, u \ne v : u \rightsquigarrow v \,\}|
```

**Meaning.** A high value indicates a large incoming reach domain.

``` r

centrality_prestige_domain(student_interactions)
```

**Equivalence reference.** Based on Wasserman-Faust prestige concepts
and `sna`-style prestige measures.

### Prestige Domain Proximity Centrality

Prestige domain proximity measures how close nodes in the prestige
domain are to the focal node.

``` math
C(v) = \frac{|R^{-}(v)|^2}{(n - 1) \sum_{u \in R^{-}(v)} d(u, v)}, \quad R^{-}(v) = \{ u \ne v : u \rightsquigarrow v \}
```

**Meaning.** A high value indicates that many predecessors can reach the
node through short directed paths.

``` r

centrality_prestige_domain_proximity(student_interactions)
```

**Equivalence reference.** Based on Wasserman-Faust prestige proximity
concepts.

### Local Reaching Centrality

Local reaching centrality measures how much of the network is reachable
from a node through directed paths.

``` math
\mathrm{LRC}(v) = \frac{|\{\, u : v \rightsquigarrow u \,\}|}{n - 1}
```

**Meaning.** A high value indicates broad directed reach from the node.

``` r

centrality_reaching_local(student_interactions)
```

**Equivalence reference.** Matches `networkx.local_reaching_centrality`
in package equivalence tests.

### Pairwise Disconnectivity Centrality

Pairwise disconnectivity measures how directed reachability between
pairs changes when a node is removed.

``` math
\mathrm{Dis}(v) = \frac{P(G) - P(G \setminus v)}{P(G)}, \quad P(G) = |\{ (s, t) : s \rightsquigarrow t \}|
```

**Meaning.** A high value indicates structural importance for preserving
directed reachability.

``` r

centrality_pairwisedis(student_interactions)
```

**Equivalence reference.** Validated against reference implementations
in package tests.

### Trophic Level Centrality

Trophic level estimates hierarchical position in a directed flow
network.

``` math
(I - W)\,\mathbf{s} = \mathbf{1}, \quad W_{ji} = \frac{a_{ij}}{k_j^{\mathrm{in}}}, \quad \text{basal nodes have level } 1
```

**Meaning.** A higher value indicates a higher position in the inferred
directed hierarchy. This is appropriate only for flow-like relations.

``` r

centrality(student_interactions, measures = "trophic_level")
```

**Equivalence reference.** Checked against NetworkX trophic-level
behavior in package tests.

The measures below need a **community membership vector**. Build one
with
[`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md)
(`walktrap` works on the directed `student_interactions` graph), then
pass it as `membership =`:

``` r

comm   <- detect_communities(student_interactions, method = "walktrap")
groups <- setNames(comm$community, comm$node)
centrality_participation(student_interactions, membership = groups)
#>        Ac        Ad        Fi        Ik        Vx        Rt        Km        Gj 
#> 0.6078972 0.5450000 0.4965278 0.5612245 0.6213018 0.5150000 0.4628099 0.5540166 
#>        Bd        Ce        Oq        Ya        Mo        Hj        Tv        Eg 
#> 0.4861111 0.4600000 0.4897959 0.4733728 0.5416667 0.6250000 0.5400000 0.5400000 
#>        Pr        Qs        Xz        Np        Dg        Hk        Wy        Jl 
#> 0.4297521 0.5244444 0.4897959 0.4687500 0.5562130 0.6015625 0.6446281 0.4177778 
#>        Fh        Zb        Eh        Be        Df        Cf        Su        Ln 
#> 0.5396825 0.6938776 0.6938776 0.6020408 0.3750000 0.6805556 0.4444444 0.4081633 
#>        Gi        Uw 
#> 0.6666667 0.6250000
```

## Community and group-based

### Participation Coefficient

Participation coefficient measures how evenly a node’s ties are
distributed across communities.

``` math
P(v) = 1 - \sum_{m} \left( \frac{k_v^{(m)}}{k_v} \right)^2, \quad k_v^{(m)} = \text{ties from } v \text{ to module } m
```

**Meaning.** A high value indicates ties spread across multiple
communities. It requires a meaningful membership vector.

``` r

centrality_participation(student_interactions, membership = groups)
```

**Equivalence reference.** Validated against `brainGraph` participation
coefficient conventions.

### Within-Module Z Centrality

Within-module z centrality standardises a node’s within-community degree
against other nodes in the same community.

``` math
z(v) = \frac{k_v^{(m_v)} - \mu_{m_v}}{\sigma_{m_v}}, \quad m_v = \text{community of } v
```

**Meaning.** A high value indicates unusually high within-module
connectivity.

``` r

centrality_within_module_z(student_interactions, membership = groups)
```

**Equivalence reference.** Validated against `brainGraph` within-module
z-score conventions.

### Gateway Centrality

Gateway centrality measures inter-community brokerage weighted by
centrality of communities or nodes involved.

``` math
g(v) = 1 - \frac{1}{k_v^2} \sum_{s} k_{vs}^2\, \gamma_{vs}^2, \quad k_{vs} = \text{ties from } v \text{ to module } s
```

**Meaning.** A high value indicates a structurally prominent position
between communities.

``` r

centrality_gateway(student_interactions, membership = groups)
```

**Equivalence reference.** Validated against `brainGraph::gateway()`
conventions.

### Brokerage Coordinator Centrality

Coordinator brokerage occurs when a node mediates between two nodes in
its own group. It counts open directed two-paths $`a \to v \to c`$ (with
no direct $`a \to c`$).

``` math
\mathrm{Coord}(v) = |\{\, a \to v \to c : g(a) = g(v) = g(c) \,\}|
```

**Meaning.** A high value indicates within-group mediation under the
supplied membership vector.

``` r

centrality_brokerage_coordinator(student_interactions, membership = groups)
```

**Equivalence reference.** Based on Gould-Fernandez brokerage roles.

### Brokerage Itinerant Centrality

Itinerant brokerage occurs when a node mediates between two nodes in
another group.

``` math
\mathrm{Itin}(v) = |\{\, a \to v \to c : g(a) = g(c) \ne g(v) \,\}|
```

**Meaning.** A high value indicates brokerage among members outside the
node’s own group.

``` r

centrality_brokerage_itinerant(student_interactions, membership = groups)
```

**Equivalence reference.** Based on Gould-Fernandez brokerage roles.

### Brokerage Representative Centrality

Representative brokerage occurs when a node mediates from its own group
to another group.

``` math
\mathrm{Rep}(v) = |\{\, a \to v \to c : g(a) = g(v) \ne g(c) \,\}|
```

**Meaning.** A high value indicates outward brokerage from the node’s
group.

``` r

centrality_brokerage_representative(student_interactions, membership = groups)
```

**Equivalence reference.** Based on Gould-Fernandez brokerage roles.

### Brokerage Gatekeeper Centrality

Gatekeeper brokerage occurs when a node mediates from another group into
its own group.

``` math
\mathrm{Gate}(v) = |\{\, a \to v \to c : g(a) \ne g(v) = g(c) \,\}|
```

**Meaning.** A high value indicates inward brokerage into the node’s
group.

``` r

centrality_brokerage_gatekeeper(student_interactions, membership = groups)
```

**Equivalence reference.** Based on Gould-Fernandez brokerage roles.

### Brokerage Liaison Centrality

Liaison brokerage occurs when a node mediates between two groups,
neither of which is its own.

``` math
\mathrm{Liai}(v) = |\{\, a \to v \to c : g(a),\, g(v),\, g(c) \text{ all distinct} \,\}|
```

**Meaning.** A high value indicates between-group brokerage outside the
node’s own group.

``` r

centrality_brokerage_liaison(student_interactions, membership = groups)
```

**Equivalence reference.** Based on Gould-Fernandez brokerage roles.

## Argument catalogue

All measures are reachable through the single
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
verb. These arguments control how they are computed. Measure-specific
knobs are repeated in each measure’s **Arguments** line.

| Argument | Default | Affects | What it does |
|----|----|----|----|
| `type` | `"basic"` | all | Curated tier of measures: `"basic"`, `"extended"`, or `"all"`. |
| `measures` | `NULL` | all | Character vector of specific measures to compute (overrides `type`). |
| `mode` | `"all"` | directed measures | Traversal direction: `"all"`, `"in"`, or `"out"`. |
| `normalized` | `FALSE` | most | Divide each score by its maximum. |
| `weighted` | `TRUE` | weighted measures | Use edge weights when present. |
| `directed` | `NULL` | all | Force directed/undirected; `NULL` auto-detects. |
| `loops` | `TRUE` | degree, strength | Keep self-loops. |
| `simplify` | `"sum"` | multigraphs | How to combine parallel edges. |
| `cutoff` | `-1` | path-based | Cap path length (`-1` = no cap). |
| `invert_weights` | `NULL` | distance/path | Treat weights as distances vs. strengths. |
| `alpha` | `1` | weight transform | Exponent applied when `invert_weights = TRUE`. |
| `damping` | `0.85` | PageRank | Random-walk damping factor. |
| `personalized` | `NULL` | PageRank | Personalization vector. |
| `transitivity_type` | `"local"` | transitivity | `"local"`, `"global"`, or weighted variants. |
| `isolates` | `"nan"` | transitivity | How isolates are scored. |
| `lambda` | `1` | diffusion | Diffusion scaling factor. |
| `k` | `3` | k-reach | Path-length radius. |
| `states` | `NULL` | percolation | Per-node percolation state vector. |
| `decay_parameter` | `0.5` | decay, generalized closeness | Distance-decay base. |
| `dmnc_epsilon` | `1.7` | DMNC | Density exponent. |
| `katz_alpha` | `0.1` | Katz | Attenuation factor. |
| `hubbell_weight` | `0.5` | Hubbell | Weight factor $`w`$. |
| `membership` | `NULL` | group-based | Community assignment vector. |
| `digits` | `NULL` | all | Round numeric columns. |
| `sort_by` | `NULL` | all | Order rows by a column name. |
