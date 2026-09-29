# Centrality catalogue

``` r

library(cograph)
data(student_interactions)
```

Centrality measures quantify the position of a node in a network. The
simplest, degree, counts the ties of a node. Other measures are built
from the distances between nodes, the shortest paths that pass through a
node, walks of all lengths, or the community structure of the network.
Each measure describes a different aspect of position, and the choice
between them depends on the process the network is assumed to carry.

This catalogue documents the 191 measures available through
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md),
organised by family. Each entry gives a short description, the formula
as implemented, a guide to interpreting the scores, and an example. The
examples use `student_interactions`, a data set of observed interactions
between students included in the package. Tuning parameters are
arguments of
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
and are listed in the [argument catalogue](#argument-catalogue).

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
#> 33   Gi          3            4    0.01351351    0.000000 0.000000e+00
#> 34   Uw          4            7    0.01250000    0.000000 0.000000e+00
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

Pass `type = "all"` for all ordinary-cost measures, `include = "costly"`
to add costly ones, `measures = c(...)` to pick a subset, `digits =` to
round, and `sort_by =` to order the result.

## How to read this catalogue

Every measure below is documented with four fields:

- **Concept** — what the measure captures, in words.
- **Definition** — the formula as `cograph` computes it. Where its
  normalisation differs from a textbook statement, the formula shown is
  the one `cograph` uses.
- **Meaning** — how to read a high (or low) value.
- **Example** — a runnable call.

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

[X-degree](#cent-x_degree) · [Clustering Degree Algorithm](#cent-cda) ·
[Extended Neighborhood Coreness](#cent-extended_coreness) · [Malatya
Centrality](#cent-malatya) · [Volume Centrality](#cent-volume) · [Degree
Centrality](#cent-degree) · [Indegree Centrality](#cent-indegree) ·
[Outdegree Centrality](#cent-outdegree) · [Strength
Centrality](#cent-strength) · [Instrength Centrality](#cent-instrength)
· [Outstrength Centrality](#cent-outstrength) · [Expected
Centrality](#cent-expected) · [Leverage Centrality](#cent-leverage) ·
[Lobby Centrality](#cent-lobby) · [H-Index Strength
Centrality](#cent-hindex_strength) · [Local H-Index
Centrality](#cent-local_hindex) · [Semi-Local
Centrality](#cent-semilocal) · [ClusterRank
Centrality](#cent-clusterrank) · [Collective Influence
Centrality](#cent-collective_influence) · [MNC Centrality](#cent-mnc) ·
[DMNC Centrality](#cent-dmnc) · [LAC Centrality](#cent-lac) · [Gravity
Centrality](#cent-gravity) · [Neighborhood
Connectivity](#cent-neighborhood_connectivity) · [Entropy Variation,
Degree](#cent-entropy_variation_degree) · [Flow
Coefficient](#cent-flow_coefficient) · [Local
Entropy](#cent-local_entropy) · [Weighted
h-index](#cent-weighted_h_index) · [Redundancy](#cent-redundancy)

**Distance and closeness**

[Improved Global Structure Model](#cent-improved_global_structure) ·
[Global Structure Model](#cent-global_structure) · [Hybrid Global
Structure Model](#cent-hybrid_global_structure) · [Exogenous
Centrality](#cent-exogenous) · [Improved Closeness
Centrality](#cent-improved_closeness) · [Extended Gravity
Centrality](#cent-extended_gravity) · [Closeness
Centrality](#cent-closeness) · [Incloseness
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
Centrality](#cent-entropy) · [Centroid Centrality](#cent-centroid) ·
[Distance Entropy](#cent-distance_entropy) · [Local
Dimension](#cent-local_dimension) · [Local Information
Dimensionality](#cent-local_information_dimension) · [Access
Information](#cent-access_information) · [Hide
Information](#cent-hide_information) · [Local Dimension, Fixed
Radius](#cent-local_dimension_fixed) · [Fuzzy Local
Dimension](#cent-fuzzy_local_dimension) · [Local Volume
Dimension](#cent-local_volume_dimension) · [Heatmap
Centrality](#cent-heatmap) · [Geodesic k-path](#cent-geodesic_kpath) ·
[k-path Census](#cent-kpath) · [Distance-weighted
Fragmentation](#cent-fragmentation) · [Geodesic Power
Closeness](#cent-delta_closeness)

**Shortest-path brokerage and flow**

[Randomized shortest paths (RSP) betweenness](#cent-rsp_betweenness) ·
[Relative-entropy integrated evaluation](#cent-relative_entropy) ·
[DK-based gravity model](#cent-dkgm) · [Mixed gravitational
centrality](#cent-mixed_gravity) · [Extended mixed gravitational
centrality](#cent-extended_mixed_gravity) · [Localized bridging
centrality](#cent-localized_bridging) · [Extended local bridging
centrality](#cent-extended_local_bridging) · [Proximal
betweenness](#cent-proximal_betweenness) · [Betweenness
Centrality](#cent-betweenness) · [Stress Centrality](#cent-stress) ·
[Load Centrality](#cent-load) · [Length-scaled
Betweenness](#cent-length_scaled_betweenness) · [Distance-decayed
Betweenness](#cent-delta_betweenness) · [Ego
Betweenness](#cent-ego_betweenness) · [Bottleneck
Centrality](#cent-bottleneck) · [Bridging Centrality](#cent-bridging) ·
[Local bridging (legacy degree product)](#cent-local_bridging) ·
[Percolation Centrality](#cent-percolation) · [Flow Betweenness
Centrality](#cent-flow_betweenness) · [Current-Flow Betweenness
Centrality](#cent-current_flow_betweenness) · [Current-Flow Closeness
Centrality](#cent-current_flow_closeness) · [Entropy Variation,
Betweenness](#cent-entropy_variation_betweenness)

**Spectral, walk and influence**

[Trust-PageRank](#cent-trust_pagerank) · [Iterative resource allocation
(IRA)](#cent-ira) · [Improved iterative resource allocation
(IIRA)](#cent-iira) · [Multi-characteristics gravity model](#cent-mcgm)
· [SpectralRank](#cent-spectralrank) · [ControlRank](#cent-controlrank)
· [Node and Neighbor Layer Information](#cent-ninl) · [Expected
Force](#cent-expected_force) · [Modified Expected
Force](#cent-modified_expected_force) · [Bridging
capital](#cent-bridging_capital) · [LineRank](#cent-linerank) · [Random
walk decay](#cent-random_walk_decay) · [Graph regularization
centrality](#cent-graph_regularization) · [Adaptive
LeaderRank](#cent-adaptive_leaderrank) · [Weighted
LeaderRank](#cent-weighted_leaderrank) · [Node Resistance
Curvature](#cent-resistance_curvature) · [Dynamics-Sensitive
Centrality](#cent-dynamics_sensitive) · [Finite-Horizon Diffusion
Centrality](#cent-diffusion_centrality) · [Dynamical
Importance](#cent-dynamical_importance) · [Eigenvector
Centrality](#cent-eigenvector) · [PageRank Centrality](#cent-pagerank) ·
[Authority Centrality](#cent-authority) · [Hub Centrality](#cent-hub) ·
[SALSA Centrality](#cent-salsa) · [LeaderRank
Centrality](#cent-leaderrank) · [Alpha Centrality](#cent-alpha) ·
[Bonacich Power Centrality](#cent-power) · [Katz Centrality](#cent-katz)
· [Hubbell Centrality](#cent-hubbell) · [Subgraph
Centrality](#cent-subgraph) · [Laplacian Centrality](#cent-laplacian) ·
[Communicability Centrality](#cent-communicability) · [Communicability
Betweenness Centrality](#cent-communicability_betweenness) · [Random
Walk Centrality](#cent-random_walk) · [Markov Centrality](#cent-markov)
· [Immediate Effects Centrality (IEC)](#cent-iec) · [Second-Order
Centrality](#cent-second_order) · [Information
Centrality](#cent-information) · [Nonbacktracking
Centrality](#cent-nonbacktracking) · [Diffusion Degree](#cent-diffusion)
· [Infection Centrality](#cent-infection) · [Edge Percolated
Component](#cent-epc) · [VoteRank Centrality](#cent-voterank) ·
[Expected Influence 1-Step](#cent-expected_influence_1) · [Expected
Influence 2-Step](#cent-expected_influence_2) · [Spanning Tree
Centrality](#cent-spanning_tree) · [Shapley Value, Game
1](#cent-shapley_game1) · [Shapley Value, Game 2](#cent-shapley_game2) ·
[Shapley Value, Game 3](#cent-shapley_game3) · [Rumor
Centrality](#cent-rumor) · [DegreeDiscountIC](#cent-degree_discount) ·
[SingleDiscount](#cent-single_discount) · [NCVoteRank](#cent-ncvoterank)
· [WVoteRank](#cent-wvoterank) · [EnRenew](#cent-enrenew) ·
[VoteRank++](#cent-voterank_plus) · [Node
Contraction](#cent-node_contraction) · [Improved Node
Contraction](#cent-node_contraction_improved) · [Two-Way Random Walk
Betweenness](#cent-two_way_rw)

**Neighbourhood structure and cohesion**

[Degree and importance of lines (DIL)](#cent-dil) · [Lhc
index](#cent-lhc) · [Hybrid characteristic centrality (HCC)](#cent-hcc)
· [Extended hybrid characteristic centrality (EHCC)](#cent-ehcc) · [KED
method](#cent-ked) · [Local neighbor contribution (LNC)](#cent-lnc) ·
[Neighborhood (neighbor distance) centrality](#cent-neighbor_distance) ·
[Coleman-Theil hierarchy](#cent-coleman_theil) · [Maximal Clique
Centrality](#cent-mcc) · [Node Truss Number](#cent-truss) · [Mixed
Degree Decomposition](#cent-mdd) · [Bridging
Coefficient](#cent-bridging_coefficient) · [Godfather
Index](#cent-godfather) · [Supported Relationships](#cent-support) ·
[Transitivity Centrality](#cent-transitivity) · [Constraint
Centrality](#cent-constraint) · [Effective Size
Centrality](#cent-effective_size) · [Topological Coefficient
Centrality](#cent-topological_coefficient) · [Diversity
Centrality](#cent-diversity) · [Cross-Clique
Centrality](#cent-cross_clique) · [Coreness Centrality](#cent-coreness)
· [Onion Centrality](#cent-onion) · [K-Reach Centrality](#cent-kreach) ·
[s-shell Index](#cent-s_shell) · [Weighted
k-shell](#cent-weighted_kshell) · [Renewed
Coreness](#cent-renewed_coreness) · [s-core Index](#cent-s_core) ·
[Local Efficiency](#cent-local_efficiency)

**Directed prestige and hierarchy**

[BG-index (beta power)](#cent-beta_measure) · [Prestige Domain
Centrality](#cent-prestige_domain) · [Prestige Domain Proximity
Centrality](#cent-prestige_domain_proximity) · [Local Reaching
Centrality](#cent-reaching_local) · [Pairwise Disconnectivity
Centrality](#cent-pairwisedis) · [Trophic Level
Centrality](#cent-trophic_level)

**Community and group-based**

[Map equation centrality](#cent-map_equation) · [Participation
Coefficient](#cent-participation) · [Within-Module Z
Centrality](#cent-within_module_z) · [Gateway Centrality](#cent-gateway)
· [Brokerage Coordinator Centrality](#cent-brokerage_coordinator) ·
[Brokerage Itinerant Centrality](#cent-brokerage_itinerant) · [Brokerage
Representative Centrality](#cent-brokerage_representative) · [Brokerage
Gatekeeper Centrality](#cent-brokerage_gatekeeper) · [Brokerage Liaison
Centrality](#cent-brokerage_liaison) · [Modularity
Vitality](#cent-modularity_vitality) · [Community
Hub-Bridge](#cent-community_hub_bridge) · [Community-Based
Centrality](#cent-community_based) · [Comm
Centrality](#cent-comm_centrality) · [Community-Based
Mediator](#cent-community_mediator)

## Degree, strength and local connectivity

### X-degree

Nonbacktracking four-edge walks centered at each node.

``` math
Xdeg(i)=\left[\sum_{j\in N(i)}(d_j-1)\right]^2-\sum_{j\in N(i)}(d_j-1)^2
```

**Meaning.** A high value indicates a node at the centre of many
non-backtracking four-step walks, a sign that its neighbours connect it
to the wider network. Isolates, leaves and every node of a star score
zero.

``` r

centrality_x_degree(student_interactions)
```

### Clustering Degree Algorithm

Degree and strength adjusted by Barrat weighted clustering, plus
weighted contributions from immediate neighbors.

``` math
PC_i=CD_i+\sum_j\frac{w_{ij}}{w_{\max}}CD_j,\quad CD_i=\frac{\alpha d_i+(1-\alpha)s_i}{1+e^{-C_i^w}}
```

**Meaning.** A high value indicates a node that is well connected and
clustered, with neighbours that are too. `cda_alpha` (default 0.5)
balances degree against strength. Because weights enter directly, their
units matter.

``` r

centrality_cda(student_interactions)
```

### Extended Neighborhood Coreness

The sum of neighbors’ neighborhood-coreness scores, using core numbers
from the original simple undirected graph.

``` math
C_{nc+}(i)=\sum_{j\in N(i)}\sum_{l\in N(j)}k_s(l)=(A^2 k_s)_i
```

**Meaning.** Rewards access to core-rich neighborhoods. Every length-two
walk contributes, including returns to the focal node and multiple walks
reaching the same endpoint. Isolates score zero; on a regular graph of
degree d it equals d cubed. Other inputs are projected to the simple
undirected skeleton.

``` r

centrality_extended_coreness(student_interactions)
```

### Malatya Centrality

The sum of a node’s degree divided by each neighbour’s degree,
calculated on the original simple undirected graph.

``` math
M(i)=\sum_{j\in N(i)}\frac{d_i}{d_j}
```

**Meaning.** Favours nodes with many low-degree neighbours. Isolates
score zero. On nonisolated vertices the score is exactly the reciprocal
of the bridging coefficient; on a regular graph it equals degree.

``` r

centrality_malatya(student_interactions)
```

### Volume Centrality

The total original-graph degree inside a closed hop neighbourhood,
including its centre.

``` math
V_r(i) = \sum_{j:\,d(i,j)\leq r,\ d(i,j)<\infty} k_j
```

**Meaning.** Measures connectivity within and leaving the neighbourhood.
Radius 0 gives degree, default 2 uses two hops, and infinite radius
gives twice the component’s edge count. Uses the simple undirected
skeleton.

``` r

centrality_volume(student_interactions)
```

### Degree Centrality

Degree centrality counts the number of direct ties incident on a node.
In directed graphs, it can be separated into incoming and outgoing ties.

``` math
C_D(v) = \sum_{u \in V} a_{vu} = k_v
```

**Meaning.** A high degree value indicates many direct observed
connections. It is a local measure of connectivity.

``` r

centrality_degree(student_interactions)
```

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

### Outstrength Centrality

Outstrength sums outgoing edge weights.

``` math
s^{\mathrm{out}}(v) = \sum_{u \in V} w_{vu}
```

**Meaning.** A high outstrength value indicates high total outgoing
weight. Diversity describes how that weight is spread across ties.

``` r

centrality_outstrength(student_interactions)
```

### Expected Centrality

Expected centrality sums the degrees of a node’s neighbours.

``` math
C_{\mathrm{exp}}(v) = \sum_{u \in N(v)} k_u
```

**Meaning.** A high value indicates adjacency to well-connected nodes.

``` r

centrality_expected(student_interactions)
```

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

### Gravity Centrality

Gravity centrality treats each node’s mass as attracting the mass of
others over graph distance, and can be truncated at a radius.

``` math
G(v) = \sum_{u \ne v,\; d(u,v) \le R} \frac{m_v\, m_u}{d(u, v)^2}, \quad m = \text{k-shell or degree}
```

**Meaning.** A high value indicates proximity to massive nodes. By
default mass is the k-shell and only nodes within three steps
contribute; `gravity_mass = "degree", gravity_radius = NULL` gives the
degree-based gravity model over all reachable nodes, and
`gravity_radius = "auto"` sets the radius from the network.

``` r

centrality_gravity(student_interactions)
```

### Neighborhood Connectivity

Neighborhood connectivity is the mean degree of a node’s neighbours
(average neighbour degree).

``` math
C_{NC}(v) = \frac{1}{k_v} \sum_{u \in N(v)} k_u
```

**Meaning.** A high value indicates attachment to hubs. Isolates score
0. Direction is respected under `mode`.

``` r

centrality_neighborhood_connectivity(student_interactions)
```

### Entropy Variation, Degree

Entropy variation is the drop in the Shannon entropy of the degree
distribution when the node and its links are removed.

``` math
EnV_k(i) = I_k(G) - I_k(G - i), \quad I_k(G) = -\sum_j \frac{k_j}{\sum_l k_l} \ln \frac{k_j}{\sum_l k_l}
```

**Meaning.** A high value indicates a node whose removal makes the
remaining degree distribution more concentrated. Signed; negative values
occur.

``` r

centrality(student_interactions, measures = "entropy_variation_degree")
```

### Flow Coefficient

The flow coefficient is the share of a node’s neighbour pairs that are
connected only through the node.

``` math
fc(v) = \frac{|\{(j, k) : j \to v \to k,\ j \not\to k\}|}{k_v (k_v - 1)}
```

**Meaning.** A high value indicates a node that mediates local flow. On
an undirected graph it equals one minus the clustering coefficient.

``` r

centrality_flow_coefficient(student_interactions)
```

### Local Entropy

Local entropy sums $`-k \log k`$ over a node’s neighbours.

``` math
LE(v) = -\sum_{u \in N(v)} k_u \ln k_u
```

**Meaning.** Always non-positive; a lower (more negative) value
indicates a larger, denser neighbourhood. Isolates score 0.

``` r

centrality_local_entropy(student_interactions)
```

### Weighted h-index

The weighted h-index takes the h-index over topological link weights,
each neighbour’s weight repeated by its degree.

``` math
h^w_v = H\big(\{k_v k_u \text{ repeated } k_u \text{ times} : u \in N(v)\}\big)
```

**Meaning.** A high value indicates a node with many well-connected
neighbours. Input edge weights are ignored.

``` r

centrality_weighted_h_index(student_interactions)
```

### Redundancy

Redundancy is the mean degree of a node’s neighbours inside its ego
network.

``` math
r(v) = \frac{2 t_v}{k_v} = k_v - \text{effective size}(v)
```

**Meaning.** A high value indicates neighbours that are connected to
each other, so fewer structural holes.

``` r

centrality_redundancy(student_interactions)
```

## Distance and closeness

### Improved Global Structure Model

Focal degree influence combined with degrees of reachable partners and a
distance exponent set by global mean degree.

``` math
IGSM_i=e^{k_i/N}\sum_{j\ne i}\frac{k_j}{d_{ij}^{a}},\quad a=\lceil\log_2\overline{k}\rceil
```

**Meaning.** A high value indicates a high-degree node lying close to
other high-degree nodes. The distance penalty is set from the network’s
mean degree, so it is steeper in denser networks. Only reachable nodes
contribute.

``` r

centrality_improved_global_structure(student_interactions)
```

### Global Structure Model

Focal coreness combined with distance-discounted coreness of all
reachable partners.

``` math
GSM_i=e^{k_s(i)/N}\sum_{j\ne i}\frac{k_s(j)}{d_{ij}}
```

**Meaning.** A high value indicates a node in the network’s core that
lies close to many other core nodes. Every reachable node contributes,
discounted by distance. Isolates score zero.

``` r

centrality_global_structure(student_interactions)
```

### Hybrid Global Structure Model

Exponential degree-coreness influences with a distance penalty set by
their global mean.

``` math
s_i=e^{k_s(i)k_i/N},\quad a=\lceil\log_2\overline{s}\rceil,\quad H\!GSM_i=s_i\sum_{j\ne i}\frac{s_j}{d_{ij}^{a}}
```

**Meaning.** A high value indicates a node that combines high degree and
coreness and lies close to other such nodes. The distance penalty is set
from the network’s average of these combined masses. Only reachable
nodes contribute.

``` r

centrality_hybrid_global_structure(student_interactions)
```

### Exogenous Centrality

Contribution of a node to the base centrality of all other nodes,
measured by deleting it.

``` math
E_i=\sum_{j\ne i}\left[C_G(j)-C_{G-i}(j)\right]
```

**Meaning.** A high value indicates a node that contributes much to the
centrality of others: removing it lowers their centrality most. The base
measure is set with `exogenous_base` (reverse closeness by default, or
degree or betweenness). With betweenness, removing a node can raise
other scores, so values can be negative.

``` r

centrality_exogenous(student_interactions)
```

### Improved Closeness Centrality

Closeness adjusted for the number of shortest paths connecting each pair
of nodes.

``` math
ICC_i=\frac{n-1}{\sum_{j\ne i}d_{ij}/\sigma_{ij}^{\alpha}},\quad 0\le\alpha\le1
```

**Meaning.** A high value indicates a node that is close to others and
joined to them by many shortest paths. `icc_alpha` (default 0.2) sets
how much multiple shortest paths shorten the effective distance; at 0
the measure is ordinary closeness. Disconnected graphs score zero at
every node.

``` r

centrality_improved_closeness(student_interactions)
```

### Extended Gravity Centrality

The sum of immediate neighbors’ raw gravity scores, with k-shell masses
and hop distances.

``` math
G^+(i)=\sum_{j\in N(i)}\sum_{l:0<d(j,l)\le r}\frac{k_s(j)k_s(l)}{d(j,l)^2}
```

**Meaning.** A high value indicates a node whose neighbours have high
gravity centrality. Because the radius (`gravity_radius`, default 3) is
measured from each neighbour, contributions can come from one step
beyond it.

``` r

centrality_extended_gravity(student_interactions)
```

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

### Generalized Closeness Centrality

Generalized closeness sums \$\\alpha^d\$, where $`d`$ is the
shortest-path distance.

``` math
C_G(v) = \sum_{u \in V} \alpha^{\,d(v, u)}, \quad \alpha = 0.5
```

**Meaning.** A high value indicates reach under the chosen
distance-decay parameter.

``` r

centrality_generalized_closeness(student_interactions)
```

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

### Closeness Vitality

Closeness vitality measures how total network distance changes when a
node is removed, using the Wiener index \$W(G) = \\sum\_{s, t} d(s,
t)\$.

``` math
\mathrm{CV}(v) = W(G) - W(G \setminus v)
```

**Meaning.** A high value indicates that removing the node substantially
changes shortest-path structure.

``` r

centrality_closeness_vitality(student_interactions)
```

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

### Distance Entropy

Distance entropy is the normalised Shannon entropy of a node’s
hop-distance profile, so it summarises the spread of distances where
closeness summarises their mean.

``` math
h(v) = -\frac{1}{\log(M_v - m_v + 1)} \sum_{k = m_v}^{M_v} p_k \log p_k, \quad p_k = \frac{n_k(v)}{R_v}
```

**Meaning.** A high value (up to 1) indicates reach spread evenly across
many network layers; 0 means every reachable node sits at the same
distance. Hop counts only; weights are ignored.

``` r

centrality_distance_entropy(student_interactions)
```

### Local Dimension

Local dimension is the growth exponent of the ball around a node: how
fast the number of nodes within $`r`$ hops grows with $`r`$.

``` math
D_v = \frac{d \ln B_v(r)}{d \ln r}, \quad B_v(r) = 1 + |\{u : d(v, u) \le r\}|, \; r = 1, \ldots, d_{\max}(v)
```

**Meaning.** A low value indicates a node that reaches most of the
network within a few hops, so lower is more influential. With a single
radius the discretised derivative $`r\, n_v(r) / B_v(r)`$ is reported.

``` r

centrality_local_dimension(student_interactions)
```

### Local Information Dimensionality

Local information dimensionality replaces the ball count of local
dimension by its Shannon information and grows the box only to half the
node’s eccentricity.

``` math
D^I_v = -\frac{d I_v(l)}{d \ln l}, \quad I_v(l) = -p_v(l) \ln p_v(l), \; p_v(l) = \frac{B_v(l)}{n}, \; l = 1, \ldots, \lceil d_{\max}(v) / 2 \rceil
```

**Meaning.** A high value indicates a more influential node. With a
single box size the discretised derivative
$`l (1 + \ln p_v(l))\, n_v(l) / n`$ is reported.

``` r

centrality_local_information_dimension(student_interactions)
```

### Access Information

Access information is the mean number of bits a map-less walker needs to
reach every other node along shortest paths.

``` math
A_i = \frac{1}{N} \sum_j S(i \to j), \quad S(i \to j) = -\log_2 \sum_{p(i, j)} \frac{1}{k_i} \prod_{l \in p,\, l \ne i, j} \frac{1}{k_l - 1}
```

**Meaning.** A low value indicates a node that reaches the network with
few decisions. Hubs score high, because a walker leaving a hub has many
links to choose from.

``` r

centrality_access_information(student_interactions)
```

### Hide Information

Hide information is the mean number of bits the rest of the network
needs to locate a node.

``` math
H_i = \frac{1}{N} \sum_j S(j \to i)
```

**Meaning.** A high value indicates a hidden, peripheral node; hubs have
low hide information.

``` r

centrality_hide_information(student_interactions)
```

### Local Dimension, Fixed Radius

The Silva-Costa local dimension is the discretised growth exponent of
the ball around a node at one chosen radius.

``` math
D_v(r) = \frac{r\, n_v(r)}{B_v(r)}
```

**Meaning.** A structural descriptor: higher values mean the
neighbourhood is still growing fast at that radius. Nodes with
eccentricity below the radius score 0.

``` r

centrality_local_dimension_fixed(student_interactions)
```

### Fuzzy Local Dimension

Fuzzy local dimension replaces the ball count by a Gaussian-weighted
average and takes its log-log slope.

``` math
N_v(r) = \frac{\sum_{d_{vu} \le r} e^{-d_{vu}^2 / r^2}}{|\{u : d_{vu} \le r\}|}, \quad FLD(v) = \frac{d \log N_v(r)}{d \log r}
```

**Meaning.** A high value indicates a more influential node (the
opposite orientation to the rest of the dimension family).

``` r

centrality_fuzzy_local_dimension(student_interactions)
```

### Local Volume Dimension

Local volume dimension is the log-log slope of the total degree inside
the ball around a node.

``` math
V_v(l) = \sum_{d_{vu} \le l} k_u, \quad LVD(v) = \frac{d \ln V_v(l)}{d \ln l}
```

**Meaning.** A low value indicates a more important node. The source
article is closed access; the definition follows the authors’ later
preprint.

``` r

centrality_local_volume_dimension(student_interactions)
```

### Heatmap Centrality

Heatmap centrality is a node’s farness minus the mean farness of its
neighbours.

``` math
C_{HM}(v) = f(v) - \frac{1}{k_v} \sum_{u \in N(v)} f(u)
```

**Meaning.** A low (more negative) value indicates a more central node.
Isolates are undefined.

``` r

centrality_heatmap(student_interactions)
```

### Geodesic k-path

Geodesic k-path centrality counts the shortest paths of length at most
$`k`$ that start at a node, with multiplicity.

``` math
C_k(v) = \sum_{0 < d(v, u) \le k} \sigma(v, u)
```

**Meaning.** A high value indicates many short geodesics leaving the
node. Counting nodes instead of paths gives m-reach, which is what
[`centiserve::geokpath`](https://rdrr.io/pkg/centiserve/man/geokpath.html)
computes.

``` r

centrality_geodesic_kpath(student_interactions)
```

### k-path Census

The k-path census counts the simple paths of length at most $`k`$ that a
node lies on, endpoints included.

``` math
C_{kP}(v) = |\{\, P : |P| \le k,\; v \in P \,\}|
```

**Meaning.** A high value indicates a node embedded in many short walks.
Length 1 alone reproduces degree. Enumeration is exhaustive, so cost
grows with the branching factor to the power $`k`$.

``` r

centrality_kpath(student_interactions)
```

### Distance-weighted Fragmentation

Distance-weighted fragmentation asks how much worse the network
communicates once a node is deleted.

``` math
F_d(v) = 1 - \frac{\sum_{i \ne j \ne v} 1 / d_{ij}^{\,G - v}}{(n-1)(n-2)}
```

**Meaning.** A high value indicates a node whose removal fragments the
network or stretches its distances. A node inside a clique scores near
0.

``` r

centrality_fragmentation(student_interactions)
```

### Geodesic Power Closeness

Geodesic power closeness raises every distance to a negative power, so
one exponent moves the measure between a local and a global reading.

``` math
c_\delta(i) = \frac{1}{n-1} \sum_{j \ne i} d_{ij}^{-\delta}
```

**Meaning.** A high value indicates a node close to many others. The
exponent spans the family: $`\delta = 1`$ is harmonic centrality over
$`n-1`$, $`\delta = 2`$ is the inverse-square sum, a large $`\delta`$
approaches degree, and $`\delta = 0`$ counts the reachable set.
Unreachable nodes contribute nothing but stay in the denominator.

``` r

centrality_delta_closeness(student_interactions)
```

## Shortest-path brokerage and flow

### Randomized shortest paths (RSP) betweenness

Count the visits a node gets from walks that lie between shortest paths
and pure random walks, with one parameter setting the balance.

``` math
bet_i=\sum_{s=1}^{n}\sum_{t=1}^{n}\Bigl(\frac{z_{si}}{z_{st}}-\frac{z_{ti}}{z_{tt}}\Bigr)z_{it},\qquad \mathbf{Z}=(\mathbf{I}-\mathbf{W})^{-1},\quad \mathbf{W}=(\mathbf{D}^{-1}\mathbf{A})\circ\exp(-\beta\mathbf{C})
```

**Meaning.** A high value indicates a node that walks between other
pairs of nodes pass through often. `rsp_beta` moves the measure between
a random walk (small values; the default 0.01 lies near this end) and
shortest-path betweenness (large values). `rsp_cost` sets whether a
weight is read as an affinity or as a distance.

``` r

centrality_rsp_betweenness(student_interactions)
```

### Relative-entropy integrated evaluation

Turn several indexes into distributions and take the distribution
closest to all of them.

``` math
u_{ji}=C_j(i)/\textstyle\sum_k C_j(k)\ \text{or}\ (1-C_j(i)/\sum_k C_j(k))/\sum_l(1-C_j(l)/\sum_k C_j(k)),\quad w_i=\prod_{j=1}^{m}u_{ji}^{1/m}\Big/\sum_{i}\prod_{j=1}^{m}u_{ji}^{1/m}
```

**Meaning.** A high value indicates a node that ranks highly on several
centrality indexes at once. The score is the normalised geometric mean
of the indexes chosen with `re_indexes`, and it sums to one over the
nodes. A node that scores zero on any one index scores zero overall.

``` r

centrality_relative_entropy(student_interactions)
```

### DK-based gravity model

Degree, k-shell and the stage at which peeling reached the node act
together as gravitational mass.

``` math
k_s^*(i)=k_s(i)+p(i)/(\max_k q(k)+1),\quad DK(i)=k(i)+k_s^*(i),\quad DKGM_i=\sum_{j\ne i,\,d(i,j)\le R}DK(i)DK(j)/d(i,j)^2
```

**Meaning.** A high value indicates a node of large mass lying close to
other nodes of large mass, where mass combines degree, k-shell and how
late in the peeling the node was removed. Only nodes within
`dkgm_radius` steps (default 2) contribute. The score depends on the
whole graph, so adding a component can change it.

``` r

centrality_dkgm(student_interactions)
```

### Mixed gravitational centrality

Core-number mass at the source interacts with degree mass at nearby
nodes.

``` math
MGC_i=k_s(i)\sum_{j:0<d(i,j)\le r}k(j)/d(i,j)^2
```

**Meaning.** A high value indicates a node deep in the network’s core
that lies close to many high-degree nodes: the node’s own mass is its
coreness and its partners’ mass is their degree. Only nodes within
`gravity_radius` steps (default 3) contribute; a radius of 1 restricts
the sum to neighbours.

``` r

centrality_mixed_gravity(student_interactions)
```

### Extended mixed gravitational centrality

Sum immediate neighbors’ raw mixed gravitational scores.

``` math
EMGC_i=\sum_{j\in N(i)}k_s(j)\sum_{l:0<d(j,l)\le r}k(l)/d(j,l)^2
```

**Meaning.** A high value indicates a node whose neighbours have high
mixed gravitational centrality. Because the radius is measured from each
neighbour, contributions can come from up to `gravity_radius` + 1 steps
away (default 3).

``` r

centrality_extended_mixed_gravity(student_interactions)
```

### Localized bridging centrality

Brokerage in the one-hop ego network, adjusted for neighbor degrees.

``` math
LBC(v)=B_{G[N_{\leq1}(v)]}(v)\,\frac{1/d_v}{\sum_{u\in N(v)}1/d_u}
```

**Meaning.** A high value indicates a node that brokers among its
immediate neighbours and has few ties itself while its neighbours have
many. The score is betweenness within the node’s one-step neighbourhood,
multiplied by the bridging coefficient. Isolates and leaves score zero.

``` r

centrality_localized_bridging(student_interactions)
```

### Extended local bridging centrality

Brokerage in the two-hop ego network, adjusted for neighbor degrees.

``` math
LBC_2(v)=B_{G[N_{\leq2}(v)]}(v)\,\frac{1/d_v}{\sum_{u\in N(v)}1/d_u}
```

**Meaning.** A high value indicates a node that brokers within its
two-step neighbourhood and has few ties itself while its neighbours have
many. The score is betweenness within the subgraph of nodes up to two
steps away, multiplied by the bridging coefficient.

``` r

centrality_extended_local_bridging(student_interactions)
```

### Proximal betweenness

Brokerage at the first or last intermediate vertex of shortest paths.

``` math
C_{ps}(v)=\sum_{s,t:(v,t)\in E}\sigma_{st}(v)/\sigma_{st}
```

**Meaning.** A high value indicates a node that often lies on shortest
paths directly next to one of the endpoints. By default a node is
counted as the last intermediary before the destination;
`proximal_variant` selects the first after the origin, or both.

``` r

centrality_proximal_betweenness(student_interactions)
```

### Betweenness Centrality

Betweenness centrality measures how often a node lies on shortest paths
between other pairs of nodes.

``` math
C_B(v) = \sum_{s \ne v \ne t} \frac{\sigma_{st}(v)}{\sigma_{st}}
```

**Meaning.** A high value is consistent with a bridge or brokerage
position under the shortest-path model.

``` r

centrality_betweenness(student_interactions)
```

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

### Length-scaled Betweenness

Length-scaled betweenness counts the same brokered pairs as betweenness
but weights each pair by the reciprocal of its distance.

``` math
C_{LS}(v) = \sum_{s \ne v \ne t} \frac{1}{d(s,t)} \cdot \frac{\sigma_{st}(v)}{\sigma_{st}}
```

**Meaning.** A high value indicates brokerage between pairs that were
already close, which ordinary betweenness treats the same as brokerage
across the graph.

``` r

centrality_length_scaled_betweenness(student_interactions)
```

### Distance-decayed Betweenness

Distance-decayed betweenness discounts each brokered pair by a power of
its distance, so the exponent tunes how local the measure is.

``` math
C_\delta(v) = \sum_{s \ne v \ne t} (d(s,t) - 1)^{-\delta} \cdot \frac{\sigma_{st}(v)}{\sigma_{st}}
```

**Meaning.** A high value indicates brokerage concentrated among nearby
pairs. At $`\delta = 0`$ the measure is ordinary betweenness. Adjacent
pairs have no intermediary, so the singularity at $`d = 1`$ is avoided.

``` r

centrality_delta_betweenness(student_interactions)
```

### Ego Betweenness

Ego betweenness is betweenness computed inside a node’s own ego network.

``` math
C_{EB}(v) = \sum_{i < j \in N(v),\; A_{ij} = 0} \frac{1}{(A^2)_{ij}}
```

**Meaning.** A high value indicates a node that brokers among its own
neighbours, which is what an egocentric survey can measure. A node with
fewer than two neighbours scores 0.

``` r

centrality_ego_betweenness(student_interactions)
```

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

### Local bridging (legacy degree product)

Inverse focal degree multiplied by the bridging coefficient.

``` math
\mathrm{LBr}(v) = \frac{1}{k_v} \cdot \beta(v), \quad \beta(v) = \frac{1/k_v}{\sum_{u \in N(v)} 1/k_u}
```

**Meaning.** Retains the original cograph degree-only score. The formula
uses degrees only.

``` r

centrality_local_bridging(student_interactions)
```

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

### Flow Betweenness Centrality

Flow betweenness measures brokerage through maximum flows between pairs
of nodes.

``` math
C_{FB}(v) = \sum_{s \ne v \ne t} f_{st}(v), \quad f_{st}(v) = \text{flow through } v \text{ in a max } s\text{-}t \text{ flow}
```

**Meaning.** A high value indicates importance for potential flow
capacity between other nodes under the graph model.

``` r

centrality_flow_betweenness(student_interactions)
```

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

### Current-Flow Closeness Centrality

Current-flow closeness measures closeness with effective-resistance
distances, treating the network as an electrical circuit.

``` math
C_{CFC}(v) = \frac{n - 1}{\sum_{u \ne v} R_{vu}}, \quad R_{vu} = \text{effective resistance between } v \text{ and } u
```

**Meaning.** A high value indicates closeness under an all-path flow
model. It generally requires connected graphs.

``` r

centrality_current_flow_closeness(student_interactions)
```

### Entropy Variation, Betweenness

Entropy variation of the betweenness distribution: the drop in its
Shannon entropy when the node is removed.

``` math
EnV_b(i) = I_b(G) - I_b(G - i)
```

**Meaning.** A high value indicates a node whose removal concentrates
shortest-path traffic on fewer nodes. Betweenness is recomputed per
removal.

``` r

centrality(student_interactions, measures = "entropy_variation_betweenness")
```

## Spectral, walk and influence

### Trust-PageRank

Replace PageRank’s even split of a node’s score among its neighbours by
a trust-value that mixes how similar the two nodes are with how large
the receiver’s degree is.

``` math
TPR_i=\frac{1-\alpha}{n}+\alpha\sum_{j\in N_i}T(i,j)TPR_j,\qquad T(i,j)=(1-k)\frac{s(i,j)}{\sum_{l\in N_j}s(j,l)}+k\frac{d_i}{\sum_{l\in N_j}d_l}
```

**Meaning.** A high value indicates a node that receives a large share
of a PageRank-style flow in which each node passes more of its score to
neighbours that are similar to it and well connected. `tpr_k` sets the
balance between similarity and degree, and `tpr_alpha` is the damping
factor. A component without triangles returns `NA` with a warning,
because the similarity is undefined there.

``` r

centrality_trust_pagerank(student_interactions)
```

### Iterative resource allocation (IRA)

Give every node one unit of resource, hand it repeatedly to neighbours
in proportion to the receiver’s centrality, and read the steady state.

``` math
I(t+1)=AI(t),\quad a_{ij}=\frac{\theta_i^{\alpha}}{\sum_{u\in\Gamma(j)}\theta_u^{\alpha}}\delta_{ij},\quad I(0)=\mathbf{1}
```

**Meaning.** A high value indicates a node that accumulates resource
when every node repeatedly passes its resource to its neighbours in
proportion to their centrality (`ira_mass`, default coreness). The
scores of a connected component sum to its number of nodes. On a
bipartite component with unequal sides the iteration oscillates, and a
warning is raised.

``` r

centrality_ira(student_interactions)
```

### Improved iterative resource allocation (IIRA)

The same resource iteration, with each node’s share scaled by how much
of a spreading process it could actually carry.

``` math
a_{ij}=\bigl[1-(1-\beta)^{k_i}\bigr]\theta_i\Bigl(\sum_{u\in\Gamma(j)}\theta_u\Bigr)^{-1}\delta_{ij},\quad I(t)=A^{t}\mathbf{1}
```

**Meaning.** A high value indicates a node that accumulates resource
when resource is passed to neighbours in proportion to their centrality
and to their chance of being reached by a spreading process at rate
`iira_beta`. The resource decays over the `iira_steps` iterations, so
only the ranking is meaningful, and only within a connected component.

``` r

centrality_iira(student_interactions)
```

### Multi-characteristics gravity model

Combine degree, coreness and eigenvector features in distance-decaying
node interactions.

``` math
m_i=K_i+\alpha S_i+X_i,\quad \alpha=\max\{\mathrm{med}(K),\mathrm{med}(X)\}/\mathrm{med}(S),\quad MCGM_i=\sum_{j:0<d(i,j)\le R}m_i m_j/d(i,j)^2
```

**Meaning.** A high value indicates a node of large mass lying close to
other nodes of large mass, where mass combines degree, coreness and
eigenvector centrality, each scaled to its maximum. `mcgm_alpha` sets
the weight of coreness, and only nodes within `mcgm_radius` steps
(default 2) contribute.

``` r

centrality_mcgm(student_interactions)
```

### SpectralRank

Outgoing influence after adding a ground node and optional node priors.

``` math
B=\begin{pmatrix}A+\operatorname{diag}(p)&\mathbf{1}\\\mathbf{1}^{T}&0\end{pmatrix},\quad Bs=\rho(B)s,\quad SR_i=s_i/\max_{j\le n+1}s_j
```

**Meaning.** A high value indicates a node with strong outgoing
influence, measured by the leading eigenvector of the network augmented
with a ground node linked to every node. `sr_prior` adds optional prior
information for each node; with the default of zero the score is plain
SpectralRank. Transpose the input to measure incoming influence.

``` r

centrality_spectralrank(student_interactions)
```

### ControlRank

Smallest grounded eigenvalue after pinning each node in turn.

``` math
CR_i=\lambda_{\min}(((L+L^T)/2)_{-i,-i}),\quad L=\operatorname{diag}(A\mathbf{1})-A
```

**Meaning.** A high value indicates a node whose grounding leaves the
rest of the network most tightly coupled, as measured by the smallest
eigenvalue of the Laplacian with that node removed. Disconnected
undirected graphs score zero at every node.

``` r

centrality_controlrank(student_interactions)
```

### Node and Neighbor Layer Information

Propagate degree volume through a chosen number of neighbor steps.

``` math
b_i=\sum_{j:d(i,j)\le r}d_j,\quad r=\lceil L\rceil,\quad NINL_p=A^p b
```

**Meaning.** A high value indicates a node surrounded by high-degree
nodes. Degrees are first summed within a radius and then propagated
through `ninl_order` steps (default 3). With order 0 the score is the
total degree of the node’s neighbourhood.

``` r

centrality_ninl(student_interactions)
```

### Expected Force

Entropy of onward transmission opportunities after two infection events.

``` math
ExF(i)=-\sum_{j=1}^{J}p_j\log p_j,\quad p_j=D_j/\sum_kD_k
```

**Meaning.** A high value indicates a node from which an infection has
many, and varied, ways to spread after its first two transmissions. The
score is the entropy of these spreading opportunities.

``` r

centrality_expected_force(student_interactions)
```

### Modified Expected Force

Expected Force adjusted for the seed degree.

``` math
ExF^M(i)=\log(\alpha d_i)ExF(i),\quad\alpha>1
```

**Meaning.** Expected force weighted by the logarithm of the node’s
degree, so that among nodes with similar spreading opportunities the
better-connected one scores higher. `exf_alpha` (default 2, and greater
than 1) scales the degree inside the logarithm.

``` r

centrality_modified_expected_force(student_interactions)
```

### Bridging capital

Loss of valued information walks after deleting each outgoing matrix
entry.

``` math
Brid_i=\sum_j\sum_{s,t}v_{st}\sum_{h=1}^{T}[P^h-(P-P_{ij}E_{ij})^h]_{st}
```

**Meaning.** A high value indicates a node whose outgoing ties carry
many of the network’s valued information walks, measured by the loss
when each tie is removed in turn. Walks of up to `bridging_steps` steps
(default 2) are counted, and `bridging_values` weights
source-destination pairs. Edge weights are read as transmission
probabilities.

``` r

centrality_bridging_capital(student_interactions, weighted = FALSE)
```

### LineRank

Stationary edge-state probabilities aggregated at original endpoints.

``` math
p=cQ^T p+(1-c)\mathbf{1}/m,\quad LR_v=\sum_{e\text{ incident to }v}p_e
```

**Meaning.** A high value indicates a node whose ties are visited often
by a random walk that moves from tie to tie. Tie probabilities are
summed at their end nodes; `linerank_aggregation = "weight"` also
multiplies them by the tie weights. `damping` is the usual random-walk
damping factor.

``` r

centrality_linerank(student_interactions)
```

### Random walk decay

Discounted first arrival at each node, summed over starting-node
weights.

``` math
RWD_v=\sum_u b_u\,\mathbb{E}_u[a^{T_v};T_v<\infty],\quad 0\leq a<1
```

**Meaning.** A high value indicates a node that random walks starting
elsewhere reach early. Each first arrival is discounted by `rwd_decay`
(default 0.5) per step, and starting nodes can be weighted with
`rwd_node_weights`. A node’s own outgoing ties cannot change its own
score.

``` r

centrality_random_walk_decay(student_interactions)
```

### Graph regularization centrality

Reciprocal retention of a unit impulse under weighted Laplacian
smoothing.

``` math
GRC_i=1/[(I+\gamma L)^{-1}]_{ii},\quad L=D-W,\quad \gamma\geq0
```

**Meaning.** A high value indicates a node whose signal spreads quickly
across the network under Laplacian smoothing, so that little of it is
retained at the node. `grc_gamma` (default 1) sets the strength of
smoothing. Scores range from 1 to the size of the node’s component, and
isolates score 1.

``` r

centrality_graph_regularization(student_interactions)
```

### Adaptive LeaderRank

Stationary resource scores with every destination weighted by its
original H-index and a ground node with H-index one.

``` math
h_g=1,\quad w_{ji}=a_{ji}h_i,\quad s_i=\sum_j\frac{w_{ji}}{\sum_k w_{jk}}s_j,\quad \sum_{i\cup g}s_i=N
```

**Meaning.** A high value indicates a node that attracts a large share
of a random resource flow in which each node sends more to neighbours
with a high H-index. `alr_h_mode` sets how H-indices are computed on
directed graphs.

``` r

centrality_adaptive_leaderrank(student_interactions)
```

### Weighted LeaderRank

Stationary resource scores with a ground node that distributes according
to original in-degree powers.

``` math
w_{gi}=(k_i^{in})^{\alpha},\quad w_{ig}=1,\quad s_i=\sum_j\frac{w_{ji}}{\sum_l w_{jl}}s_j,\quad \sum_{i\cup g}s_i=N+1
```

**Meaning.** A high value indicates a node that attracts a large share
of a random resource flow in which a ground node, linked to every node,
sends more to nodes with high in-degree. `wlr_alpha` sets how strongly
in-degree is favoured; at zero the measure is ordinary LeaderRank.

``` r

centrality_weighted_leaderrank(student_interactions)
```

### Node Resistance Curvature

A geometric descriptor based on electrical resistance, evaluated
separately within each connected component.

``` math
p_i=1-\frac{1}{2}\sum_{j\sim i}w_{ij}R_{ij}
```

**Meaning.** Low or negative values identify tree-like junctions that
hold parts of the network together; higher values indicate nodes with
redundant connections. Edge weights are read as conductances. The scores
within each connected component sum to one.

``` r

centrality_resistance_curvature(student_interactions)
```

### Dynamics-Sensitive Centrality

A finite-time linearized spreading score with explicit spreading and
recovery rates, on the simple undirected skeleton.

``` math
S(T)=\sum_{r=0}^{T-1}\beta A[\beta A+(1-\mu)I]^r\mathbf{1}
```

**Meaning.** Higher scores indicate more cumulative spreading activity
in this approximation. Values can exceed the number of nodes. Defaults
are beta=0.1, mu=1, T=5. Recovery mu=1 recovers finite diffusion; mu=0
selects the SI case. Isolates and horizon zero score zero; overflow
raises an error.

``` r

centrality_dynamics_sensitive(student_interactions)
```

### Finite-Horizon Diffusion Centrality

Total weighted walks of lengths 1 through T starting at a node, allowing
repeated visits and returns.

``` math
DC(A;q,T) = \sum_{t=1}^{T}(qA)^t\mathbf{1}
```

**Meaning.** High values indicate more weighted walk activity from the
source. Probability interpretation requires entries of qA in \[0,1\].
Defaults q=1 and T=3 are cograph choices. Directed arcs carry
information outwards. Overflow raises an error.

``` r

centrality_diffusion_centrality(student_interactions)
```

### Dynamical Importance

Relative loss of adjacency spectral radius after removing the node,
recomputed directly for each deletion.

``` math
I_i = \frac{\rho(A)-\rho(A_{-i})}{\rho(A)}
```

**Meaning.** A high value indicates a node whose removal most reduces
the network’s spectral radius, which governs how easily spreading
processes take off. Each deletion is recomputed exactly.

``` r

centrality_dynamical_importance(student_interactions)
```

### Eigenvector Centrality

Eigenvector centrality gives high scores to nodes connected to other
high-scored nodes.

``` math
A\,\mathbf{x} = \lambda_{\max}\,\mathbf{x}, \quad C_E(v) = x_v
```

**Meaning.** A high value indicates embeddedness in a central
neighbourhood. Reading it as influence requires a relation that
transmits influence.

``` r

centrality_eigenvector(student_interactions)
```

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

### Bonacich Power Centrality

Bonacich power centrality scores nodes using the centrality of their
neighbours and a parameter \$\\beta\$ controlling dependence.

``` math
\mathbf{c}(\alpha, \beta) = \alpha (I - \beta A)^{-1} A\,\mathbf{1}
```

**Meaning.** A high value indicates a favourable recursive position
under the selected Bonacich parameterization.

``` r

centrality_power(student_interactions)
```

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

### Hubbell Centrality

Hubbell centrality is an input-output centrality where status is
recursively reinforced through ties.

``` math
\mathbf{x} = (I - w W)^{-1} \mathbf{1}, \quad w = 0.5,\; W = \text{weighted adjacency}
```

**Meaning.** A high value indicates recursive prominence under the
chosen weight factor. Some parameter settings make the system singular.

``` r

centrality_hubbell(student_interactions)
```

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

### Communicability Betweenness Centrality

Communicability betweenness measures walk-based communicability between
other pairs through a node.

``` math
C_{CB}(v) = \frac{1}{(n-1)(n-2)} \sum_{s \ne t \ne v} \frac{G_{st} - G_{st}^{(v)}}{G_{st}}, \quad G = e^{A}
```

**Meaning.** A high value indicates a node that lies on many walks
between other nodes. $`G^{(v)}`$ is computed with $`v`$ removed.

``` r

centrality_communicability_betweenness(student_interactions)
```

### Random Walk Centrality

Random walk centrality uses expected random-walk access times as
distances.

``` math
C_{RW}(v) = \frac{1}{\sum_{u \ne v} \tilde{m}_{vu}}, \quad \tilde{m}_{vu} = \tfrac{1}{2}\bigl( m_{vu} + m_{uv} \bigr)
```

**Meaning.** A high value indicates that the node is reached efficiently
under a random-walk process. $`m_{uv}`$ is the mean first-passage time.

``` r

centrality_random_walk(student_interactions)
```

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

### Immediate Effects Centrality (IEC)

Score a node by how quickly everyone else’s influence reaches it, along
a chain in which every actor also listens to itself.

``` math
c_{IEC}(j)=\Bigl(\frac{\sum_{i \ne j} m_{ij}}{n-1}\Bigr)^{-1},\qquad \mathbf{M}=(\mathbf{I}-\mathbf{Z}+\mathbf{E}\mathbf{Z}_{dg})\operatorname{diag}(1/c),\qquad \mathbf{Z}=(\mathbf{I}-\mathbf{W}+\mathbf{1}c')^{-1}
```

**Meaning.** A high value indicates a node that the influence of all
other nodes reaches quickly, through short chains of influence. The
score is defined only when every node can reach every other; otherwise
every node returns `NA` with a warning.

``` r

centrality_iec(student_interactions)
```

### Second-Order Centrality

Second-order centrality summarises variability in random-walk return
times.

``` math
\mathrm{SO}(v) = \operatorname{sd}_{u}\bigl( m_{uv} \bigr)
```

**Meaning.** The statistic describes the variability of a random walk’s
return times to the node; lower values are more central.

``` r

centrality(student_interactions, measures = "second_order")
```

### Information Centrality

Information centrality measures centrality through resistance distance,
the information carried by all paths between two nodes.

``` math
C_I(v) = \left( C_{vv} + \frac{T - 2 R_v}{n} \right)^{-1}, \quad C = (D - A + J)^{-1},\; T = \operatorname{tr} C,\; R_v = \textstyle\sum_j C_{vj}
```

**Meaning.** A high value indicates a central position under the
information-flow model.

``` r

centrality_information(student_interactions)
```

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

### Diffusion Degree

The default method adds the scaled degree of the focal node and its
neighbours. On a simple undirected graph:

``` math
DD(i) = \lambda\left(k_i + \sum_{j\in N(i)} k_j\right)
```

**Meaning.** High values indicate local connectivity through the node
and its neighbours. With `diffusion_method = "power_series"` (automatic
for TNA inputs), the function instead returns row sums of W + W^2 + … +
W^n. That variant fixes the horizon at n and ignores `lambda` and
`mode`.

``` r

centrality_diffusion(student_interactions)
```

### Infection Centrality

Infection centrality estimates spreading potential through
infection-style self-avoiding walks with attenuation.

``` math
\mathrm{Inf}(v) = \sum_{d=1}^{L} \beta^{\,d+1} (1 - \mu)^{d}\, w_d(v), \quad \beta = 0.8,\; \mu = 0,\; L = 6
```

**Meaning.** A high value indicates a favourable position under the
assumed infection process. $`w_d(v)`$ counts length-$`d`$ self-avoiding
walks from $`v`$.

``` r

centrality(student_interactions, measures = "infection")
```

### Edge Percolated Component

The edge percolated component averages the size of the component a node
ends up in when edges survive at random.

``` math
\mathrm{EPC}(v) = \frac{1}{R\,n} \sum_{r=1}^{R} |C_r(v)|, \quad \Pr(\text{edge survives}) = 1 - t
```

**Meaning.** A high value indicates a node that stays connected to a
large share of the network under random link failure. The value is a
Monte Carlo estimate; set `epc_seed` to make it reproducible.

``` r

centrality_epc(student_interactions)
```

### VoteRank Centrality

VoteRank is an iterative voting algorithm for ranking spreader
candidates: each round the highest-voted node is selected, its voting
ability is zeroed, and its neighbours’ voting abilities are reduced.

**Meaning.** A high value indicates early selection by the VoteRank
rule.

``` r

centrality_voterank(student_interactions)
```

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

### Shapley Value, Game 1

Shapley value of the node in the coalition game whose worth is the
number of nodes a coalition covers within one hop.

``` math
SV_1(v) = \sum_{u \in \{v\} \cup N(v)} \frac{1}{1 + k_u}
```

**Meaning.** A high value indicates a node whose presence adds much
one-hop coverage to a typical coalition. Values sum to $`n`$.

``` r

centrality_shapley_game1(student_interactions)
```

### Shapley Value, Game 2

Shapley value in the game where a node is covered once at least $`k`$
coalition members are adjacent to it.

``` math
SV_2(v) = \min\!\left(1, \frac{k}{1 + k_v}\right) + \sum_{u \in N(v)} \max\!\left(0, \frac{k_u - k + 1}{k_u (1 + k_u)}\right)
```

**Meaning.** A high value indicates a node that helps push many
neighbours over the $`k`$-neighbour threshold. With $`k = 1`$ this is
game 1.

``` r

centrality_shapley_game2(student_interactions)
```

### Shapley Value, Game 3

Shapley value in the game where a coalition covers every node within a
hop cutoff.

``` math
SV_3(v) = \sum_{u \in \{v\} \cup N_d(v)} \frac{1}{1 + |N_d(u)|}, \quad N_d(u) = \{w : d(u, w) \le d_{cut}\}
```

**Meaning.** A high value indicates a node that reaches many otherwise
hard-to-reach nodes within the cutoff. With cutoff 1 this is game 1.

``` r

centrality_shapley_game3(student_interactions)
```

### Rumor Centrality

Rumor centrality counts the spreading orders that could have started at
a node; on a general graph it is evaluated on the node’s breadth-first
tree.

``` math
\log R(v) = \log N! - \sum_{u} \log T^v_u
```

**Meaning.** A high value indicates a plausible origin of a spread,
typically a node near the centre. Returned on the log scale.

``` r

centrality_rumor(student_interactions)
```

### DegreeDiscountIC

DegreeDiscountIC is the greedy seed-selection order under degree
discounting for the independent-cascade model.

``` math
dd_v = d_v - 2 t_v - (d_v - t_v)\, t_v\, p
```

**Meaning.** A high score indicates an early selection: the first node
selected scores 1, the last $`1/n`$. Ties follow node order.

``` r

centrality_degree_discount(student_interactions)
```

### SingleDiscount

SingleDiscount is the greedy seed-selection order where each neighbour
of a new seed discounts its degree by one.

``` math
dd_v = d_v - t_v
```

**Meaning.** A high score indicates an early selection. Equivalent to
repeatedly removing the highest-degree node.

``` r

centrality_single_discount(student_interactions)
```

### NCVoteRank

NCVoteRank is VoteRank with each voter’s ability weighted by its
normalised neighbourhood coreness, and two-hop weakening after each
election.

``` math
s_u = \sum_{v \in N(u)} va_v \,[\theta + (1 - \theta)\, nc_v]
```

**Meaning.** A high score indicates an early election. With
$`\theta = 1`$ and two-hop weakening switched off, this is VoteRank.

``` r

centrality_ncvoterank(student_interactions)
```

### WVoteRank

WVoteRank is VoteRank for weighted graphs: votes are weighted by edge
weight and the score takes a square root.

``` math
s_v = \sqrt{k_v \sum_{u \in N(v)} va_u w_{vu}}
```

**Meaning.** A high score indicates an early election. Neighbours of an
elected node lose $`1/\langle w \rangle`$, with $`\langle w \rangle`$
the average strength.

``` r

centrality_wvoterank(student_interactions)
```

### EnRenew

EnRenew elects the node whose neighbours supply the most entropy, then
renews the entropies around it.

``` math
E_v = \sum_{u \in N(v)} -p_{uv} \ln p_{uv}, \quad p_{uv} = \frac{k_u}{\sum_{l \in N(v)} k_l}
```

**Meaning.** A high score indicates an early election. Terms within the
renewal radius are scaled by $`1 - 1/(2^{d-1} \ln\langle k \rangle)`$.

``` r

centrality_enrenew(student_interactions)
```

### VoteRank++

VoteRank++ starts abilities from degree, splits votes in proportion to
neighbour degree, and suppresses abilities multiplicatively after each
election.

``` math
s_v = \sqrt{k_v \sum_{u \in N(v)} va_u\, w_{u \to v}}, \quad va_v^{(0)} = \ln(1 + k_v / k_{\max})
```

**Meaning.** A high score indicates an early election. Abilities are
multiplied by $`\lambda`$ one step away and $`\sqrt{\lambda}`$ two steps
away.

``` r

centrality_voterank_plus(student_interactions)
```

### Node Contraction

Node contraction importance measures how much the network’s cohesion
rises when a node and its neighbours are merged into one.

``` math
IMC(v) = 1 - \frac{\partial(G)}{\partial(G_v)}, \quad \partial(G) = \frac{1}{N \bar{L}}
```

**Meaning.** A high value indicates a node whose contraction shortens
paths most.

``` r

centrality_node_contraction(student_interactions)
```

### Improved Node Contraction

Improved node contraction adds the contraction scores of a node’s edges,
computed on the line graph.

``` math
IIMC(v) = \alpha\, IMC(v) + \beta \sum_{e \ni v} IMC_{L(G)}(e), \quad \alpha / \beta = 5
```

**Meaning.** A high value indicates a node that is important both itself
and through its edges. Values can exceed 1.

``` r

centrality_node_contraction_improved(student_interactions)
```

### Two-Way Random Walk Betweenness

Two-way random walk betweenness counts, over all node pairs, how often a
node lies on the most likely two-step out-and-back route.

``` math
T_{ij}[t, k] = P_{itj} P_{jki}, \quad P_{itj} = \frac{w_{it} w_{tj}}{d_i d_j}
```

**Meaning.** A high count indicates a node on many dominant two-way
routes. Cost grows as $`n^4`$.

``` r

centrality_two_way_rw(student_interactions)
```

## Neighbourhood structure and cohesion

### Degree and importance of lines (DIL)

Add to a node’s degree the share it can claim of the importance of the
lines that touch it, where a line matters when its endpoints reach far
beyond it and few triangles offer a way round.

``` math
L_{v_i}=k_i+\sum_{v_j\in\Gamma_i}W_{v_iv_j},\qquad W_{v_iv_j}=I_{e_{ij}}\frac{k_i-1}{k_i+k_j-2},\qquad I_{e_{mn}}=\frac{(k_m-p-1)(k_n-p-1)}{p/2+1}
```

**Meaning.** A high value indicates a node with many ties, several of
them important: ties whose endpoints reach far beyond them and that few
triangles bypass. The score is at least the node’s degree, and equals it
in complete graphs and stars.

``` r

centrality_dil(student_interactions)
```

### Lhc index

Score a node by how much degree, inflated by each contributor’s share of
the network’s triangles, sits within a couple of steps of its
neighbours.

``` math
Lhc(v)=\sum_{w\in\tau(v)}C(w),\qquad C(v)=\sum_{u\in\Phi(v)}\frac{k_u\bigl(1+TP(u)\bigr)}{d^{2}(uv)},\qquad TP(u)=\frac{NTS(u)}{TNTS}
```

**Meaning.** A high value indicates a node whose surroundings, within
`lhc_radius` steps (default 2), hold many well-connected nodes that sit
on many triangles. A node of modest degree can therefore outscore a
busier one if its neighbours are well embedded. On a graph without
triangles the index reduces to a distance-discounted sum of degrees.

``` r

centrality_lhc(student_interactions)
```

### Hybrid characteristic centrality (HCC)

Add a node’s blended local degree to how late a repeated minimum-degree
peel gets round to removing it.

``` math
HCC(u)=\frac{k^{ex}(u)}{k^{ex}_{\max}}+\frac{pos(u)}{pos_{\max}},\qquad k^{ex}(u)=\delta k(u)+(1-\delta)\!\!\sum_{v\in\phi(u)}\!\!k(v)
```

**Meaning.** A high value indicates a node that combines a high degree,
blended with its neighbours’ degrees, with a position deep in the
network, reached late when low-degree nodes are repeatedly peeled away.
`hcc_delta` (default 0.5) weights the node’s own degree against its
neighbours’. Scores range from 0 to 2.

``` r

centrality_hcc(student_interactions)
```

### Extended hybrid characteristic centrality (EHCC)

Collect a node’s own hybrid characteristic score together with every
neighbour’s.

``` math
EHCC(u)=HCC(u)+\sum_{v\in\phi(u)}HCC(v)
```

**Meaning.** A high value indicates a node that, together with its
neighbours, scores highly on hybrid characteristic centrality. It
rewards nodes with well-connected, deeply embedded neighbours, whatever
the node’s own position. An isolate scores its own hybrid characteristic
centrality.

``` r

centrality_ehcc(student_interactions)
```

### KED method

Weight the degree by how evenly a node’s neighbours carry its local
paths, then by how many second-step paths there are.

``` math
KED(i)=k_i\bigl(1+H_i\bigr)\exp\!\Bigl(\frac{K_i}{N}\Bigr),\qquad H_i=\frac{\sum_{j\in N(i)}-p_j\log p_j}{\log k_i},\quad p_j=\frac{k_j}{K_i},\quad K_i=\sum_{j\in N(i)}k_j
```

**Meaning.** A high value indicates a node with many neighbours whose
onward connections are numerous and evenly spread among them. Scores
depend on the number of nodes in the whole graph, so only rankings are
comparable across graphs of different size.

``` r

centrality_ked(student_interactions)
```

### Local neighbor contribution (LNC)

Multiply what a node contributes on its own by what its neighbourhood
contributes to it.

``` math
LNC(i)=\underbrace{d_i\bigl(1-1/d_i\bigr)^{d_i-1}}_{ownCon(i)}\cdot\underbrace{d_i^{2}\frac{\sum_{j\in N(i)}d_j}{n-1}}_{neiCon(i)},\qquad 0^0:=1
```

**Meaning.** A high value indicates a node with many neighbours that are
themselves well connected. A node with a single neighbour scores its
neighbour’s degree centrality. Scores depend on the number of nodes in
the whole graph, so only rankings are comparable across graphs of
different size.

``` r

centrality_lnc(student_interactions)
```

### Neighborhood (neighbor distance) centrality

Add up a benchmark centrality over the non-backtracking walks that leave
the node, discounted once per step.

``` math
C^n_i(\theta)=\theta_i+\sum_{k=1}^{n}a^k\!\!\sum_{w\in W_k(i)}\!\!\theta_{\mathrm{end}(w)},\quad W_k(i)=\{\text{non-backtracking walks of length }k\text{ from }i\}
```

**Meaning.** A high value indicates a node that reaches many central
nodes, by `nd_mass` (default degree), within a few steps. Each step of
distance is discounted by `nd_decay` (default 0.2), up to `nd_order`
steps (default 2). With either set to zero, the score is the base
centrality itself.

``` r

centrality_neighbor_distance(student_interactions)
```

### Coleman-Theil hierarchy

Concentration of Burt’s dyadic constraints over a node’s contacts.

``` math
H_i=\frac{\sum_{j\in N(i)}r_{ij}\log(r_{ij})}{d_i\log d_i},\quad r_{ij}=c_{ij}/\operatorname{mean}_{k\in N(i)}c_{ik}
```

**Meaning.** A high value indicates that the constraint on a node is
concentrated in a few of its contacts. Scores lie between 0 and 1; a
node with one contact scores 1 and an isolate 0.

``` r

centrality_coleman_theil(student_interactions)
```

### Maximal Clique Centrality

Sum of factorial contributions from maximal cliques containing the node,
on the simple undirected skeleton.

``` math
MCC(v) = \sum_{C\in\mathcal{M}(v),\,|C|\geq2} (|C|-1)!
```

**Meaning.** Larger cliques contribute much more than edges or
triangles. Contained cliques are excluded. Isolates score 0 by explicit
convention. Exponential enumeration is held back from the default all
tier; raw-score overflow raises an error.

``` r

centrality_mcc(student_interactions)
```

### Node Truss Number

The largest truss number of an incident edge, on the simple undirected
skeleton.

``` math
t(v) = \max\{k : v \in V(T_k)\},\quad T_k:\ \text{each edge has at least } k-2\text{ triangles}
```

**Meaning.** Higher values indicate triangle-supported cohesion. Tree
vertices score 2, a k-clique scores k, and isolates score 0.

``` r

centrality_truss(student_interactions)
```

### Mixed Degree Decomposition

Shell thresholds from residual degree plus attenuated exhausted degree.

``` math
k_i^{m} = k_i^{r} + \lambda k_i^{e}
```

**Meaning.** Higher means membership of a stronger mixed-degree shell.
Lambda 0 gives coreness; lambda 1 gives degree. Default 0.7; scores can
be fractional.

``` r

centrality_mdd(student_interactions)
```

### Bridging Coefficient

The reciprocal-degree ratio before multiplication by betweenness.

``` math
B(i) = \frac{1/k_i}{\sum_{j\in N(i)}1/k_j}
```

**Meaning.** Higher values identify low-degree nodes adjacent to
high-degree nodes. Isolates score 0. Uses the simple undirected
skeleton.

``` r

centrality_bridging_coefficient(student_interactions)
```

### Godfather Index

The number of unconnected unordered pairs of a node’s neighbours.

``` math
GF(i) = {k_i\choose 2} - t_i
```

**Meaning.** Higher values indicate more local brokerage opportunities.
Here t_i counts focal triangles. Uses the simple undirected skeleton.

``` r

centrality_godfather(student_interactions)
```

### Supported Relationships

The number of neighbours sharing at least one common neighbour with the
node.

``` math
S(i) = |\{j\in N(i):(A^2)_{ij}>0\}|
```

**Meaning.** Higher values mean more triangle-supported relationships; a
relationship counts once even if several common neighbours support it.
Uses the simple undirected skeleton.

``` r

centrality_support(student_interactions)
```

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

### Diversity Centrality

Diversity centrality measures entropy in the distribution of edge
weights around a node.

``` math
\mathrm{Div}(v) = \frac{-\sum_{u \in N(v)} p_{vu} \log_2 p_{vu}}{\log_2 k_v}, \quad p_{vu} = \frac{w_{vu}}{\sum_{u'} w_{vu'}}
```

**Meaning.** A high value indicates that weighted ties are relatively
evenly distributed.

``` r

centrality_diversity(student_interactions)
```

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

### Coreness Centrality

Coreness assigns nodes to k-core shells.

``` math
C_{\mathrm{core}}(v) = \max \{\, k : v \in (k\text{-core of } G) \,\}
```

**Meaning.** A high value indicates membership in a dense core.

``` r

centrality_coreness(student_interactions)
```

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

### s-shell Index

The s-shell index peels the graph by node strength built from asymmetric
topological link weights, generalising k-shell.

``` math
w_{ij} = 1 + (k_i\, k^{out}_j)^a, \quad s_i = \sum_{j \in N(i)} w_{ij}
```

**Meaning.** A high shell index indicates a node deep in the
strength-based core. With $`a = 0`$ the shells are the dense ranks of
k-core.

``` r

centrality_s_shell(student_interactions)
```

### Weighted k-shell

The weighted k-shell peels the graph by a generalised degree that mixes
degree and strength.

``` math
k'_v = \big(k_v^{\alpha} s_v^{\beta}\big)^{1 / (\alpha + \beta)}
```

**Meaning.** A high shell index indicates a node deep in the weighted
core. Unit weights give the k-core number.

``` r

centrality_weighted_kshell(student_interactions)
```

### Renewed Coreness

Renewed coreness is the k-core number after removing links that lead
nowhere new.

``` math
D_{ij} = \frac{|N(j) \setminus N[i]| + |N(i) \setminus N[j]|}{2}, \quad \text{keep } D_{ij} \ge 2
```

**Meaning.** A high value indicates a core node whose links reach beyond
shared neighbourhoods. An isolated clique scores 0.

``` r

centrality_renewed_coreness(student_interactions)
```

### s-core Index

The s-core index is the weighted k-core: the largest strength threshold
whose core still contains the node.

``` math
s\text{-core}(s) = \text{maximal } H \subseteq G \text{ with } s_i^H \ge s \;\; \forall i \in H
```

**Meaning.** A high value indicates a node deep in the strength-based
core. Unit weights give the k-core number exactly.

``` r

centrality_s_core(student_interactions)
```

### Local Efficiency

Local efficiency is how well a node’s neighbours still communicate once
the node itself is gone, using only the links among them.

``` math
E_{loc}(v) = \frac{1}{k_v (k_v - 1)} \sum_{i \ne j \in N(v)} \frac{1}{d_{ij}^{\,G_v}}
```

**Meaning.** A high value indicates a fault-tolerant neighbourhood; a
node whose neighbours are mutually unconnected scores 0. Note that
[`igraph::local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html)
measures those distances through the rest of the network instead, so it
reports larger values.

``` r

centrality_local_efficiency(student_interactions)
```

## Directed prestige and hierarchy

### BG-index (beta power)

Expected number of times a node is selected as a predecessor.

``` math
\beta^+(i)=\sum_{i\to j}1/d^-(j),\quad\beta^-(i)=\sum_{j\to i}1/d^+(j)
```

**Meaning.** A high value indicates a node that is the sole predecessor,
or one of few, of many others: each node divides one unit equally among
the nodes that point to it. The default credits senders;
`beta_direction = "negative"` credits receivers. The two coincide on
undirected graphs.

``` r

centrality_beta_measure(student_interactions)
```

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

### Map equation centrality

Bits saved by redesigning a fixed module’s codebook after silencing a
node.

``` math
MEC(i)=-(s_m-p_i)\log_2((s_m-p_i)/s_m),\quad s_m=\sum_{j\in m}p_j+q_m
```

**Meaning.** A high value indicates a node whose flow matters to the
description of its module: removing it saves many bits in the module’s
codebook. The partition is supplied through `membership` (one module
when `NULL`) and held fixed. `map_convention` sets whether module-exit
flow is included, and the conventions can rank nodes differently.

``` r

centrality_map_equation(student_interactions)
```

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

### Brokerage Coordinator Centrality

Coordinator brokerage occurs when a node mediates between two nodes in
its own group. It counts open directed two-paths \$a \\to v \\to c\$
(with no direct \$a \\to c\$).

``` math
\mathrm{Coord}(v) = |\{\, a \to v \to c : g(a) = g(v) = g(c) \,\}|
```

**Meaning.** A high value indicates within-group mediation under the
supplied membership vector.

``` r

centrality_brokerage_coordinator(student_interactions, membership = groups)
```

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

### Brokerage Liaison Centrality

Liaison brokerage occurs when a node mediates between two groups, both
different from its own.

``` math
\mathrm{Liai}(v) = |\{\, a \to v \to c : g(a),\, g(v),\, g(c) \text{ all distinct} \,\}|
```

**Meaning.** A high value indicates between-group brokerage outside the
node’s own group.

``` r

centrality_brokerage_liaison(student_interactions, membership = groups)
```

### Modularity Vitality

Modularity vitality is the drop in Newman modularity when a node is
deleted and the remaining nodes keep their communities.

``` math
V_Q(v) = Q(G, C) - Q(G - v,\; C \setminus \{v\})
```

**Meaning.** A positive value indicates a community hub; a negative
value indicates a bridge whose removal sharpens the partition. Weighted
and directed graphs use the corresponding modularity.

``` r

centrality_modularity_vitality(student_interactions, membership = groups)
```

### Community Hub-Bridge

Community hub-bridge scores nodes that are hubs inside their community
and bridges between communities.

``` math
CHB(v) = |C_v|\, k^{intra}_v + NNC_v\, k^{inter}_v
```

**Meaning.** A high value indicates a node with many intra-community
links in a large community and links into several other communities.

``` r

centrality_community_hub_bridge(student_interactions, membership = groups)
```

### Community-Based Centrality

Community-based centrality weights every link of a node by the size of
the community it lands in.

``` math
CbC(v) = \frac{1}{N} \sum_w d_{vw} S_w
```

**Meaning.** A high value indicates many links into large communities.

``` r

centrality_community_based(student_interactions, membership = groups)
```

### Comm Centrality

Comm centrality combines a node’s scaled intra-community degree with the
square of its scaled inter-community degree, weighted by how
outward-looking its community is.

``` math
CC(v) = (1 + \mu_C)\,\frac{k^{in}_v}{\max_{u \in C} k^{in}_u} R + (1 - \mu_C)\left(\frac{k^{out}_v}{\max_{u \in C} k^{out}_u} R\right)^2
```

**Meaning.** A high value indicates a node that is central within its
community and well linked outside it. $`R`$ defaults to the community’s
maximum intra-degree.

``` r

centrality_comm_centrality(student_interactions, membership = groups)
```

### Community-Based Mediator

The community-based mediator score is the entropy of a node’s link
distribution over communities times its share of total degree.

``` math
CbM(v) = H_v \frac{d_v}{\sum_u d_u}, \quad H_v = -\sum_k p_{vk} \log_2 p_{vk}
```

**Meaning.** A high value indicates a well-connected node whose links
are spread over several communities; nodes linked to one community score
0.

``` r

centrality_community_mediator(student_interactions, membership = groups)
```

## Argument catalogue

All measures are reachable through the single
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
verb. These arguments control how they are computed. Measure-specific
knobs are repeated in each measure’s **Arguments** line.

| Argument | Default | Affects | What it does |
|----|----|----|----|
| `type` | `"basic"` | all | Curated tier: `"basic"`, `"extended"`, or `"all"` (ordinary-cost measures). |
| `include` | `NULL` | all | Add named measures, or `"costly"` for every costly measure. |
| `map_flow` | `"unrecorded"` | Map equation | Unrecorded link teleportation, or `"recorded"` uniform node teleportation. |
| `map_convention` | `"paper"` | Map equation | Include module exits as in eq11, or `"infomap"` to reproduce its implementation and Table1. |
| `ninl_order` | `3` | NINL | Nonnegative iteration count; zero returns initial degree volume. |
| `sr_prior` | `0` | SpectralRank | Nonnegative scalar or node vector; zero gives ordinary SR and nonzero values give diagonal-prior WSR. Ground prior stays zero. |
| `ninl_radius` | `NULL` | NINL | Automatic ceiling of mean path length, or explicit nonnegative integer/`Inf`. Disconnected automatic radius is infinite; only reachable nodes contribute. |
| `beta_direction` | `"positive"` | BG-index / beta power | Positive credits predecessors; negative reverses the graph and credits destinations. |
| `mdd_lambda` | `0.7` | MDD | Weight assigned to exhausted degree, between 0 and 1. |
| `volume_radius` | `2` | Volume | Closed hop neighbourhood; nonnegative integer or `Inf`. |
| `diffusion_q` | `1` | Finite-horizon diffusion | Walk multiplier in \[0, 1\]. |
| `diffusion_steps` | `3` | Finite-horizon diffusion | Nonnegative integer horizon; independent of the TNA method. |
| `ds_beta` | `0.1` | Dynamics-sensitive | Spreading rate between zero and one. |
| `ds_mu` | `1` | Dynamics-sensitive | Recovery rate between zero and one; zero selects SI. |
| `ds_steps` | `5` | Dynamics-sensitive | Nonnegative integer time horizon. |
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
