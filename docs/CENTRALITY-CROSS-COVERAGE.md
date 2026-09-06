# Cross-package coverage of the Centrality Zoo

Coverage of the 349 node measures catalogued in the
[Centrality Zoo](https://centralityzoo.github.io/) (Shvydun 2025) by
cograph and by nine other centrality packages. Counting is by measure,
not by function: two functions for the same measure count once, and a
function whose measure the Zoo does not catalogue is not counted.

| Package | Centrality functions counted | Zoo measures reachable |
|---|---|---|
| **cograph** | 136 | **110** |
| CINNA | 49 | 37 |
| centiserve | 33 | 28 |
| networkx | 30 | 24 |
| tidygraph | 25 | 20 |
| igraph | 18 | 14 |
| sna | 11 | 8 |
| influenceR | 4 | 6 |
| brainGraph | 7 | 4 |
| keyplayer | 4 | 3 |

The nine other packages together reach 60 distinct Zoo
measures. cograph reaches 110; 50 of those are in no other package.

## Counts

### The Zoo's 349 node measures

`tau` is the largest average Kendall rank correlation between the Zoo
measure and any measure cograph implements, read from the Zoo's own
matrix over its 648 networks.

| Status | Count |
|---|---|
| Implemented in cograph | 110 |
| Not implemented, nearest tau >= 0.99 | 20 |
| Not implemented, nearest 0.90 <= tau < 0.99 | 148 |
| Not implemented, nearest 0.70 <= tau < 0.90 | 44 |
| Not implemented, nearest tau < 0.70 | 27 |
| **Total** | **349** |

### The 181 functions counted in the other packages

| Package | In `centrality()` | Elsewhere in cograph | Not in cograph | Total |
|---|---|---|---|---|
| CINNA | 47 | 1 | 1 | 49 |
| centiserve | 32 | 0 | 1 | 33 |
| networkx | 26 | 4 | 0 | 30 |
| tidygraph | 25 | 0 | 0 | 25 |
| igraph | 18 | 0 | 0 | 18 |
| sna | 11 | 0 | 0 | 11 |
| brainGraph | 6 | 1 | 0 | 7 |
| influenceR | 4 | 0 | 0 | 4 |
| keyplayer | 3 | 0 | 1 | 4 |
| **Total** | **172** | **6** | **3** | **181** |

"Elsewhere in cograph" means the measure exists as a separate verb
because it is not node-level: a set, a pair or the graph as a whole.

### cograph's 136 measures, by nearest other measure

Average Kendall tau between every pair of cograph measures on 19 real
undirected networks (`docs/zoo/near_duplicates.R`). Each measure is
placed by its single closest partner.

| Nearest partner at | Count |
|---|---|
| tau >= 0.99 | 40 |
| 0.95 <= tau < 0.99 | 17 |
| 0.90 <= tau < 0.95 | 17 |
| 0.70 <= tau < 0.90 | 34 |
| tau < 0.70 | 16 |
| Not in the matrix (directed-only, `NA` here) | 12 |
| **Total** | **136** |

## Measures in cograph and in no other package

| Zoo measure | cograph measure |
|---|---|
| Access information | `access_information` |
| CollInf | `collective_influence` |
| Comm Centrality | `comm_centrality` |
| Community Hub‑Bridge measure | `community_hub_bridge` |
| Community-based centrality (CbC) | `community_based` |
| Community-based mediator (CbM) | `community_mediator` |
| DegreeDiscountIC | `degree_discount` |
| delta-betweenness | `delta_betweenness` |
| delta-closeness | `delta_closeness` |
| Distance entropy | `distance_entropy` |
| Egocentric betweenness | `ego_betweenness` |
| EnRenew | `enrenew` |
| Entropy variation (betweenness) | `entropy_variation_betweenness` |
| Entropy variation (degree) | `entropy_variation_degree` |
| Flow coefficient | `flow_coefficient` |
| Fuzzy local dimension (FLD) | `fuzzy_local_dimension` |
| Gravity centrality | `gravity` |
| Gravity model | `gravity` |
| h-index strength | `hindex_strength` |
| Heatmap centrality | `heatmap` |
| Hide information | `hide_information` |
| Improved IMC | `node_contraction_improved` |
| Infection number | `infection` |
| Length-scaled betweenness | `length_scaled_betweenness` |
| Local dimension (LD) | `local_dimension_fixed` |
| Local dimension (Pu) | `local_dimension` |
| Local entropy (LE) | `local_entropy` |
| Local gravity model | `gravity` |
| Local H-index | `local_hindex` |
| Local information dimensionality (LID) | `local_information_dimension` |
| Local volume dimension (LVD) | `local_volume_dimension` |
| Modularity vitality | `modularity_vitality` |
| NCVoteRank | `ncvoterank` |
| Node contraction (IMC) | `node_contraction` |
| Non-backtracking centrality | `nonbacktracking` |
| Participation coefficient | `participation` |
| Redundancy | `redundancy` |
| Renewed coreness | `renewed_coreness` |
| Rumor centrality | `rumor` |
| s-shell index | `s_shell` |
| Shapley value (game 1) | `shapley_game1` |
| Shapley value (game 2) | `shapley_game2` |
| Shapley value (game 3) | `shapley_game3` |
| SingleDiscount | `single_discount` |
| Spanning tree centrality (STC) | `spanning_tree` |
| Two-way random walk betweenness (2RW) | `two_way_rw` |
| VoteRank++ | `voterank_plus` |
| Weighted h-index | `weighted_h_index` |
| Weighted k-shell decomposition (Wks) | `weighted_kshell` |
| WVoteRank | `wvoterank` |

## Measures in another package and not in `centrality()`

Some of these cograph provides as a separate verb rather than a column
of `centrality()`, because the measure is not node-level.

| Package | Function | Note |
|---|---|---|
| centiserve | `communitycent` | link-community centrality (Kalinka & Tomancak 2011); not implemented, its reference package linkcomm is archived so no equivalence check is possible |
| CINNA | `Community Centrality` | link-community centrality; not implemented, see centiserve communitycent |
| CINNA | `Group Centrality` | cograph has group_centrality() as a set-level verb |
| networkx | `dispersion` | cograph has dispersion() as a pair-level verb |
| networkx | `estrada_index` | cograph has estrada_index() as a graph-level verb |
| networkx | `group_betweenness_centrality` | cograph has group_centrality() as a set-level verb |
| networkx | `global_reaching_centrality` | cograph has reaching_global() as a graph-level verb |
| brainGraph | `vulnerability` | cograph has vulnerability() as a separate verb |
| keyplayer | `kpset` | greedy key-player set selection; cograph evaluates a given set with group_centrality() but does not search for one |

## Measures added to `centrality()` from other packages

Five node measures available in other packages and not in
`centrality()`, each implemented from its source paper and verified
against the package that provides it
(`local_testing_and_equivalence/batch10/run_equivalence.R`).

| Package | Function | cograph measure | Checked against |
|---|---|---|---|
| brainGraph | `efficiency(type = "local")` | `local_efficiency` | brainGraph and networkx, 25 random graphs |
| brainGraph | `s_core` | `s_core` | `igraph::coreness()` unweighted, brute force weighted |
| keyplayer | `fragment` | `fragmentation` | `keyplayer::fragment()` on 25 random graphs |
| sna | `kpath.census` | `kpath` | `sna::kpath.census()`, k = 2 and 3, directed and undirected |
| centiserve, CINNA | `epc` | `epc` | exact bond-percolation mean, and `centiserve::epc()` up to its run count |

Two of the five differ from a reference implementation, and both help
pages state the difference. `s_core` reports the strength threshold of
Eidsaa & Almaas; `brainGraph::s_core()` reports the peeling round.
`local_efficiency` measures the neighbour distances inside the induced
subgraph; `igraph::local_efficiency()` measures them through the rest of
the network.

## Measures cograph provides under another name

Three functions in other packages compute a measure `centrality()`
provides under a different name. Each equivalence is verified by a test
in `tests/testthat/test-centrality-batch10.R`.

| Package | Function | cograph measure | Relationship |
|---|---|---|---|
| centiserve | `closeness.latora` | `harmonic` | identical |
| centiserve | `communibet` | `communicability_betweenness` | identical (both are Estrada, Higham & Hatano 2009 normalised) |
| brainGraph | `efficiency(type = "nodal")` | `harmonic` | identical after dividing by *n* - 1 |

## Rank correlation between cograph measures

Kendall tau between every pair of measures, computed on 19 real
undirected networks and averaged over them (`docs/zoo/near_duplicates.R`).
The first table gives, for each of the five measures added from other
packages, the closest measure already in `centrality()`.

| Added measure | Closest measure already present | tau |
|---|---|---|
| `local_efficiency` | `transitivity` | 0.92 |
| `s_core` | `coreness` | 1.00 |
| `fragmentation` | `dangalchev` | 0.94 |
| `kpath` | `laplacian` | 0.97 |
| `epc` | `degree` | 0.88 |


These networks are unweighted. With unit weights `s_core` equals the
k-core number by construction, as does `weighted_kshell`, so the 1.00
against `coreness` is a property of the definition. Both differ from
`coreness` only on weighted input.

Across the whole catalogue 52 pairs of measures reach |tau| >= 0.99 on
these networks. The 40 closest:

| Measure | Measure | tau |
|---|---|---|
| `average_distance` | `barycenter` | -1.000 |
| `average_distance` | `closeness` | -1.000 |
| `barycenter` | `closeness` | 1.000 |
| `dangalchev` | `decay` | 1.000 |
| `degree` | `expected_influence_1` | 1.000 |
| `dangalchev` | `generalized_closeness` | 1.000 |
| `decay` | `generalized_closeness` | 1.000 |
| `delta_closeness` | `gilschmidt` | 1.000 |
| `average_distance` | `lin` | -1.000 |
| `barycenter` | `lin` | 1.000 |
| `closeness` | `lin` | 1.000 |
| `hindex_strength` | `lobby` | 1.000 |
| `coreness` | `local_hindex` | 1.000 |
| `average_distance` | `radiality` | -1.000 |
| `barycenter` | `radiality` | 1.000 |
| `closeness` | `radiality` | 1.000 |
| `lin` | `radiality` | 1.000 |
| `harmonic` | `reaching_local` | 1.000 |
| `lac` | `redundancy` | 1.000 |
| `dangalchev` | `residual_closeness` | 1.000 |
| `decay` | `residual_closeness` | 1.000 |
| `generalized_closeness` | `residual_closeness` | 1.000 |
| `coreness` | `s_core` | 1.000 |
| `local_hindex` | `s_core` | 1.000 |
| `degree` | `strength` | 1.000 |
| `expected_influence_1` | `strength` | 1.000 |
| `coreness` | `weighted_kshell` | 1.000 |
| `local_hindex` | `weighted_kshell` | 1.000 |
| `s_core` | `weighted_kshell` | 1.000 |
| `average_distance` | `wiener` | 1.000 |
| `barycenter` | `wiener` | -1.000 |
| `closeness` | `wiener` | -1.000 |
| `lin` | `wiener` | -1.000 |
| `radiality` | `wiener` | -1.000 |
| `betweenness` | `percolation` | 1.000 |
| `flow_coefficient` | `transitivity` | -0.999 |
| `delta_closeness` | `harmonic` | 0.998 |
| `gilschmidt` | `harmonic` | 0.998 |
| `delta_closeness` | `reaching_local` | 0.998 |
| `gilschmidt` | `reaching_local` | 0.998 |

The remaining 12 are in `docs/zoo/near_duplicates.csv`, which also carries every weaker pair.

### Distinct measures after collapsing near-duplicates

Chains of near-duplicates collapsed into single-linkage groups, and
the groups counted. Of the 136 measures, 124 are in this matrix; the
other 12 are directed-only and return `NA` on these undirected networks.

| Twins at | Distinct groups | Measures with no twin | Largest group |
|---|---|---|---|
| tau >= 0.99 | **96** | 84 | 6 |
| tau >= 0.95 | **80** | 67 | 17 |
| tau >= 0.90 | **60** | 50 | 48 |
| tau >= 0.80 | **34** | 28 | 84 |

At the 0.95 cut the 124 measures fall into 80 groups: 67 of one member and 13 of more
than one. The membership is listed below and in `docs/zoo/measure_groups.csv`.

### Groups of two or more at |tau| >= 0.95

| Size | Measures | What the members share |
|---|---|---|
| 17 | `average_distance`, `barycenter`, `closeness`, `dangalchev`, `decay`, `delta_closeness`, `generalized_closeness`, `gilschmidt`, `harary`, `harmonic`, `integration`, `lin`, `node_contraction_improved`, `radiality`, `reaching_local`, `residual_closeness`, `wiener` | sums of shortest-path distances, or their reciprocals |
| 6 | `authority`, `communicability`, `eigenvector`, `hub`, `nonbacktracking`, `subgraph` | the leading eigenvector of the adjacency matrix, or a function of its spectrum |
| 5 | `betweenness`, `delta_betweenness`, `length_scaled_betweenness`, `load`, `percolation` | shortest paths passing through the node |
| 5 | `current_flow_closeness`, `information`, `markov`, `random_walk`, `spanning_tree` | random walks, effective resistance and the Laplacian spectrum |
| 4 | `coreness`, `local_hindex`, `s_core`, `weighted_kshell` | peeling by degree or strength |
| 4 | `diffusion`, `expected`, `expected_influence_2`, `semilocal` | quantities built from the two- or three-step neighbourhood |
| 3 | `degree`, `expected_influence_1`, `strength` | the count or sum of adjacent edges |
| 3 | `katz`, `kpath`, `laplacian` | counts of short walks or paths, and so of degree and neighbour degree |
| 2 | `alpha`, `power` | solutions of (I - alpha A) x = b |
| 2 | `flow_coefficient`, `transitivity` | linkage among the node's neighbours; on an undirected graph one is 1 minus the other, hence the negative tau |
| 2 | `hindex_strength`, `lobby` | an h-index over neighbour degrees |
| 2 | `lac`, `redundancy` | mean degree inside the ego network |
| 2 | `local_bridging`, `pagerank` | no shared construction; the agreement is empirical |

### Measures with no partner at |tau| >= 0.95

Each measure with its single closest partner and that tau. A negative
tau is the same ranking reversed, so the grouping uses |tau|:
`average_distance` and `closeness` sit at -1.00 and form one group.

| Measure | Closest other measure | tau |
|---|---|---|
| `access_information` | `diversity` | 0.32 |
| `bottleneck` | `stress` | 0.70 |
| `bridging` | `flow_coefficient` | 0.72 |
| `centroid` | `dangalchev` | 0.92 |
| `closeness_vitality` | `average_distance` | 0.88 |
| `clusterrank` | `dmnc` | 0.65 |
| `collective_influence` | `node_contraction` | 0.74 |
| `comm_centrality` | `degree` | 0.80 |
| `communicability_betweenness` | `laplacian` | 0.93 |
| `community_based` | `degree` | 0.92 |
| `community_hub_bridge` | `community_based` | 0.77 |
| `community_mediator` | `centroid` | 0.80 |
| `constraint` | `hide_information` | 0.82 |
| `cross_clique` | `mnc` | 0.86 |
| `current_flow_betweenness` | `delta_betweenness` | 0.87 |
| `degree_discount` | `single_discount` | 0.39 |
| `distance_entropy` | `entropy_variation_betweenness` | 0.52 |
| `diversity` | `coreness` | 0.51 |
| `dmnc` | `clusterrank` | 0.65 |
| `eccentricity` | `kreach` | -0.85 |
| `effective_size` | `ego_betweenness` | 0.91 |
| `ego_betweenness` | `effective_size` | 0.91 |
| `enrenew` | `leverage` | 0.79 |
| `entropy` | `closeness_vitality` | -0.44 |
| `entropy_variation_betweenness` | `entropy_variation_degree` | 0.65 |
| `entropy_variation_degree` | `leverage` | 0.69 |
| `epc` | `degree` | 0.88 |
| `flow_betweenness` | `local_bridging` | -0.82 |
| `fragmentation` | `dangalchev` | 0.94 |
| `fuzzy_local_dimension` | `local_dimension` | -0.86 |
| `gateway` | `participation` | 0.86 |
| `geodesic_kpath` | `diversity` | 0.42 |
| `gravity` | `katz` | 0.93 |
| `heatmap` | `node_contraction` | -0.85 |
| `hide_information` | `spanning_tree` | -0.86 |
| `infection` | `nonbacktracking` | 0.83 |
| `kreach` | `eccentricity` | -0.85 |
| `leverage` | `shapley_game1` | 0.93 |
| `local_dimension` | `fuzzy_local_dimension` | -0.86 |
| `local_dimension_fixed` | `degree` | -0.80 |
| `local_efficiency` | `transitivity` | 0.92 |
| `local_entropy` | `semilocal` | -0.90 |
| `local_information_dimension` | `fuzzy_local_dimension` | 0.70 |
| `local_volume_dimension` | `diffusion` | -0.85 |
| `mnc` | `degree` | 0.92 |
| `modularity_vitality` | `participation` | -0.53 |
| `ncvoterank` | `voterank` | 0.82 |
| `neighborhood_connectivity` | `entropy_variation_degree` | -0.68 |
| `node_contraction` | `pagerank` | 0.91 |
| `onion` | `hindex_strength` | 0.91 |
| `participation` | `gateway` | 0.86 |
| `renewed_coreness` | `coreness` | 0.84 |
| `rumor` | `kreach` | 0.58 |
| `s_shell` | `coreness` | 0.84 |
| `second_order` | `local_entropy` | 0.57 |
| `shapley_game1` | `leverage` | 0.93 |
| `shapley_game2` | `shapley_game1` | 0.79 |
| `shapley_game3` | `eccentricity` | -0.77 |
| `single_discount` | `voterank` | 0.79 |
| `stress` | `betweenness` | 0.90 |
| `topological_coefficient` | `heatmap` | 0.81 |
| `two_way_rw` | `degree` | 0.74 |
| `voterank` | `wvoterank` | 0.93 |
| `voterank_plus` | `wvoterank` | 0.79 |
| `weighted_h_index` | `expected` | 0.92 |
| `within_module_z` | `leverage` | 0.67 |
| `wvoterank` | `voterank` | 0.93 |

### Measures absent from the matrix

Directed-only measures, which return `NA` on the undirected networks
the tau run uses, so they have no measured partner:

`brokerage_coordinator`, `brokerage_gatekeeper`, `brokerage_itinerant`, `brokerage_liaison`, `brokerage_representative`, `hubbell`, `leaderrank`, `pairwisedis`, `prestige_domain`, `prestige_domain_proximity`, `salsa`, `trophic_level`.

## Method

- Package function to cograph measure is mapped by name in
  `docs/zoo/package_measures.csv`. The mapping is checked at build time:
  every cograph name in it must be one `list_centralities()` reports.
- Reachability is not equivalence: two packages can implement the same
  measure and return different values. Documented cases include
  `?centrality_dmnc` against centiserve and the NetworkX divergence in
  `?group_centrality`.
- Some packages reach a measure through a wrapper rather than their own
  code. CINNA dispatches to igraph and centiserve, so its count overlaps
  theirs almost entirely.
- The Zoo lists a few measures twice under different names, effective
  size among them, so a package can reach more Zoo labels than it has
  functions. The convention applies to every row, cograph's included.
- The "what the members share" column is read off the measures' own
  definitions. It is the only column in this document that is not a
  computed value.
- The grouping is single-linkage: a group is a chain of near-duplicates,
  not a set in which every pair is close. Two measures at opposite ends
  of the 16-member distance group can correlate well below 0.95 with
  each other.
- Every measure is evaluated at its default arguments, which matters
  for the tunable ones. `delta_closeness` at its default exponent of 1
  is harmonic centrality over n - 1, so it sits at tau 1.00 with the
  distance group; it separates from that group only at other
  exponents. The same holds for `delta_betweenness` at delta = 0 and
  for `gravity` under a different mass or radius.
- Two tau sources appear in this document. The Zoo bands come from the
  Zoo's matrix over its 648 networks and its implementations; the
  cograph-to-cograph tau comes from `docs/zoo/near_duplicates.R`, over
  19 networks and cograph's implementations.
- Measures a package implements that the Zoo does not catalogue are not
  counted. This is a count of Zoo coverage, not of package size.
- Generated by `docs/zoo/cross_coverage.R`.
