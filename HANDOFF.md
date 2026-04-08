# Session Handoff — 2026-04-08

## Completed

### Batch 4 — Directed Prestige Family (Wasserman-Faust / sna)

Added 2 classical directed-graph prestige measures from the `sna::prestige` family, bringing the total to **82 measures**. Both implemented natively (no sna runtime dependency) and matched bit-exact against sna where sna's formula is well-defined:

| Measure | Reference | Status |
|---|---|---|
| `centrality_prestige_domain()` | `sna::prestige(cmode = "domain")` | **BIT-EXACT** (12 random directed graphs) |
| `centrality_prestige_domain_proximity()` | `sna::prestige(cmode = "domain.proximity")` | **BIT-EXACT** on strongly connected graphs; diverges (correctly) from sna's `FALSE * Inf = NaN` bug on graphs with unreachable pairs |

**Uncovered an sna bug** during validation: `prestige(cmode = "domain.proximity")` zeros out every node when any pair is unreachable because `(counts > 0) * gdist` produces `NaN` in the denominator (`FALSE * Inf = NaN` in IEEE 754). cograph uses `is.finite()` masking before summing and produces the mathematically correct values. Test suite covers both bit-exact match on sc graphs AND the divergence test on disconnected graphs.

Committed as 2 per-measure commits (`2aee770`, `e665386`) + doc refresh.

### Batch 3 — 5 Classical Measures with Bit-Exact Reference Validation

Added 5 node centrality measures + 1 graph-level hierarchy statistic, bringing the total to 80 measures. Every Batch 3 measure matches its primary reference implementation *bit-exact* (via `expect_identical`) — implementations mirror the references' exact LAPACK call sequences so no floating-point rounding difference remains:

| Measure | Reference | Status |
|---|---|---|
| `centrality_katz()` | `centiserve::katzcent` | **BIT-EXACT** (12 random graphs) |
| `centrality_hubbell()` | `centiserve::hubbell` (with explicit `weights`) | **BIT-EXACT** (8 random graphs) |
| `centrality_information()` | `sna::infocent` | **BIT-EXACT** (12 connected random graphs) |
| `centrality_pairwisedis()` | `centiserve::pairwisedis` | **BIT-EXACT** (12 random directed graphs) |
| `centrality_reaching_local()` (undirected) | `igraph::harmonic_centrality(normalized=TRUE)` | **BIT-EXACT** (8 random graphs) |
| `centrality_reaching_local()` (directed) | `nx.local_reaching_centrality` | **BIT-EXACT** (integer counts) |
| `reaching_global()` | `nx.global_reaching_centrality` | Machine-epsilon (karate) |

NetworkX cross-language comparisons are at 1–2 ULPs (unavoidable across R↔Python LAPACK builds). Tests: `tests/testthat/test-centrality-batch3.R` (85 tests, all passing).

Committed as 5 per-measure commits (`bf3a929`, `736a593`, `c14733c`, `4cf8459`, `583af2d`) + doc refresh.

### 75 Centrality Measures with Full Validation (Prior Batch)

Implemented 75 node centrality measures callable via `centrality(g)` — tidy data frame output, mode suffixes, consistent parameters, 40+ convenience wrappers.

**Reference: [Zoo of Centralities](https://centralityzoo.github.io/comparison/) catalogs 409 measures. Batch 1+2+3+4 cover 82 (~20%), including all foundational ones plus recent high-impact measures.**

### Validation Summary (4,000+ graph comparisons)

| Type | Count | Packages |
|------|------:|----------|
| Exact match (100 graphs) | 33 | centiserve (21), sna (3), brainGraph (3), influenceR (1), igraph (1), tidygraph (4) |
| Exact match (NetworkX) | 6 | onion, trophic_level, current_flow_betweenness, percolation, laplacian, voterank |
| Formula verified (100 graphs) | 10 | gravity, collective_influence, harary, wiener, expected, bridging, local_bridging, etc. |
| Rank correlation | 5 | flow_betweenness (r=0.85 vs sna), nonbacktracking (r=0.93 vs eigenvector), second_order (r=0.69 vs NetworkX), dmnc, salsa |
| Biological validation | 2 | LAC + DMNC on BioGRID yeast PPI (replicated Li et al. 2011 Fig. 2 pattern) |

### Measures by Category

**Core (16):** degree, strength, closeness, betweenness, eigenvector, pagerank, authority, hub, eccentricity, coreness, constraint, transitivity, harmonic, alpha, power, subgraph

**Native (9):** diffusion, leverage, kreach, laplacian, load, current_flow_closeness, current_flow_betweenness, voterank, percolation

**Distance-based (11):** radiality, lin, decay, residual_closeness, dangalchev, generalized_closeness, harary, average_distance, barycenter, wiener, closeness_vitality

**Spectral/walk (3):** communicability, communicability_betweenness, random_walk

**Path-based (2):** stress, flow_betweenness

**Local/neighborhood (16):** lobby, entropy, semilocal, clusterrank, bottleneck, centroid, mnc, dmnc (epsilon configurable), lac, topological_coefficient, bridging, local_bridging, effective_size, diversity, cross_clique, markov

**Influence (3):** integration, expected, gilschmidt

**Directed-only (3):** salsa, leaderrank, trophic_level

**Community-aware (3):** participation, within_module_z, gateway

**Zoo batch 2 (9):** onion, gravity, collective_influence, local_hindex, infection, nonbacktracking, second_order, spanning_tree, hindex_strength

### Implementation Fixes Applied
- diversity: normalized Shannon entropy (log2/log2(k)) matching igraph
- leaderrank: pure random walk power iteration (not PageRank damping)
- topological_coefficient: compute for degree-1 nodes (centiserve-compatible)
- within_module_z: NaN for sigma=0 (brainGraph convention)
- dmnc: epsilon parameter (default 1.7 per Lin et al. 2008)
- network_local_efficiency: igraph::average_local_efficiency (Gemini audit)
- erdos.renyi.game → sample_gnm/sample_gnp (Gemini audit)

### External Bugs Found
- centiserve::centroid() — stale loop variable
- centiserve::salsa() — returns eigenvalues not eigenvectors
- centiserve::dmnc() — local→global vertex ID mapping error (values 15× theoretical max)
- centiserve::closeness.vitality() — errors on graphs with bridges

### Also Completed (prior work in same branch)
- Tier 2 features: rich_club, core_periphery, vulnerability, bipartite, paths, edge-metrics, fit-distribution, assortativity, plot-distributions, plot-temporal
- Gemini audit integration: local efficiency fix, deprecation cleanup, NetworkX tests

## Files Modified
- `R/centrality.R` — 75-measure dispatcher, 40+ wrappers
- `R/centrality-extended.R` — all native implementations
- `R/network-summary.R` — local efficiency fix, deprecation cleanup
- `tests/testthat/test-centrality-extended.R` — 203 tests
- `tests/testthat/test-centrality-equivalence-report.R` — 59 tests
- `tests/testthat/test-centrality-zoo.R` — 37 tests
- 12 other R source files, 42+ man pages

## Next Steps
- More Zoo measures: Shapley value, DomiRank, ViralRank, community centrality
- Infection number performance optimization (SAW enumeration is O(exp) for large L)
- CRAN readiness check for new functions
- Remaining Tier 1: link prediction, structural holes, network comparison, graph generators

## Context
- R 4.5+, macOS
- Reference packages: centiserve, sna, influenceR, brainGraph, igraph, tidygraph, NetworkX (via reticulate)
- BioGRID yeast PPI at /tmp/ (SGD phenotype data for essentiality labels)
- Zoo of Centralities: https://centralityzoo.github.io/list/ (409 measures cataloged)
