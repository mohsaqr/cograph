# Session Handoff — 2026-04-08

## Completed

### Extended Centrality: 65 measures, 76 exports, full equivalence validation

**Measures added this session (40 new, 25 pre-existing):**

Core (igraph-backed, 16): degree, strength, closeness, betweenness, eigenvector, pagerank, authority, hub, eccentricity, coreness, constraint, transitivity, harmonic, alpha, power, subgraph

Native implementations (9): diffusion, leverage, kreach, laplacian, load, current_flow_closeness, current_flow_betweenness, voterank, percolation

Distance-based closeness variants (11): radiality, lin, decay, residual_closeness, dangalchev, generalized_closeness, harary, average_distance, barycenter, wiener, closeness_vitality

Spectral/walk-based (3): communicability, communicability_betweenness, random_walk

Path-based (2): stress, flow_betweenness

Local/neighborhood (16): lobby, entropy, semilocal, clusterrank, bottleneck, centroid, mnc, dmnc, **lac**, topological_coefficient, bridging, local_bridging, effective_size, diversity, cross_clique, markov

Influence (3): integration, expected, gilschmidt

Directed-only (2): salsa, leaderrank

Community-aware (3): participation, within_module_z, gateway

**All 65 callable via `centrality(g)` — tidy data frame, mode suffixes, consistent params.**

### Implementation fixes during equivalence testing
- `diversity`: changed to normalized Shannon entropy (log2/log2(k)) matching igraph::diversity()
- `topological_coefficient`: removed early return for degree-1 nodes (matches centiserve)
- `within_module_z`: returns NaN when sigma=0 (matches brainGraph)
- `leaderrank`: pure random walk power iteration (was incorrectly using PageRank damping=0.85)
- `dmnc`: epsilon now a user parameter (default 1.7 per Lin et al. 2008; centiserve uses 1.67)
- `salsa`, `leaderrank`: return NA with warning on undirected graphs
- `participation`, `within_module_z`, `gateway`: return NA with warning when membership=NULL

### Equivalence report (3,550+ graph comparisons)

**Exact numerical match (100% × 100 graphs):**
- centiserve (21): radiality, lin, decay, residual_closeness, lobby, barycenter, bottleneck, mnc, average_distance, closeness_vitality, cross_clique, semilocal, clusterrank, entropy, markov, topological_coefficient, leverage, diffusion, laplacian, kreach, leaderrank
- sna (3): stress, gilschmidt, load
- brainGraph (3): participation, within_module_z, gateway (centr=degree)
- influenceR (1): effective_size
- igraph (1): diversity
- tidygraph (4): communicability, communicability_betweenness, integration, residual_closeness

**Rank correlation (reference has bugs or non-unique algorithm):**
- flow_betweenness vs sna: r=0.847 (max-flow decomposition is non-unique)
- dmnc vs centiserve: r=0.19 (centiserve vertex ID mapping bug — uses local subgraph indices as global IDs, produces values up to 15× theoretical max)
- salsa vs centiserve: r=-0.015 (centiserve returns `eigen(M)$values` instead of `$vectors[,1]`)

**Self-consistency verified:** dangalchev==residual_closeness, generalized_closeness==decay, communicability>=subgraph, harary/wiener/expected/bridging/local_bridging formula verification, random_walk finite+positive, comm_betweenness in [0,1]

### LAC + DMNC biological validation (BioGRID yeast PPI)

Replicated Li et al. (2011) essential protein prediction on BioGRID S. cerevisiae (6473 proteins, 181K interactions, 1169 essential from SGD):

```
Top%     LAC     DC   DMNC     BC     EC     SC
  5%     157    148    122    129    162    162
 10%     297    294    269    254    296    296
 15%     423    414    375    364    413    413
 20%     539    517    459    468    514    514
```

Paper BioGRID (2011 version): LAC=151 DC=138 DMNC=128 BC=137 EC=149 SC=59 at top 5%.
Pattern confirmed: LAC > DC > DMNC consistently across all thresholds.

Correlation with essentiality: LAC (0.372) > DC (0.356) > EC (0.348) > SC (0.348) > BC (0.324) > DMNC (0.261) > Clust (0.034)

LAC vs DMNC Spearman: r=0.73 on BioGRID (r=0.96 on igraphdata yeast). DMNC captures neighborhood cohesion (r=0.68 with clustering) orthogonal to degree — different from LAC which is degree-dominated (r=0.95 with DC).

### External bugs discovered
- centiserve::centroid() — stale variable `u` instead of `w` in min loop
- centiserve::salsa() — returns eigenvalues instead of eigenvectors
- centiserve::dmnc() — uses local subgraph indices as global vertex IDs
- centiserve::closeness.vitality() — errors on graphs with bridges

## Current State

- `main` branch, all changes uncommitted
- 65 centrality measures, 76 exports, 40 convenience wrappers
- All tests pass (test-centrality-extended: 197, test-centrality-equivalence-report: 59)
- Full test suite passes except pre-existing brainGraph API issue
- NAMESPACE updated, 42 man pages generated

## Files Modified

- `R/centrality.R` — 65-measure dispatcher, 40 convenience wrappers, dmnc_epsilon param, lac wired in
- `R/centrality-extended.R` — LAC implementation, DMNC epsilon parameter, fixed diversity/topological_coefficient/within_module_z/leaderrank/salsa
- `tests/testthat/test-centrality-extended.R` — 197 tests
- `tests/testthat/test-centrality-equivalence-report.R` — 59 tests, structured report
- `docs/CHANGES.md`, `docs/LEARNINGS.md`, `HANDOFF.md`
- 42 new man pages in `man/`

## Next Steps

- Commit all changes
- Implement infection number centrality (Bauer & Lizier 2012, self-avoiding walks)
- CRAN readiness check for new functions
- Remaining Tier 1 features: link prediction, structural holes, network comparison, graph generators

## Context

- R 4.5+, macOS
- Reference packages: centiserve, sna, influenceR, brainGraph, igraph, tidygraph (all Suggests)
- BioGRID yeast PPI data at /tmp/BIOGRID-ORGANISM-Saccharomyces_cerevisiae_S288c-5.0.256.tab3.txt
- SGD phenotype data at /tmp/sgd_phenotype.tab
