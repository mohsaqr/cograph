# cograph Validation Suite

Comprehensive numerical equivalence testing for the cograph R package. Every measure is validated against an **external reference implementation** (igraph, centiserve, sna), not self-referential tests.

## Quick Start

```bash
# Run all tests with 1000 networks (default seed=42)
Rscript validation/run_comprehensive_tests.R 1000

# Custom network count and seed
Rscript validation/run_comprehensive_tests.R 500 123

# Run individual suites
Rscript validation/test_centrality_comprehensive.R 1000 42
Rscript validation/test_communities_comprehensive.R 1000 42
Rscript validation/test_network_properties_comprehensive.R 166 42
```

## Folder Structure

```
validation/
  README.md                                  # This file
  run_comprehensive_tests.R                  # Master runner (all 3 suites + reports)
  test_centrality_comprehensive.R            # Centrality measures validation
  test_communities_comprehensive.R           # Community detection validation
  test_network_properties_comprehensive.R    # Network metrics validation
  generate_html_reports.R                    # HTML report generator (no pandoc)
  reports/                                   # Generated HTML reports
    centrality_validation.html
    communities_validation.html
    network_properties_validation.html
    combined_validation.html                 # Dashboard with all 3
  results_centrality_comprehensive.rds       # Raw results (R objects)
  results_centrality_comprehensive.txt       # Console output log
  results_communities_comprehensive.rds
  results_communities_comprehensive.txt
  results_network_properties_comprehensive.rds
  results_network_properties_comprehensive.txt
```

Older validation scripts (retained for reference):
```
  test_centrality_igraph.R                   # Original centrality vs igraph
  test_centiserve_centralities.R             # Original centiserve validation
  test_diffusion_centrality.R                # Original diffusion validation
  test_network_metrics.R                     # Original network metrics
  test_network_metrics_numerical.R           # Original numerical metrics
  test_communities.R                         # Original community detection
  test_scale_nodes_by.R                      # scale_nodes_by parameter
```

## Comprehensive Test Suites

### 1. Centrality Measures (`test_centrality_comprehensive.R`)

Tests 25+ centrality measures across N random networks (4 types: undirected/directed x weighted/unweighted, plus networks with self-loops).

| Measure | Validated Against | Notes |
|---------|------------------|-------|
| degree (all/in/out) | `igraph::degree()` | Exact match |
| strength (all/in/out) | `igraph::strength()` | Exact match |
| betweenness | `igraph::betweenness()` | + cutoff, noloops, unweighted variants |
| closeness (all/in/out) | `igraph::closeness()` | + normalized, cutoff variants |
| eigenvector | `igraph::eigen_centrality()` | Exact match |
| pagerank | `igraph::page_rank()` | damping=0.5/0.85/0.95, personalized |
| authority | `igraph::authority_score()` | Exact match |
| hub | `igraph::hub_score()` | Exact match |
| eccentricity (all/in/out) | `igraph::eccentricity()` | Exact match |
| coreness (all/in/out) | `igraph::coreness()` | Exact match |
| constraint | `igraph::constraint()` | + unweighted variant |
| transitivity | `igraph::transitivity()` | local, global, barrat types |
| harmonic (all/in/out) | `igraph::harmonic_centrality()` | + normalized, cutoff variants |
| alpha | `igraph::alpha_centrality()` | Exact match |
| power | `igraph::power_centrality()` | Exact match |
| subgraph | `igraph::subgraph_centrality()` | Exact match |
| diffusion | `centiserve::diffusion.degree()` | lambda=0.5/1.0/2.5, mode=in/out |
| leverage (all/in/out) | `centiserve::leverage()` | Exact match |
| kreach | `centiserve::geokpath()` | k=1/2/5 |
| laplacian | `centiserve::laplacian()` | Undirected, ≤20 nodes |
| current_flow_closeness | `centiserve::closeness.currentflow()` | Undirected, connected, ≤15 nodes |
| current_flow_betweenness | Structural validation | Bounds check (different algorithm from communibet) |
| load | `sna::loadcent()` | Hop-based shortest paths, ≤20 nodes |
| percolation | Math identity: `bet/((n-1)(n-2))` | Piraveenan et al. 2013 uniform-state identity |
| voterank | Structural validation | Score bounds [0,1], ordering check |
| edge_betweenness | `igraph::edge_betweenness()` | Exact match |

### 2. Community Detection (`test_communities_comprehensive.R`)

Tests all 12 community detection algorithms.

| Algorithm | Type | Validated Against | Parameters Tested |
|-----------|------|------------------|-------------------|
| fast_greedy | deterministic | `igraph::cluster_fast_greedy()` | membership + modularity |
| walktrap | deterministic | `igraph::cluster_walktrap()` | steps=2/4/8 |
| edge_betweenness | deterministic | `igraph::cluster_edge_betweenness()` | membership + modularity |
| leading_eigenvector | deterministic | `igraph::cluster_leading_eigen()` | membership + modularity |
| louvain | stochastic | `igraph::cluster_louvain()` | resolution=0.5/1.0/2.0 |
| leiden | stochastic | `igraph::cluster_leiden()` | CPM/modularity objectives |
| infomap | stochastic | `igraph::cluster_infomap()` | nb.trials=1/10 |
| label_propagation | stochastic | `igraph::cluster_label_prop()` | modularity check |
| spinglass | stochastic | `igraph::cluster_spinglass()` | spins=10/25/50 |
| optimal | deterministic | `igraph::cluster_optimal()` | small networks only (≤20 nodes) |
| fluid | stochastic | modularity + count validation | no.of.communities=2/3/5 |
| consensus | stochastic | modularity + count validation | n_runs=20/50, threshold=0.3/0.5 |

Also tests: `compare_communities()` (NMI, VI, Rand, adjusted Rand, split.join), helper functions (n_communities, community_sizes, membership, modularity), directed networks, weighted networks.

### 3. Network Properties (`test_network_properties_comprehensive.R`)

Tests network-level metrics on known graphs and simulated networks.

| Category | Metrics | Validated Against |
|----------|---------|------------------|
| Known graphs (K5, K10, P5, P10, C5, C8, S6, S10, Grid, Petersen) | girth, radius, connectivity, clique size, cut vertices, bridges, efficiency | Exact values + igraph |
| Simulated (6 models x N networks) | Same 7 metrics | igraph on ER, BA, WS, SBM, Regular, Geometric |
| Directed networks | radius, efficiency, girth | igraph with mode='out' |
| Weighted networks | efficiency, connectivity | igraph with weights |
| network_summary() | All columns at 4 detail levels | Individual igraph function calls |
| degree_distribution() | Frequency table | igraph::degree() |
| small_world | sigma coefficient | transitivity / mean_distance ratio |
| rich_club | phi coefficient | igraph rich_club |

## Dependencies

**Required:**
- R ≥ 4.1
- igraph
- cograph (loaded via `devtools::load_all(".")`)
- Saqrlab (for `simulate_edge_list()`)

**Optional (for full centrality validation):**
- centiserve (diffusion, leverage, kreach, laplacian, current_flow)
- sna (load centrality)
- expm (required by centiserve::communibet)

## Output

Each test script produces:
- `.rds` file: Full R object with per-test results, summary, failures, config
- `.txt` file: Console output log

The report generator reads all `.rds` files and produces self-contained HTML reports (no pandoc needed — pure string concatenation with embedded CSS).

## Approach

1. **External references only**: Every measure is compared against an established external R package, never against a self-written reference implementation.
2. **Random network diversity**: Tests use `Saqrlab::simulate_edge_list()` to generate networks across 4 types (undirected/directed x weighted/unweighted), with varying sizes (5-30 nodes) and densities.
3. **Tolerance levels**: Strict (`1e-10`) for exact algorithms, loose (`1e-6`) for numerical methods (SVD, iterative).
4. **Size guards**: Expensive measures (communibet O(n^4), closeness.currentflow O(n^3)) are capped at small network sizes to keep runtime practical.
5. **Deterministic vs stochastic**: Deterministic algorithms require exact membership match; stochastic algorithms require modularity within tolerance and community count within ±2.
6. **Known graph baselines**: Network properties are first validated against analytically known values (e.g., K5 girth=3, Star radius=1) before testing on random graphs.
