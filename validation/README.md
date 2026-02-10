# Validation Tests

This folder contains validation scripts that verify cograph functions produce identical results to their igraph equivalents.

## Centrality Validation

**Script:** `test_centrality_igraph.R`

Validates all centrality measures against direct igraph calls using randomized networks.

### Usage

```bash
# Default: 100 tests, seed=42
Rscript validation/test_centrality_igraph.R

# Custom number of tests
Rscript validation/test_centrality_igraph.R 500

# Custom tests and seed
Rscript validation/test_centrality_igraph.R 100 123
```

### Measures Tested

| Measure | Modes | Variants |
|---------|-------|----------|
| degree | all, in, out | - |
| strength | all, in, out | - |
| betweenness | - | with cutoff |
| closeness | all, in, out | normalized, with cutoff |
| eigenvector | - | - |
| pagerank | - | custom damping, personalized |
| authority | - | - |
| hub | - | - |
| eccentricity | all, in, out | - |
| coreness | all, in, out | - |
| constraint | - | - |
| transitivity | - | local, global |
| harmonic | all, in, out | - |

### Output Files

- `results_centrality_igraph.txt` - Human-readable report
- `results_centrality_igraph.rds` - Full R data for further analysis

### Latest Results

```
Timestamp: 2026-02-10
Tests: 100 networks (2530 individual comparisons)
Pass Rate: 100.00%
```

All centrality measures produce identical results to igraph (within floating-point tolerance of 1e-10).

### Re-running Tests

To re-run and update results:

```bash
Rscript validation/test_centrality_igraph.R 100 42
```

For more thorough testing:

```bash
Rscript validation/test_centrality_igraph.R 500 $(date +%s)
```

---

## Centiserve Centrality Measures Validation

**Script:** `test_centiserve_centralities.R`

Validates cograph's implementations of centrality measures:
- **leverage** - Relative degree influence over neighbors
- **kreach** - Geodesic k-path centrality (nodes within distance k)
- **resilience** - Vertex connectivity (nodes to remove before disconnection)

### Usage

```bash
Rscript validation/test_centiserve_centralities.R
```

### Tests by Measure

| Measure | Tests |
|---------|-------|
| leverage | undirected, directed (in/out/all), random, complete, star |
| kreach | various k values (1,2,3), directed modes, random networks |
| resilience | path (0s), cycle (1s), complete K4 (2s), star (0s), bridge |

### Performance

| Measure | Avg Speedup | Notes |
|---------|------------|-------|
| leverage | 3.0x | Vectorized using adjacency matrix |
| kreach | 1.0x | Already efficient (matrix-based) |
| resilience | N/A | New measure (O(n*m) complexity) |

### Latest Results

```
Timestamp: 2026-02-10
Tests: 21/21 passed (100% pass rate)
```

### Output Files

- `results_centiserve_centralities.txt` - Human-readable report

### Example Usage

```r
# Leverage centrality - who influences their neighbors?
centrality_leverage(network, mode = "all")

# K-reach - how many nodes within distance k?
centrality_kreach(network, k = 2)  # Neighbors of neighbors

# Resilience - how many nodes must be removed to disconnect?
# Higher = harder to isolate
centrality_resilience(network)
# Path graph: all 0s (single paths, easily broken)
# Cycle: all 1s (need remove 1 node to break alternative path)
# Complete K4: all 2s (highly redundant connections)
```

---

## Diffusion Centrality Validation

**Script:** `test_diffusion_centrality.R`

Validates that cograph's vectorized diffusion centrality produces identical results to the original centiserve implementation, but significantly faster.

### Usage

```bash
Rscript validation/test_diffusion_centrality.R
```

### Tests

| Test Category | Tests |
|--------------|-------|
| Basic | Undirected (mode=all), lambda scaling |
| Directed | mode=out, mode=in, mode=all |
| Random Networks | 30-node undirected and directed |
| Lambda Values | 0.5, 1.0, 3.0 |
| Edge Cases | Single node, disconnected, complete (K6), star |

### Performance

| Nodes | Original | Cograph | Speedup |
|-------|----------|---------|---------|
| 100 | 5ms | 1ms | 5x |
| 500 | 24ms | 1ms | 24x |
| 1000 | 53ms | 2ms | 26x |

### Latest Results

```
Timestamp: 2026-02-10
Tests: 16/16 passed (100% pass rate)
Average speedup: ~18x on large networks
```

### Output Files

- `results_diffusion_centrality.txt` - Human-readable report

---

## scale_nodes_by Validation

**Script:** `test_scale_nodes_by.R`

Validates the `scale_nodes_by` parameter for `splot()` which allows sizing nodes by centrality measures.

### Usage

```bash
Rscript validation/test_scale_nodes_by.R
```

### Features Tested

- All centrality measures: degree, strength, betweenness, closeness, eigenvector, pagerank, authority, hub, harmonic
- Custom size ranges via `node_size_range`
- Centrality parameters via list syntax: `scale_nodes_by = list("pagerank", damping = 0.5)`
- Mode parameters for directed networks

### Output

- Plots saved to `validation/plots/`
- Console summary with pass/fail status

### Example Usage in splot()

```r
# Size nodes by degree centrality
splot(network, scale_nodes_by = "degree")

# Size by betweenness with custom range
splot(network, scale_nodes_by = "betweenness", node_size_range = c(1, 12))

# Size by pagerank with custom damping
splot(network, scale_nodes_by = list("pagerank", damping = 0.9))

# Size by closeness (in-degree) for directed networks
splot(network, scale_nodes_by = list("closeness", mode = "in"), directed = TRUE)
```

---

## Network-Level Metrics Validation

**Script:** `test_network_metrics.R`

Validates cograph's network-level metric functions against igraph using simulated networks from `Saqrlab::simulate_igraph()`.

### Usage

```bash
# Default: 30 tests, seed=42
Rscript validation/test_network_metrics.R

# Custom number of tests
Rscript validation/test_network_metrics.R 50

# Custom tests and seed
Rscript validation/test_network_metrics.R 50 123
```

### Metrics Tested

| Function | igraph Equivalent | Description |
|----------|-------------------|-------------|
| `network_girth()` | `girth()` | Shortest cycle length |
| `network_radius()` | `radius()` | Minimum eccentricity |
| `network_vertex_connectivity()` | `vertex_connectivity()` | Min nodes to disconnect |
| `network_clique_size()` | `clique_num()` | Largest complete subgraph |
| `network_cut_vertices()` | `articulation_points()` | Critical nodes |
| `network_bridges()` | `bridges()` | Critical edges |
| `network_global_efficiency()` | Manual calculation | Avg inverse path length |
| `network_local_efficiency()` | Manual calculation | Avg local efficiency |
| `network_small_world()` | - | Small-world coefficient σ |
| `network_rich_club()` | - | Rich-club coefficient |

### Graph Models Tested

Uses `Saqrlab::simulate_igraph()` with various models:
- **er** - Erdos-Renyi random graphs
- **ba** - Barabasi-Albert scale-free
- **ws** - Watts-Strogatz small-world
- **sbm** - Stochastic block model
- **reg** - Regular graphs
- **grg** - Geometric random graphs

### Test Categories

1. **Known Graph Tests** - Verified against expected values (K5, path, cycle, star, bridge)
2. **Simulated Network Tests** - Random networks across all models
3. **Efficiency Metrics Tests** - Manual calculation verification
4. **Small-World and Rich-Club Tests** - Property validation
5. **network_summary() Integration** - Combined function with extended=TRUE

### Output Files

- `results_network_metrics.txt` - Human-readable report

### Latest Results

```
Timestamp: 2026-02-11
Tests: 232 total
Pass Rate: 100.00%
```

### Example Usage

```r
# Individual functions
network_girth(adj)               # Shortest cycle
network_radius(adj)              # Minimum eccentricity
network_vertex_connectivity(adj) # Robustness
network_clique_size(adj)         # Largest clique
network_cut_vertices(adj)        # Critical nodes
network_bridges(adj)             # Critical edges
network_global_efficiency(adj)   # Communication efficiency
network_local_efficiency(adj)    # Fault tolerance
network_small_world(adj)         # Small-world coefficient
network_rich_club(adj, k = 5)    # Rich-club coefficient

# Combined summary with extended metrics
network_summary(adj, extended = TRUE)
```
