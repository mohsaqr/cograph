# TODO: Network Null Models & Statistical Testing Framework

## Overview

Design and implement a comprehensive framework for network randomization, null model generation, and statistical hypothesis testing in cograph.

**Goal**: Replace 50+ lines of repetitive code with 3-4 line workflows while ensuring statistical correctness.

---

## 1. Research Summary

### 1.1 Types of Null Models

| Method | Preserves | Use Case | Reference Package |
|--------|-----------|----------|-------------------|
| Erdos-Renyi G(n,m) | n nodes, m edges | Baseline: "any structure?" | `igraph::sample_gnm()` |
| Erdos-Renyi G(n,p) | n nodes, density p | Density-matched random | `igraph::sample_gnp()` |
| Configuration model | Exact degree sequence | Most common null | `igraph::sample_degseq()` |
| Degree-preserving rewire | Degree sequence (swaps) | Efficient for large nets | `igraph::rewire(keeping_degseq())` |
| Weight shuffling | Topology + weight dist | "Does weight placement matter?" | `tnet::rg_w()` |
| Local weight shuffling | Topology + node strength | Directed weighted networks | `tnet` |
| Dyad census preserving | Mutual/asymmetric/null | Strictest null for directed | `sna::cug.test(cmode="dyad.census")` |

### 1.2 Statistical Testing Frameworks

| Framework | What It Tests | Reference |
|-----------|--------------|-----------|
| Simple null comparison | Metric vs random distribution | Manual z-score |
| CUG test | Conditional Uniform Graph | `sna::cug.test()` |
| QAP | Network correlation/regression | `sna::qaptest()` |
| Bootstrap | Edge CIs, centrality stability | `bootnet` package |
| NCT | Compare two networks | `NetworkComparisonTest` |
| ERGM GOF | Model fit via simulation | `ergm::gof()` |
| Disparity filter | Edge significance (backbone) | Serrano et al. 2009 |
| Motif z-scores | Subgraph over-representation | Milo & Alon 2002 |

### 1.3 Key Metrics to Test

**Network-level:**
- Transitivity / clustering coefficient
- Reciprocity (directed networks)
- Global efficiency
- Modularity (with caution - overfitting risk)
- Assortativity (degree correlation)
- Small-world coefficient (sigma, omega)
- Rich-club coefficient (normalized)
- Centralization (degree, betweenness)

**Node-level:**
- Centrality significance via bootstrap
- CS-coefficient for stability
- Vulnerability

**Edge-level:**
- Edge betweenness
- Backbone significance

---

## 2. Proposed Functions

### 2.1 Core Functions

```r
# Generate null model networks
network_randomize(x,
  method = c("configuration", "gnm", "gnp", "rewire",
             "shuffle_weights", "local_shuffle"),
  n = 1000,
  keep_connected = FALSE,
  seed = NULL,
  directed = NULL
)

# Test single metric against null
null_test(x,

  metric = "transitivity",        # string or function
  method = "configuration",       # null model method
  n = 1000,                       # number of random networks
  alternative = "two.sided"       # or "greater", "less"
)

# Test multiple metrics at once
null_test_batch(x,
  metrics = c("transitivity", "reciprocity", "efficiency"),
  method = "configuration",
  n = 1000
)

# CUG-style test (sna compatibility)
cug_test(x,
  FUN = function(g) transitivity(g),
  cmode = c("size", "edges", "dyad.census"),
  reps = 1000
)

# Compare two networks
network_compare(x, y,
  tests = c("structure", "edges", "centrality", "global_strength"),
  n_permutations = 1000
)

# Backbone extraction with significance
backbone_extract(x,
  method = c("disparity", "noise_corrected"),
  alpha = 0.05
)
```

### 2.2 Return Object Design

```r
# null_test returns:
structure(list(
  observed = 0.342,
  null_mean = 0.089,
  null_sd = 0.012,
  null_distribution = numeric(1000),
  z_score = 21.08,
  p_value = 2.2e-16,
  p_greater = 1.1e-16,      # one-tailed
  p_less = 1.0,             # one-tailed
  method = "configuration",
  metric = "transitivity",
  n_random = 1000,
  alternative = "two.sided"
), class = "cograph_null_test")

# With methods:
print.cograph_null_test()
plot.cograph_null_test()    # histogram with observed line
summary.cograph_null_test()
```

---

## 3. Validation Strategy

### 3.1 Null Model Generation

**Test**: Configuration model preserves degree sequence exactly

```r
test_configuration_preserves_degree <- function() {
  # Create test network
  mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
  g <- to_igraph(mat)
  orig_degree <- degree(g)

  # Generate 100 random networks
  randoms <- network_randomize(mat, method = "configuration", n = 100)

  # EVERY random network must have EXACT same degree sequence
  for (r in randoms) {
    r_degree <- degree(to_igraph(r))
    stopifnot(identical(sort(r_degree), sort(orig_degree)))
  }
}
```

**Test**: Compare against igraph::sample_degseq()

```r
test_vs_igraph_sample_degseq <- function() {
  mat <- create_test_network(n = 20, directed = TRUE)
  g <- to_igraph(mat)

  in_deg <- degree(g, mode = "in")
  out_deg <- degree(g, mode = "out")

  # Our implementation
  our_randoms <- network_randomize(mat, method = "configuration", n = 500)

  # igraph reference
  ig_randoms <- replicate(500,
    sample_degseq(out.deg = out_deg, in.deg = in_deg, method = "simple"),
    simplify = FALSE
  )

  # Compare distributions of metrics
  our_trans <- sapply(our_randoms, function(r) transitivity(to_igraph(r)))
  ig_trans <- sapply(ig_randoms, transitivity)

  # Distributions should be similar (KS test)
  ks_result <- ks.test(our_trans, ig_trans)
  stopifnot(ks_result$p.value > 0.05)  # Not significantly different
}
```

### 3.2 CUG Test Validation

**Test**: Compare against sna::cug.test()

```r
test_cug_vs_sna <- function() {
  library(sna)
  library(intergraph)

  # Create network
  mat <- create_test_network(n = 30)
  net_sna <- asNetwork(to_igraph(mat))

  set.seed(42)
  sna_result <- cug.test(net_sna, gtrans, cmode = "edges", reps = 1000)

  set.seed(42)
  our_result <- cug_test(mat, FUN = transitivity, cmode = "edges", reps = 1000)

  # P-values should be very close (same seed, same algorithm)
  stopifnot(abs(sna_result$pgreq - our_result$p_greater) < 0.01)
  stopifnot(abs(sna_result$pleeq - our_result$p_less) < 0.01)

  # Also test with different cmodes
  for (cmode in c("size", "dyad.census")) {
    sna_r <- cug.test(net_sna, gtrans, cmode = cmode, reps = 500)
    our_r <- cug_test(mat, FUN = transitivity, cmode = cmode, reps = 500)
    # Compare...
  }
}
```

### 3.3 Z-Score Mathematics

**Test**: Verify z-score and p-value calculations

```r
test_zscore_math <- function() {
  # Known distribution
  set.seed(123)
  null_values <- rnorm(10000, mean = 0.5, sd = 0.1)
  observed <- 0.8

  # Our calculation
  result <- .compute_null_stats(observed, null_values)

  # Manual calculation
  expected_z <- (observed - mean(null_values)) / sd(null_values)
  expected_p_two <- 2 * pnorm(-abs(expected_z))
  expected_p_greater <- pnorm(expected_z, lower.tail = FALSE)
  expected_p_less <- pnorm(expected_z, lower.tail = TRUE)

  stopifnot(abs(result$z_score - expected_z) < 1e-10)
  stopifnot(abs(result$p_value - expected_p_two) < 1e-10)
  stopifnot(abs(result$p_greater - expected_p_greater) < 1e-10)
  stopifnot(abs(result$p_less - expected_p_less) < 1e-10)
}
```

### 3.4 Full Workflow Comparison

**Test**: Replicate user's exact workflow and compare results

```r
test_replicate_user_workflow <- function() {
  # Load user's actual data or create similar
  mat <- create_directed_weighted_network(n = 50)
  g <- to_igraph(mat)

  # ========== USER'S ORIGINAL CODE ==========
  degree_in <- degree(g, mode = "in")
  degree_out <- degree(g, mode = "out")

  transitivity_ls <- c()
  reciprocity_ls <- c()
  efficiency_ls <- c()

  for(i in 1:1000) {
    rg <- sample_degseq(out.deg = degree_out, in.deg = degree_in, method = "simple")
    rg <- simplify(rg)
    transitivity_ls <- c(transitivity_ls, transitivity(rg))
    reciprocity_ls <- c(reciprocity_ls, reciprocity(rg))
    efficiency_ls <- c(efficiency_ls, brainGraph::efficiency(rg, type = "global"))
  }

  trans_orig <- transitivity(g)
  trans_z_manual <- (trans_orig - mean(transitivity_ls)) / sd(transitivity_ls)
  trans_p_manual <- 2 * pnorm(-abs(trans_z_manual))

  # ========== OUR NEW CODE ==========
  result <- null_test_batch(mat,
    metrics = c("transitivity", "reciprocity", "efficiency"),
    method = "configuration",
    n = 1000
  )

  # ========== COMPARE ==========
  # Z-scores should be similar (within Monte Carlo error ~0.1)
  stopifnot(abs(result["transitivity", "z_score"] - trans_z_manual) < 0.3)

  cat("Manual transitivity z:", trans_z_manual, "\n")
  cat("Our transitivity z:", result["transitivity", "z_score"], "\n")
}
```

### 3.5 Edge Cases

```r
test_edge_cases <- function() {
  # 1. Empty network (no edges)
  empty <- matrix(0, 5, 5)
  rownames(empty) <- colnames(empty) <- LETTERS[1:5]
  result <- null_test(empty, metric = "transitivity", n = 100)
  # Should handle gracefully (NA or 0)

  # 2. Complete network
  complete <- matrix(1, 5, 5) - diag(5)
  randoms <- network_randomize(complete, method = "configuration", n = 10)
  # All random networks should be identical (only one way to arrange)

  # 3. Single node
  single <- matrix(0, 1, 1)
  # Should return NA or error gracefully

  # 4. Star network (one hub)
  star <- matrix(0, 6, 6)
  star[1, 2:6] <- star[2:6, 1] <- 1
  randoms <- network_randomize(star, method = "configuration", n = 100)
  # Degree sequence preserved, but structure may vary

  # 5. Disconnected network
  disc <- matrix(0, 8, 8)
  disc[1:4, 1:4] <- 1 - diag(4)
  disc[5:8, 5:8] <- 1 - diag(4)
  # Should work, connectedness not guaranteed


  # 6. Weighted network with negative weights
  neg <- matrix(c(0, -0.5, 0.8, -0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
  # Should handle or warn appropriately

  # 7. Very large network (performance test)
  large <- create_test_network(n = 500, density = 0.1)
  system.time({
    result <- null_test(large, metric = "transitivity", n = 100)
  })
  # Should complete in reasonable time (<30 seconds)
}
```

---

## 4. Validation Test Files

```
validation/
├── test_null_model_degree.R        # Degree preservation tests
├── test_null_model_vs_igraph.R     # Compare to igraph::sample_*
├── test_cug_vs_sna.R               # Compare to sna::cug.test
├── test_qap_vs_sna.R               # Compare to sna::qaptest
├── test_zscore_math.R              # Statistical calculations
├── test_metrics_vs_igraph.R        # Metric calculations
├── test_workflow_replication.R     # End-to-end vs user code
├── test_edge_cases.R               # Edge cases
└── test_performance.R              # Large network benchmarks
```

---

## 5. Implementation Order

### Phase 1: Core Null Model Generation
- [ ] Implement `network_randomize()` with methods:
  - [ ] `"configuration"` - wrapper around igraph::sample_degseq
  - [ ] `"gnm"` - wrapper around igraph::sample_gnm
  - [ ] `"gnp"` - wrapper around igraph::sample_gnp
  - [ ] `"rewire"` - wrapper around igraph::rewire(keeping_degseq())
- [ ] Write validation tests against igraph
- [ ] Verify degree preservation

### Phase 2: Simple Null Testing
- [ ] Implement `.compute_null_stats()` helper
- [ ] Implement `null_test()` for single metric
- [ ] Implement `print.cograph_null_test()`
- [ ] Implement `plot.cograph_null_test()`
- [ ] Write z-score validation tests

### Phase 3: CUG Test Compatibility
- [ ] Implement `cug_test()` with cmodes: size, edges, dyad.census
- [ ] Write validation tests against sna::cug.test
- [ ] Ensure identical results with same seed

### Phase 4: Batch Testing
- [ ] Implement `null_test_batch()` for multiple metrics
- [ ] Implement grid plot for multiple metrics
- [ ] Add progress bar for long-running tests

### Phase 5: Network Comparison
- [ ] Implement `network_compare()` for two networks
- [ ] Compare against NetworkComparisonTest package
- [ ] Add edge-wise and centrality comparisons

### Phase 6: Weighted Network Support
- [ ] Implement `"shuffle_weights"` method
- [ ] Implement `"local_shuffle"` method
- [ ] Validate against tnet package

### Phase 7: Backbone Extraction
- [ ] Implement `backbone_extract()` with disparity filter
- [ ] Add noise-corrected backbone option
- [ ] Validate against disparityfilter package

---

## 6. Key References

### Packages to Study
- **sna**: `cug.test()`, `qaptest()` - [CRAN](https://cran.r-project.org/package=sna)
- **igraph**: `sample_degseq()`, `rewire()` - [Docs](https://r.igraph.org/)
- **tnet**: Weighted network randomization - [Website](https://toreopsahl.com/tnet/)
- **bootnet**: Bootstrap methods - [CRAN](https://cran.r-project.org/package=bootnet)
- **NetworkComparisonTest**: Network comparison - [GitHub](https://github.com/cvborkulo/NetworkComparisonTest)
- **brainGraph**: Efficiency, vulnerability - [CRAN](https://cran.r-project.org/package=brainGraph)
- **bipartite**: Two-mode null models - [CRAN](https://cran.r-project.org/package=bipartite)

### Key Papers
1. **Milo et al. 2002** - Network motifs z-scores
2. **Serrano et al. 2009** - Disparity filter for backbone extraction
3. **Epskamp et al. 2018** - Bootstrap methods for network accuracy (bootnet)
4. **van Borkulo et al. 2022** - Network Comparison Test
5. **Telesford et al. 2011** - Small-world metrics (sigma vs omega)

### Documentation to Read
- [sna::cug.test documentation](https://rdrr.io/cran/sna/man/cug.test.html)
- [igraph rewiring tutorial](https://r.igraph.org/reference/rewire.html)
- [tnet random networks](https://toreopsahl.com/tnet/weighted-networks/random-networks/)
- [bootnet tutorial](https://reisrgabriel.com/blog/2021-09-27-bootnet/)

---

## 7. Questions to Investigate

1. **Configuration model edge cases**: What happens when degree sequence cannot be realized? How does igraph handle this?

2. **Weighted null models**: Is weight shuffling sufficient, or do we need strength-preserving methods?

3. **Directed networks**: How to handle dyad census preservation efficiently?

4. **Large networks**: What's the performance ceiling? Can we parallelize?

5. **Multiple testing correction**: When testing many metrics, should we apply FDR/Bonferroni?

6. **Bipartite networks**: Do we need specialized null models?

7. **Temporal networks**: Future extension for longitudinal data?

---

## 8. Success Criteria

- [ ] All validation tests pass (>95% agreement with reference packages)
- [ ] User's original 50-line workflow reduced to <5 lines
- [ ] Performance: 1000 random networks for n=100 in <10 seconds
- [ ] Clear documentation with examples
- [ ] Comprehensive error handling and warnings

---

## 9. Notes

*Add investigation notes here as you explore...*

```
Date:
Finding:
```
