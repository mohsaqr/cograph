# Merge Changelog: cograph_plotonly + upstream/main

**Date:** 2026-02-10
**Branch:** `cograph_merged`
**Base:** `cograph_plotonly` (commit `fa91917`)
**Upstream:** `sonsoleslp/cograph` (main)

---

## Summary

This merge combines the plotting-focused `cograph_plotonly` branch with selected infrastructure and analysis functions from upstream, creating a comprehensive network visualization package with basic analysis capabilities.

---

## Branch Structure

| Branch | Purpose | Status |
|--------|---------|--------|
| `cograph_plotonly` | Original plotting branch | Preserved (reference) |
| `cograph_plotonly_BACKUP_20260210` | Timestamped backup | Preserved (safety net) |
| `cograph_merged` | New working branch | Active development |

---

## Commits Made

```
8d66122 Clean up generated HTML files
3230235 Add tests for centrality and network_summary functions
37db7ed Update NAMESPACE and documentation for restored functions
be827b3 Restore centrality and network_summary for non-estimated networks
d4e3dd6 Import CI/CD and pkgdown infrastructure from upstream
472cf5a Create aside/ folder for estimation functions (kept for future use)
```

---

## Changes by Category

### 1. New Directory: `aside/`

Created a dedicated folder for estimation/validation functions that require raw data. These are kept for future integration but excluded from the package build.

**Files:**
```
aside/
├── README.md
├── estimation/
│   ├── bootstrap.R
│   ├── disparity.R
│   ├── permutation.R
│   ├── stability.R
│   └── validation-utils.R
└── tests/
    ├── test-bootstrap.R
    ├── test-disparity.R
    ├── test-permutation.R
    ├── test-stability.R
    └── test-validation.R
```

**Rationale:** These functions require raw sequence data and the `tna` package for estimation. They are preserved for future use when estimation capabilities are added back.

---

### 2. CI/CD Infrastructure (from upstream)

Imported GitHub Actions workflows and pkgdown configuration.

**Files Added:**
```
.github/
├── .gitignore
└── workflows/
    ├── R-CMD-check.yaml    # Multi-platform R CMD check
    ├── pkgdown.yaml        # Package website deployment
    └── test-coverage.yaml  # Code coverage reporting

_pkgdown.yml                # Package website configuration
```

**Benefits:**
- Automated testing on Linux, macOS, Windows
- Automatic package website generation
- Code coverage tracking with codecov

---

### 3. Restored Analysis Functions

#### 3.1 Centrality Functions (`R/centrality.R`)

**Main Function:**
```r
centrality(x, measures = "all", mode = "all", normalized = FALSE,
           weighted = TRUE, directed = NULL, loops = TRUE,
           simplify = "sum", digits = NULL, sort_by = NULL, ...)
```

**Convenience Functions:**
| Function | Description |
|----------|-------------|
| `centrality_degree()` | Node degree (in/out/all) |
| `centrality_strength()` | Weighted degree |
| `centrality_betweenness()` | Shortest path centrality |
| `centrality_closeness()` | Inverse distance centrality |
| `centrality_eigenvector()` | Influence-based centrality |
| `centrality_pagerank()` | Random walk centrality |
| `centrality_authority()` | HITS authority score |
| `centrality_hub()` | HITS hub score |
| `centrality_eccentricity()` | Maximum distance to others |
| `centrality_coreness()` | K-core membership |
| `centrality_constraint()` | Burt's structural holes |
| `centrality_transitivity()` | Local clustering coefficient |

**Input Support:**
- Adjacency matrices
- igraph objects
- network objects (statnet)
- cograph_network objects
- tna objects

**Important Edit:** Removed duplicate `to_igraph()` function (lines 161-278) that conflicted with the existing implementation in `R/network-utils.R`.

#### 3.2 Network Summary Functions (`R/network-summary.R`)

**Functions:**
```r
network_summary(x, directed = NULL, weighted = TRUE, mode = "all",
                loops = TRUE, simplify = "sum", detailed = FALSE,
                digits = 3, ...)

degree_distribution(x, mode = "all", directed = NULL, loops = TRUE,
                    simplify = "sum", cumulative = FALSE,
                    main = "Degree Distribution", ...)
```

**Metrics Computed by `network_summary()`:**

| Metric | Description |
|--------|-------------|
| `node_count` | Number of nodes |
| `edge_count` | Number of edges |
| `density` | Edge density |
| `component_count` | Connected components |
| `diameter` | Longest shortest path |
| `mean_distance` | Average path length |
| `min_cut` | Edge connectivity |
| `centralization_degree` | Degree centralization |
| `centralization_betweenness` | Betweenness centralization |
| `centralization_closeness` | Closeness centralization |
| `centralization_eigen` | Eigenvector centralization |
| `transitivity` | Global clustering |
| `reciprocity` | Mutual edges (directed) |
| `assortativity_degree` | Degree correlation |
| `hub_score` | Max HITS hub |
| `authority_score` | Max HITS authority |

With `detailed = TRUE`, also computes mean/sd/median for degree, strength, betweenness, closeness, eigenvector, pagerank, constraint, and local transitivity.

---

### 4. New Tests

**Files Added:**
```
tests/testthat/test-centrality.R      (18 tests)
tests/testthat/test-network-summary.R (12 tests)
```

**Test Coverage:**
- Basic matrix input
- Specific measures selection
- Individual centrality functions
- Normalization
- Directed networks
- cograph_network input
- igraph input
- Sorting and rounding
- Error handling
- network_summary basic and detailed modes
- degree_distribution (regular and cumulative)

---

### 5. Documentation Updates

**New Man Pages:**
```
man/centrality.Rd
man/degree_distribution.Rd
man/network_summary.Rd
```

**NAMESPACE Exports Added:**
```r
export(centrality)
export(centrality_authority)
export(centrality_betweenness)
export(centrality_closeness)
export(centrality_constraint)
export(centrality_coreness)
export(centrality_degree)
export(centrality_eccentricity)
export(centrality_eigenvector)
export(centrality_hub)
export(centrality_pagerank)
export(centrality_strength)
export(centrality_transitivity)
export(degree_distribution)
export(network_summary)
```

---

### 6. Build Configuration Updates

**.Rbuildignore:**
```
^aside$    # Added - excludes aside/ from package build
```

**.gitignore:**
```
*.html     # Added - excludes generated HTML files
```

---

## Test Results

| Metric | Before Merge | After Merge |
|--------|--------------|-------------|
| Tests Passing | 1,579 | 1,643 |
| Tests Added | - | 64 |
| Skipped | 7 | 7 |
| Warnings | 6 | 21 |

**R CMD check:**
- Errors: 0
- Warnings: 0
- Notes: 4 (minor, non-blocking)

---

## What Was NOT Included

### Estimation/Validation Functions (in `aside/`)

These require raw sequence data and are kept aside for future integration:

| Function | Purpose |
|----------|---------|
| `bootstrap()` | Edge significance via resampling |
| `permutation_test()` | Network comparison p-values |
| `estimate_cs()` | Centrality stability coefficient |
| `disparity_filter()` | Backbone extraction |

### Upstream R6 Structure

The upstream uses an R6 `CographNetwork` class. We kept our lean S3 named list structure which is simpler and more compatible with the existing codebase.

### Upstream Layout Functions

New layout functions in upstream (`layout_grid_fn`, `layout_random_fn`, `layout_star_fn`, `layout_bipartite_fn`) use the R6 class and were not imported.

---

## Usage Examples

### Centrality Analysis

```r
library(cograph)

# Create adjacency matrix
mat <- matrix(c(0, 1, 1, 0,
                1, 0, 1, 1,
                1, 1, 0, 1,
                0, 1, 1, 0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

# All centrality measures
centrality(mat)

# Specific measures, sorted by pagerank
centrality(mat, measures = c("degree", "pagerank", "betweenness"),
           sort_by = "pagerank", digits = 3)

# Individual measure
centrality_betweenness(mat)

# Directed network
centrality(mat, directed = TRUE, mode = "in")
```

### Network Summary

```r
# Basic summary
network_summary(mat)

# Detailed summary with centrality statistics
network_summary(mat, detailed = TRUE)

# Degree distribution plot
degree_distribution(mat)
degree_distribution(mat, cumulative = TRUE)
```

### With cograph_network

```r
net <- as_cograph(mat)
centrality(net)
network_summary(net)
```

### With tna Objects

```r
# If tna package is available
library(tna)
model <- tna(sequences)
centrality(model)
network_summary(model)
```

---

## Rollback Instructions

If issues are discovered:

```bash
# Return to original state
git checkout cograph_plotonly

# Delete merged branch if needed
git branch -D cograph_merged

# Or restore from backup
git checkout cograph_plotonly_BACKUP_20260210
```

To restore estimation functions later:

```bash
# Copy from aside/ back to R/
cp aside/estimation/*.R R/
cp aside/tests/*.R tests/testthat/
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

---

## Next Steps

1. **Review and test** the merged branch thoroughly
2. **Push to remote** when satisfied: `git push -u origin cograph_merged`
3. **Create PR** to merge into main branch
4. **Update documentation** if needed for new functions
5. **Consider** adding estimation functions back when raw data support is implemented

---

## File Summary

| Action | Count | Files |
|--------|-------|-------|
| Added | 17 | aside/*, .github/*, _pkgdown.yml, R/centrality.R, R/network-summary.R, tests/testthat/test-centrality.R, tests/testthat/test-network-summary.R, man/*.Rd |
| Modified | 3 | .Rbuildignore, .gitignore, NAMESPACE |
| Deleted | 2 | mixed_edges_8nodes.html, mixed_edges_combined.html |

**Total lines added:** ~3,200
**Total lines removed:** ~120
