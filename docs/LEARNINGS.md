# Learnings

## Extended centrality batch (2026-04-08)

- igraph::diversity() uses **normalized Shannon entropy**: H/log2(k) where
  k is degree, with log2 (not ln). Our original implementation used raw
  Shannon entropy with natural log — completely different scale.
- LeaderRank is a **pure random walk** on the extended graph (no damping
  factor). Using `igraph::page_rank(damping=0.85)` gives wrong values.
  Must use power iteration on the transition matrix directly.
- centiserve::topocoefficient() computes normally for degree-1 nodes.
  Returning 0 early for k<=1 gives wrong results — let the computation
  run and handle 0/0 naturally.
- brainGraph::within_module_deg_z_score() returns NaN (not 0) when all
  nodes in a module have the same within-degree (sigma=0, so 0/0=NaN).
  This is semantically correct — z-score is undefined, not "average."
- centiserve::salsa() has a bug: returns `eigen(M)$values` (eigenvalues)
  instead of `$vectors[,1]` (principal eigenvector). Eigenvalues are
  meaningless as SALSA scores.
- centiserve::centroid() has a bug: self-exclusion check uses stale
  loop variable `u` instead of current node `w`.
- centiserve::closeness.vitality() errors on ~31% of random connected
  graphs with "Subgraph of graph is not strongly connected." This is a
  centiserve precondition check that shouldn't apply to undirected graphs.
- centiserve returns `numeric` (double) for integer-valued measures.
  Use `all.equal(as.numeric(), ...)` not `identical()` for equivalence.
- influenceR::bridging() uses Valente & Fujimoto (2010), not Hwang et al.
  (2006). Different measures sharing a name.
- brainGraph::gateway_coeff() uses a simplified formula without centrality
  weighting. Not directly comparable to Vargas & Wahl (2014) implementation.
- sna::flowbet() uses a different max-flow decomposition algorithm.
  Both are valid flow betweenness but produce different absolute values.

## Assortativity implementation (2026-04-07)

- `stats::sd()` returns `NA` (not `NaN` or 0) when given a length-1
  vector. Must check `is.na(sd_val)` before comparing `sd_val == 0`
  in if-conditions, otherwise R throws "missing value where TRUE/FALSE
  needed".
- igraph returns `NaN` for degenerate assortativity (constant degree,
  single category), cograph returns `NA`. Both are correct — test
  equivalence with `(is.nan(ig) && is.na(co))` check.
- Nominal assortativity mixing matrix: for undirected graphs, must
  symmetrize `e_mat <- (e_mat + t(e_mat)) / 2` since each undirected
  edge is counted once in the edge list but should contribute to both
  `e[i,j]` and `e[j,i]`.

## Equivalence testing (2026-04-03)

- Equivalence tests that compare your implementation against your own
  reference computation catch implementation bugs but miss formula bugs.
  The vulnerability denominator was wrong in both the function AND the
  test reference — 100 networks × every node showed 0 failures.
- Always include known-answer tests (star, K4, path) where you can verify
  by hand. Negative vulnerability on a star leaf is obviously wrong.
- Cross-package validation (brainGraph, tnet, sna) catches what
  self-referential equivalence tests miss.

## Oblique projection for 3D plots (2026-04-03)

- Vanishing-point perspective looks wrong for network visualizations —
  back planes shrink making nodes illegible. Use oblique/cabinet
  projection instead (constant shear, parallel lines, no shrinking).
- `pch` values 0-20 use `col` for color. `pch` 21-25 use `bg` for fill
  and `col` for border. Must detect and handle both in plot functions.

## igraph API quirks (2026-04-02)

- `igraph::is_bipartite()` checks for a `type` vertex attribute, NOT
  graph-theoretic bipartiteness. Use `igraph::bipartite_mapping()$res`
  to test if a graph is actually bipartite.
- `igraph::get.edge.ids()` returns `numeric`, not `integer`. Cast with
  `as.integer()` before using in `vapply(..., integer(1))`.
- `igraph::shortest_paths()` emits warnings when vertices are unreachable
  (expected during Yen's algorithm edge removal). Wrap in
  `suppressWarnings()`.

## CRAN check timing (2026-03-31)

- Windows CRAN check time is ~2.2x macOS. A 5 min macOS check → 11 min
  Windows.
- `\donttest` examples trigger a full re-run of ALL examples in a
  separate `--run-donttest` phase. Even if the code itself is trivial,
  the phase overhead (R session startup, package loading, Rd processing)
  costs 45s on macOS / ~100s on Windows.
- Converting `\donttest` → `\dontrun` eliminates that entire phase. This
  is valid when examples depend on Suggests packages.
- `skip_on_cran()` in test files works because `R CMD check --as-cran`
  does NOT set `NOT_CRAN=true`. But
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  DOES set it, so local runs are unaffected.
- Profile with `--timings` flag before changing anything. The bottleneck
  is not always where you expect.
