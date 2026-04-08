# Changes

## 2026-04-08 — Extended centrality batch: 64 measures + 39 convenience wrappers

- R/centrality.R: Wired 5 remaining measures (salsa, leaderrank, participation, within_module_z, gateway) into `calculate_measure` dispatcher. Updated measure count from 59 to 64. Added `@param` docs for `decay_parameter` and `membership`.
- R/centrality.R: Added 39 `centrality_*` convenience wrappers for all extended measures
- R/centrality-extended.R: Made salsa/leaderrank return NA with warning on undirected graphs. Made participation/within_module_z/gateway return NA with warning when membership is NULL.
- R/centrality-extended.R: Fixed diversity to use normalized Shannon entropy (log2, divided by log2(degree)) matching igraph::diversity()
- R/centrality-extended.R: Fixed topological_coefficient to compute for degree-1 nodes (was returning 0, now matches centiserve)
- R/centrality-extended.R: Fixed within_module_z to return NaN when sigma=0 (matching brainGraph convention)
- R/centrality-extended.R: Fixed leaderrank to use pure random walk power iteration (was incorrectly using PageRank damping=0.85)
- Tests: 197 tests in test-centrality-extended.R + 44 tests in test-centrality-equivalence-report.R
- Equivalence report: 3,200 graph comparisons across 32 measures vs centiserve (21), sna (4), influenceR (1), brainGraph (2), igraph (1), plus self-consistency (3). 0 failures.
- Found centiserve bugs: centroid() stale variable, salsa() returns eigenvalues instead of eigenvectors

## 2026-04-07 — Assortativity & homophily module

- R/assortativity.R: New `assortativity()` (degree assortativity, Newman 2002), `assortativity_attribute()` / `homophily()` (nominal + scalar attribute assortativity). Print method with interpretation.
- Fixed: `sd()` returning NA on degenerate directed graphs caused crash in degree assortativity helpers
- Tests: 38 tests including 400 cross-package equivalence comparisons vs igraph (0 failures across `assortativity_degree`, `assortativity_nominal`, `assortativity` on 100 random networks each)

## 2026-04-03 — Rich club, plot functions, bug fixes, code quality

- R/rich-club.R: New `rich_club()` (weighted/unweighted curve with null model), `rich_club_local()` (per-node ratio). Print + plot methods (curve + network overlay)
- R/plot-distributions.R: New `plot_centrality_distribution()`, `plot_edge_weights()`, `plot_degree_correlation()`, `plot_network_evolution()`
- R/plot-temporal.R: New `plot_temporal()` — 3D oblique-projection glass box with time-sliced network planes
- R/centrality.R: `edge_centrality()` gained overlap, simmelian, reciprocity measures (single-pass computation)
- 13 bugs fixed across vulnerability, core-periphery, rich-club, fit-distribution, bipartite, degree_distribution
- Code quality: replaced `<<-`, extracted `.make_fit_result()`, rich_club null model 20-50x faster, vulnerability 50% less memory
- Merged upstream (Sonsoles' vignette reorganization, logo, favicon)
- Tests: 2,600 new tests; 162,967 equivalence values vs igraph/brainGraph/tnet, 0 failures

## 2026-04-02 — Tier 2 network science features

- R/edge-metrics.R: New `neighborhood_overlap()`, `simmelian_strength()`, `edge_reciprocity()`
- R/vulnerability.R: New `vulnerability()` — per-node efficiency drop (Latora & Marchiori)
- R/core-periphery.R: New `core_periphery()` — Borgatti-Everett continuous + discrete
- R/fit-distribution.R: New `fit_degree_distribution()` — power-law/exponential/Poisson/geometric MLE
- R/paths.R: New `shortest_paths()`, `k_shortest_paths()` (Yen's algorithm)
- R/bipartite.R: New `project_bipartite()` (5 methods), `is_bipartite()`
- R/network-summary.R: Rewrote `degree_distribution()` with bin control, normalize, log, CCDF

## 2026-03-31 — CRAN check time reduction

- Added `skip_on_cran()` to 92 coverage test files and 23 non-coverage
  test files. 5 core functional tests remain active on CRAN.
- Converted `\donttest` → `\dontrun` in 19 R source files. All affected
  examples depend on Suggests packages, making `\dontrun` appropriate
  per CRAN policy.
- Result: macOS CRAN check dropped from 5m02s to 1m46s. Estimated
  Windows time ~3m53s (was ~11 min).
