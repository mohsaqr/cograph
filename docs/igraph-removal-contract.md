# Porting contract: centrality measures off igraph

Read this before touching any `calculate_*` function. It is the shared
contract between the parallel porting agents and the verification harness.

## The graph context `cg`

Every `calculate_*` function and `calculate_measure()` now receives `cg`,
an environment of class `cg_graph` built by `.cg_graph()` in
`R/kernels-graph.R`. Fields (read-only, never assign into `cg`):

| Field | Type | Meaning |
|---|---|---|
| `cg$n` | integer | node count |
| `cg$directed` | logical | directedness in force (after any `directed =` override) |
| `cg$labels` | character(n) | node labels (row names or `"1".."n"`) |
| `cg$has_names` | logical | whether the input carried names |
| `cg$w` | n x n numeric | weight matrix; loops already removed when `loops = FALSE`; duplicates already combined |
| `cg$b` | n x n numeric | binary adjacency `(w != 0) * 1` |
| `cg$edges` | integer matrix, 2 cols | canonical edge order: row-major over non-zero cells; upper triangle incl. diagonal when undirected |
| `cg$weights` | numeric or NULL | weights in canonical edge order; NULL only for unweighted igraph input |
| `cg$weighted` | logical | whether `cg$weights` exists |
| `cg$cache` | environment | memo store; use `.cg_memo(cg, key, expr)` |

The `weights` argument that `calculate_*` functions receive is still a
**vector in canonical edge order** (possibly inverted for path-based
measures, possibly NULL when `weighted = FALSE`). Turn it into a matrix
with `.cg_path_matrix(cg, weights)` (NULL gives the binary matrix).
Distances: `.cg_distances(.cg_path_matrix(cg, weights), mode, cutoff)`;
hop distances: `.cg_hop_distances(cg, mode)` (memoised).

## The bridge to remove

Each function currently starts with `g <- .cg_igraph(cg)`, which builds the
igraph object the old code used. **Porting a function means deleting that
line and every `igraph::` call in the body**, replacing them with the
kernels below. When `options(cograph.forbid_igraph = TRUE)` is set,
`.cg_igraph()` errors with class `cograph_igraph_leak`; the verification
harness uses this to list what is still unported.

`.cg_path_matrix()` and `.cg_igraph()` accept an igraph object for code
that builds reduced graphs (`g_red`, deletions) during the transition, but
the goal is that no such object exists in the ported function.

## Kernels available (all in `R/kernels-*.R`, all take dense matrices)

Notation: `w` weight matrix, `b` binary matrix, `d` distance matrix, `n`,
`directed`, `mode` in `"all" | "out" | "in"`.

| Need | Kernel | Replaces |
|---|---|---|
| degree | `.cg_degree(b, directed, mode, loops = TRUE)` | `igraph::degree(g, mode)` |
| strength | `.cg_strength(w, directed, mode, loops = TRUE)` | `igraph::strength(g, mode, weights)` |
| all-pairs distances | `.cg_distances(m, mode, cutoff)` | `igraph::distances(g, mode, weights)` |
| single-source distances | `.cg_dijkstra(w_mode, s, n)` after `.cg_mode_weights(m, mode)` | `igraph::distances(g, v = s, ...)` |
| diameter | `.cg_diameter(d)` | `igraph::diameter` |
| betweenness | `.cg_betweenness(w, n, directed, cutoff, ...)` | `igraph::betweenness` |
| closeness / harmonic / eccentricity | `.cg_closeness(d, n)`, `.cg_harmonic(d, n)`, `.cg_eccentricity(d, n)` | `igraph::closeness`, `harmonic_centrality`, `eccentricity` |
| pagerank / eigenvector / HITS | `.cg_pagerank(w, n, damping, ...)`, `.cg_eigenvector(w, n)`, `.cg_hits(w, n)` | `igraph::page_rank`, `eigen_centrality`, `hits_scores` |
| coreness | `.cg_coreness(b, n, directed, mode)` | `igraph::coreness` |
| transitivity | `.cg_local_transitivity(b, n, directed)`, `.cg_global_transitivity(b)` | `igraph::transitivity` |
| constraint / effective size | `.cg_constraint(w, n)`, `.cg_effective_size(b, directed)` | `igraph::constraint` |
| alpha / power / katz / hubbell | `.cg_alpha(a, n, alpha)`, `.cg_power(b, n, alpha)`, `.cg_katz(m, alpha)`, `.cg_hubbell(m, factor)` | `igraph::alpha_centrality`, `power_centrality` |
| subgraph / communicability | `.cg_subgraph(w, n, directed)`, `.cg_communicability(w, n)` | `igraph::subgraph_centrality` |
| laplacian | `.cg_laplacian_matrix(w, directed)` | `igraph::laplacian_matrix` |
| components | `.cg_component_labels(b)`, `.cg_n_components(b)`, `.cg_strong_components(b)`, `.cg_largest_component(b)` | `igraph::components` |
| adjacency lists | `.cg_adjlist(b, directed, mode)`, `.cg_neighbors(b, directed, mode)` | `igraph::neighbors`, `as_adj_list` |
| local efficiency | `.cg_local_efficiency(...)` | `igraph::local_efficiency` |
| cliques | `.cg_cross_clique(b)` | `igraph::cliques` (for cross-clique) |
| max flow | none (see below) | `igraph::max_flow` |

Open `R/kernels-*.R` and read a kernel's roxygen before using it; several
take `n` explicitly and several return unnamed vectors that you must name
with `cg$labels` only if the old code returned named vectors.

## Kernels to add (write them in `R/kernels-graph-extra.R`, one owner)

- `.cg_edge_betweenness(w, n, directed, cutoff)` — Brandes accumulation
  over edges; needed by `edge_centrality()` and `calculate_bridging()`.
- `.cg_knn(b, w, directed, mode)` — average neighbour degree, for
  `centrality_modularity_vitality`.
- `.cg_articulation_points(b)`, `.cg_bridges(b)` — Tarjan low-link with an
  explicit stack; needed by the wrangling verbs later.

## Rules

1. **Behaviour is pinned by the golden file.** After every function you
   port, run
   `Rscript local_testing_and_equivalence/golden/compare_golden.R --networks <a,b,c>`
   on at least the networks that exercise the measure (see
   `tests/testthat/networks/manifest.csv`), and the full compare before you
   report. Zero differences at `sqrt(.Machine$double.eps)` relative
   tolerance, same NA/NaN/Inf pattern, same error class. Reporting a
   number you did not produce is a defect.
2. **Do not change what a measure returns.** If the kernel and igraph
   disagree, the kernel is wrong until proven otherwise with a hand
   computation on a network from `tests/testthat/networks/degenerate.rds`.
   Documented exceptions (`docs/igraph-removal-plan.md` section 5) need a
   note in your report, not a silent tolerance bump.
3. **No new `for` loops** without a comment justifying them; vectorise or
   use the apply family.
4. **No `tryCatch(..., error = function(e) NULL)`**. Kernels raise classed
   conditions; propagate them.
5. **Stay in your files.** The task assignment names them. `R/kernels-graph.R`
   and `R/centrality-metadata.R` are read-only for everyone.
6. **Run the existing tests for your files** (`testthat::test_file`) as
   well as the golden compare; both must pass.
7. Leave the flow-betweenness family on igraph, guarded by
   `.cg_need_igraph("flow_betweenness")` (defined in `R/kernels-graph.R`),
   and list it in your report.
