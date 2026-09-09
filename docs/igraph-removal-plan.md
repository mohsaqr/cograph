# Finishing the igraph removal

## Status (2026-09-09)

Scope A is implemented and verified; Scope B (communities, layouts, motifs,
generators) untouched. Evidence and residual items in `HANDOFF.md`.

| Phase | State |
|---|---|
| 0 golden baseline | done: per-measure records, two files, six timed-out records recaptured with a 1200 s limit |
| 1 graph context + leak detector | done (`R/kernels-graph.R`) |
| 2 main switch | done (core agent), Barrat transitivity added |
| 3 extended file | done (56 functions + centralization) |
| 4 batch adapters | done (7-11, 12-51, kernels-batch41) |
| 5 adjacent surface | done (network-summary verbs, wrangling vocabulary, structure kernels) |
| 6 hygiene and proof | CI job added; no-igraph harness and benchmark scripts in `local_testing_and_equivalence/golden/` |


Date: 2026-09-08. Continues the work recorded in `CHANGES.md` under
"2026-08-23 — dependency-free centrality kernels". Every count below was
produced this session by grepping the working tree at commit `8074890e`
on branch `feat/centrality-zoo-batches-12-51`; the kernel equivalence
test was run and passed (50 tests, 0 failed, 0 skipped, 0 errors).

## 1. Where things stand

### 1.1 What the kernel branch delivered

- 45 `R/kernels-*.R` files, 190+ internal `.cg_*` functions, all taking
  **dense matrices**: `w` (weights), `b` (binary), `d` (distances), plus
  `n`, `directed`, `mode`. Not one of them needs an igraph object to
  compute; the only igraph calls inside the kernel files (11 in total)
  are in the two adapters that *build* matrices from an igraph object
  (`.cg_path_matrix(g)`, `.cg_hop_distances(g)`) and in
  `kernels-batch41.R`, which delegates constraint, betweenness and
  component counting back to igraph.
- Equivalence tests: `tests/testthat/test-kernels-distance-equivalence.R`
  (50 tests, 57 kernels referenced, igraph as oracle, guarded by
  `skip_if_not_installed("igraph")`).
- The branch pointer `feat/dependency-free-kernels` is stale: 0 commits
  ahead of main. The work was merged into main as part of the batch 7 and
  8 commit and the batch 12-51 commit. Delete the branch pointer or leave
  it; nothing lives there.

### 1.2 What is still on igraph

| Where | igraph calls | Kind |
|---|---|---|
| `R/centrality*.R` (49 files) | 465 | 124 are `vcount`, ~90 are `V`/`E`/`is_directed`/edgelist plumbing, the rest algorithmic |
| `R/kernels*.R` | 11 | adapters + batch 41 delegation |
| everything else in `R/` | 577 | communities, layouts, motifs, summary, generators, wrangling |

Inside the centrality surface the algorithmic dependence is concentrated:

- **Entry point.** `centrality()` line 115: `g <- to_igraph(x, directed)`.
  Every downstream function receives an igraph object. `hits_result` is
  precomputed once with `igraph::hits_scores()`.
- **Main switch** (`calculate_measure()`, `R/centrality.R` ~1875-1925):
  14 arms call igraph directly — degree, strength, closeness,
  eccentricity, coreness, harmonic, alpha, power, subgraph, betweenness,
  eigenvector, pagerank, constraint, transitivity. **A kernel exists for
  every one of them** (`.cg_degree`, `.cg_strength`, `.cg_closeness`,
  `.cg_eccentricity`, `.cg_coreness`, `.cg_harmonic`, `.cg_alpha`,
  `.cg_power`, `.cg_subgraph`, `.cg_betweenness`, `.cg_eigenvector`,
  `.cg_pagerank`, `.cg_constraint`, `.cg_local_transitivity`,
  `.cg_global_transitivity`, `.cg_hits`).
- **`R/centrality-extended.R`** (58 `calculate_*` functions): 26 calls to
  `igraph::distances()` plus per-function calls to `degree`, `strength`,
  `betweenness`, `harmonic_centrality`, `components`, `transitivity`,
  `cliques`, `all_shortest_paths`, `neighborhood`, `laplacian_matrix`,
  `alpha_centrality`, `max_flow`, `diameter`, `shortest_paths`. Kernels
  exist for all of them except `max_flow` (`flow_betweenness`, left on
  igraph by design) and edge betweenness.
- **`R/centrality.R` helpers** (12 `calculate_*`): `distances` (kreach,
  load, percolation), `laplacian_matrix` (current-flow pair), `degree`
  (diffusion, laplacian, leverage, voterank).
- **Batch adapters** (188 functions in `R/centrality-batch*.R`): 63 calls
  to `.cg_path_matrix(g)` and 50 to `igraph::vcount(g)` — pure plumbing.
  Six adapters call igraph algorithms: `calculate_epc`
  (`local_efficiency`), `.cg_gravity_mass` (`degree`, `coreness`),
  `calculate_relative_entropy` (`constraint`),
  `centrality_modularity_vitality` (`knn`), `calculate_entropy_variation`
  (`betweenness`), `calculate_ncvoterank` (`coreness`).
- **Adjacent public surface** outside `centrality()`: `centralization()`
  (degree, betweenness, closeness, eigen_centrality),
  `edge_centrality()` (`edge_betweenness`, **no kernel**),
  `group_centrality()`, `dispersion()`, `estrada_index()`,
  `trophic_incoherence()` in `R/network-summary.R`, and the 12-measure
  filter vocabulary in `R/network-utils.R`.
- **Package metadata.** igraph is in `Suggests`, yet none of the above is
  guarded by `requireNamespace()`. Under `_R_CHECK_FORCE_SUGGESTS_=FALSE`
  the centrality module fails outright. Two S3 methods
  (`disparity_filter.igraph`, `simplify.igraph`) and `to_igraph()` are
  legitimately igraph-facing and stay.

### 1.3 The oracle problem

`CHANGES.md` cites a "100-fixture cograph reference set" and "1,153
public-API comparisons". That set is not in `tests/`, `inst/`, or
`local_testing_and_equivalence/` (searched this session). The available
oracles are: igraph itself through the existing equivalence tests, the
per-batch tests (`test-centrality-batch*.R`), and the reference packages
used in `test-centrality.R` (igraph 125 calls, reticulate/NetworkX 24,
centiserve 15, sna 4, influenceR 2). Phase 0 therefore creates a golden
file from the **current, igraph-backed** `centrality()` before any flip.

## 2. Scope decision

Two honest scopes. Recommendation is **Scope A now, Scope B as a separate
plan**.

**Scope A — the centrality surface runs without igraph.** `centrality()`,
the 191 exported `centrality_*` wrappers, `edge_centrality()`,
`centralization()`, `group_centrality()`, `dispersion()`,
`estrada_index()`, `trophic_incoherence()`, and the centrality vocabulary
inside the wrangling verbs. igraph stays in `Suggests` for input
conversion and for the modules in Scope B. This is what the 2026-08-23
work set out to do.

**Scope B — the whole package runs without igraph.** Requires native
implementations of 11 community algorithms (`R/communities.R`), 13 layout
algorithms (`R/input-igraph.R`), triad and motif census with degree-
preserving rewiring (`R/motifs.R`), five random-graph generators used by
null models (`sample_gnp`, `sample_gnm`, `sample_pa`, `sample_degseq`,
`sample_smallworld`), girth, min-cut, vertex connectivity, cliques, power-
law fitting, and bipartite mapping. That is a second project of similar
size; some of it (Leiden, Infomap, spinglass, DrL) is not reasonable to
reimplement in R.

## 3. Design

### 3.1 One graph context instead of an igraph object

Introduce an internal constructor and pass its result where `g` is passed
today:

```r
.cg_graph(x, directed = NULL, simplify = TRUE, loops = TRUE, weighted = TRUE)
# returns an environment of class "cg_graph":
#   $w        n x n numeric weight matrix (0 = no edge), loops kept/dropped per `loops`
#   $b        n x n binary matrix
#   $n        integer
#   $directed logical
#   $labels   character(n)
#   $weights  edge-weight vector in the canonical edge order (for wrappers that still expose it)
#   cache     lazily filled: distances by (mode, cutoff, path-weights), hop distances by mode,
#             betweenness, hits, components, adjacency lists
```

Built without igraph for matrix, `cograph_network`, `tna`, edge-list and
`netobject` input through `to_matrix()` and `as_cograph()`. igraph,
`network` and `qgraph` input keep using their own package to reach a
matrix — if the user holds such an object, that package is loaded.

An environment is used so the caches fill once per `centrality()` call
and are shared by every measure without threading arguments through 260
function signatures.

### 3.2 A leak detector, not a hope

Add an internal accessor that every remaining igraph use in the
centrality surface must go through:

```r
.cg_igraph <- function(ctx) {
  if (isTRUE(getOption("cograph.forbid_igraph"))) {
    stop(errorCondition("igraph reached from the native centrality path",
                        class = "cograph_igraph_leak", call = NULL))
  }
  ctx$g %||% (ctx$g <- .cg_to_igraph_from_matrix(ctx))
}
```

Phase 1 routes all existing igraph calls through it; the test suite is
then run once with the option set, and the failures **are** the work
list. When the list is empty, the option and the accessor are removed for
all measures except the one deliberately left on igraph.

### 3.3 What stays on igraph inside Scope A

- `flow_betweenness`: the bit-exact port was written and removed at ~510
  lines. Keep it on igraph behind `requireNamespace()` with a classed
  error `cograph_needs_igraph`, list it in `?centrality` as the one
  measure with an external dependency. (Decision Q2 below.)

## 4. Phases

### Phase 0 — golden baseline (before any code change)

1. Script `local_testing_and_equivalence/golden/make_golden.R` builds a
   graph zoo (the same generator as the kernel equivalence test:
   n in {1,2,3,5,8,12,20,50}, density in {0,.1,.35,.7,1}, directed x
   weighted, plus the packaged datasets and three graphs with self-loops
   and one with negative weights) and records `centrality(x, type = "all",
   mode = m)` for `m` in all/in/out, with `weighted` TRUE/FALSE, as RDS.
   Store scores in IEEE hex (`sprintf("%a")`) so ties survive.
2. Record, per measure and graph, whether the current output is finite,
   NA, an error, or a warning, so a later change from error to value is
   also visible.
3. Add `tests/testthat/test-centrality-golden.R` that reads the RDS and
   compares with `all.equal(tolerance = sqrt(.Machine$double.eps))`, with
   an explicit allow-list of measures whose divergence is documented (see
   section 5). The file skips when the RDS is absent so CRAN is unaffected.
   Decide (Q5) whether the RDS is committed under `tests/testthat/golden/`
   (about 5 MB expected) or kept in the ignored local directory.
4. Commit nothing yet; the baseline must come from the igraph-backed code.

### Phase 1 — graph context and leak detector

1. Implement `.cg_graph()` in a new `R/kernels-graph.R`; move
   `.cg_path_matrix()` and `.cg_hop_distances()` to read from the context
   instead of an igraph object (their bodies already produce the matrices
   the kernels want; the loop in `.cg_path_matrix()` becomes
   `m[cbind(i, j)] <- pmax(m[cbind(i, j)], w)`).
2. `centrality()` builds `ctx` instead of `g`. Every `calculate_*`,
   `calculate_measure()`, and batch adapter gets `ctx` in place of `g`.
   Mechanical: 63 `.cg_path_matrix(g, ...)` become `ctx$w` or
   `.cg_path_matrix(ctx, weights)`; 50 `igraph::vcount(g)` become `ctx$n`.
3. All remaining igraph calls in the centrality surface go through
   `.cg_igraph(ctx)`. No behaviour change; golden test and full suite must
   pass unchanged.
4. Run the suite with `options(cograph.forbid_igraph = TRUE)`; save the
   failure list to `docs/igraph-leaks.md` as the Phase 2-5 checklist.

### Phase 2 — flip the main switch (14 arms + HITS)

One arm per commit, each verified against the golden file and the
existing per-measure tests. Known convention traps to check explicitly:

| Arm | Kernel | Trap |
|---|---|---|
| degree, strength | `.cg_degree`, `.cg_strength` | self-loops count twice under undirected and directed `mode = "all"` (already fixed in the kernel, verify with the loop fixtures) |
| closeness, harmonic | `.cg_closeness`, `.cg_harmonic` | igraph closeness uses reachable vertices only on disconnected graphs; `normalized` scaling |
| eccentricity | `.cg_eccentricity` | unreachable = ignored, not Inf |
| coreness | `.cg_coreness` | `mode` semantics on directed graphs |
| betweenness | `.cg_betweenness` | `cutoff`, `normalized`, undirected halving, tie tolerance at large weights |
| eigenvector | `.cg_eigenvector` | scale to max 1; ill-posed on non-strongly-connected directed graphs (see 5) |
| pagerank | `.cg_pagerank` | `personalized`, damping, dangling-node treatment, negative weights raise `cograph_negative_weights` |
| alpha, power | `.cg_alpha`, `.cg_power` | singular systems must raise, not return garbage (`.cg_solve_or_stop` already exists) |
| subgraph | `.cg_subgraph` | `diag = FALSE` convention |
| constraint | `.cg_constraint` | isolates return NaN in igraph |
| transitivity | `.cg_local_transitivity` / `.cg_global_transitivity` | `isolates = "nan"` vs `"zero"`, `type` argument |
| hits | `.cg_hits` | sign and scaling of the dominant vector; acyclic graphs |

### Phase 3 — extended file and core helpers

1. Replace the 26 `igraph::distances()` calls with `ctx` cached distances
   (the shared matrix already exists for 13 measures; extend the cache key
   to (mode, cutoff, weights-in-force) so every caller hits it).
2. Replace the per-function algorithmic calls listed in 1.2 with the
   matching kernels. Order by blast radius: `degree`/`strength` (14
   functions), `components` (dmnc, mnc), `transitivity` (clusterrank),
   `cliques` (cross_clique), `all_shortest_paths` (bottleneck),
   `neighborhood` (semilocal), `laplacian_matrix` + `alpha_centrality`
   (spanning_tree, current-flow pair), `diameter` (radiality),
   `harmonic_centrality` + `shortest_paths` (pairwisedis, reaching_local),
   `max_flow` (stress: verify the kernel `.cg_stress` covers the arm that
   used max_flow; if not, that arm follows flow_betweenness to the guarded
   path).
3. **New kernel: edge betweenness.** Brandes accumulation over edges is a
   ten-line extension of `.cg_betweenness`; add `.cg_edge_betweenness(w,
   n, directed, cutoff)` with an equivalence test against
   `igraph::edge_betweenness()` on the graph zoo. Needed by
   `edge_centrality()` and `calculate_bridging()`.
4. **Communicability defect.** `calculate_communicability()` uses `t(V)`
   where asymmetric input needs `solve(V)`. The kernel reproduces the
   defect on purpose. Fix both together in this phase, record the change
   in `NEWS.md`, and add the 33 asymmetric zoo graphs to the golden
   allow-list with the corrected value as the new reference.

### Phase 4 — batch adapters and batch 41

1. Six adapters with direct igraph algorithms -> kernels:
   `calculate_epc` -> `.cg_local_efficiency`; `.cg_gravity_mass` ->
   `.cg_degree` / `.cg_coreness`; `calculate_relative_entropy` ->
   `.cg_constraint`; `calculate_entropy_variation` -> `.cg_betweenness`;
   `calculate_ncvoterank` -> `.cg_coreness`;
   `centrality_modularity_vitality` -> new `.cg_knn` (average neighbour
   degree, ~8 lines, test against `igraph::knn()`).
2. `kernels-batch41.R`: replace `graph_from_adjacency_matrix`,
   `constraint`, `betweenness`, `components` + `delete_vertices` with
   `.cg_constraint`, `.cg_betweenness`, `.cg_component_labels` on the
   matrix with a row/column zeroed.
3. Remove the two roxygen warnings noted in `HANDOFF.md`
   (`kernels-neighborhood.R:7` dangling link, `plot-temporal.R:294`).

### Phase 5 — adjacent surface

1. `centralization()`: use the kernels; igraph's `centr_*` theoretical
   maxima are closed-form and already documented in the function.
2. `edge_centrality()`: `.cg_edge_betweenness`.
3. `group_centrality()`, `dispersion()`, `estrada_index()`,
   `trophic_incoherence()`: build `ctx`, use `.cg_distances`,
   `.cg_betweenness` with the group's vertices removed, and the existing
   `expm` path.
4. Wrangling verbs (`R/network-utils.R`): the 12-measure eager list and
   the 13-measure lazy resolver both become one call to
   `centrality(ctx, measures = needed)`. Context variables:
   `component*` -> `.cg_component_labels`; `k_core` -> `.cg_coreness`;
   `is_articulation`, `is_bridge`, `is_bridge_endpoint` -> new
   `.cg_articulation_points()` / `.cg_bridges()` (one Tarjan low-link DFS
   with an explicit stack, ~40 lines, tested against
   `igraph::articulation_points()` and `igraph::bridges()`). This also
   removes the `network_to_igraph()` isolate crash from these verbs'
   path, independently of the wrangling plan's own fix.
5. `select_edges(..., community = )`: `same_community` still needs a
   community algorithm. Leave on `detect_communities()` (Scope B) with a
   `requireNamespace()` guard and a classed error.

### Phase 6 — dependency hygiene and proof

1. Guard every igraph call that remains anywhere in `R/` with
   `requireNamespace("igraph", quietly = TRUE)` and the classed error
   `cograph_needs_igraph` naming the function and the reason. Central
   helper `.need_igraph(what)`.
2. Delete the `.cg_igraph()` leak accessor and the option; the only
   igraph reference left in the centrality surface is the
   `flow_betweenness` guard.
3. CI: add a job to `R-CMD-check.yaml` that runs the test suite with
   igraph **uninstalled** (`remove.packages("igraph")` after setup) and
   `_R_CHECK_FORCE_SUGGESTS_=FALSE`. Centrality tests must run (not
   skip); equivalence tests that use igraph as the oracle skip as they do
   today. This is the durable proof that the removal holds.
4. Performance table, measured, not estimated: `centrality(type = "basic")`
   and the five most expensive measures at n = 100, 500, 1000, native vs
   the pre-flip igraph path from Phase 0. R-level Brandes is O(nm) in
   interpreted code; expect a real slowdown on betweenness-family
   measures at n = 1000. Report it in `NEWS.md`. (Decision Q3, Q4.)
5. Docs: `NEWS.md` entry; remove "requires igraph" from roxygen where no
   longer true; `README` dependency claim; `CHANGES.md`; `HANDOFF.md`;
   `docs/LEARNINGS.md`. Version bump per repo rule.

### Phase 7 — Scope B (separate plan, not started here)

Sized for the decision only:

| Module | igraph algorithms | Native feasibility |
|---|---|---|
| `communities.R` | louvain, leiden, walktrap, fast_greedy, label_prop, infomap, leading_eigen, edge_betweenness, spinglass, optimal, fluid | louvain, label_prop, fast_greedy, edge_betweenness, leading_eigen, fluid: feasible in R. leiden, infomap, spinglass, optimal: keep on igraph. |
| `input-igraph.R` layouts | fr, kk, drl, graphopt, lgl, mds, circle, star, tree, grid, sphere, random, nicely | cograph already ships its own FR (`layout-gephi-fr.R`) and target layouts; kk and mds are short; drl/graphopt/lgl keep on igraph. |
| `motifs.R` | triad_census, motifs, rewire (degseq) | triad census is a 16-class table over dyad types, feasible; size-4 motif census and degree-preserving rewiring are larger. |
| null models | sample_gnp, sample_gnm, sample_pa, sample_degseq, sample_smallworld | gnp/gnm trivial; pa and smallworld short; degseq (Viger-Latapy) non-trivial. |
| `network-summary.R` | girth, min_cut, vertex_connectivity, clique_num, centr_* | girth BFS feasible; min-cut and vertex connectivity via max-flow, keep on igraph. |
| `fit-distribution.R`, `bipartite.R`, `dyad-census.R` | fit_power_law, bipartite_mapping, dyad_census | all short. |

## 5. Known and expected divergences to state, not hide

- **Eigenvector on non-strongly-connected directed graphs**: the dominant
  eigenvector is not unique; igraph returns zeros or an arbitrary basis
  vector. The kernel picks maximum modulus tie-broken by real part. Golden
  comparison must classify these inputs as ill-posed and compare only the
  well-posed ones; document the rule in `?centrality`.
- **Closeness / harmonic on disconnected graphs**: igraph's closeness
  averages over reachable vertices only. Match it and say so.
- **`Inf` for `average_distance`**: cograph returns `Inf`; keep it.
- **Communicability**: corrected value differs from the current igraph-era
  output on asymmetric input (Phase 3.4).
- **Speed**: interpreted R versus igraph's C. Report numbers.
- **`flow_betweenness`**: needs igraph; classed error otherwise.

## 6. Decisions needed

- **Q1 Scope.** A now, B later (recommended), or both in one push.
- **Q2 flow_betweenness.** Guarded igraph call (recommended) or reinstate
  the 510-line push-relabel port under `R/kernels-flow-exact.R`.
- **Q3 Accelerator.** Add `engine = c("native", "igraph")` to
  `centrality()` so users with igraph can opt into the C path, with the
  equivalence suite guaranteeing identical results? Recommended: **no**
  for the first release; two code paths doubles the test surface and the
  point of the exercise is one implementation. Revisit if Phase 6.4
  numbers are bad.
- **Q4 Performance acceptance.** What slowdown on betweenness-family
  measures at n = 1000 is acceptable before we must write the hot loops
  in C via `.Call` (which adds a compiled-code build step to a package
  that currently has none)?
- **Q5 Golden file location.** Committed under `tests/testthat/golden/`
  (reproducible on CI, adds ~5 MB to the source tarball) or ignored local
  directory with a documented regeneration script (recommended for the
  CRAN tarball; commit a 200 KB subset for CI).

## 7. Effort and order

| Phase | Touches | Size | Gate |
|---|---|---|---|
| 0 | 1 script, 1 test | small | golden RDS exists; test passes on current code |
| 1 | `kernels-graph.R` (new), `centrality.R`, 45 batch adapters (mechanical) | medium-large, mostly sed-able | suite green; leak list written |
| 2 | `centrality.R` switch | medium, 15 small commits | golden green per arm |
| 3 | `centrality-extended.R`, `centrality.R` helpers, 1 new kernel | large | golden green; leak list shrinks to batch items |
| 4 | 6 adapters, `kernels-batch41.R` | small | leak list empty except flow_betweenness |
| 5 | `centrality-extended.R`, `network-summary.R`, `network-utils.R`, 2 new kernels | medium | forbid-option suite green |
| 6 | DESCRIPTION guards, CI, docs, benchmarks | small | CI job without igraph green |

Phases 2, 3 and 4 can run in parallel once Phase 1 lands, one agent per
phase, each gated by the golden test and each forbidden from touching
the others' files.
