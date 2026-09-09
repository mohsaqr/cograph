# Network wrangling verbs: review and plan

Date: 2026-09-08. Scope: the verbs in `R/network-utils.R`, `R/simplify.R`
and the accessors in `R/class-network.R`. Every defect below marked **P#**
was reproduced this session on synthetic data with
`scratchpad/probe.R` (4-node undirected matrix `und`, 4-node directed
matrix `dir`, 5-node matrix `iso` with an isolated node E, edge list `el`
with a `session` column). Items without a probe id come from code reading
and are marked *unverified*.

## 1. Inventory

| Group | Verbs | Notes |
|---|---|---|
| Eager expression filters | `filter_edges()`, `filter_nodes()`, aliases `subset_edges()`, `subset_nodes()` | `filter_nodes` computes all 12 centralities on every call |
| Lazy node selection | `select_nodes()` + wrappers `select_neighbors()`, `select_component()`, `select_top()` | six AND-combined modes, lazy centralities and context vars |
| Lazy edge selection | `select_edges()` + wrappers `select_bridges()`, `select_top_edges()`, `select_edges_involving()`, `select_edges_between()` | six AND-combined modes |
| Structure | `simplify()` (S3: matrix, cograph_network, igraph, tna, default) | only the cograph_network method merges duplicates |
| Conversion | `as_cograph()`, `to_matrix()`, `to_igraph()`, `to_network()`, `to_data_frame()` / `to_df()` | everything routes through igraph except stored `$weights` |
| Accessors | `get_nodes()`, `get_edges()`, `get_labels()`, `set_nodes()`, `set_edges()`, `set_layout()`, `n_nodes()`, `n_edges()`, `nodes()` (deprecated) | no `as.data.frame()` method |
| Adjacent, elsewhere | `ego_networks()` (`ego.R`), `shortest_paths()` (`paths.R`), `disparity_filter()`, `detect_communities()`, `cluster_summary()`, `aggregate_layers()` | overlap with the selectors is undocumented |

Common shape of every verb: `as_cograph()` in, work on the nodes and edges
data frames, rebuild through `.create_cograph_network()`, optional
`.convert_to_format()` out when `keep_format = TRUE`.

## 2. Confirmed defects (ordered by blast radius)

### D1. Any network with a trailing isolate crashes every igraph-backed verb (P10, P21)

`network_to_igraph()` in `R/input-igraph.R` builds the graph with
`igraph::graph_from_edgelist()`, so the vertex count is the largest index
that appears in an edge. Nodes after that index vanish, and the next line
`V(g)$name <- nodes$label` aborts with
`Length of new attribute value must be 1 or 4 ... not 5`.

Reproduced with a plain 5 x 5 matrix whose node E has no edges:

```r
filter_nodes(iso, label != "E")          # ERROR
select_nodes(kept, component = "largest")  # ERROR
to_df(kept)                              # ERROR
```

`filter_nodes()`, `select_nodes()`, `select_edges()`, `to_df()`,
`to_network()` and `to_matrix()` without a stored weight matrix all go
through this path. `to_matrix()` survived only because the stored
`$weights` short-circuits (P21). Root cause fix: build with
`igraph::graph_from_data_frame(edges, vertices = nodes, directed = ...)`
or `make_empty_graph(n) + add_edges()`, which also carries extra edge
columns as attributes for free.

### D2. Undirected results come back as asymmetric weight matrices (P2, P22, P23)

`.subset_cograph_network()` and `.update_cograph_edges()` rebuild
`$weights` with `new_weights[from, to] <- weight` only. For an undirected
network the edges table stores one row per pair, so the rebuilt matrix is
upper-triangular:

```r
to_matrix(filter_edges(und, weight > 0.35))
#   A   B   C   D
# A 0 0.5 0.8 0.0
# B 0 0.0 0.0 0.6
# C 0 0.0 0.0 0.4
# D 0 0.0 0.0 0.0
isSymmetric(...)  # FALSE
```

With `keep_format = TRUE` the user receives that asymmetric matrix, and
`as_cograph()` on it auto-detects **directed** (P22 printed `TRUE` for the
undirected input). Anything downstream that reads `$weights` directly
(`to_matrix`, `splot` on a matrix, `centrality` on the matrix) sees a
directed network with half the strength. `filter_nodes()` has the same
defect (P23).

### D3. `filter_nodes()` errors outright on negative weights (P9)

The eager `.compute_centrality_vars()` passes weights to
`igraph::betweenness()` unguarded, so `filter_nodes(neg, degree >= 2)`
fails with `Weight vector must be positive` even though the expression
never touched betweenness. The lazy path (`select_nodes`) handles this
correctly with a warning and NA. Correlation and partial-correlation
networks routinely carry negative weights, so `filter_nodes()` is unusable
on the psych-network inputs `splot.netobject` is built for.

### D4. Empty result with `keep_format = TRUE` crashes for matrix input (P17)

`filter_edges(und, weight > 5, keep_format = TRUE)` reaches `to_matrix()`
on an empty cograph_network without stored weights, and
`igraph::as_adjacency_matrix(attr = "weight")` aborts with
`No such edge attribute`. `filter_nodes()` special-cases this with
`matrix(0, 0, 0)`; `filter_edges()` and `select_edges()` do not.

### D5. `set_edges()` leaves `$weights` stale and breaks conversion (P6)

`set_edges()` replaces the edges table but never touches `$weights`.
`to_matrix()` prefers `$weights`, so it returns the old matrix; and
because the new edges referenced only nodes 1 and 2, `to_igraph()` hit D1.
`set_nodes()` has the same staleness for labels in `dimnames($weights)`.
*Unverified for `set_nodes`.*

### D6. Custom edge columns never survive the entry point (P5)

`.create_cograph_network()` has a loop whose comment says it preserves
extra columns such as `session` or `time`, but `parse_edgelist()` only
forwards `from`, `to`, `weight`. An edge list with a `session` column
loses it at `as_cograph()`, so the documented ability to filter on "any
edge column" is unreachable from a data frame, and
`set_edges()` drops extra columns explicitly.

### D7. Metadata is silently discarded by every filter (P8, P14, P18)

- `node_groups` survives `as_cograph` but is `NULL` after `filter_nodes()`
  (P18), so grouped or htna-style inputs lose their group assignment.
- `$data` (tna sequence data) is `NULL` after `filter_edges()` (P14), so
  `plot_simplicial()` auto-HON and bootstrap paths cannot follow a filter.
- `meta$layout` is dropped, while the `x`/`y` node columns survive (P8).
  The two layout carriers disagree after any filter.
- `meta$source` is overwritten with `"filtered"`, so the original source
  type is lost after the first verb.

### D8. `keep_format = TRUE` is silently ignored for tna and qgraph input (P14)

`filter_edges(tna_model, weight > .1, keep_format = TRUE)` returns a
`cograph_network` with no message. `.convert_to_format()` falls through
for `tna`, `qgraph` and `unknown`. Since a tna object is a plain list with
`$weights`, rebuilding one is straightforward and expected by tna users.

### D9. Unknown `by` silently falls back to degree / weight (P4)

`select_top(und, n = 2, by = "bogus")` returns the same nodes as
`by = "degree"` with no condition. `.compute_single_centrality()` and
`.compute_single_edge_metric()` both end in a default branch. Should be
`match.arg()`.

### D10. Warnings where errors belong, and inconsistent between paths

- `between` that is not a two-element list warns and **selects every
  edge** (returns `rep(TRUE, n)`), the opposite of the safe default.
- Numeric `involving` out of range is dropped silently (P13 printed only
  the downstream "No edges match" warning), while character names warn.
- Fractional indices (`index = 2.7`) are silently truncated (P15).
- No classed conditions anywhere in the family, so callers and tests can
  only match on message text.

### D11. Eager and lazy centrality helpers disagree

`.compute_centrality_vars()` (eager) and `.compute_single_centrality()`
(lazy) implement the same twelve measures twice, with different guards
(D3) and with `closeness`, `eigenvector`, `hub`, `authority` wrapped in
`tryCatch(..., error = function(e) rep(NA_real_, n))`. The catch-all
swallows every igraph error, not just the documented "cannot compute"
cases, and the eager path fires all twelve on every call including
`igraph::hits_scores()`, which is the most expensive.

### D12. `filter_edges()` removes isolates that pre-existed the filter (P16)

`.keep_isolates = FALSE` drops **all** degree-zero nodes after the filter,
including node E which was already isolated. igraph `delete_edges()` and
tidygraph `filter()` on edges never remove nodes. This is a semantics
decision to make explicitly (see section 6, Q1).

## 3. Code quality findings (no behaviour change, but blocking under the style rules)

| Finding | Location | Rule |
|---|---|---|
| Six `for` loops: three rebuild the weight matrix cell by cell, two are O(n^2) reciprocity scans, one iterates `needed` measures | `.subset_cograph_network`, `.update_cograph_edges` (x2), `.select_edges_mutual`, `.compute_lazy_edge_metrics`, `.compute_lazy_centralities` | no `for` loops |
| Node-remap + matrix-rebuild block duplicated three times | same helpers | single `.rebuild_network()` |
| Empty-result branch duplicated in `filter_nodes`, `select_nodes`, `filter_edges`, `select_edges` | four verbs | one `.finish_result()` |
| `exists("deg", inherits = FALSE)` to reuse a local | `.compute_lazy_edge_metrics` | kludge |
| Blanket `tryCatch(... NULL/NA)` on four centralities | both centrality helpers | no silent failure |
| Bracket subsetting `edges[mask, , drop = FALSE]` is fine inside bodies; but no `as.data.frame()` for a `cograph_network` means users reach for `get_edges()` and integer ids | `class-network.R` | Rule 0 accessor |
| `message()` on every call with igraph or network input | four verbs | noisy; make it a documented one-time note or drop |
| `.keep_isolates` / `.keep_edges` dot-prefixed but `keep_format` not | all verbs | naming consistency |
| `directed` argument only honoured for non-cograph input, undocumented for the rest | all verbs | contract |

## 4. Gap analysis against igraph and tidygraph

Legend: **have** = exists as a verb; *partial* = reachable through an
argument or another function; missing = no verb.

### 4.1 Subgraph extraction

| Capability | igraph | tidygraph | cograph |
|---|---|---|---|
| Induced subgraph by node set | `induced_subgraph` | `to_subgraph` | **have** `select_nodes(name = )` |
| Subgraph by edge set | `subgraph_from_edges` | `to_subgraph(subset_by = "edges")` | **have** `select_edges` |
| Ego / neighbourhood | `make_ego_graph` | `to_local_neighborhood` | **have** `select_neighbors`; `ego_networks()` returns list of igraphs, inconsistent |
| Largest / k-th component | `largest_component`, `components` | `to_largest_component`, `to_components` | **have** `select_component`; missing `split_components()` returning a list |
| k-core | `coreness` | `node_coreness` | *partial* (`k_core` context var); missing `select_k_core(x, k)` |
| Minimum spanning tree | `mst` | `to_minimum_spanning_tree` | missing |
| Shortest-path subgraph | `shortest_paths` + subgraph | `to_shortest_path` | *partial* (`shortest_paths()` returns paths, no subgraph verb) |
| BFS / DFS tree, unfolded tree | `bfs`, `dfs` | `to_bfs_tree`, `to_dfs_tree` | missing, low priority |
| Backbone / disparity | none | none | **have** `disparity_filter` |

### 4.2 Structural transformation

| Capability | igraph | tidygraph | cograph |
|---|---|---|---|
| Directed -> undirected with weight combination | `as_undirected(mode = collapse/each/mutual)` | `to_undirected` | missing (done ad hoc inside `detect_communities`) |
| Undirected -> directed | `as_directed(mode = mutual/arbitrary/acyclic)` | `to_directed` | missing |
| Reverse edge direction | `reverse_edges` | `reroute` | missing; TNA users ask for reversed transitions |
| Complement | `complementer` | `to_complement` | missing |
| Contract nodes by group | `contract` | `to_contracted` | *partial* (`cluster_summary()$macro` is a matrix inside an analysis object) |
| Line graph | `make_line_graph` | `to_linegraph` | missing, low priority |
| Simplify loops / multi-edges | `simplify` | `to_simple` | **have** |
| Remove isolates | `delete_vertices(degree == 0)` | `filter(!node_is_isolated())` | *partial* (`.keep_isolates` flag only) |
| Permute / reorder nodes | `permute` | `arrange` | missing; needed to align plotting order |
| Rename nodes | `set_vertex_attr("name")` | `mutate(name = )` | missing |

### 4.3 Editing

| Capability | igraph | tidygraph | cograph |
|---|---|---|---|
| Add / remove nodes | `add_vertices`, `delete_vertices` | `bind_nodes` | missing (only whole-table `set_nodes`) |
| Add / remove edges | `add_edges`, `delete_edges` | `bind_edges` | missing (only whole-table `set_edges`, which drops columns and stales weights, D5/D6) |
| Mutate node attributes | `set_vertex_attr` | `mutate()` on nodes | missing |
| Mutate edge attributes | `set_edge_attr` | `mutate()` on edges | missing |
| Combine networks | `union`, `intersection`, `difference`, `disjoint_union` | `bind_graphs`, `graph_join` | missing |

### 4.4 Weight operations (network-analysis specific, neither igraph nor tidygraph verbs but standard in qgraph / tna / sna)

| Capability | Reference | cograph |
|---|---|---|
| Threshold by absolute value, proportion of edges, density, or top-N | qgraph `minimum`/`cut`, `tna::prune(threshold, lowest, percent)` | *partial* (`select_top_edges`, `splot(minimum = )` at plot time only) |
| Binarize | sna `event2dichot` | missing |
| Symmetrize (max, min, mean, sum, upper, lower) | `sna::symmetrize` | missing |
| Normalize weights (row, column, max, sum, min-max) | tna row-normalised transitions | missing |
| Invert weights (similarity <-> distance) | `1/w`, `max - w` | *partial* (`invert_weights` arg on efficiency only) |

### 4.5 Predicate vocabulary inside expressions

tidygraph exposes `node_is_*`, `edge_is_*`, `node_rank_*`, `local_*`.
cograph's filter environment has 13 centralities, 7 node context variables
and 11 edge metrics. Missing predicates that are cheap and commonly used:

- nodes: `is_isolated`, `is_source`, `is_sink`, `is_leaf`, `is_cut`
  (alias of `is_articulation`), `local_transitivity`, `local_triangles`,
  `neighbors_of(...)` as an expression helper
- edges: `is_loop`, `is_multiple`, `weight_rank`, `is_reciprocal` (alias),
  `from_community` / `to_community`
- centrality vocabulary: only 13 igraph measures while `centrality()`
  offers 87+. The lazy resolver should delegate unknown names to
  `centrality(x, measures = name)` instead of maintaining a parallel list.

### 4.6 Explicit non-goals

tidygraph's `activate()` / `morph()` / `unmorph()` pipeline is a different
paradigm (tibble-backed, dplyr masking). cograph verbs are one call with
named arguments and return a network; there is no reason to add a
context switch. Random rewiring and sampling belong with `robustness()`,
not here.

## 5. Proposed API

All verbs: `x` any supported input, return a `cograph_network` unless
`keep_format = TRUE`, classed conditions, one call with named arguments.

```r
# accessor (Rule 0)
as.data.frame(net)                          # edges with from/to labels + weight + extra cols
as.data.frame(net, what = "nodes")

# structure
to_undirected(x, method = c("max", "sum", "mean", "min", "mutual"))
to_directed(x, mode = c("mutual", "arbitrary"))
reverse_edges(x)
complement_network(x, weight = 1)
contract_nodes(x, groups, weight = c("sum", "mean", "max"), loops = FALSE)
split_components(x, min_size = 1)           # list of cograph_network
select_k_core(x, k)
spanning_tree(x, weights = c("weight", "none"), maximum = FALSE)
remove_isolates(x)
reorder_nodes(x, order)                     # names, indices, or by = "degree"
rename_nodes(x, from, to)                   # or a named vector

# editing
add_nodes(x, labels, ...)                   # ... = attribute columns
remove_nodes(x, nodes)
add_edges(x, from, to, weight = 1, ...)
remove_edges(x, from, to)
mutate_nodes(x, ...)                        # dplyr-style expressions, lazy centralities available
mutate_edges(x, ...)
bind_networks(x, y, method = c("union", "intersection", "difference"), weight = c("sum", "first", "mean"))

# weights
threshold_edges(x, minimum = NULL, maximum = NULL, proportion = NULL, density = NULL, top = NULL, absolute = TRUE)
binarize(x, threshold = 0)
symmetrize(x, method = c("max", "min", "mean", "sum", "upper", "lower"))
normalize_weights(x, method = c("row", "column", "max", "sum", "minmax"))
invert_weights(x, method = c("reciprocal", "max_minus"))
```

Existing verbs keep their names and signatures. `filter_nodes()` becomes
a thin wrapper on `select_nodes()` so both are lazy and share one
centrality resolver. `subset_*` aliases stay.

## 6. Decisions needed from the user

- **Q1 (D12).** Should `filter_edges()` / `select_edges()` keep all nodes
  by default (igraph and tidygraph semantics) and expose
  `remove_isolates()` for the current behaviour? Recommended: yes, with a
  one-release soft change: `.keep_isolates = NULL` means "keep, but warn
  if isolates were created", then flip the default.
- **Q2 (D8).** For tna input with `keep_format = TRUE`, return a rebuilt
  `tna` object (copy, swap `$weights`, subset `$labels` and `$inits`)?
  Recommended: yes; this is the tna convention already used elsewhere.
- **Q3.** Should the `message("Result converted to cograph_network ...")`
  be dropped? Recommended: yes; document once in `?filter_edges`.
- **Q4.** Argument naming: keep `.keep_isolates` / `.keep_edges` with the
  dot, or rename to `keep_isolates` / `keep_edges` with soft-deprecated
  aliases? Recommended: rename; the dot convention exists to avoid
  clashing with `...` expression names, which `match.arg`-style
  named-argument matching already prevents.

## 7. Implementation plan

### Phase 0: safety net (before touching logic)

1. Add `tests/testthat/test-wrangling-invariants.R` with property tests
   that currently **fail** and pin the defects: symmetry of undirected
   results (D2), trailing-isolate survival (D1), negative-weight tolerance
   (D3), empty + `keep_format` (D4), `set_edges` then `to_matrix` (D5),
   extra edge columns round-trip (D6), metadata survival (D7), unknown
   `by` errors (D9), malformed `between` errors (D10).
2. Snapshot the current print output of one result per verb so the
   refactor cannot silently change formatting.

### Phase 1: root-cause fixes (all defects, no new verbs)

1. **`network_to_igraph()`**: build from `graph_from_data_frame(edges,
   vertices = nodes)`. Fixes D1 for every caller. Carries extra edge
   columns as igraph edge attributes.
2. **One rebuild helper** `.rebuild_network(net, nodes_idx, edges)` that
   remaps indices with `match()`, writes the weight matrix with
   `w[cbind(from, to)] <- weight` and mirrors it when undirected (D2),
   and carries `node_groups`, `data`, `meta$layout`, `meta$source` (D7).
   Replace the three duplicated blocks and all six loops.
3. **`set_edges()` / `set_nodes()`** rebuild or invalidate `$weights` and
   keep extra columns (D5, D6). `parse_edgelist()` forwards extra columns.
4. **One centrality resolver** used by both filter and select paths, with
   `match.arg()` on `by` (D9), negative-weight guard returning NA with a
   classed warning `cograph_negative_weights` (D3, D11), and `tryCatch`
   narrowed to the specific igraph condition classes.
5. **Empty results**: one `.finish_result()` helper handling
   `keep_format` for every input class including the empty matrix (D4)
   and tna (D8, pending Q2).
6. **Validation**: classed errors `cograph_bad_selection` for malformed
   `between`, out-of-range indices, non-integer indices, unknown names
   (D10). Warnings only for partial matches.
7. Re-run Phase 0 tests; all must pass. Run the existing
   `test-coverage-network-utils-40/42.R` and `test-simplify.R`.

### Phase 2: accessor and consolidation

1. `as.data.frame.cograph_network(x, what = c("edges", "nodes"))` with
   labelled endpoints; point `print.cograph_network` at it.
2. `filter_nodes()` delegates to `select_nodes()`; `filter_edges()`
   delegates to `select_edges()`. Remove the eager helper.
3. Decide Q1, Q3, Q4 and apply.
4. Documentation: one `?network_wrangling` topic page listing the family,
   a pkgdown section "Wrangling", and cross-links from `ego_networks`,
   `shortest_paths`, `disparity_filter`.

### Phase 3: new verbs, in priority order

Each verb ships with roxygen (`@return` structure, failure modes,
`@references` where a method is cited), a formula test against igraph or
sna on the same synthetic input, an invariant test, and an error-path
test by class.

1. Weight verbs: `threshold_edges`, `symmetrize`, `normalize_weights`,
   `binarize`, `invert_weights`. Reference: qgraph `minimum`/`cut`,
   `sna::symmetrize`, tna row normalisation. Highest user demand.
2. Structure: `to_undirected`, `to_directed`, `reverse_edges`,
   `remove_isolates`, `contract_nodes`, `split_components`,
   `select_k_core`. Reference: igraph equivalents, equality tested on
   adjacency matrices.
3. Editing: `add_nodes`, `remove_nodes`, `add_edges`, `remove_edges`,
   `mutate_nodes`, `mutate_edges`, `rename_nodes`, `reorder_nodes`,
   `bind_networks`.
4. Predicate vocabulary additions (section 4.5) and delegation of unknown
   centrality names to `centrality()`.
5. Low priority: `spanning_tree`, `complement_network`, line graph, BFS /
   DFS trees.

### Phase 4: close out

1. `R CMD check --as-cran` clean; `lintr` clean on touched files.
2. Update `NEWS.md` (bug fixes D1-D12 are user-visible), `docs/CHANGES.md`,
   `docs/LEARNINGS.md`, `HANDOFF.md`.
3. Bump `DESCRIPTION` version per the repo rule.
4. Edit the existing `vignettes/introduction.Rmd` wrangling section to use
   the verbs (no new vignette).

## 8. Effort estimate

| Phase | Files touched | Size |
|---|---|---|
| 0 | 1 new test file | small |
| 1 | `input-igraph.R`, `network-utils.R`, `class-network.R`, `input-edgelist.R` | medium; D1 and D2 are one-line root causes, the helper consolidation is the bulk |
| 2 | `network-utils.R`, `methods-print.R`, `_pkgdown.yml`, one Rd topic | small |
| 3 | one new file per group (`wrangle-weights.R`, `wrangle-structure.R`, `wrangle-edit.R`) + tests | large; do group 1 first, ship, then 2 and 3 |
| 4 | docs | small |

---

## 9. Status (2026-09-09)

All four phases are done. Released as part of 2.6.0.

### Phase 0 — safety net

`tests/testthat/test-wrangling-invariants.R`: 29 property tests written
against the defects. On the pre-fix tree they produced **31 failures and 6
errors**; on the fixed tree, 61 expectations pass.

D3 needed no fix: the igraph removal (2.4.9) had already replaced the
unguarded `igraph::betweenness()` call, so `filter_nodes()` on negative
weights now warns (`cograph_negative_weights`) and returns NA.

### Phase 1 — root causes

| Defect | Fix |
|---|---|
| D1 | `network_to_igraph()` builds `make_empty_graph(n)` then `add_edges()` |
| D2 | `.network_weight_matrix()` mirrors the entries when undirected |
| D4 | `.empty_cograph_network()` stores a 0x0 matrix; `.finish_result()` is the single keep_format path |
| D5 | `set_edges()`/`set_nodes()` rebuild `$weights`; extra columns kept |
| D6 | `parse_edgelist()` forwards extra columns; the constructor keeps them at 0 rows too |
| D7 | `.rebuild_network()` carries groups, `$data`, layout and source |
| D8 | `.rebuild_tna()` copies the model and swaps weights, labels, inits |
| D9, D10 | `cograph_bad_selection` errors from `.validate_indices()`, `.resolve_node_selection()`, `.validate_measure()`, `.validate_edge_metric()` |
| D11 | one resolver, `.node_filter_vars()`; `filter_nodes()` is lazy |
| D12 | `keep_isolates = TRUE` default plus a `cograph_isolates_created` warning (user decision, Q1) |

Code quality: the six `for` loops and three duplicated rebuild blocks are gone,
replaced by `.rebuild_network()` + `.network_weight_matrix()`; the constructor's
column loop is a single frame assignment.

### Phase 2 — accessor and consolidation

`as.data.frame.cograph_network(x, what = c("edges", "nodes"))`; the print
method points at it. Q2 (rebuild tna), Q3 (drop the message) and Q4 (rename
`.keep_isolates`/`.keep_edges`, dotted names soft-deprecated) applied.

### Phase 3 — new verbs

24 new exports across `R/wrangle-weights.R`, `R/wrangle-structure.R`,
`R/wrangle-edit.R`, plus `?network_wrangling` in `R/wrangle-overview.R`. Every
gap in section 4 is closed except the deliberate non-goals and the low-priority
line graph and BFS/DFS trees.

Vocabulary (section 4.5): node predicates `is_isolated`, `is_source`,
`is_sink`, `is_leaf`, `is_cut`, `local_transitivity`, `local_triangles`; edge
predicates `is_loop`, `is_multiple`, `is_reciprocal`, `weight_rank`,
`from_community`, `to_community`; unknown measure names delegate to
`centrality()`, so all 191 measures are reachable from an expression and from
`by =`.

### Phase 4 — evidence

- New tests: 265 expectations across `test-wrangling-invariants.R`,
  `test-wrangle-weights.R`, `test-wrangle-structure.R`,
  `test-wrangle-edit.R`, `test-wrangle-vocabulary.R`. All pass.
- Existing tests updated where they pinned a behaviour this plan changed:
  `test-coverage-network-utils-40.R` (4 blocks), `-42.R` (10 blocks),
  `test-coverage-round5.R` (1), `test-port-adjacent.R` (1).
- `R CMD check --as-cran` on a tree without the previous session's uncommitted
  plot-mcml files: **0 errors, 0 warnings, 1 note** (the environmental
  "unable to verify current time").

### Found but not fixed

`parse_matrix()` reads an undirected matrix from the strict upper triangle, so
a non-zero diagonal never becomes an edge — while `.cg_graph()` keeps the
diagonal, so `centrality()` counts loops that `as_cograph()` and `splot()`
never see. Fixing it changes what `splot()` draws for any undirected matrix
with a diagonal, so it is left as a decision rather than folded into this work.
