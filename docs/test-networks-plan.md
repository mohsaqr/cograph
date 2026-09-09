# Test network corpus: plan

## Status (2026-09-08, evening)

Built and verified. Counts are from the manifests written by the build
scripts this session.

| Tier | Networks | Where | Size |
|---|---|---|---|
| real_small | 32 | `tests/testthat/networks/` (committed) | 19 KB |
| degenerate | 35 | `tests/testthat/networks/` (committed) | 4 KB |
| real_large | 7 | local only | 338 KB |
| synthetic_scale | 72 | local only | 1.6 MB |
| icon | 1,075 catalogued, 914 stored as matrices (n <= 3000) | local only | 1.7 MB + 22 MB raw |

Across all stored tiers: 1,060 matrices, of which 131 directed and
weighted, 5 signed, 19 with self-loops, 7 multigraphs, 116 bipartite, 98
above 1,000 nodes. Access: `test_networks(tier = , directed = , weighted = ,
signed = , min_n = , max_n = )` and `test_network(name)` in
`tests/testthat/helper-networks.R`. Verification:
`tests/testthat/test-networks-manifest.R`, 28 expectations, 0 failures.

Deviations from the plan below: ICON was added as a fifth tier after the
plan was written (source: the 1,075 raw edge lists behind the archived ICON
R package, mirrored from GitHub). The 19 byte-identical copies of the tiny zoo in
`batch31` to `batch51` were deleted (Q4 done); the origin
`batch30/matrices.rds` is kept and each batch's `run_equivalence.R`
regenerates its copy from it. Q2 (a public dataset in
`data/`) was not done. Tier D stops at n = 1000, not 5000.


Date: 2026-09-08. Companion to `docs/igraph-removal-plan.md` (Phase 0
needs this corpus) and `docs/network-wrangling-plan.md` (its invariant
tests need the degenerate cases). Every source below was checked as
installed and loadable on this machine this session; sizes are from
`igraph::vcount()` / `ecount()` on the loaded objects.

## 1. What exists and what is missing

| Stash | Count | Real? | Directed | Weighted | Signed | Loops | Multi | > 81 nodes | Where |
|---|---|---|---|---|---|---|---|---|---|
| Packaged `data/` | 1 network (`student_interactions`, 389-row edge list) + 6 sequence tables | yes | edge list | no | no | ? | ? | no | package |
| Correlation-audit networks | 19 igraph objects | yes | **no** (projected) | **no** (projected) | no | no | no | no | ignored local dir |
| Exhaustive tiny zoo | 5,532 matrices, byte-identical copy in 22 batch dirs | no | yes | **no** | no | ? | no | no | ignored local dir |
| Batch generated sets | 104 to 410 per batch, 13 files | no | mixed | mixed | no | ? | no | no | ignored local dir |
| `create_test_topology()` | 5 shapes | no | no | no | no | no | no | no | test helper |
| Kernel equivalence zoo | 280 combos, built at test time | no | yes | yes | no | no | no | no | test file |

Gaps that no stash covers today: weighted real networks, directed real
networks, anything above 81 nodes, self-loops, negative or mixed-sign
weights, multi-edges, a bipartite graph kept bipartite, near-ties and
extreme weight scales, and a manifest that says what each network is.

## 2. Sources verified available

| Source | Version checked | Networks usable | Notes |
|---|---|---|---|
| `igraphdata` | installed | karate (34, **weighted**), kite (10), UKfaculty (81, **directed weighted**), macaque (45, directed), Koenigsberg (4, **multigraph**), rfid (75, multi 32,424 edges), enron (184, directed, **loops + multi**), USairports (755, directed, loops + multi), immuno (1,316), yeast (2,617), foodwebs (20 originals, **directed weighted with loops**) | The audit used only the projections; the originals are richer |
| NetworkX via reticulate | 3.6.1 | les_miserables (77, weighted), florentine (15), davis (32, bipartite), karate_club, petersen (10), krackhardt_kite, dodecahedral (20) | classic named graphs, offline |
| `tna` | installed | `tna(group_regulation)$weights` 9 x 9 **directed weighted transition matrix**, `tna(engagement)` 3 x 3 | the canonical TNA case; row-stochastic |
| `Nestimate` | installed | `chatgpt_srl` (1000 x 5) -> partial-correlation network, **8 negative weights** verified; `learning_activities`, `srl_strategies` for larger psych networks | signed undirected via `build_network(method = "pcor" / "glasso")` |
| `qgraph`, `psych` | installed | `big5` (25-node correlation / GLASSO network), `bfi` | signed undirected, the qgraph reference case |
| `ergm` | installed | flomarriage + flobusiness (two relations on 16 nodes), samplk1-3 + samplike (18, directed, three waves), faux.mesa.high (205), faux.magnolia.high (1,461), faux.desert.high, ecoli1/2, kapferer, molecule | attributes on nodes (grade, sex) for group tests |
| `sna`, `network` | installed | coleman (73, two waves, directed), flo, emon (7 emergency networks) | |

Not installed, not needed: `networkdata`, `statnet.data`.

## 3. Corpus design

### 3.1 One format, one manifest

Every network is stored as a **dense numeric adjacency matrix with
dimnames**, the native cograph representation, plus one manifest row:

```
name, tier, family, source, source_version, license,
n, m, directed, weighted, signed, has_loops, has_multi, n_components,
n_isolates, bipartite, min_weight, max_weight, sha256, projection_of
```

Multigraphs are stored twice: as the collapsed count matrix (the
canonical entry) and as the raw edge list with duplicate rows
(`<name>__edgelist`), because the wrangling verbs and `simplify()` need
the duplicates. Projections are stored as separate named entries with
`projection_of` set, never in place of the original.

`sha256` is over `serialize(matrix, NULL, version = 3)`. The build script
refuses to overwrite a manifest row whose hash changed unless run with
`--rehash`, and prints the diff. This is how a silently changed upstream
dataset (igraphdata converting "older igraph version" objects on the fly,
which it does today for every load) is caught.

### 3.2 Four tiers

**Tier A — real, small, committed** (`tests/testthat/networks/real_small.rds`).
Budget 300 KB compressed; measured at build time and asserted by a test.

| Name | n | Direction | Weights | Why it is here |
|---|---|---|---|---|
| karate | 34 | undirected | weighted | weighted undirected classic; the audit stored it unweighted |
| kite | 10 | undirected | binary | textbook centrality disagreement (degree vs betweenness vs closeness) |
| florentine_marriage, florentine_business | 16 | undirected | binary | two relations on the same nodes; one isolate (Pucci) |
| davis | 32 | undirected | binary | genuine bipartite, kept two-mode; also the projected one-mode |
| les_miserables | 77 | undirected | weighted (co-occurrence counts) | integer weights, heavy tail |
| macaque | 45 | directed | binary | dense directed, reciprocity |
| UKfaculty | 81 | directed | weighted | the largest directed weighted real case |
| foodweb_ChesLower, foodweb_Rhode, foodweb_StMarks | 37, 20, 54 | directed | weighted | **self-loops** (cannibalism), flows spanning orders of magnitude; Rhode has an isolate after projection |
| samplk1, samplk2, samplk3 | 18 | directed | binary | three waves of the same network for stability tests |
| coleman_wave1, coleman_wave2 | 73 | directed | binary | disconnected, isolates |
| koenigsberg | 4 | undirected | multi (counts) | the multigraph |
| tna_group_regulation | 9 | directed | weighted, row-stochastic | canonical TNA matrix; every example currently hand-builds a 4 x 4 |
| tna_engagement | 3 | directed | weighted | smallest TNA case |
| pcor_chatgpt_srl | 5 | undirected | **signed** | 8 negative entries; psych-network case |
| glasso_big5 | 25 | undirected | **signed**, sparse | qgraph reference network |
| petersen, dodecahedral | 10, 20 | undirected | binary | vertex-transitive: every centrality must be constant (invariant test) |

**Tier B — real, large, local only**
(`local_testing_and_equivalence/networks/real_large.rds`, regenerated by
script, never committed). enron (184, directed, loops, multi), rfid (75,
multi), USairports (755, directed, loops, multi), faux.mesa.high (205),
immuno (1,316), faux.magnolia.high (1,461), yeast (2,617). These are the
scale and timing cases for the igraph-removal benchmarks.

**Tier C — synthetic degenerate, committed**
(`tests/testthat/networks/degenerate.rds`, generated deterministically,
seed recorded per entry, expected size under 50 KB). One entry per
convention trap:

- `empty_0`, `single_node`, `single_edge_undirected`, `single_edge_directed`
- `self_loop_only` (1 node, 1 loop), `loops_everywhere` (5 nodes, full diagonal)
- `isolates_trailing` (last node isolated: the `network_to_igraph()` crash), `isolates_leading`, `isolates_only`
- `two_components`, `three_components_unequal`
- `star_10`, `path_10`, `ring_10`, `complete_6`, `tree_15`, `grid_4x4`
- `bipartite_k33`, `bipartite_unbalanced`
- `dag_10` (acyclic), `directed_cycle_6` (periodic: eigenvector ill-posed), `strongly_connected_8`, `weakly_only_8`
- `reciprocated_asymmetric` (A->B 0.3, B->A 0.7: mode-collapse rules), `one_way_only`
- `negative_weights_all`, `mixed_signs`, `zero_and_negative`
- `weights_1e9` (tie tolerance at scale), `weights_1e-9`, `near_ties` (0.5 vs 0.5 + 1e-13)
- `duplicate_edges__edgelist` (edge list with repeated rows, for `simplify()` and the parser)
- `unlabeled` (no dimnames), `duplicate_labels`, `non_syntactic_labels` (spaces, unicode)
- `row_stochastic_5` (TNA-style), `non_square_bipartite_incidence`

**Tier D — synthetic at scale, local only**
(`local_testing_and_equivalence/networks/synthetic_scale.rds`). For each
of n in {100, 500, 1000, 5000}: Erdős–Rényi G(n, p) at three densities,
Barabási–Albert, Watts–Strogatz, a two-block stochastic block model, and
a configuration-model graph from a power-law degree sequence; each in
undirected binary, undirected weighted (log-normal), directed binary and
directed weighted variants. Seeds fixed and recorded. Generated with
`igraph::sample_*` today (igraph is a Suggests dependency of the build
script only, never of the tests that read the RDS).

**The exhaustive tiny zoo** is kept, once: deduplicate the 22 identical
`matrices.rds` copies into `local_testing_and_equivalence/networks/exhaustive_tiny.rds`
with a manifest row and hash, and point the batch tests at that path.
Delete the copies only after the batch tests pass from the single file.

### 3.3 Access from tests

One helper in `tests/testthat/helper-networks.R`:

```r
test_networks(tier = c("real_small", "degenerate", "real_large", "synthetic_scale", "all"),
              directed = NULL, weighted = NULL, signed = NULL, min_n = 0, max_n = Inf)
# -> tidy manifest data.frame, one row per network, with a list-column `matrix`
test_network("karate")   # -> one matrix
```

Local-only tiers return zero rows with a `skip()`-friendly attribute when
the RDS is absent, so CRAN and CI without the local directory are
unaffected. Nothing goes into `data/` and nothing new is exported.

### 3.4 Build script

`local_testing_and_equivalence/networks/build_networks.R`, run with
`Rscript`, deterministic, records `sessionInfo()`, package versions and
the NetworkX version into `versions.csv`, writes the four RDS files and
`manifest.csv`, and copies only the two committed tiers into
`tests/testthat/networks/`. Ingestion rules:

- Keep originals: direction, weights, loops and multi-edges as found.
  Collapse multi-edges to counts for the matrix entry; keep the raw edge
  list alongside.
- Record `projection_of` for any derived entry (largest component,
  undirected collapse, one-mode projection).
- No `na.rm`, no silent coercion: a weight attribute that is not numeric
  stops the build with the offending network named.
- Label policy: keep source labels; add `unlabeled` variants only in
  Tier C.

## 4. Verification of the corpus itself

`tests/testthat/test-networks-manifest.R`, always run:

1. Every manifest row's `sha256` matches the stored matrix.
2. `directed` agrees with `!isSymmetric()`; `signed` with `any(m < 0)`;
   `has_loops` with `any(diag(m) != 0)`; `n_isolates` and `n_components`
   with a native BFS on the matrix (no igraph).
3. Every matrix round-trips through `as_cograph()` and `to_matrix()`
   bit-for-bit, including `isolates_trailing` (this test fails today and
   is the first fix in the wrangling plan).
4. Committed RDS sizes are under budget.
5. Vertex-transitive graphs (`petersen`, `dodecahedral`, `ring_10`,
   `complete_6`) give a constant vector for every centrality measure that
   is defined on them (property test, reused by the golden file).

## 5. Consumers, in order

1. `docs/igraph-removal-plan.md` Phase 0 golden file: Tiers A + C for the
   committed golden subset, B + D for the local full run and the timing
   table.
2. `docs/network-wrangling-plan.md` Phase 0 invariant tests: Tier C
   (`isolates_*`, `duplicate_edges`, `negative_weights_all`,
   `reciprocated_asymmetric`, `unlabeled`).
3. Existing equivalence tests that build ad-hoc matrices inline can move
   to the helper over time; not a goal of this plan.

## 6. Decisions needed

- **Q1 Committed size.** 300 KB + 50 KB is proposed. CRAN's soft limit is
  5 MB for the whole tarball; `tests/` is already 4.1 MB, so this is
  within reach but should be measured before committing.
- **Q2 One public dataset.** Add `tna_group_regulation` (9 x 9) to `data/`
  as the one real matrix for roxygen examples, replacing hand-built
  4 x 4 matrices across the docs? Recommended: yes, one dataset only, and
  a follow-up sweep of `@examples` is a separate task.
- **Q3 Licenses.** igraphdata is GPL-2+, NetworkX data is BSD-3, ergm
  data GPL-3, Nestimate/tna data as those packages state. Committed
  Tier A copies need a `LICENSE_NOTES.md` beside the RDS listing origin
  and license per network. Confirm this is acceptable for the CRAN
  tarball; otherwise Tier A stays local and CI regenerates it.
- **Q4 Deleting the 22 copies** of the tiny zoo after deduplication.

## 7. Steps and effort

| Step | Output | Size |
|---|---|---|
| 1 | `build_networks.R` with Tier A ingestion, manifest, hashing, versions | medium |
| 2 | Tier C generator (33 entries, seeds recorded) | small |
| 3 | Tiers B and D generators | small |
| 4 | `helper-networks.R` + `test-networks-manifest.R` | small |
| 5 | Deduplicate the tiny zoo, repoint batch tests, verify they pass | small |
| 6 | Size check, `LICENSE_NOTES.md`, `.Rbuildignore` unchanged (local dir already ignored), `docs/CHANGES.md`, `HANDOFF.md` | small |

Steps 1 to 3 are independent of each other and of steps 4 and 5 until
the manifest format is fixed; fix the manifest columns first, then run
them in parallel.
