# Session Handoff — 2026-04-08

## Completed

### Centrality expansion: Batches 3, 4, 5, 6

Added **12 new centrality measures + 4 new top-level functions** across four numbered batches, all with bit-exact validation against established references wherever the references are well-defined. Committed as 17 per-feature commits on `main`, all pushed to `origin` (mohsaqr/Sonnet), `cograph` (mohsaqr/cograph), and `upstream` (sonsoleslp/cograph). Current HEAD: `58d2491`.

Running count: **87 per-node centrality measures** (up from 75 at session start) + **4 new standalone graph/set/pair-level functions**.

#### Batch 3 — Classical per-node measures via `centrality()`

| Measure | Reference | Status |
|---|---|---|
| `centrality_katz()` | `centiserve::katzcent`, `igraph::alpha_centrality(exo=1)`, `nx.katz_centrality_numpy` | BIT-EXACT vs centiserve |
| `centrality_hubbell()` | `centiserve::hubbell` (with explicit weights) | BIT-EXACT |
| `centrality_information()` | `sna::infocent` | BIT-EXACT |
| `centrality_pairwisedis()` | `centiserve::pairwisedis` | BIT-EXACT |
| `centrality_reaching_local()` | `igraph::harmonic_centrality` + `nx.local_reaching_centrality` | BIT-EXACT |
| `reaching_global()` (graph-level) | `nx.global_reaching_centrality` | machine epsilon |

**Key implementation move**: for measures involving `solve()`, cograph mirrors the reference's exact LAPACK call sequence so results are bit-identical (not just "numerically close"). E.g., `calculate_katz` uses `solve(I - α A^T) %*% matrix(1, n, 1)` to match centiserve, not the more stable `solve(M, b)` form.

Commits: `bf3a929`, `736a593`, `c14733c`, `4cf8459`, `583af2d`, `09c86cb`.

#### Batch 4 — Directed prestige family (Wasserman-Faust / sna)

| Measure | Reference | Status |
|---|---|---|
| `centrality_prestige_domain()` | `sna::prestige(cmode = "domain")` | BIT-EXACT (12 random directed) |
| `centrality_prestige_domain_proximity()` | `sna::prestige(cmode = "domain.proximity")` | BIT-EXACT on strongly connected; **correct where sna has a bug** |

**sna bug discovered**: `sna::prestige(cmode = "domain.proximity")` has a `FALSE * Inf = NaN` trap in its denominator calculation that zeros every node on any graph with an unreachable pair. cograph uses `is.finite()` masking before summing and produces the mathematically correct values on every directed graph. The divergence is documented in the `@section` "Divergence from sna".

Commits: `2aee770`, `e665386`, `13afcb9`.

#### Batch 5 — Gould-Fernandez brokerage (5 roles)

All five brokerage roles added as per-node measures in `centrality()`, following the existing community-aware pattern (require `membership` arg, warn+NA if missing):

| Role | sna label | Interpretation |
|---|---|---|
| `brokerage_coordinator` | w_I | A → A → A (all in broker's group) |
| `brokerage_itinerant` | w_O | A → B → A (broker outside, endpoints inside) |
| `brokerage_representative` | b_IO | A → A → B (ingroup broker mediating outward) |
| `brokerage_gatekeeper` | b_OI | A → B → B (broker on in-group receiving from outside) |
| `brokerage_liaison` | b_O | A → B → C (all three in different groups) |

**Key empirical finding**: sna's `.C("brokerage_R", ...)` has no R-level source. By working backward from sna's outputs, I derived the counting rule: **open 2-paths only** — triads where a direct edge `a -> c` already exists are excluded (matches the Gould-Fernandez convention that a broker mediates a relationship that doesn't exist directly). Bit-exact vs `sna::brokerage$raw.nli` across 20 random directed graphs.

Commit: `5560705`.

#### Batch 6 — New-API standalone functions (graph / set / pair level)

These measures don't fit the per-node `centrality()` data frame and live as top-level functions in `R/network-summary.R`:

| Function | Scope | Reference | Status |
|---|---|---|---|
| `estrada_index(g)` | graph → scalar | `nx.estrada_index` | max rel 3.6e-15 (10 graphs) |
| `trophic_incoherence(g)` | graph → scalar | `nx.trophic_incoherence_parameter` | max rel 1.7e-16 |
| `group_centrality(g, nodes, "closeness")` | set → scalar | `nx.group_closeness_centrality` | BIT-EXACT (18 graphs) |
| `group_centrality(g, nodes, "degree")` (+mode=in/out) | set → scalar | `nx.group_*_degree_centrality` | BIT-EXACT |
| `group_centrality(g, nodes, "betweenness")` | set → scalar | textbook Everett-Borgatti 1999 | **textbook-exact** (diverges from NX) |
| `dispersion(g, u = NULL, v = NULL)` | pair → scalar/vec/df | `nx.dispersion` | BIT-EXACT (156/156 karate edges) |

**Internal consistency check**: `estrada_index(g) == sum(centrality(g, measures = "subgraph")$subgraph)` at ~1e-11 absolute across 10 random graphs (both paths compute trace of matrix exp via different routes).

**Third documented divergence found**: NetworkX's `group_betweenness_centrality` uses the Puzis-Yahalom-Elovici iterative algorithm, which produces results inconsistent with its own stated "at least one node in C" definition on graphs with overlapping shortest paths. I verified this via an independent Python brute-force (`/tmp/python_any_gbc.py`): on a 12-node strongly connected graph, NX gives 10.083 while the textbook formula gives 9.333 — a 7.5% relative difference, not rounding noise. cograph matches the textbook; the divergence is documented in the roxygen "Divergence from NetworkX on betweenness" section.

Hand-verified betweenness cases (all exact):
- 4-cycle, `C = {2}`: 3.0 ✓
- 4-cycle, `C = {2, 3}`: 1.0 ✓
- 6-node directed, `C = {1, 2}`: 7.5 ✓

Commits: `6544531`, `c7a1cd6`, `83d16c7`, `9040d12`, `58d2491`.

---

### Three documented divergences where cograph is more correct than the reference

1. **`prestige_domain_proximity`** vs `sna::prestige`: sna's `FALSE * Inf = NaN` zeros all nodes on graphs with unreachable pairs.
2. **`hubbell`** vs `centiserve::hubbell`: centiserve silently ignores `E(g)$weight` when `weights = NULL`, producing wrong values on weighted graphs unless weights are passed explicitly.
3. **`group_centrality("betweenness")`** vs `nx.group_betweenness_centrality`: NX's Puzis iterative algorithm diverges from the textbook definition on graphs with overlapping shortest paths.

All three are flagged in roxygen docs with explicit "Divergence from reference" sections and accompanying tests that assert cograph's correct behavior.

---

### Test coverage

- `tests/testthat/test-centrality-batch3.R` — **467 tests, all passing** (53 test groups covering Batches 3, 4, 5, and Batch 6 measures plus deterministic hand-verified cases)
- All prior test files (`test-centrality-extended.R`, `test-centrality-zoo.R`, `test-coverage-centrality-40.R`) continue to pass
- Bit-exact assertions use `expect_identical()` where truly integer-valued; tight numerical tolerances (`1e-13` or smaller) elsewhere

---

### CLAUDE.md / README / NEWS refresh

- `CLAUDE.md` updated: 75 → 87 centralities, mention of all 4 batches, three divergences flagged
- `README.md` / `README.Rmd`: "23+ centrality measures" → "87 centrality measures, validated against centiserve/sna/igraph/NetworkX"
- `NEWS.md`: consolidated dev-version section at top with per-batch bullets; preserved 1.8.2 entry below
- Main `centrality()` roxygen: total count updated, new classes surfaced, Community-aware subsection now includes the 5 brokerage roles

---

## Current State

### What works

- `devtools::load_all(".")` — loads cleanly, no errors or warnings (apart from pre-existing `core_periphery.Rd` newline warning unrelated to this session)
- `devtools::document(".")` — regenerates all Rd files and NAMESPACE cleanly; all new functions export correctly
- `devtools::test(".")` for `test-centrality-batch3.R` — 467/467 passing, ~16–30 seconds runtime
- All three remotes (`origin`, `cograph`, `upstream`) in sync at `58d2491`

### What's broken / partially done

- **Nothing broken.** All intended work is complete and committed.
- **Not yet done** (see Next Steps): comprehensive `R CMD check --as-cran` pass, stress-test script across all new measures, and consolidation of the batch3 test file (which is starting to sprawl).

### Files modified this session (summary)

- `R/centrality.R` — 7 new wrapper functions + dispatcher updates + master docstring refresh (~700 lines added)
- `R/centrality-extended.R` — 6 new `calculate_*` functions including the 5-role brokerage helper (~430 lines added)
- `R/network-summary.R` — 4 new top-level functions (`estrada_index`, `trophic_incoherence`, `group_centrality`, `dispersion`) with 3 internal helpers (~400 lines added)
- `tests/testthat/test-centrality-batch3.R` — **new file**, 467 tests, ~530 lines
- `man/*.Rd` — 12 new auto-generated Rd files
- `NAMESPACE` — 12 new exports
- `NEWS.md` — dev-version section added with 4 batch sub-sections
- `CLAUDE.md` — centrality description rewritten, divergences flagged
- `HANDOFF.md` — this file
- `README.md` / `README.Rmd` — centrality count updated

---

## Key Decisions

1. **"Absolute equivalence" interpretation**: After the user said "I need absolute equivalence", I rewrote 3 Batch 3 calculators (`calculate_katz`, `calculate_hubbell`, `calculate_information`) to mirror their references' exact LAPACK call sequences, trading a slightly higher-precision delegation-to-igraph for bit-identical match with centiserve/sna. The tests use `expect_identical` (not `expect_equal`), which catches any future ULP-level regression.

2. **Per-measure commits**: The user explicitly asked to "divide them and update and commit", so each measure (or closely-coupled pair, in Batch 5's case) gets its own commit. This enables clean `git bisect` across the expansion and a much more useful history than one mega-commit would give.

3. **Textbook over reference**: When NetworkX's `group_betweenness_centrality` disagreed with my implementation, I wrote an independent Python brute-force of the textbook Everett-Borgatti formula and confirmed cograph matches the textbook, not NX. cograph sides with the published definition; the divergence is documented but not "worked around" by matching NX's bug.

4. **Native implementations, no runtime sna/centiserve dep**: All Batch 3/4/5 measures use igraph primitives + base R, with sna/centiserve/NetworkX only invoked by the *tests* (as `skip_if_not_installed()` gates). cograph doesn't pull in any new runtime dependencies.

5. **Brokerage API**: For the 5 Gould-Fernandez roles, I chose "5 separate measures in `centrality()`" over "1 top-level `brokerage()` function returning a matrix" because it fits the existing per-node data-frame contract and the existing community-aware measure pattern (participation, within_module_z, gateway).

6. **New-API functions in `R/network-summary.R`**: `estrada_index`, `trophic_incoherence`, `group_centrality`, `dispersion` all live in `network-summary.R` rather than getting their own file, since they share the "graph → scalar/summary" conceptual scope with the existing `network_summary()` family.

---

## Open Issues

None blocking. The following are deferred but not urgent:

- **Suggested `R CMD check --as-cran`** on the expanded package. 12 new exported functions, all with `@examples`. Likely clean (docs regenerate cleanly, namespace is consistent), but not yet verified as CRAN-ready.
- **Comprehensive stress test** across Batches 3+4+5+6 on diverse graph topologies (karate, random GNP, BA, WS, SBM, grid, paths, cycles, stars, trees). I ran targeted stress tests during development but haven't bundled them into a single reproducible script or added them as vignettes.
- **NetworkX `group_betweenness` bug report**: The Puzis-Yahalom-Elovici algorithm divergence from the textbook definition is a real NX bug that could be filed upstream at `networkx/networkx`. Out of scope for this session but worth mentioning in a future write-up.
- **test-centrality-batch3.R file is sprawling** (467 tests, ~530 lines, 53 test groups covering 4 batches). Should probably be split into `test-batch3.R`, `test-batch4.R`, `test-batch5.R`, `test-batch6.R` for readability, but this is cosmetic.
- **Zoo coverage**: Still ~82/409 (~20%). Remaining gaps require new APIs or are single-paper measures without accessible reference implementations. The investigation phase (documented mid-session) enumerated all the candidates; none are "just add to the dispatcher" anymore.

---

## Next Steps

Priority order if the next session picks up centrality work:

1. **CRAN readiness check** — run `rcmdcheck::rcmdcheck(".", args = c("--as-cran", "--no-manual"))` and fix any NOTEs/WARNINGs introduced by the 12 new functions and their examples.
2. **Full validation stress script** — bundle `/tmp/batch3_stress.R`, `/tmp/new_measures_validation.R`, and `/tmp/batch6_full_validation.R` into a single `scripts/validate_centrality.R` that covers Batches 3+4+5+6 in one reproducible pass, and store the expected-output snapshot.
3. **Split `test-centrality-batch3.R`** into 4 files by batch for readability.
4. **File NetworkX bug report** for `group_betweenness_centrality` vs the Puzis algorithm divergence, with the 12-node reproducing case from this session.
5. **Future Zoo additions** (deferred — all require new APIs or license verification): DomiRank (no license), ViralRank (no code found), Expected Force (non-commercial license), Shapley value centrality (no reference impl), EigenTrust (specialized), brainGraph hubness (niche composite).

Alternatives (completely different direction):
- Plot-side work: new layouts, styling improvements, or the deferred multi-network meta-visualization from `docs/TODO.md`
- Vignette pass: no new vignettes were written for the Batch 3+4+5+6 expansion; users won't discover the new measures without them
- `cluster_quality` / `cluster_significance` tightening
- Anything in `docs/TODO.md` that's become relevant

---

## Context

### Environment
- R 4.5.2, macOS Darwin
- cograph package loaded via `devtools::load_all()` for all dev work
- sna 2.8, centiserve 1.0.0, brainGraph 3.1.1, igraph 2.2.2, reticulate 1.43.0, tidygraph 1.3.1, influenceR 0.1.5
- NetworkX available via reticulate (Python 3.14)

### Prerequisites for the new functions
- All Batch 3/4/5/6 measures use only `igraph` + base R at runtime (no sna/centiserve/NetworkX dependency outside of tests, which are properly gated with `skip_if_not_installed()`)
- `centrality_brokerage_*()` measures require `membership` argument; warn + NA if missing (matching the existing community-aware pattern)
- All directed-only measures (`pairwisedis`, `prestige_domain`, `prestige_domain_proximity`, `brokerage_*`, `trophic_incoherence`) warn + NA (not error) on undirected input, so they coexist peacefully with `measures = "all"`

### Git remotes (all pushed and in sync at `58d2491`)
- `origin` → `github.com/mohsaqr/Sonnet.git`
- `cograph` → `github.com/mohsaqr/cograph.git`
- `upstream` → `github.com/sonsoleslp/cograph.git`

### Reference validation strategy (pattern to follow for future measures)
1. Inspect the reference implementation on a small synthetic graph
2. Match the reference's exact call sequence (LAPACK path for linear-algebra measures, enumeration order for combinatorial measures) to get bit-exact agreement
3. Write an independent brute-force (in R or Python) for the textbook definition
4. If reference and brute-force agree → use `expect_identical`
5. If they disagree → investigate; side with the textbook; document the divergence in roxygen
6. Commit each measure separately with its tests so `git bisect` remains useful

### Session artifacts created in /tmp (can be regenerated from the committed code)
- `/tmp/batch3_validation.R`, `/tmp/batch3_stress.R` — Batch 3 stress tests
- `/tmp/new_measures_validation.R` — Batch 5 validation report
- `/tmp/batch6_full_validation.R` — Batch 6 validation report
- `/tmp/python_any_gbc.py` — independent Python brute-force of textbook group betweenness (the critical oracle that resolved the NX divergence)
- `/tmp/brokerage_verify.R` — the empirical derivation of sna's "open 2-paths only" counting rule

These can be re-derived from the test file any time; they're not needed for ongoing work.
