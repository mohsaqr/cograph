# Motif and Subgraph Audit — Conservative Plan

Status: Gates 1 and the repository-local portion of Gate 2 are implemented and
uncommitted. No executable motif logic has been changed. No behavioral change
is authorized by this plan.

Date: 2026-09-04

## Conclusion first

The August 2026 motif overhaul already fixed the broad correctness failures.
The current directed MAN classifier, census counts, default `edge_method =
"any"` inference, individual aggregation, empirical p-values, and motif glyph
classes are protected by regression and equivalence tests.

The first draft of this plan was too broad. It treated unsupported or ambiguous
inputs as confirmed regressions and proposed new policies for directedness,
undirected motifs, actorless windows, input validation, zero-row
materialization, and plotting. The motif vignette does not establish those
policies, and they must not be introduced as fixes.

## Intended contract recovered from the vignettes

- `motifs()` and `subgraphs()` analyze the 16 MAN classes of **directed**
  triads.
- The primary workflow is a directed TNA/transition model.
- Matrix examples are asymmetric transition matrices.
- The igraph example is explicitly created with `mode = "directed"`.
- Raw edge-list examples contain a session/actor column and represent directed
  transitions from `from` to `to`.
- Windowing is documented as splitting **each actor's** ordered transitions.
- `subgraphs()` identifies which node triples participate in a MAN class. The
  vignette does not promise that a canonical class panel recovers the nodes'
  observed source/sink roles.
- The `edge_method = "expected"` tutorial example explicitly disables
  significance.
- Nestimate's cograph tutorial distinguishes directed transition estimators
  from undirected co-occurrence estimators. Separately, the cograph motif
  examples use a directed TNA transition model.

## Verified baseline that must remain unchanged

1. All 64 directed three-node adjacency patterns map to the same MAN class as
   `igraph::triad_census()`.
2. Every canonical motif glyph belongs to its labeled MAN isomorphism class.
3. `motifs(pattern = "all")` classifies every triad and its counts sum to
   `choose(n, 3)`; absent classes do not need public zero rows to satisfy that
   census identity.
4. Individual census and named-instance totals agree per MAN type.
5. The simple-graph null in `motif_census()` and the weighted directed
   stub-matching null in individual analysis remain separate, intentionally
   different null models.
6. The deterministic 1,000-dataset equivalence sweep remains at zero failures.
7. The current motif-focused test suite remains green.

## Audit disposition

### Already fixed — do not reopen

The fixes in `3c62819b` and `6e362b71` cover the earlier MAN-labeling,
directed census, 003 inclusion, instance aggregation, null-statistic,
self-loop, eligibility, fractional-stub balancing, p-value, and plot-ranking
defects. These are non-regression targets, not new work.

### Confirmed open limitations — already documented in the August handoff

The non-`"any"` cases must be kept distinct:

1. **Deferred A — named-instance statistic mismatch.** In individual
   named-instance significance (`subgraphs()` or
   `motifs(named_nodes = TRUE)`) with `edge_method = "percent"` or
   `"expected"`, the observed triad is thresholded but the optimized null path
   classifies raw stub presence. The observed and null statistics therefore do
   not measure exactly the same event.
2. **Deferred B — fractional mass scale.** For fractional transition weights,
   the support-preserving integerization used by the weighted stub null
   promotes every positive sub-0.5 value to one stub. This preserves edge
   support but can distort mass-sensitive `percent`/`expected` inference in
   unified individual census, unified individual named-instance analysis, and
   both individual and aggregate `extract_motifs()` significance. This remains
   true where a path correctly reapplies the threshold to each replicate.
3. **Accepted limitation C — aggregate threshold asymmetry.** Unified
   aggregate census significance with a non-`"any"` edge method deliberately
   delegates to the unthresholded `motif_census()` and emits a warning. This is
   a visible limitation, not one of the two deferred handoff findings; the
   tutorial does not demonstrate thresholded aggregate significance.

Default `edge_method = "any"` analyses are unaffected by these limitations.
Descriptive counts with significance disabled are also unaffected.

### Not established as defects

The following items are removed from the fixing scope unless a separate public
contract is approved:

- actorless two-column edge-list directedness;
- actorless windowing or treating overlapping windows as independent actors;
- routing unified MAN APIs to an undirected four-class census;
- adding a new public `directed` argument to `motifs()`/`subgraphs()`;
- materializing zero-count MAN rows in the public result;
- changing `percent` threshold units or defaults;
- redefining `min_count`, `min_transitions`, `top`, or include/exclude
  precedence;
- introducing new weight aliases or broad input-schema coercion;
- changing named-subgraph diagrams to claim an exact observed node-role
  orientation; and
- changing any MAN description or canonical glyph without a minimal,
  externally verified counterexample.

The observed difference between `motifs(raw_edge_list)` and
`motifs(as_cograph(raw_edge_list))` for an actorless one-way cycle is real, but
the vignette does not define the intended semantics of that ambiguous generic
input. It is therefore a contract question, not evidence of incorrect motif
directedness.

## Conservative fixing plan

### Gate 1 — Freeze the known-good behavior

Before any implementation proposal:

- retain the 64-pattern MAN oracle and the 1,000-dataset sweep;
- add small vignette-contract tests for directed TNA, directed igraph,
  asymmetric matrix, grouped edge list, and per-actor windows;
- pin seeded matrix-versus-directed-igraph equality;
- pin actor-column auto-detection and explicit actor override;
- pin all four pattern filters plus include/exclude precedence;
- pin one row per `(node triple, MAN type)` and session-occurrence meaning for
  individual `subgraphs()` results;
- pin default `edge_method = "any"` result tables and seeded inference on
  representative aggregate and individual fixtures;
- pin non-vacuous canaries for the triad-total `percent` denominator and the
  three documented non-`"any"` inference boundaries;
- record that no executable motif logic differs before work begins.

This gate adds tests only and must not alter output.

### Gate 2 — Clarify documentation only

Without changing runtime behavior, make the supported boundary explicit:

- unified `motifs()`/`subgraphs()` are directed MAN analyses;
- edge-list examples are directed transitions, and windowing is per actor;
- aggregate non-`"any"` significance uses the unthresholded null and warns;
- individual named-instance non-`"any"` significance has the documented null
  limitation; and
- `percent` uses the current six-edge triad-total denominator; the vignette's
  statement that it uses all outgoing transitions from a node is documentation
  drift and does not authorize a runtime change;
- “configuration model” must distinguish the aggregate simple-graph,
  degree-preserving rewiring null from the individual weighted target-stub null
  with integerized margins and loopless simple projection; these models are not
  interchangeable;
- concrete labels in an aggregated subgraph panel identify participating
  nodes, not their observed node-role orientation.

The dedicated tutorial at
`/Users/mohammedsaqr/Documents/Github/cograph_local_artifacts/tutorials/cograph-tutorial-motifs.qmd`
is an external artifact and is not writable or committable from this cograph
worktree. It still contains the broad “exact configuration model” wording and
the incorrect per-source description of `percent`. Documentation completeness
remains pending until that source is synchronized before its next publication.

The vignette's `201` label (“Double mutual”) and the internal description
(“Mutual + in-star”) are also wording drift. Reconcile the label only if a
single presentation term is desired; do not alter classification. The prior
decision not to swap `111D`/`111U` remains intact.

If the repository-local warning and documentation are judged sufficient, the
repository-local portion of this gate can close with no further changes. The
external tutorial synchronization remains pending as stated above.

### Gate 3 — Statistical design decision (requires explicit approval)

Choose one policy for deferred A and B only:

1. **Restrict inference:** reject or disable significance for affected
   `percent`/`expected` paths while retaining descriptive counts; or
2. **Implement matched inference:** make every null replicate reconstruct
   weighted edge multiplicities and apply the identical thresholding statistic,
   with a separately approved rule for fractional-weight integerization.

Neither option is selected by this audit. Both change behavior and require
owner approval plus statistical review. Accepted limitation C remains
documentation-only unless it receives its own, separate approval decision.

Any new mass-preserving conversion must be isolated to non-`"any"` inference.
It must not modify the existing `.motif_configuration_stubs()` behavior or RNG
sequence used by default `edge_method = "any"` analyses.

### Gate 4 — If, and only if, Gate 3 is approved

- write failing tests for the two already documented reproductions;
- implement the smallest path-specific change rather than refactoring every
  motif wrapper;
- rerun all motif tests, all 64 MAN patterns, the 1,000-dataset sweep, and the
  relevant vignette examples;
- report exact before/after deltas only for non-`"any"` inference; and
- stop if any default count, MAN label, supported directedness result, or
  default seeded inference changes unexpectedly.

## Release acceptance criteria

- Default `edge_method = "any"` behavior is unchanged.
- Default `"any"` seeded RNG results are unchanged.
- MAN classification and directedness in all documented workflows are
  unchanged.
- No public signature or result schema changes without separate approval.
- Any approved statistical change is limited to the explicitly approved
  non-`"any"` significance paths and is documented in NEWS.
- The external dedicated motif tutorial is synchronized before publication;
  until then, documentation completeness is explicitly pending.
- All motif tests and equivalence sweeps pass.

## Explicit non-goal

This plan does not redesign motifs, broaden their domain, or reinterpret old
results. It protects the fixes already landed and leaves the two previously
deferred statistical choices behind an approval gate.
