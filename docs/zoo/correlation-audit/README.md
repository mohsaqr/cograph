# Centrality correlation audit — 2026-09-07

For the wider implementation and source-verification workflow, see the
[next-agent guide](../NEXT-AGENT-GUIDE.md).

Open [the interactive report](index.html) or [the exportable heatmap](candidate_heatmap.png).
Implementation remains paused at **48/160 candidates**, with 112 pending.
This audit adds no centrality implementation and grants no new equivalence status.

## Findings

- Considered 177 native defaults and three candidate parameter variants across
  19 packaged networks (10–81 nodes). The 48 covered candidate labels map to
  47 distinct configurations; BG-index and beta-measure share one configuration.
- 165 native measures produced usable rankings somewhere. There are 68 native
  pairs with absolute mean Kendall tau-b at least .99 on at least 10 networks.
  Of those, 57 meet the absolute threshold on every observed network, 61 retain
  the mean threshold with equal domain weights, and 66 retain it without kite.
  These counts describe this sample, not universal redundancy.
- Degree, strength, expected influence 1 and the default one-module map equation
  belong to one close-ranking group. Closeness-related measures form another;
  their signs are preserved in the matrix. Complete-link grouping produces 15
  groups with two or more members. Individual pair counts and ranges matter.
- Against native baselines outside the candidate cohort, Coleman–Theil's closest
  baseline is shapley_game2 (mean tau .626, range .377–.780, 18 networks).
  For k-truss it is lac (mean .810, range .643–.897, 18 networks; redundancy ties).
  These are leads for distinct ranking behavior, not proof of novelty.
- Near-ties matter. On foodweb_Rhode, alpha versus power has raw Kendall .687
  but rounded Kendall 1.000. Unrounded Kendall and raw Pearson remain available
  alongside the rounded rank statistics; rounding is an explicit convention.

## Independent verification

[validation.json](validation.json) records **241,907 pair observations and
967,628 correlation comparisons, zero failures**, tolerance 1e-12. Maximum
absolute discrepancy was 4.33e-15. R stats correlations were independently
recomputed using SciPy Kendall tau-b and SciPy average ranks with NumPy Pearson
correlation. Both rounded and raw Kendall were checked for every pair.

Raw R doubles are exported as hexadecimal strings, preserving the exact near-ties.
Validation also checks dimensions, unique nodes/configurations, binary symmetric
adjacency, status eligibility, expected pair counts, and the original candidate
CSV SHA256. The export checks every retained adjacency hash against the manifest.
This validates the correlation calculation and fixture integrity. Function-level
source and numerical equivalence evidence remains in
[PARAMETER-CANDIDATE-EVIDENCE.md](../PARAMETER-CANDIDATE-EVIDENCE.md); the 48-candidate
evidence status must not be generalized to all 177 existing runtime measures.

## Dataset and calculation scope

The [manifest](network_manifest.csv) lists karate, kite, UKfaculty, macaque,
12 igraphdata foodwebs, Les Miserables, Florentine families and Davis.
Kite is an illustrative fictional graph, Les Miserables is literary, and Davis
retains its bipartite affiliation graph. Twelve foodwebs dominate the collection;
it is not a representative sample of network domains. Equal domain means in
[pairs.csv](pairs.csv) weight the six recorded families equally, using only those
available for each pair. The single-network families make that sensitivity
analysis imperfect too.

Projection collapses directions, removes loops and parallel edges, retains the
largest connected component, and discards weights. Rhode loses one of 20 nodes.
The Les Miserables source has 254 weighted edges. Its initial matrix import
represented integer weights as 820 parallel edges before simplification; source
metadata was corrected after confirming the retained binary graph and all its
node labels were unchanged. The import script now preserves weights until the
explicit projection step.

Every measure uses native parameter defaults unless the candidate ledger requests
a variant. Common settings are `weighted=FALSE`, `normalized=FALSE`, and mode all.
The full API default expressions are pinned in
[default_parameters.json](default_parameters.json); runtime R sources, audit
scripts and input files are fingerprinted in [source_hashes.json](source_hashes.json).
The three extra calls cover degree-mass gravity with unlimited or automatic
radius, and betweenness cutoff 3. Membership-dependent measures receive retained
Louvain partitions; map equation keeps its one-module default. R seeds are
20261004 plus network index, reset per call, with EPC seed 20261004.

Rank inputs divide by maximum absolute score and round to 12 decimals. Only
complete finite nonconstant vectors enter a network's matrix. Kendall tau-b
and Spearman use these inputs; Pearson and sensitivity Kendall use raw scores.
Networks are averaged equally, without pooling nodes. No p-values or independent
sampling claims are made. Cluster distance is one minus absolute mean Kendall,
with complete linkage and a minimum of 10 shared networks.

Of 3,420 network/configuration records, 3,039 are usable, 240 nonfinite, 54
constant, 75 skipped by the costly-measure rule above 40 nodes, and 12 errors.
Ten errors are 30-second time limits; two are singular alpha/power systems on
Les Miserables. Eleven directed-only measures are unavailable after projection;
Hubbell fails its default attenuation condition on every graph. Invalid defaults
were recorded rather than tuned. See [availability.csv](availability.csv) for
every warning, error, timing and exclusion. Timeout coverage is machine-dependent.

## What Zoo contributes

Zoo describes a correlation benchmark of **648 empirical ICON networks** on
its [comparison page](https://centralityzoo.github.io/comparison/), checked on
2026-09-07. Its [code page](https://centralityzoo.github.io/code/) still says the
implementation code is not released. We use the retained 349-label matrix in
`../correlation.json.gz`; we have not obtained the exact 648-network manifest,
per-node outputs, or established its parameter, sign and tie conventions.
This is a correlation reference, not an independent numerical verification oracle.

Across 1,127 candidate-label pairs with at least 10 native networks, excluding
the identical-configuration alias pair, signed matrix-entry Pearson is .446,
Spearman .779, mean absolute gap .182, and median absolute gap .038.
Zoo's retained matrix has no negative entries. Our bridging coefficient versus
Malatya correlation is -1 on all 19 graphs, versus Zoo's +.907. As a separate
sensitivity check, comparing mean absolute native Kendall against Zoo gives
Pearson .750, Spearman .817, mean gap .071 and median gap .035.
This does not establish Zoo's convention or isolate implementation errors:
the networks and potentially definitions differ. Alias labels remain dependent
even after excluding the direct alias pair; all matrix-entry summaries are
descriptive. See [zoo_comparison.csv](zoo_comparison.csv).

## Reproduce

From the repository root, with the packages recorded in [versions.csv](versions.csv)
and [validation.json](validation.json):

```sh
Rscript -e 'source("docs/zoo/correlation_audit.R")'
Rscript -e 'source("docs/zoo/correlation_export.R")'
python3 docs/zoo/correlation_validate.py
python3 docs/zoo/correlation_summary.py
python3 docs/zoo/correlation_report.py
```

The numerical audit has already finished; the last three commands can run against
retained public artifacts without recomputing centralities. Exact transformed
graphs, node labels, configurations, raw hexadecimal doubles and rank inputs
are in [score_fixtures.json.gz](score_fixtures.json.gz). RDS originals/checkpoints
remain in `local_testing_and_equivalence/correlation_audit/`.
The original 160-row CSV is unchanged, SHA256
`4006d16ec1d0d40db43aab48cf0301cf2057604ffffff9fbedf4a3ea2173aae2`.

The exported heatmap was visually inspected. HTML links, matrix dimensions and
symmetry, candidate mappings and every group threshold passed checks. The initial
mock DOM check missed a browser-global collision: the chart's `const top`
conflicted with `window.top`, preventing any drawing. The chart now runs inside
its own function scope. Actual Chrome verification reproduced that error before
the fix and passed afterward: 24 scope/statistic/coverage combinations, populated
canvas pixels, search and hover details. The repaired browser rendering was also
visually inspected. See [browser_validation.json](browser_validation.json).
