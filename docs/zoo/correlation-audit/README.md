# Centrality correlation audit

## Rerun of 2026-11-26 after batch 69

All data files were regenerated on the ci VM for 274 measures, 282
configurations and 19 networks. Validation against SciPy and NumPy passed:
590,768 pair observations, 2,363,072 values, 0 failures at 1e-12.
The HTML report, the heatmap, `browser_validation.json` and
`source_hashes.json` were not regenerated.

Ledger calls that leave a value to the user were given one for the audit
(`correlation_audit.R`, `placeholders`): `hybrid_degree` p = 0.05, `wksd`
alpha = mu = 0.5, `physarum_spreader` 52 steps, `inward_accessibility`
2 steps, EigenTrust weight 0.15 with a uniform pre-trust.

Redundancy of the 57 measures added in batches 64 to 69: no pair has
|tau| = 1 on every network, so none is a duplicate. The closest are
`mlc` and `semilocal` (mean |tau| 0.996, at least 0.974), `ehc` and
`diffusion_centrality` (0.995, at least 0.981), `gls` and `lnc` (0.993, at
least 0.982), `hybrid_degree` at p = 0.05 and `lnc` (0.987), and `mcde` and
`mcdwe` with each other (0.986). `articlerank` has no value on these
undirected networks (its iteration diverges on cycles) and is not screened.

## Run of 2026-11-26 (current data files)

The audit was rerun on the ci VM on 2026-11-26, after implementation batch 63.
Every data file in this folder (CSV, JSON, gz) is from that run. **The
interactive report `index.html`, `candidate_heatmap.png`,
`browser_validation.json` and `source_hashes.json` are still those of the
2026-09-07 run and have not been regenerated**; the text below this section
also describes the 2026-09-07 run. The method, the 19 networks and every
convention are unchanged. Two scripts were changed only to read the number of
configurations from `configurations.csv`, where they had 180 written in
(`correlation_export.R`, `correlation_validate.py`).

- 218 native measures and three parameter variants, 221 configurations, on the
  same 19 networks. The ledger had 89 covered candidate labels, mapping to 87
  configurations.
- Of 4,199 network/configuration records: 3,728 usable, 276 nonfinite, 135
  skipped by the costly-above-40-nodes rule, 53 constant, 7 errors (five
  30-second time limits: flow betweenness twice, infection three times; two
  singular alpha/power systems on Les Miserables). 206 native measures
  produced usable rankings somewhere.
- 73 native pairs have absolute mean Kendall tau-b of at least .99 on at least
  10 networks; 68 of them meet the threshold on every observed network, 70 keep
  it with equal domain weights and 71 without kite. Complete linkage gives 20
  groups with two or more members.
- Independent validation ([validation.json](validation.json)): 364,970 pair
  observations and 1,459,880 correlation values recomputed with SciPy and
  NumPy, zero failures at 1e-12.
- Software: see [versions.csv](versions.csv); `igraphdata` 1.0.1 was installed
  on the VM for this run.

### The 41 measures added since batch 40

The 2026-09-07 run stopped at batch 39, so these had never been screened.
Pairs that reach |mean tau| of .99 with another native measure:

| Added measure | Close to | Mean tau | Minimum abs tau | Networks | Reading |
|---|---|---|---|---|---|
| `inf` (batch 57) | `beta_measure` | 1.0000 | 1.0000 | 19 | The same numbers on every undirected graph (difference exactly 0 on the 1,312 undirected retained fixtures). A duplicate |
| `mu_pci` (58) | `lobby`, `hindex_strength` | 1.0000 | 1.0000 | 19 | Equal to `lobby` at its default `pci_mu = 1`, as its help page states; differs for larger `pci_mu` |
| `tec` (53) | `degree`, `strength`, `expected_influence_1`, `map_equation` | 1.0000 | 1.0000 | 14 | On an undirected network it is (degree + 1) / sum, a monotone function of degree; differs on directed networks, which this audit does not contain |
| `iec` (49) | `markov` | 0.9992 | 0.9926 | 14 | Close, not equal; the two differ in the self-loop and the divisor |
| `lnc` (44) | `modified_expected_force` | 0.9967 | 0.9904 | 19 | Close on these networks |
| `neighbor_distance` (42) | `spectralrank` | 0.9917 | 0.9726 | 19 | Above the mean threshold, below it on 5 networks |

No other measure of batches 40 to 63 reaches .99. The closest of the rest:
`wksdn`-`lnc` .980, `counting_betweenness`-`weighted_leaderrank` .980,
`con`-`expected` .978, `x_nonbacktracking`-`nonbacktracking` .976,
`mec`-`graph_regularization` .976, `dkgm` .976, `ink`-`counting_betweenness`
.972, `niem`-`expected` .972, `density_centrality`-`harary` .971,
`iksd`-`onion` .971. The most distinct are `emi` (nearest `iec`, .686),
`dil` (.858), `wfca` (.872) and `topsis_re` (.873).

Three measures that enumerate all simple paths or cycles were hardly screened:
`all_cycle_betweenness` is usable on 3 networks, `transportation` and
`path_transfer` on 2. On the others they pass `enumeration_limit` and return
`NA` (11 or 12 networks), or are skipped as costly above 40 nodes (5). They
have no pair with 10 shared networks, so the audit says nothing about their
redundancy.

These statements describe 19 small undirected networks, twelve of them
foodwebs. A correlation of 1 here is a lead to check, as was done for `inf`;
a lower correlation does not establish that a measure is new.

## Run of 2026-09-07 (the text from here on, the HTML report and the heatmap)

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
