# Zoo consistency check

Do cograph's measures correlate with each other the way the Centrality Zoo
says they should? `local_testing_and_equivalence/zoo_consistency.R` computes
every covered measure on 19 real undirected graphs (10 to 81 nodes: karate,
kite, UKfaculty, macaque, 12 igraphdata food webs, les_miserables,
florentine, davis), takes pairwise Kendall tau between measures, averages
over networks, and correlates each measure's tau profile with its row in the
Zoo matrix.

Caveats: the Zoo averaged over 648 networks of many sizes; this uses 19
small ones. Established measures therefore set the achievable band, not 1.0.
Katz is computed with a per-network attenuation of `0.5 / rho(A)`, because
the 0.1 default does not converge on several of these graphs.

## Whole-matrix agreement

| Statistic over 4,654 measure pairs | Value |
|---|---|
| Correlation between our matrix and the Zoo's | 0.70 |
| Mean absolute difference | 0.14 |
| Pairs agreeing within 0.20 | 78% |

## Per-measure agreement

| Group | Measures | Median profile correlation |
|---|---|---|
| Pre-existing cograph measures | 59 | 0.70 |
| Batches 7 to 9 | 38 | 0.69 |

Established measures for scale: eigenvector 0.77, coreness 0.76, closeness
0.72, degree 0.67, betweenness 0.64.

29 of the 38 new measures sit inside that band (profile correlation at
least 0.55). The rest, with the most likely reason:

| Measure | Profile cor | Nearest (ours, tau) | Nearest (Zoo, tau) | Reading |
|---|---|---|---|---|
| local_dimension | 0.02 | fuzzy_local_dimension (0.86) | eccentricity (0.50) | our nearest partner is fuzzy_local_dimension (0.86); the Zoo's row for this label peaks at 0.50 and its page is still unpublished, so the Zoo's version is probably not the regression form of Pu et al. |
| degree_discount | 0.22 | single_discount (0.39) | constraint (0.44) | a greedy seed order with heavy tie structure; the Zoo's own row peaks at 0.44 and the propagation probability it used is unknown. |
| distance_entropy | 0.30 | entropy_variation_betweenness (0.52) | local_volume_dimension (0.34) | both rows are weak; there is no single partner to disagree about. |
| shapley_game2 | 0.30 | shapley_game1 (0.79) | transitivity (0.52) | depends on k, which the paper does not fix and the Zoo does not state; cograph uses 2. |
| flow_coefficient | 0.33 | transitivity (1.00) | enrenew (0.60) | the Zoo prints an unbounded formula that differs from the paper's; ours equals one minus the clustering coefficient on an undirected graph, which is forced by algebra. |
| rumor | 0.38 | kreach (0.58) | closeness (0.64) | the BFS tie rule is unspecified in the paper and changes the values; the Zoo's rule is unknown. |
| access_information | 0.46 | diversity (0.32) | entropy_variation_degree (0.43) | a cost measure rather than a prominence measure; both rows are weak. |
| two_way_rw | 0.47 | degree (0.74) | lobby (0.71) | an argmax counting statistic with many ties and many zeros on sparse graphs. |
| geodesic_kpath | 0.53 | diversity (0.42) | kreach (0.70) | ours counts paths, as the paper defines it; the Zoo's nearest partner is m-reach, which is the node-counting variant that centiserve implements. |

## What the check found

The first run flagged `katz` as an outlier at 0.56, with a nearest partner
of `redundancy` at 0.37, which is not how a walk-based measure behaves.
The cause was the default attenuation: Katz converges only for
`alpha < 1 / rho(A)`, and the 0.1 default is invalid on any graph whose
spectral radius exceeds 10, which several of these food webs do. The solve
still returns numbers, so nothing looked wrong. With a valid attenuation
`katz` moves to 0.76 with a nearest partner of `laplacian` at 0.96, against
the Zoo's `subgraph` at 0.97. `centrality()` now warns when the attenuation
cannot converge.

Three measures still sit far below the band and are **not** defects: they
match their reference implementations exactly. `constraint` (-0.20) and
`clusterrank` (-0.01) reproduce `igraph::constraint` and
`centiserve::clusterrank` bit for bit. `dmnc` (0.21) differs from
`centiserve::dmnc` because that implementation counts the component's edges
in the wrong index space; see `?centrality_dmnc`.

## Reproducing

```
Rscript local_testing_and_equivalence/zoo_consistency.R
```
writes `zoo_consistency.csv` (per measure) and `zoo_consistency_matrix.csv`
(the averaged tau matrix). `tmp/zoo-coverage-matrix.Rmd` knits both against
the Zoo's matrix.
