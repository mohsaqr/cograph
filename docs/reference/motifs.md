# Network Motif Analysis

Two modes of motif analysis for networks:

- **Census** (`named_nodes = FALSE`, default): Counts MAN type
  frequencies with significance testing. Nodes are exchangeable.

- **Instances** (`named_nodes = TRUE`, or use
  [`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md)):
  Lists specific node triples forming each pattern. Nodes are NOT
  exchangeable.

## Usage

``` r
motifs(
  x,
  named_nodes = FALSE,
  actor = NULL,
  window = NULL,
  window_type = c("rolling", "tumbling"),
  pattern = c("triangle", "network", "closed", "all"),
  include = NULL,
  exclude = NULL,
  significance = TRUE,
  n_perm = 1000L,
  min_count = if (named_nodes) 5L else NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  min_transitions = 5,
  top = NULL,
  seed = NULL
)

# S3 method for class 'cograph_motif_result'
plot(
  x,
  type = c("triads", "types", "significance", "patterns"),
  n = 15,
  ncol = 5,
  colors = c("#2166AC", "#B2182B"),
  ...
)
```

## Arguments

- x:

  Input data: a tna object, cograph_network, matrix, igraph, or
  data.frame (edge list).

- named_nodes:

  Logical. If FALSE (default), performs census (type-level counts). If
  TRUE, extracts specific node triples (instance-level).
  [`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md) is
  a convenience wrapper that sets this to TRUE.

- actor:

  Character. Column name in the edge list metadata to group by. If NULL
  (default), auto-detects standard column names (session_id, session,
  actor, user, participant). If no grouping column found, performs
  aggregate analysis.

- window:

  Numeric. Window size for windowed analysis. Splits each actor's
  transitions into windows of this size. NULL (default) means no
  windowing.

- window_type:

  Character. Window type: "rolling" (default) or "tumbling". Only used
  when `window` is set.

- pattern:

  Pattern filter: "triangle" (default), "network", "closed", "all".

- include:

  Character vector of MAN types to include exclusively. Overrides
  `pattern`.

- exclude:

  Character vector of MAN types to exclude. Applied after `pattern`
  filter.

- significance:

  Logical. Run permutation significance test? Default TRUE.

- n_perm:

  Number of permutations for significance. Default 1000.

- min_count:

  Minimum observed count to include a triad (instance mode only).
  Default 5 for instances, NULL for census.

- edge_method:

  Method for determining edge presence: "any" (default), "expected", or
  "percent".

- edge_threshold:

  Threshold for "expected" or "percent" methods. Default 1.5.

- min_transitions:

  Minimum total transitions for a unit to be included. Default 5.

- top:

  Return only the top N results. NULL returns all.

- seed:

  Random seed for reproducibility.

- type:

  Plot type: "triads" (network diagrams), "types" (bar chart),
  "significance" (z-score plot), "patterns" (abstract MAN diagrams).

- n:

  Number of items to plot. Default 15.

- ncol:

  Number of columns in triad grid. Default 5.

- colors:

  Colors for visualization. Default blue/red.

- ...:

  Additional arguments passed to plot helpers.

## Value

A `cograph_motif_result` object with:

- results:

  Data frame of results. Census: type, count, (z, p, sig). Instances:
  triad, type, observed, (z, p, sig).

- type_summary:

  Named counts by MAN type

- level:

  Analysis level: "individual" or "aggregate"

- named_nodes:

  Whether nodes are identified (TRUE) or exchangeable (FALSE)

- n_units:

  Number of units analyzed

- params:

  List of parameters used

## Details

Detects input type and analysis level automatically. For inputs with
individual/group data (tna objects, cograph networks from edge lists
with metadata), performs per-group analysis. For aggregate inputs
(matrices, igraph), analyzes the single network.

## See also

[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)

Other motifs:
[`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
[`plot.cograph_motif_analysis()`](http://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](http://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Census from a matrix
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
motifs(mat, significance = FALSE)

if (requireNamespace("tna", quietly = TRUE)) {
  # Census from tna object
  Mod <- tna::tna(tna::group_regulation)
  motifs(Mod)

  # Instances: specific node triples
  subgraphs(Mod)
}
} # }
```
