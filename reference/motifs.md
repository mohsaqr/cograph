# Network Motif Analysis

Two modes of motif analysis for networks:

- **Census** (`named_nodes = FALSE`, default): Counts MAN type
  frequencies with significance testing. Nodes are exchangeable.

- **Instances** (`named_nodes = TRUE`, or use
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)):
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
print(x, ...)

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
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md) is
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

  Which MAN triad types to include in the analysis:

  `"triangle"`

  :   (default) Only the 7 closed triangle types: 030C, 030T, 120C,
      120D, 120U, 210, 300. Excludes trivial open patterns (empty
      triads, single edges, chains, stars, mutual pairs).

  `"network"`

  :   All types except trivially open ones. Excludes 003 (empty), 012
      (single edge), 021C (chain).

  `"closed"`

  :   Like `"network"` but also excludes 120C (mixed regulated).
      Excludes 003, 012, 021C, 120C.

  `"all"`

  :   All 16 MAN types, including empty and trivial patterns.

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

- ...:

  Additional arguments passed to internal plot helpers.

- type:

  Plot type:

  `"triads"`

  :   Network diagrams of specific node triples (instance mode) or falls
      back to patterns (census mode). Arranged in a grid.

  `"types"`

  :   Bar chart of MAN type frequencies.

  `"significance"`

  :   Z-score plot showing over- and under-represented types relative to
      a null model. Requires `significance = TRUE` in the `motifs()`
      call.

  `"patterns"`

  :   Abstract MAN pattern diagrams showing the edge structure of each
      triad type.

- n:

  Maximum number of items to plot. Default 15.

- ncol:

  Number of columns in the triad/pattern grid. Default 5.

- colors:

  Two-element color vector: first color for over-represented or positive
  values, second for under-represented or negative values. Default
  `c("#2166AC", "#B2182B")` (blue/red).

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

Invisibly returns the input `x`.

## Details

Detects input type and analysis level automatically. For inputs with
individual/group data (tna objects, cograph networks from edge lists
with metadata), performs per-group analysis. For aggregate inputs
(matrices, igraph), analyzes the single network.

## See also

[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
# Census from a matrix (no significance test — fastest path)
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
motifs(mat, significance = FALSE)
#> Motif Census 
#> Level: aggregate | States: 4 | Pattern: triangle 
#> 
#> Type distribution:
#> 
#> 030C 030T 
#>    1    1 
#> 
#> Top 2 results:
#>  type count
#>  030C     2
#>  030T     2

# With a minimal significance test (set n_perm >= 500 in practice)
motifs(mat, n_perm = 10L, seed = 1)
#> Motif Census 
#> Level: aggregate | States: 4 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> 
#> Type distribution:
#> 
#> 030C 030T 
#>    1    1 
#> 
#> Top 2 results:
#>  type count expected     z      p   sig
#>  030C     2      0.2 -0.47 0.6353 FALSE
#>  030T     2      0.0  0.00 1.0000 FALSE

Mod <- tna::tna(tna::group_regulation)
motifs(Mod, n_perm = 10L, seed = 1)
#> Motif Census 
#> Level: individual | 2000 units | States: 9 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> 
#> Type distribution:
#> 
#> 030C 030T 120C 120D 120U  210  300 
#>    1    1    1    1    1    1    1 
#> 
#> Top 7 results:
#>  type count expected     z      p   sig
#>   210   581    766.2 -9.78 0.0000  TRUE
#>  030T   620    436.3  7.49 0.0000  TRUE
#>   300    79    166.2 -5.81 0.0000  TRUE
#>  120C  1482   1343.0  3.26 0.0011  TRUE
#>  030C  1054   1073.1 -0.90 0.3681 FALSE
#>  120U   190    180.3  0.90 0.3681 FALSE
#>  120D   178    171.8  0.52 0.6031 FALSE
subgraphs(Mod, n_perm = 10L, seed = 1)
#> Showing triangle patterns (count > 5). For all MAN types use pattern = 'all'.
#> Motif Subgraphs 
#> Level: individual | 2000 units | States: 9 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> Min count: > 5 
#> 
#> Type distribution:
#> 
#> 030C 120C 030T  210 
#>   30   14    6    1 
#> 
#> Top 20 results:
#>                             triad observed type expected       z p  sig
#>       cohesion - consensus - plan      151 120C   1384.0 -255.26 0 TRUE
#>        consensus - discuss - plan      278  210   1496.7 -224.13 0 TRUE
#>       coregulate - discuss - plan       61 030C   1218.6 -182.93 0 TRUE
#>          discuss - emotion - plan       76 030T   1313.3 -159.13 0 TRUE
#>        consensus - emotion - plan      436 120C   1439.8 -141.08 0 TRUE
#>        consensus - monitor - plan      187 120C   1334.9 -130.45 0 TRUE
#>      consensus - plan - synthesis       13 120C   1269.1 -127.84 0 TRUE
#>        discuss - plan - synthesis        9 030C   1004.6 -120.50 0 TRUE
#>     consensus - discuss - emotion      238 120C   1350.9 -120.09 0 TRUE
#>    cohesion - consensus - discuss      125 120C   1238.4 -109.62 0 TRUE
#>     consensus - coregulate - plan      301 120C   1391.6 -106.69 0 TRUE
#>       coregulate - emotion - plan       64 030T   1126.1  -90.82 0 TRUE
#>      cohesion - emotion - monitor       17 030C    552.4  -90.01 0 TRUE
#>    adapt - consensus - coregulate       16 030C    763.5  -87.52 0 TRUE
#>     consensus - discuss - monitor      127 120C   1184.0  -85.99 0 TRUE
#>         cohesion - discuss - plan       18 030C   1201.9  -85.92 0 TRUE
#>          discuss - monitor - plan       62 030T   1123.2  -84.44 0 TRUE
#>  consensus - coregulate - emotion      165 030C   1165.5  -83.42 0 TRUE
#>  consensus - coregulate - discuss      330 120C   1246.1  -83.29 0 TRUE
#>    cohesion - consensus - emotion      261 120C   1109.7  -75.94 0 TRUE
```
