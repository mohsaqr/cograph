# Extract Motifs from Network Data

Extract and analyze triad motifs from network data with flexible
filtering, pattern selection, and statistical significance testing.
Supports both individual-level analysis (with tna objects or grouped
data) and aggregate analysis (with matrices or networks).

## Usage

``` r
extract_motifs(
  x = NULL,
  data = NULL,
  id = NULL,
  level = NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  pattern = c("triangle", "network", "closed", "all"),
  exclude_types = NULL,
  include_types = NULL,
  top = NULL,
  by_type = FALSE,
  min_transitions = 5,
  significance = FALSE,
  n_perm = 100,
  seed = NULL
)

# S3 method for class 'cograph_motif_analysis'
print(x, n = 20, ...)
```

## Arguments

- x:

  Input data. Can be:

  - A `tna` object (supports individual-level analysis)

  - A matrix (aggregate analysis only, unless `data` and `id` provided)

  - A `cograph_network` object

  - An `igraph` object

- data:

  Optional data.frame containing transition data with an ID column for
  individual-level analysis. Required columns: `from`, `to`, and the
  column(s) specified in `id`. If provided, `x` should be NULL or a
  matrix of node labels.

- id:

  Column name(s) identifying individuals/groups in `data`. Can be a
  single string or character vector for multiple grouping columns.
  Required for individual-level analysis with non-tna inputs.

- level:

  Analysis level: "individual" counts how many people have each triad,
  "aggregate" analyzes the summed/single network. Default depends on
  input: "individual" for tna or when id provided, "aggregate"
  otherwise.

- edge_method:

  Method for determining edge presence:

  "any"

  :   Edge exists if count \> 0 (simple, recommended)

  "expected"

  :   Edge exists if observed/expected \>= threshold

  "percent"

  :   Edge exists if edge/total \>= threshold

  Default "any".

- edge_threshold:

  Threshold value for "expected" or "percent" methods. For "expected", a
  ratio (e.g., 1.5 means 50\\ The default 1.5 is calibrated for this
  method. For "percent", a proportion (e.g., 0.15 for 15\\ When using
  "percent", set this explicitly (e.g., 0.15). Ignored when edge_method
  = "any". Default 1.5.

- pattern:

  Pattern filter for which triads to include:

  "triangle"

  :   All 3 node pairs must be connected (any direction). Types: 030C,
      030T, 120C, 120D, 120U, 210, 300. Default.

  "network"

  :   Exclude simple sequential patterns (chains/single edges).
      Excludes: 003, 012, 021C. Includes stars and triangles.

  "closed"

  :   Network without chain patterns. Excludes: 003, 012, 021C, 120C.
      Similar to network but also removes mutual+chain (120C).

  "all"

  :   Include all 16 MAN types, no filtering.

- exclude_types:

  Character vector of MAN types to explicitly exclude. Applied after
  pattern filter. E.g., c("300") to exclude cliques.

- include_types:

  Character vector of MAN types to exclusively include. If provided,
  only these types are returned (overrides pattern/exclude).

- top:

  Return only the top N results (by observed count or z-score). NULL
  returns all results. Default NULL.

- by_type:

  If TRUE, group results by MAN type in output. Default FALSE.

- min_transitions:

  At individual level: minimum total transitions for a person to be
  included in the analysis. At aggregate level: minimum triad weight to
  count as present. Default 5.

- significance:

  Logical. Run permutation significance test? Default FALSE.

- n_perm:

  Number of permutations for significance test. Default 100.

- seed:

  Random seed for reproducibility.

## Value

A `cograph_motif_analysis` object (list) containing:

- results:

  Data frame with triad, type, observed count, and (if
  significance=TRUE) expected, z-score, p-value

- type_summary:

  Summary counts by motif type

- params:

  List of parameters used

## MAN Notation

The 16 triad types use MAN (Mutual-Asymmetric-Null) notation where:

- First digit: number of Mutual (bidirectional) pairs

- Second digit: number of Asymmetric (one-way) pairs

- Third digit: number of Null (no edge) pairs

- Letter suffix: subtype variant (C=cycle, T=transitive, D=down, U=up)

## Pattern Types

- Triangle patterns (all pairs connected)::

  030C (cycle), 030T (feed-forward), 120C (regulated cycle), 120D (two
  out-stars), 120U (two in-stars), 210 (mutual+asymmetric), 300 (clique)

- Network patterns (has structure)::

  021D (out-star), 021U (in-star), 102 (mutual pair), 111D
  (out-star+mutual), 111U (in-star+mutual), 201 (mutual+in-star), plus
  all triangle patterns

- Sequential patterns (chains)::

  012 (single edge), 021C (A-\>B-\>C chain)

- Empty::

  003 (no edges)

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)

Other motifs:
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
# Small aggregate example -- no significance test for speed
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
m <- extract_motifs(mat, significance = FALSE)
print(m)
#> Motif Analysis
#> Pattern: triangle | Edge method: any
#> Individuals: 1 | States: 4 | Total triads: 4
#> 
#> Type distribution:
#> 
#> 030C 030T 
#>    2    2 
#> 
#> Top 4 triads:
#>                       triad type observed
#> 1 Execute - Monitor - Adapt 030T        1
#> 2    Plan - Execute - Adapt 030C        1
#> 3  Plan - Execute - Monitor 030T        1
#> 4    Plan - Monitor - Adapt 030C        1

if (FALSE) { # \dontrun{
Mod <- tna::tna(tna::group_regulation)
# Individual-level from tna -- keep n_perm tiny for example speed
extract_motifs(Mod, top = 10, significance = TRUE, n_perm = 10L, seed = 1)
# Filter to feed-forward loops only
extract_motifs(Mod, include_types = "030T", significance = FALSE)
} # }
```
