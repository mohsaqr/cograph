# Extract Specific Motif Instances (Subgraphs)

Calls `motifs(x, named_nodes = TRUE, ...)`. The result has one row per
node triple and MAN type. At individual level, `observed` counts the
units in which the triple has that type, so one triple can occupy
several rows. One MAN type can also appear in many rows, each with its
own `z` and `p`. Per-triple significance is plotted by
`plot(., type = "significance")` and `plot(., type = "triads")`. The
per-type plots (`"types"`, `"patterns"`) omit significance for instance
results.

## Usage

``` r
subgraphs(...)
```

## Arguments

- ...:

  Arguments forwarded to
  [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md), which
  documents them (`x`, `actor`, `window`, `window_type`, `pattern`,
  `include`, `exclude`, `significance`, `n_perm`, `cores`, `min_count`,
  `edge_method`, `edge_threshold`, `min_transitions`, `top`, `seed`).
  `named_nodes` is fixed to `TRUE` and must not be supplied.

## Value

A `cograph_motif_result` object with `named_nodes = TRUE`, described in
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md), or NULL
with a message when no triple passes the filters. Its results table has
the columns `triad`, `node1`, `node2`, `node3`, `type` and `observed`,
and with `significance = TRUE` also `expected`, `z`, `p` and `sig`. Its
`type_summary` counts the node triples of each MAN type.

## Details

The `"triads"` diagram shows a canonical representative of the MAN
isomorphism class of each row. The labels name the participating nodes.
Their positions in the diagram do not encode the observed source and
sink roles of the nodes.

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
subgraphs(regulation_net, significance = FALSE, min_count = 1)
#> Showing triangle patterns (count >= 1). For all MAN types use pattern = 'all'.
#> Motif Subgraphs 
#> Level: aggregate | States: 10 | Pattern: triangle 
#> Min count: >= 1 
#> 
#> Type distribution:
#> 
#> 120C 030T 120D 120U 
#>    3    2    1    1 
#> 
#> Top 7 results:
#>                        triad   node1    node2   node3 type observed
#>  Monitor - Evaluate - Create Monitor Evaluate  Create 120C     1.26
#>     Monitor - Create - Share Monitor   Create   Share 120C     1.26
#>       Plan - Monitor - Share    Plan  Monitor   Share 120U     1.19
#>     Plan - Evaluate - Create    Plan Evaluate  Create 030T     1.08
#>  Explore - Reflect - Discuss Explore  Reflect Discuss 120D     1.05
#>      Monitor - Adapt - Share Monitor    Adapt   Share 030T     1.04
#>        Plan - Create - Share    Plan   Create   Share 120C     1.00
```
