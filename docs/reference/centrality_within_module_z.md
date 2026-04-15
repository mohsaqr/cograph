# Within-Module Degree Z-Score

Z-score of intra-community connectivity. High values indicate hubs
within their own community. Requires community membership.

## Usage

``` r
centrality_within_module_z(x, membership = NULL, mode = "all", ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- membership:

  Integer vector of community assignments (one per node).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of within-module z-score values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md)
for between-community diversity.

## Examples

``` r
adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
centrality_within_module_z(adj, membership = c(1, 1, 1, 2, 2))
#>   A   B   C   D   E 
#> NaN NaN NaN NaN NaN 
```
