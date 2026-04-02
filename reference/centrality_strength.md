# Strength Centrality (Weighted Degree)

Sum of edge weights connected to each node. For directed networks,
`centrality_instrength` sums incoming weights and
`centrality_outstrength` sums outgoing weights.

## Usage

``` r
centrality_strength(x, mode = "all", ...)

centrality_instrength(x, ...)

centrality_outstrength(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- mode:

  For directed networks: `"all"` (default), `"in"`, or `"out"`.

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`).

## Value

Named numeric vector of strength values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_degree`](http://sonsoles.me/cograph/reference/centrality_degree.md)
for the unweighted version.

## Examples

``` r
mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
rownames(mat) <- colnames(mat) <- c("A", "B", "C")
centrality_strength(mat)
#>   A   B   C 
#> 0.8 1.3 1.1 
```
