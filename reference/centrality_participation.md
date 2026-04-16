# Participation Coefficient

Measures diversity of inter-community connections. Nodes connecting to
many communities have high participation. Requires community membership.

## Usage

``` r
centrality_participation(x, membership = NULL, mode = "all", ...)
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

Named numeric vector of participation coefficient values (0-1).

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_within_module_z`](https://sonsoles.me/cograph/reference/centrality_within_module_z.md)
for within-community connectivity.

## Examples

``` r
adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
centrality_participation(adj, membership = c(1, 1, 1, 2, 2))
#>         A         B         C         D         E 
#> 0.0000000 0.0000000 0.4444444 0.5000000 0.0000000 
```
