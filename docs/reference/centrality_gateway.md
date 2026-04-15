# Gateway Coefficient

Inter-community brokerage weighted by centrality. Combines participation
with degree information. Requires community membership.

## Usage

``` r
centrality_gateway(x, membership = NULL, mode = "all", ...)
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

Named numeric vector of gateway coefficient values (0-1).

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_participation`](https://sonsoles.me/cograph/reference/centrality_participation.md)
for the simpler participation coefficient.

## Examples

``` r
adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
centrality_gateway(adj, membership = c(1, 1, 1, 2, 2))
#>         A         B         C         D         E 
#> 0.4195011 0.4195011 0.6520534 0.7028061 0.2653061 
```
