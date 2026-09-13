# HITS Authority and Hub Scores

Kleinberg's HITS algorithm. `centrality_authority` scores nodes pointed
to by good hubs. `centrality_hub` scores nodes that point to good
authorities.

## Usage

``` r
centrality_authority(x, ...)

centrality_hub(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of authority or hub scores.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_authority(adj)
#>        A        B        C 
#> 0.618034 0.000000 1.000000 
centrality_hub(adj)
#>        A        B        C 
#> 0.618034 1.000000 0.000000 
```
