# Betweenness Centrality

Betweenness centrality (Freeman 1977) sums, over pairs of other nodes,
the fraction of shortest paths between them that pass through the node:
\$\$B(v) = \sum\_{s \ne v \ne t}
\frac{\sigma\_{st}(v)}{\sigma\_{st}},\$\$ where \\\sigma\_{st}\\ is the
number of shortest paths from \\s\\ to \\t\\ and \\\sigma\_{st}(v)\\ the
number of those through \\v\\.

## Usage

``` r
centrality_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `invert_weights` (default `NULL`, which is `TRUE` for
  tna input), `alpha` (inversion exponent, default 1), `cutoff` (largest
  path length considered, default -1 for no limit) and `normalized`
  (default `FALSE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

On a directed network the sum runs over ordered pairs along the edge
direction, and on an undirected network over unordered pairs. Edge
weights are read as path lengths, and `invert_weights = TRUE` uses
\\1/w^\alpha\\ instead. `weighted = FALSE` uses hop counts. `cutoff`
drops paths longer than the given length. `normalized = TRUE` divides
the scores by their maximum.

## References

Freeman, L. C. (1977). A set of measures of centrality based on
betweenness. Sociometry, 40(1), 35-41.
[doi:10.2307/3033543](https://doi.org/10.2307/3033543) .

## See also

[`centrality_load`](https://sonsoles.me/cograph/reference/centrality_load.md),
[`centrality_stress`](https://sonsoles.me/cograph/reference/centrality_stress.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        5.0       15.5       18.0       15.0       10.0        0.5        6.5 
#>   Evaluate     Create      Share 
#>        3.0       13.0        9.0 
```
