# Stress Centrality

Stress centrality (Shimbel 1953) counts the shortest paths between other
pairs of nodes that pass through a node: \$\$S(v) = \sum\_{s \ne v \ne
t} \sigma\_{st}(v),\$\$ where \\\sigma\_{st}(v)\\ is the number of
shortest paths from \\s\\ to \\t\\ through \\v\\. Betweenness divides
each count by the number of shortest paths between the pair; stress
keeps the counts.

## Usage

``` r
centrality_stress(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which inverts for tna input only) and `alpha`
  (inversion exponent, default 1).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are read as distances. `weighted = FALSE` uses hop counts,
and `invert_weights = TRUE` converts a weight \\w\\ to the distance
\\1/w^\alpha\\. On a directed network the paths follow edge direction,
and on an undirected network each pair is counted once. The values equal
[`sna::stresscent()`](https://rdrr.io/pkg/sna/man/stresscent.html).

## References

Shimbel, A. (1953). Structural parameters of communication networks. The
Bulletin of Mathematical Biophysics, 15(4), 501-507.
[doi:10.1007/BF02476438](https://doi.org/10.1007/BF02476438) .

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_load`](https://sonsoles.me/cograph/reference/centrality_load.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_stress(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5         16         19         16         11          1          7 
#>   Evaluate     Create      Share 
#>          3         13         10 
```
