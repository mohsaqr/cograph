# Load Centrality

Load centrality (Goh et al. 2001) sends one unit of load from every node
to every other node along shortest paths. At each branching the load is
split equally among the shortest-path predecessors, and the score of a
node is the total load that passes through it.

## Usage

``` r
centrality_load(x, ...)
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
\\1/w^\alpha\\. Paths follow edge direction on a directed network. The
values equal
[`sna::loadcent()`](https://rdrr.io/pkg/sna/man/loadcent.html), which
credits the endpoints of each path as well as the intermediate nodes, so
the scores are larger than betweenness.

## References

Goh, K.-I., Kahng, B., & Kim, D. (2001). Universal behavior of load
distribution in scale-free networks. Physical Review Letters, 87(27),
278701.
[doi:10.1103/PhysRevLett.87.278701](https://doi.org/10.1103/PhysRevLett.87.278701)
.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_stress`](https://sonsoles.me/cograph/reference/centrality_stress.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_load(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       24.0       34.5       37.0       34.0       29.0       19.5       25.5 
#>   Evaluate     Create      Share 
#>       22.0       32.0       28.0 
```
