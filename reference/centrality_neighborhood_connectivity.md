# Neighborhood Connectivity

Neighborhood connectivity (Maslov and Sneppen 2002) is the mean degree
of the neighbors of a node, the average neighbor degree reported by
Cytoscape: \$\$C\_{NC}(i) = \frac{1}{k_i} \sum\_{j \in N(i)} k_j.\$\$

## Usage

``` r
centrality_neighborhood_connectivity(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `loops` (keep self-loops, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. Under `mode = "out"` the out-degrees of the
out-neighbors are averaged, and under `mode = "in"` the in-degrees of
the in-neighbors. Self-loops change the degrees, and `loops = FALSE`
drops them. Isolated nodes score 0. High values mark nodes attached to
hubs.

## References

Maslov, S., & Sneppen, K. (2002). Specificity and stability in topology
of protein networks. Science, 296(5569), 910-913.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_neighborhood_connectivity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   5.400000   5.333333   5.285714   5.166667   5.200000   5.600000   6.000000 
#>   Evaluate     Create      Share 
#>   6.000000   5.500000   6.000000 
```
