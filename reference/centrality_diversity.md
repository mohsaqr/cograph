# Diversity Centrality

Diversity centrality (Eagle, Macy and Claxton 2010) is the Shannon
entropy of the weights on the edges incident to a node, divided by its
maximum \\\log_2 k_v\\: \$\$D(v) = -\frac{\sum\_{j} p\_{vj} \log_2
p\_{vj}}{\log_2 k_v}, \qquad p\_{vj} = \frac{\|w\_{vj}\|}{\sum\_{l}
\|w\_{vl}\|}.\$\$ The score lies between 0 and 1 and reaches 1 when the
weights are equal.

## Usage

``` r
centrality_diversity(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

On a directed network incoming and outgoing edges are separate entries,
and \\k_v\\ counts both. Edge weights are always used, and
`weighted = FALSE` has no effect. On an unweighted input every node with
two or more edges scores 1. A node with fewer than two edges, or with
edge weights summing to zero, scores 0.

## References

Eagle, N., Macy, M., & Claxton, R. (2010). Network diversity and
economic development. Science, 328(5981), 1029-1031.
[doi:10.1126/science.1186605](https://doi.org/10.1126/science.1186605) .

## See also

[`centrality_entropy`](https://sonsoles.me/cograph/reference/centrality_entropy.md),
[`centrality_strength`](https://sonsoles.me/cograph/reference/centrality_strength.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_diversity(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.9320667  0.9361137  0.9217224  0.9643772  0.8742906  0.9697116  0.8368385 
#>   Evaluate     Create      Share 
#>  0.9262201  0.9582511  0.9748653 
```
