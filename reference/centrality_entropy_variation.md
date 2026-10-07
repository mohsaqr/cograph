# Entropy Variation

Entropy variation (Ai 2017) is the change in the Shannon entropy of a
node-level distribution \\f\\ when a node and its links are removed:
\$\$EnV_f(i) = I_f(G) - I_f(G - i), \qquad I_f(G) = -\sum_j p_j \ln p_j,
\quad p_j = \frac{f(j)}{\sum_l f(l)}.\$\$ The distribution \\f\\ is the
degree or the betweenness. Higher values mark more important nodes.

## Usage

``` r
centrality_entropy_variation(
  x,
  of = c("degree", "betweenness"),
  mode = "all",
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- of:

  Distribution: `"degree"` (default) or `"betweenness"`.

- mode:

  For the degree variant on directed networks: `"all"` (default, in plus
  out), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The degree variant uses `loops` (keep self-loops, default `TRUE`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The entropy uses the natural logarithm, as in the code of the author, so
scores are in nats. The difference is signed, and a negative value means
that removing the node evens out the distribution. Edge weights are
ignored in both variants. For the degree variant `mode` selects the in-,
out- or total degree on a directed network, and self-loops count toward
the degree, and `loops = FALSE` drops them. The betweenness variant
ignores `mode` and self-loops. A deletion that leaves every value of
\\f\\ at zero, such as betweenness on a clique, has entropy 0.

## References

Ai, X. (2017). Node importance ranking of complex networks with entropy
variation. Entropy, 19(7), 303.

## See also

[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality_distance_entropy`](https://sonsoles.me/cograph/reference/centrality_distance_entropy.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_entropy_variation(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.11832094 0.12127395 0.10553447 0.12479343 0.12898833 0.10607669 0.09757111 
#>   Evaluate     Create      Share 
#> 0.09986081 0.10338637 0.09996779 
```
