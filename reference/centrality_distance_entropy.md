# Distance Entropy

Distance entropy (Stella and De Domenico 2018) is the Shannon entropy of
the distribution of hop distances from a node to the nodes it reaches,
scaled by the logarithm of the number of distance values in its range.
With \\p_k\\ the share of reachable nodes at distance \\k\\, and \\m_i\\
and \\M_i\\ the smallest and largest distance, \$\$h_i =
-\frac{1}{\log(M_i - m_i + 1)} \sum\_{k = m_i}^{M_i} p_k \log p_k.\$\$

## Usage

``` r
centrality_distance_entropy(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. On a directed
network `mode` sets the direction of the paths. Scores lie between 0
and 1. A node whose reachable nodes all lie at one distance scores 0,
and a node that reaches no other node returns `NaN`. The source divides
by \\\log(M_i - m_i)\\, which is zero when the range holds two
distances. The implementation divides by \\\log(M_i - m_i + 1)\\, so a
uniform distribution scores 1.

## References

Stella, M., & De Domenico, M. (2018). Distance entropy cartography
characterises centrality in complex networks. Entropy, 20(4), 268.

## See also

[`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_distance_entropy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.9910761  0.9182958  0.7642045  0.9182958  0.9910761  0.9910761  0.9910761 
#>   Evaluate     Create      Share 
#>  0.9910761  0.9182958  0.9910761 
```
