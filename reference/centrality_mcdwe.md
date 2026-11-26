# Mixed Core, Degree and Weighted Entropy

Mixed core, degree and weighted entropy (MCDWE; Sheikhahmadi and
Nematbakhsh 2017) is
[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md)
with the entropy weighted toward the inner shells: \$\$MCDWE(v) =
ks(v) + k_v - \sum_i \frac{p_i(v) \log_2 p_i(v)}{K - i + 1},\$\$ where
\\p_i(v)\\ is the share of the neighbors of \\v\\ in shell \\i\\ and
\\K\\ the largest shell index of the network.

## Usage

``` r
centrality_mcdwe(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Neighbors in the innermost shell have weight 1, those one shell further
out 1/2, and so on: the weight belongs to the shell of each term, as the
source's equation (11) prints it. The Centrality Zoo gives one weight
per node, built from the number of distinct shells among the neighbors,
which is not the source's. The terms have weight 1 and are added as they
are, as for
[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md),
and the score ranks nodes much like the degree (Kendall correlation 0.97
on four test networks). The source prints no value of this measure, so
it is checked against the definition only; the shell shares it uses are
those of
[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md),
whose printed values are reproduced. Isolated nodes score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
multi-spreader users in social networks for viral marketing. Journal of
Information Science, 43(3), 412-423.
[doi:10.1177/0165551516644171](https://doi.org/10.1177/0165551516644171)
.

## See also

[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md),
[`centrality_mcsdwe`](https://sonsoles.me/cograph/reference/centrality_mcsdwe.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcdwe(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          9         10         11         10          9          9          8 
#>   Evaluate     Create      Share 
#>          9         10          9 
```
