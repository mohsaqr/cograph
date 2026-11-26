# Mixed Core, Semi-Local Degree and Weighted Entropy

Mixed core, semi-local degree and weighted entropy (MCSDWE; Sheikhahmadi
and Nematbakhsh 2017) is
[`centrality_mcsde`](https://sonsoles.me/cograph/reference/centrality_mcsde.md)
with the weighted entropy of
[`centrality_mcdwe`](https://sonsoles.me/cograph/reference/centrality_mcdwe.md):
\$\$MCSDWE(v) = \widetilde{ks}(v) + \widetilde{SL}(v) +
\widetilde{WH}(v), \qquad WH(v) = -\sum_i \frac{p_i(v) \log_2
p_i(v)}{K - i + 1},\$\$ where the tilde marks \\(x - \min x) / (\max x -
\min x)\\ over the nodes.

## Usage

``` r
centrality_mcsdwe(x, ...)
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
The semi-local term and the rescaling are those of
[`centrality_mcsde`](https://sonsoles.me/cograph/reference/centrality_mcsde.md),
and the weight of each entropy term that of
[`centrality_mcdwe`](https://sonsoles.me/cograph/reference/centrality_mcdwe.md).
A term that is the same for every node becomes 0, and the score lies
between 0 and 3. On a network with a single shell the ranking is that of
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md).
The source prints no value of the measure, so the implementation is
checked against the definition only. Isolated nodes score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
multi-spreader users in social networks for viral marketing. Journal of
Information Science, 43(3), 412-423.
[doi:10.1177/0165551516644171](https://doi.org/10.1177/0165551516644171)
.

## See also

[`centrality_mcsde`](https://sonsoles.me/cograph/reference/centrality_mcsde.md),
[`centrality_mcdwe`](https://sonsoles.me/cograph/reference/centrality_mcdwe.md),
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcsdwe(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2307692  0.6153846  1.0000000  0.5384615  0.1538462  0.3076923  0.0000000 
#>   Evaluate     Create      Share 
#>  0.4615385  0.6923077  0.4615385 
```
