# Mixed Core, Semi-Local Degree and Entropy

Mixed core, semi-local degree and entropy (MCSDE; Sheikhahmadi and
Nematbakhsh 2017) replaces the degree of
[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md)
by the semi-local centrality and rescales the three terms before adding
them: \$\$MCSDE(v) = \widetilde{ks}(v) + \widetilde{SL}(v) +
\widetilde{H}(v),\$\$ where \\SL\\ is the score
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
returns, \\H\\ the entropy of the shells of the neighbors and the tilde
marks \\(x - \min x) / (\max x - \min x)\\ over the nodes.

## Usage

``` r
centrality_mcsde(x, ...)
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
The source names Chen et al.'s semi-local method for the semi-local
degree, and says that min-max normalization is used because the three
amounts are on very different scales. Its wording reads \\(x - \min) /
\max\\; the textbook rule it cites, used here, divides by the range. A
term that is the same for every node becomes 0. Without the rescaling
the semi-local term swamps the other two and the score ranks the nodes
as
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
does. The score lies between 0 and 3. On a network with a single shell
the shell and entropy terms are constant, and the ranking is again that
of
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

[`centrality_mcsdwe`](https://sonsoles.me/cograph/reference/centrality_mcsdwe.md),
[`centrality_mcde`](https://sonsoles.me/cograph/reference/centrality_mcde.md),
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mcsde(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.2307692  0.6153846  1.0000000  0.5384615  0.1538462  0.3076923  0.0000000 
#>   Evaluate     Create      Share 
#>  0.4615385  0.6923077  0.4615385 
```
