# Integration Centrality

Integration centrality (Valente and Foreman 1998) reverses the distance
from each other node against the diameter \\D\\, the largest finite hop
distance, and averages: \$\$I(i) = \frac{1}{n - 1} \sum\_{j \ne i} (D +
1 - d\_{ji}),\$\$ where a node that does not reach \\i\\ contributes 0.

## Usage

``` r
centrality_integration(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named numeric vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. Integration reads
the paths into the node (radiality reads the paths out of it); with
`mode = "all"` the two are the same. `mode = "out"` follows the ties'
direction along those paths and `"in"` reverses it. On a network without
edges every node scores 0. cograph 2.7.19 and earlier returned
[`tidygraph::centrality_integration()`](https://tidygraph.data-imaginist.com/reference/centrality.html),
which sums \\1 - (d - 1)/D\\ over every node, the node itself included,
without dividing by \\n - 1\\.

## References

Valente, T. W., & Foreman, R. K. (1998). Integration and radiality:
Measuring the extent of an individual's connectedness and reachability
in a network. Social Networks, 20(1), 89-105.
[doi:10.1016/S0378-8733(97)00007-5](https://doi.org/10.1016/S0378-8733%2897%2900007-5)
.

## See also

[`centrality_radiality`](https://sonsoles.me/cograph/reference/centrality_radiality.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_integration(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.555556   1.666667   1.777778   1.666667   1.555556   1.555556   1.444444 
#>   Evaluate     Create      Share 
#>   1.555556   1.666667   1.555556 
```
