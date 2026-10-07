# Integration Centrality

Integration centrality (Valente and Foreman 1998) scores each distance
against the diameter \\D\\, the largest finite hop distance, and sums:
\$\$I(i) = \sum\_{j} \left(1 - \frac{d\_{ij} - 1}{D}\right),\$\$ where
an unreachable node contributes 0.

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

Distances are hop counts, so edge weights are ignored. The sum includes
the node itself, which contributes \\1 + 1/D\\, and is not divided by
\\n - 1\\; the values equal
[`tidygraph::centrality_integration()`](https://tidygraph.data-imaginist.com/reference/centrality.html).
`mode = "all"` treats edges as undirected, `"out"` uses distances from
the node and `"in"` distances to it. On a network without edges every
node scores \\n\\.

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
#>        8.5        9.0        9.5        9.0        8.5        8.5        8.0 
#>   Evaluate     Create      Share 
#>        8.5        9.0        8.5 
```
