# Radiality Centrality

Radiality (Valente and Foreman 1998) reverses each distance against the
diameter \\D\\ and averages over the network: \$\$R(i) = \frac{1}{n - 1}
\sum\_{j:\\ d\_{ij} \< \infty} (D + 1 - d\_{ij}).\$\$ Nodes close to the
others score high.

## Usage

``` r
centrality_radiality(x, mode = "all", ...)
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
  The measure uses `weighted` (default `TRUE`), `invert_weights`
  (default `NULL`, which inverts for tna input only), `alpha` (inversion
  exponent, default 1) and `cutoff` (largest distance counted, default
  -1 for no limit).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The sum includes the node itself, which contributes \\D + 1\\, and the
values equal
[`centiserve::radiality()`](https://rdrr.io/pkg/centiserve/man/radiality.html)
on undirected networks. Unreachable nodes contribute 0. Edge weights are
read as distances; `weighted = FALSE` uses hop counts, and
`invert_weights = TRUE` converts a weight \\w\\ to the distance
\\1/w^\alpha\\. `mode = "all"` treats edges as undirected, `"out"` uses
distances from the node and `"in"` distances to it. The diameter is
taken on the stored edge weights in the direction of the network,
whatever `mode` and `invert_weights` are. A single-node network gives
`NA`.

## References

Valente, T. W., & Foreman, R. K. (1998). Integration and radiality:
Measuring the extent of an individual's connectedness and reachability
in a network. Social Networks, 20(1), 89-105.
[doi:10.1016/S0378-8733(97)00007-5](https://doi.org/10.1016/S0378-8733%2897%2900007-5)
.

## See also

[`centrality_integration`](https://sonsoles.me/cograph/reference/centrality_integration.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_radiality(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.973333   1.941111   1.994444   1.895556   1.975556   1.867778   1.958889 
#>   Evaluate     Create      Share 
#>   1.913333   1.977778   1.873333 
```
