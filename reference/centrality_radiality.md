# Radiality Centrality

Radiality (Valente and Foreman 1998) reverses each distance against the
diameter \\D\\ and averages over the other nodes: \$\$R(i) =
\frac{1}{n - 1} \sum\_{j \ne i:\\ d\_{ij} \< \infty} (D + 1 -
d\_{ij}).\$\$ Nodes close to the others score high.

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

The sum runs over the other nodes, as Valente and Foreman (1998) define
it;
[`centiserve::radiality()`](https://rdrr.io/pkg/centiserve/man/radiality.html)
(and cograph 2.7.19 and earlier) also sum the node itself, which adds
\\(D + 1)/(n - 1)\\. Unreachable nodes contribute 0. Edge weights are
read as distances; `weighted = FALSE` uses hop counts, and
`invert_weights = TRUE` converts a weight \\w\\ to the distance
\\1/w^\alpha\\. `mode = "all"` treats edges as undirected, `"out"` uses
distances from the node and `"in"` distances to it. The diameter \\D\\
is the largest finite distance of the same distances (before 2.7.20 it
was taken on the stored weights in the direction of the network). A
single-node network gives `NA`.

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
#>   1.184444   1.152222   1.205556   1.106667   1.186667   1.078889   1.170000 
#>   Evaluate     Create      Share 
#>   1.124444   1.188889   1.084444 
```
