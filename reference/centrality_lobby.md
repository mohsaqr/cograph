# Lobby Index

The lobby index (Korn, Schubert and Telcs 2009, Definition 1) is the
largest \\k\\ such that the node has at least \\k\\ neighbors of degree
\\k\\ or more: the h-index of its neighbors' degrees.

## Usage

``` r
centrality_lobby(x, mode = "all", ...)
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

A named integer vector with one index per node, in input node order.

## Details

Edge weights are ignored. `mode` selects both the degree and the
neighbors, and with `mode = "all"` on a directed network the degree is
in plus out while a neighbor tied in both directions counts once. An
isolated node scores 0.
[`centiserve::lobby()`](https://rdrr.io/pkg/centiserve/man/lobby.html)
(and cograph 2.7.19 and earlier) take the closed neighborhood, the
node's own degree included, which can raise the index by one.

## References

Korn, A., Schubert, A., & Telcs, A. (2009). Lobby index in networks.
Physica A, 388(11), 2221-2226.
[doi:10.1016/j.physa.2009.02.013](https://doi.org/10.1016/j.physa.2009.02.013)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lobby(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          5          5          5          4          5          4 
#>   Evaluate     Create      Share 
#>          5          5          5 
```
