# Mu-Power Community Index

The \\\mu\\-power community index (Papadimitriou et al. 2010) of a node
is the largest \\k\\ such that at least \\\mu k\\ nodes within \\\mu\\
steps of it have a degree of at least \\k\\.

## Usage

``` r
centrality_mu_pci(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `pci_mu` (the number of steps and the multiplier,
  default 1).

## Value

A named numeric vector with one whole-number score per node, in input
node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
The node itself is not counted. `pci_mu` is 1 by default, the value the
source computes and recommends. At that value the index is the h-index
of the neighbors' degrees, the same number as
[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md).
Isolated nodes score 0. A `pci_mu` that is not a whole number of at
least one raises a `cograph_bad_parameter` error. `normalized = TRUE`
divides the scores by their maximum.

## References

Papadimitriou, A., Katsaros, D. and Manolopoulos, Y. (2010). Social
network analysis and its applications in wireless sensor and vehicular
networks. In Lecture Notes of the Institute for Computer Sciences,
Social Informatics and Telecommunications Engineering, 411-420.
[doi:10.1007/978-3-642-11631-5_37](https://doi.org/10.1007/978-3-642-11631-5_37)
.

## See also

[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mu_pci(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          5          5          5          4          5          4 
#>   Evaluate     Create      Share 
#>          5          5          5 
```
