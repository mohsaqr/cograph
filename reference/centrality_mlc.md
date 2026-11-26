# Modified Local Centrality

The modified local centrality (MLC; Ma and Ma 2017) is the semi-local
centrality of a node less twice the sum of the degrees of its neighbors:
\$\$MLC(v) = \sum\_{u \in N(v)} \sum\_{w \in N(u)} N_2(w) - 2 \sum\_{u
\in N(v)} k_u,\$\$ where \\N_2(w)\\ is the number of nodes within two
steps of \\w\\ and \\k_u\\ the degree of \\u\\. The subtraction removes
the part of the semi-local score that the node and its neighbors
contribute themselves.

## Usage

``` r
centrality_mlc(x, ...)
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
The first term is
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
and the two scores rank nodes almost alike (Kendall correlation of 0.99
on four test networks). The score can be negative on a small component:
both ends of an isolated edge score -1. Isolated nodes score 0. The
source prints no value of the measure, so the implementation is checked
against the definition only. `normalized = TRUE` divides the scores by
their maximum.

## References

Ma, Q. and Ma, J. (2017). Identifying and ranking influential spreaders
in complex networks with consideration of spreading probability. Physica
A: Statistical Mechanics and its Applications, 465, 312-330.
[doi:10.1016/j.physa.2016.08.041](https://doi.org/10.1016/j.physa.2016.08.041)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mlc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>        189        224        259        217        182        196        168 
#>   Evaluate     Create      Share 
#>        210        231        210 
```
