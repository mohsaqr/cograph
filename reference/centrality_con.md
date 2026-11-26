# Common Out-Neighbor Score

The common out-neighbor (CON) score (Bonato et al. 2019) counts the
out-neighbors a node shares with every other node: \$\$CON(u) = \sum\_{v
\ne u} \sum_k \min(A\_{uk}, A\_{vk}),\$\$ where \\A\_{uk}\\ is the
number of arcs from \\u\\ to \\k\\. Two nodes that send ties to the same
targets are competitors, and a node with a low score has few
competitors.

## Usage

``` r
centrality_con(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `weighted` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Direction is used: the targets of a node's outgoing ties are compared.
On an undirected network every tie counts in both directions, and the
score is the sum over a node's neighbors of their degree minus one. With
`weighted = TRUE` the weights are read as the numbers of interactions
and the smaller of the two is counted. Loops are ignored. A node without
outgoing ties scores 0. `normalized = TRUE` divides the scores by their
maximum.

## References

Bonato, A., Eikmeier, N., Gleich, D. F. and Malik, R. (2019). Centrality
in dynamic competition networks. In Complex Networks and Their
Applications VIII, Studies in Computational Intelligence 881, 105-116.
[doi:10.1007/978-3-030-36683-4_9](https://doi.org/10.1007/978-3-030-36683-4_9)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_con(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>       1.27       2.16       0.66       0.81       0.80       1.52       1.23 
#>   Evaluate     Create      Share 
#>       1.61       1.87       1.51 
```
