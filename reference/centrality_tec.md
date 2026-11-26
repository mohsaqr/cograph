# Total Effects Centrality

Total effects centrality (Friedkin 1991) is the average total effect of
a node on the opinions of the other nodes, \$\$TEC_j = \frac{\sum\_{i
\ne j} v\_{ij}}{n - 1},\$\$ where \\V\\ is the matrix of total
interpersonal effects of the influence process. The influence matrix
\\W\\ is the adjacency matrix with a unit diagonal, divided by its row
sums. When the process runs to consensus every row of \\V\\ is the
stationary vector \\c\\ of \\W\\, \\c' = c'W\\, and \\TEC_j = c_j\\.
This is the value returned.

## Usage

``` r
centrality_tec(x, ...)
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

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the influence chain is reducible or the network
has one node.

## Details

Direction is used, and weights, loops and parallel edges are ignored, so
the unit diagonal is calibrated against unit edges. `mode` has no
effect. The scores sum to one. On an undirected network the score of a
node is its degree plus one, divided by the sum of that quantity over
the nodes. The measure needs an irreducible chain, that is a connected
undirected or a strongly connected directed network. Otherwise, and on a
single node, every score is `NA` with a `cograph_undefined_measure`
warning. The measure is computed only when requested by name or through
`include`.

## References

Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
American Journal of Sociology, 96(6), 1478-1504.
[doi:10.1086/229694](https://doi.org/10.1086/229694) .

## See also

[`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md),
[`centrality_mec`](https://sonsoles.me/cograph/reference/centrality_mec.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_tec(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.16540806 0.04618324 0.18011100 0.12941498 0.13289055 0.05340127 0.04313833 
#>   Evaluate     Create      Share 
#> 0.03729112 0.10135565 0.11080581 
```
