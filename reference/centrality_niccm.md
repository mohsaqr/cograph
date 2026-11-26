# Node Importance by the Contribution Correlation Matrix

The node importance contribution correlation matrix method (NICCM; Hu et
al. 2015) lets each node hand its efficiency to the nodes around it, in
proportion to how much each of them depends on it: \$\$H_j = I_j
\sum\_{i:\\1 \le d\_{ij} \le n_l} \frac{\delta\_{ij} I_i}{d\_{ij}},\$\$
where \\I_i = \frac{1}{n}\sum\_{p \ne i} 1/d\_{ip}\\ is the efficiency
of node \\i\\ and \\\delta\_{ij}\\ is the efficiency node \\j\\ loses
when \\i\\ is deleted, as a share of the loss of all nodes at the same
distance from \\i\\.

## Usage

``` r
centrality_niccm(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `niccm_layers` (the number of layers a node
  contributes over, default 2).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`niccm_layers` is the largest distance \\n_l\\ a node contributes over,
2 by default as in the source; `Inf` uses every distance. The efficiency
after a deletion is still divided by the original number of nodes, which
is what the tables of the source require. Unreachable pairs add nothing
to an efficiency. Isolated nodes score 0. The implementation reproduces
the values the source prints for its 10-node example and for the ARPA
network. The measure deletes each node in turn and recomputes all
distances, so it is in the costly tier: it is computed when named in
`measures` and left out of the default set. A `niccm_layers` that is
neither a whole number of at least one nor `Inf` raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Hu, P., Fan, W. and Mei, S. (2015). Identifying node importance in
complex networks. Physica A: Statistical Mechanics and its Applications,
429, 169-176.
[doi:10.1016/j.physa.2015.02.002](https://doi.org/10.1016/j.physa.2015.02.002)
.

## See also

[`centrality_niem`](https://sonsoles.me/cograph/reference/centrality_niem.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_niccm(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.8032500  0.8413393  0.9060000  0.8807143  0.8115000  0.7665000  0.6365357 
#>   Evaluate     Create      Share 
#>  0.6697500  0.8075893  0.6697500 
```
