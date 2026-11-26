# PhysarumSpreader

PhysarumSpreader (Wang et al. 2015) lets a resource flow over the
network as in the Physarum model, through a ground node linked both ways
to every node, with the conductivity of each arc adapting to its flux,
and scores a node by the resource it holds after \\t\\ updates: \$\$S_i
= I_i(t) + \frac{I_g(t)}{N}, \qquad I_i(t+1) = \sum_j Q\_{ji}(t) I_j(t),
\qquad Q\_{ij}(t) = \frac{w\_{ij} D\_{ij}(t)}{\sum_l w\_{il}
D\_{il}(t)}, \qquad D\_{ij}(t+1) = \frac{Q\_{ij}(t) +
D\_{ij}(t)}{2},\$\$ with \\I(0) = 1\\ at every node and 0 at the ground
node \\g\\, \\D(0) = 1\\, \\w\_{ij}\\ the weight of the arc \\i \to j\\,
\\w\_{ig}\\ the mean weight of the arcs leaving \\i\\ (1 when there is
none), \\w\_{gi} = 1\\ and \\N\\ the number of nodes.

## Usage

``` r
centrality_physarum_spreader(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `physarum_steps` (the number of updates, required)
  and the edge weights (`weighted`).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure uses the direction and the weights of the arcs; an
undirected tie is an arc each way. The source reads a weight as a
strength; for a cost, give its reciprocal. Loops are ignored. The
conductivities of the ground links stay at 1, as the source's equation
(10) adapts only the arcs between nodes. `physarum_steps`, the number of
updates \\t\\, has no default, because the source gives no stopping
rule: as \\t\\ grows every score tends to 1, so a stopping rule by
tolerance would return a constant (owner decision 2026-11-26). Without
`physarum_steps` the measure warns (class `cograph_undefined_measure`)
and returns `NA`. On four test networks, with `physarum_steps = 52`, the
score ranks nodes closest to
[`centrality_beta_measure`](https://sonsoles.me/cograph/reference/centrality_beta_measure.md)
(mean Kendall correlation 0.987, 0.984 at the least) and `leverage`
(0.96); with
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md)
the correlation is 0.84 to 0.91. On the karate club the ranking after 52
updates has a Kendall correlation of 0.989 with the ranking after 10000,
while the range of the scores shrinks from 0.115 to 0.0006. The scores
the source prints for its four-node example are those after 52 updates.
With `physarum_steps = 52` the implementation reproduces the printed
scores of nodes 3 and 4 (1.0002 and 0.9801) and gives 1.0097 to node 1
and 1.0100 to node 2, where the source prints them the other way round.
The source audit found no update order consistent with the source's
printed first flux matrix that gives node 1 the larger score after the
fifth update, so the exchange is declared here, not resolved. The
printed first flux matrix is reproduced. A `physarum_steps` that is not
a whole number of at least one raises a `cograph_bad_parameter` error,
and negative weights a `cograph_negative_weights` error.
`normalized = TRUE` divides the scores by their maximum.

## References

Wang, H., Zhang, Y., Zhang, Z., Mahadevan, S. and Deng, Y. (2015).
PhysarumSpreader: a new bio-inspired methodology for identifying
influential spreaders in complex networks. PLoS ONE, 10(12), e0145028.
[doi:10.1371/journal.pone.0145028](https://doi.org/10.1371/journal.pone.0145028)
.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality_leaderrank`](https://sonsoles.me/cograph/reference/centrality_leaderrank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_physarum_spreader(regulation_net, physarum_steps = 52)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.8210857  0.8167787  1.1894243  1.1035349  1.2118676  0.9707841  0.8167788 
#>   Evaluate     Create      Share 
#>  1.2143542  1.0384194  0.8169723 
centrality_physarum_spreader(regulation_net, physarum_steps = 52, weighted = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.0087486  0.9928875  1.0196148  1.0035785  1.0099955  0.9903805  0.9861890 
#>   Evaluate     Create      Share 
#>  0.9886590  1.0008536  0.9990929 
```
