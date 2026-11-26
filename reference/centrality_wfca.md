# Weighted Formal Concept Analysis Centrality

The weighted formal concept analysis (WFCA) centrality (Sun et al. 2017)
reads the network as a table of which node is tied to which, and finds
its formal concepts: pairs \\(T, P)\\ of node sets in which \\P\\ holds
every node that all members of \\T\\ are tied to, and \\T\\ holds every
node tied to all members of \\P\\. The weight of a node is \$\$W_i =
\sum\_{(T, P):\\ i \in P} \frac{\|T\|}{\|P\|}.\$\$

## Usage

``` r
centrality_wfca(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `enumeration_limit` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the enumeration was cut short.

## Details

A node gains from every group of nodes that all point to it, and gains
more when the group is large and the set of their common targets is
small. Direction is used: a tie from \\u\\ to \\v\\ makes \\v\\ an
attribute of \\u\\. Weights, loops and parallel edges are ignored.
Isolated nodes score 0.

The number of formal concepts can grow exponentially with the size of
the network. The measure is therefore costly, computed only when
requested by name or through `include`, and it stops once more than
`enumeration_limit` concepts exist (default one million): every score is
then `NA` with a `cograph_undefined_measure` warning.
`normalized = TRUE` divides the scores by their maximum.

## References

Sun, Z., Wang, B., Sheng, J., Hu, Y., Wang, Y. and Shao, J. (2017).
Identifying influential nodes in complex networks based on weighted
formal concept analysis. IEEE Access, 5, 3777-3789.
[doi:10.1109/ACCESS.2017.2679038](https://doi.org/10.1109/ACCESS.2017.2679038)
.

## See also

[`centrality_con`](https://sonsoles.me/cograph/reference/centrality_con.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_wfca(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  5.9166667  1.6666667 12.1166667  5.1666667  6.5000000  2.5333333  0.3333333 
#>   Evaluate     Create      Share 
#>  1.1166667  4.0333333  4.6166667 
```
