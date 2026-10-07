# Bridging Centrality

Bridging centrality (Hwang et al. 2008) is the product of betweenness
\\B(v)\\ and the bridging coefficient, which compares the inverse degree
of a node with the inverse degrees of its neighbors: \$\$BrC(v) = B(v)
\frac{1/k_v}{\sum\_{u \in N(v)} 1/k_u}.\$\$ High values mark nodes that
lie on many shortest paths and connect densely linked regions.

## Usage

``` r
centrality_bridging(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `directed` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The betweenness factor reads edge weights as path lengths and follows
the edge direction of a directed network. `weighted = FALSE` uses hop
counts, and `invert_weights` has no effect. The degrees are total
degrees, and on a directed network a reciprocated neighbor enters the
sum twice. An isolated node scores 0.

## References

Hwang, W., Kim, T., Ramanathan, M., & Zhang, A. (2008). Bridging
centrality: Graph mining from element level to group level. In
Proceedings of the 14th ACM SIGKDD International Conference on Knowledge
Discovery and Data Mining (pp. 336-344).
[doi:10.1145/1401890.1401934](https://doi.org/10.1145/1401890.1401934) .

## See also

[`centrality_local_bridging`](https://sonsoles.me/cograph/reference/centrality_local_bridging.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_bridging(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.8254717  1.7697431  1.6321244  2.2556391  1.5037594  0.1272727  2.7029703 
#>   Evaluate     Create      Share 
#>  0.8064000  1.6490486  1.6912752 
```
