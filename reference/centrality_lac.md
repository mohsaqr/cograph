# Local Average Connectivity

Local average connectivity (Li et al. 2011) is the mean degree of the
neighbors of a node within the subgraph \\C_v\\ induced by those
neighbors: \$\$LAC(v) = \frac{1}{k_v} \sum\_{u \in N(v)} k_u^{C_v},\$\$
where \\k_u^{C_v}\\ is the degree of \\u\\ inside \\C_v\\. High values
mark nodes whose neighbors are tied to each other.

## Usage

``` r
centrality_lac(x, mode = "all", ...)
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

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored. `mode` selects the neighbors and the degree
counted inside \\C_v\\. With `mode = "all"` on a directed network
in-ties and out-ties are both counted, so a reciprocated tie counts
twice. An isolated node scores 0.

## References

Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
connectivity-based method for identifying essential proteins from the
network level. *Computational Biology and Chemistry*, 35(3), 143-150.

## See also

[`centrality_dmnc`](https://sonsoles.me/cograph/reference/centrality_dmnc.md),
[`centrality_mnc`](https://sonsoles.me/cograph/reference/centrality_mnc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_lac(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   1.666667   2.285714   3.000000   1.666667   1.000000   2.000000   1.500000 
#>   Evaluate     Create      Share 
#>   2.400000   2.571429   2.333333 
```
