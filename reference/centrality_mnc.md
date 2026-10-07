# Maximum Neighborhood Component

The maximum neighborhood component (Lin et al. 2008) is the number of
nodes in the largest connected component of the subgraph induced by the
neighbors of a node.

## Usage

``` r
centrality_mnc(x, mode = "all", ...)
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

A named integer vector with one size per node, in input node order.

## Details

Edge weights are ignored and the neighbor subgraph is read as
undirected. `mode` selects the neighbors. An isolated node scores 0. On
undirected networks the values equal
[`centiserve::mnc()`](https://rdrr.io/pkg/centiserve/man/mnc.html).

## References

Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
(2008). Hubba: hub objects analyzer, a framework of interactome hubs
identification for network biology. Nucleic Acids Research, 36,
W438-W443. [doi:10.1093/nar/gkn257](https://doi.org/10.1093/nar/gkn257)
.

## See also

[`centrality_dmnc`](https://sonsoles.me/cograph/reference/centrality_dmnc.md),
[`centrality_lac`](https://sonsoles.me/cograph/reference/centrality_lac.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_mnc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          5          6          7          6          3          5          4 
#>   Evaluate     Create      Share 
#>          5          6          5 
```
