# Density of Maximum Neighborhood Component

The density of maximum neighborhood component (Lin et al. 2008) looks at
the subnetwork induced by the neighbors of a node, the node itself left
out, and takes its largest connected component with \\E\\ edges and
\\N\\ nodes: \$\$DMNC(v) = \frac{E}{N^{\varepsilon}}.\$\$ A node without
neighbors scores 0.

## Usage

``` r
centrality_dmnc(x, mode = "all", dmnc_epsilon = 1.7, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- dmnc_epsilon:

  Exponent \\\varepsilon\\. Default 1.7, the value Lin et al. (2008)
  recommend. The centiserve package uses 1.67.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored, and `mode` sets the neighbor set. On an
undirected network the result follows this definition. On a directed
network the component is a strongly connected component, and its nodes
are read from the neighbor list with each reciprocated neighbor listed
twice, as in the centiserve package. The edge count can then belong to a
different node set, and scores above one occur. The value of
`dmnc_epsilon` is not checked.

## References

Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
(2008). Hubba: Hub objects analyzer, a framework of interactome hubs
identification for network biology. Nucleic Acids Research, 36(suppl 2),
W438-W443. [doi:10.1093/nar/gkn257](https://doi.org/10.1093/nar/gkn257)
.

## See also

[`centrality_mnc`](https://sonsoles.me/cograph/reference/centrality_mnc.md),
[`centrality_mcc`](https://sonsoles.me/cograph/reference/centrality_mcc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_dmnc(regulation_net, directed = FALSE)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.3241313  0.3328441  0.4024631  0.2377458  0.3089754  0.2593051  0.2841969 
#>   Evaluate     Create      Share 
#>  0.3241313  0.3803933  0.3889576 
```
