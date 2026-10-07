# Hybrid Characteristic Centrality

Hybrid characteristic centrality (Liu and Zheng 2023) adds the extended
degree \\k^{ex}(u) = \delta k(u) + (1 - \delta) \sum\_{v \in \phi(u)}
k(v)\\ to the E-shell position index \\pos(u)\\, each divided by its
maximum. The E-shell decomposition removes, round by round, the
remaining nodes of minimum extended degree, recomputed on the residual
graph, and \\pos(u)\\ is the round in which \\u\\ leaves. \$\$HCC(u) =
\frac{k^{ex}(u)}{k^{ex}\_{max}} + \frac{pos(u)}{pos\_{max}}\$\$

## Usage

``` r
centrality_hcc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `hcc_delta` (weight \\\delta\\ of the node's own
  degree in the extended degree, default 0.5).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
Scores lie in \\\[0, 2\]\\. Equation (4) uses the extended degrees of
the original graph, as the source's worked example requires. The
normalizers are global, so adding a disconnected component can change
every score. An isolate leaves in the first round, and every node of an
edgeless graph scores one. A `hcc_delta` outside \\\[0, 1\]\\ raises a
`cograph_bad_parameter` error. Step 3 of the source's E-shell procedure
prints \\\arg\max\\ where its text and Table 2 require \\\arg\min\\, and
the minimum is implemented. The Centrality Zoo describes the E-shell
decomposition as a k-shell variant, which gives different positions on
the source's Figure 1.

## References

Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
networks based on extended degree and E-shell hierarchy decomposition.
Scientific Reports, 13, 3197.
[doi:10.1038/s41598-023-30308-5](https://doi.org/10.1038/s41598-023-30308-5)
.

## See also

[`centrality_ehcc`](https://sonsoles.me/cograph/reference/centrality_ehcc.md),
[`centrality_dkgm`](https://sonsoles.me/cograph/reference/centrality_dkgm.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_hcc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.2272727  1.8636364  2.0000000  1.5075758  1.0378788  1.2500000  0.8030303 
#>   Evaluate     Create      Share 
#>  1.6287879  1.8863636  1.6287879 
```
