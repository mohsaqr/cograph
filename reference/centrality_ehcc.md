# Extended Hybrid Characteristic Centrality

Extended hybrid characteristic centrality (Liu and Zheng 2023) adds to a
node's
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md)
score the scores of its neighbors. \$\$EHCC(u) = HCC(u) + \sum\_{v \in
\phi(u)} HCC(v)\$\$

## Usage

``` r
centrality_ehcc(x, ...)
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

The measure inherits the conventions of
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md).
It is computed on the simple undirected skeleton, so direction, weights,
loops and parallel edges are ignored, and a `hcc_delta` outside \\\[0,
1\]\\ raises a `cograph_bad_parameter` error. Scores lie in \\\[0, 2(1 +
k\_{max})\]\\, and an isolate scores its own HCC.

## References

Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
networks based on extended degree and E-shell hierarchy decomposition.
Scientific Reports, 13, 3197.
[doi:10.1038/s41598-023-30308-5](https://doi.org/10.1038/s41598-023-30308-5)
.

## See also

[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ehcc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   8.537879  11.060606  12.356061  10.045455   7.946970   8.772727   7.212121 
#>   Evaluate     Create      Share 
#>   9.924242  11.484848  10.113636 
```
