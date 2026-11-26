# Extended H-Index Centrality

The extended H-index centrality (EHC; Zareie and Sheikhahmadi 2019) adds
over a node's neighbors a weighted count of the degrees of their own
neighbors: \$\$EHC(i) = \sum\_{j \in N(i)} CMC(j), \qquad CMC(j) =
\sum\_{k=1}^{h} p^{1 + kp/r} c_k(j),\$\$ with \\c_k(j)\\ the number of
neighbors of \\j\\ of degree at least \\k\\ and \\h\\ the largest degree
of the network.

## Usage

``` r
centrality_ehc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `ehc_p` (default 0.8) and `ehc_r` (default 100).

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`ehc_p = 0.8` and `ehc_r = 100` are the authors' values (their Section
4.3). The source gives \\\[0, 1\]\\ as the range of both parameters but
uses \\r\\ from 1 to 10000; `ehc_p` must lie in \\(0, 1\]\\ and `ehc_r`
be above 0. Since \\\sum_k c_k(j)\\ is the sum of the degrees of the
neighbors of \\j\\, CMC is that sum with the units of degree weighted by
the slowly decreasing \\p^{1 + kp/r}\\; at `ehc_p = 1` EHC is the sum,
over the walks of two steps from the node, of the degree of the node
reached. Despite its name the measure ranks nodes far from the h-index
family. An isolated node scores 0. With `ehc_p = 0.5` and `ehc_r = 10`
the implementation reproduces the worked example on the source's Fig. 1:
the largest degree 7, the two printed cumulative vectors, the 12 printed
CMC values and \\EHC(1) = 21.616\\, \\EHC(3) = 42.106\\. The source
prints a monotonicity of 0.9612 on the karate club, above 0.9542, the
largest value an exact structural measure can reach there, which is the
value of the implementation; its monotonicity on the dolphins network
(0.9979) is reproduced and is also the largest possible there. On four
test networks the score ranks nodes closest to
[`centrality_ninl`](https://sonsoles.me/cograph/reference/centrality_ninl.md)
(mean Kendall correlation 0.985, 0.997 on one network) and
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md)
(0.983); with
[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
the correlation is 0.91 to 0.92. A `ehc_p` outside \\(0, 1\]\\ or a
`ehc_r` that is not a finite number above 0 raises a
`cograph_bad_parameter` error. `normalized = TRUE` divides the scores by
their maximum.

## References

Zareie, A. and Sheikhahmadi, A. (2019). EHC: extended H-index centrality
measure for identification of users' spreading influence in complex
networks. Physica A: Statistical Mechanics and its Applications, 514,
141-155.
[doi:10.1016/j.physa.2018.09.064](https://doi.org/10.1016/j.physa.2018.09.064)
.

## See also

[`centrality_lobby`](https://sonsoles.me/cograph/reference/centrality_lobby.md),
[`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md),
[`centrality_ninl`](https://sonsoles.me/cograph/reference/centrality_ninl.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_ehc(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   117.7042   144.7222   163.8169   139.9535   116.1042   118.5156   100.2265 
#>   Evaluate     Create      Share 
#>   126.4714   146.3264   127.2657 
centrality_ehc(regulation_net, ehc_p = 0.5, ehc_r = 10)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   66.10316   81.05179   91.85468   78.40366   65.10770   66.72017   56.47797 
#>   Evaluate     Create      Share 
#>   71.21751   82.08875   71.65179 
```
