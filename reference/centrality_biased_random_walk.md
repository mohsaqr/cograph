# Biased Random Walk Centrality

Biased random walk centrality (Takes and Kosters 2011) is the share of
time a random walker spends at a node when it prefers neighbors that
have many ties and whose own neighbors are not tied to each other. From
node \\v\\ the walker moves to the neighbor \\w\\ with probability
proportional to \\\alpha f\_{deg}(w) + (1 - \alpha) f\_{nd}(w)\\, where
\$\$f\_{deg}(v) = 1 - \frac{1}{\|N(v)\|}, \qquad f\_{nd}(v) = 1 -
\sum\_{w \in N(v)} \frac{\|N(w) \cap N(v)\|}{(\|N(w)\| -
1)\\\|N(v)\|},\$\$ and with probability \\p\\ it jumps to a node chosen
uniformly.

## Usage

``` r
centrality_biased_random_walk(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `brw_alpha` (the weight of the degree term, default
  0.5) and `brw_jump` (the jump probability, default 0.15).

## Value

A named numeric vector with one score per node, in input node order,
summing to one.

## Details

The source estimates the share of visits with one walk of ten steps per
node from a random start, so its result varies from run to run. The
value returned here is the stationary distribution of the same walk, the
limit of that share, and needs no random numbers. The scores sum to one.

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
`brw_alpha` is 0.5 and `brw_jump` is 0.15 by default, the values of the
source. The source leaves three cases open. A neighbor of degree one
adds nothing to \\f\_{nd}\\. When every neighbor of a node has weight
zero the walker picks among them uniformly. From an isolated node the
walker always jumps. A `brw_alpha` outside \\\[0,1\]\\ or a `brw_jump`
outside \\(0,1\]\\ raises a `cograph_bad_parameter` error.

## References

Takes, F. W. and Kosters, W. A. (2011). Identifying prominent actors in
online social networks using biased random walks. In Proceedings of the
23rd Benelux Conference on Artificial Intelligence (BNAIC 2011),
215-222.

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_biased_random_walk(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.09562519 0.10717857 0.10851157 0.11682470 0.10427429 0.10025709 0.08266489 
#>   Evaluate     Create      Share 
#> 0.09577127 0.10051426 0.08837818 
```
