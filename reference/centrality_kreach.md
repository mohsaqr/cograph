# Geodesic K-Reach Centrality

Geodesic k-reach centrality (Borgatti and Everett 2006) counts the nodes
that lie within shortest-path distance \\k\\ of a node.

## Usage

``` r
centrality_kreach(x, mode = "all", k = 3, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  Direction for directed networks: `"all"` (default), `"out"` or `"in"`.

- k:

  Largest distance counted, a positive number (default 3).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
  The measure uses `invert_weights` (default `NULL`, which inverts for
  tna input only) and `alpha` (inversion exponent, default 1).

## Value

A named integer vector with one count per node, in input node order.

## Details

Edge weights are read as distances, so on a weighted network \\k\\ is
compared with the summed weights along a path; on `regulation_net`,
whose weights lie below one, every node reaches all others within \\k =
1\\. `invert_weights = TRUE` converts a weight \\w\\ to the distance
\\1/w^\alpha\\. `weighted = FALSE` has no effect, because the measure
then reads the weights stored in the network; a hop-count reach needs a
binary input such as `(x != 0) * 1`. `mode = "all"` treats edges as
undirected, `"out"` counts nodes reached from the node and `"in"` nodes
that reach it. A `k` of zero or below raises an error.

## References

Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
on centrality. Social Networks, 28(4), 466-484.
[doi:10.1016/j.socnet.2005.11.005](https://doi.org/10.1016/j.socnet.2005.11.005)
.

## See also

[`centrality_geodesic_kpath`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md),
[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_kreach(regulation_net, k = 0.2)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          4          3          6          2          4          1          3 
#>   Evaluate     Create      Share 
#>          2          5          0 
```
