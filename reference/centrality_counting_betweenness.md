# Counting Betweenness

Counting betweenness (Bloechl et al. 2011) is the expected number of
times a random walk passes a node on its way between two other nodes,
averaged over all ordered pairs. A walker at \\i\\ follows the tie to
\\j\\ with probability \\m\_{ij} = a\_{ij} / \sum_k a\_{ik}\\. For a
walk from \\s\\ that stops when it first reaches \\t\\, the expected
number of uses of the tie \\(i, j)\\ is \\N^{st}\_{ij} = m\_{ij}\\\[(I -
M\_{-t})^{-1}\]\_{si}\\, and \$\$C_c(i) = \frac{1}{n(n-1)} \sum\_{s}
\sum\_{t \ne s} N^{st}(i), \qquad N^{st}(i) = \sum\_{j \ne t}
\frac{N^{st}\_{ij} + N^{st}\_{ji}}{2},\$\$ with one added for the start
\\s\\, and \\N^{st}(t) = 1\\.

## Usage

``` r
centrality_counting_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `weighted` and `normalized`.

## Value

A named numeric vector with one score per node, in input node order, or
`NA` at every node when the score is undefined.

## Details

Direction, weights and loops are used. A walk that goes back and forth
over a tie is counted every time, which sets the measure apart from
random walk betweenness that counts net flow. A loop makes the walker
stay, and raises the score of its node. The scores are expected numbers
of visits and are not bounded by 1.

Following the source, a node without incoming or without outgoing ties
scores 0 and is left out of the walk. The remaining nodes must be
strongly connected. Otherwise every score is `NA` with a
`cograph_undefined_measure` warning. The measure inverts one matrix per
node, so it is costly and is computed only when requested by name or
through `include`.

## References

Bloechl, F., Theis, F. J., Vega-Redondo, F. and Fisher, E. O'N. (2011).
Vertex centralities in input-output networks reveal the structure of
modern economies. Physical Review E, 83(4), 046127.
[doi:10.1103/PhysRevE.83.046127](https://doi.org/10.1103/PhysRevE.83.046127)
.

## See also

[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md),
[`centrality_mec`](https://sonsoles.me/cograph/reference/centrality_mec.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_counting_betweenness(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  1.7872438  0.4572682  2.7767743  1.8243107  1.7936309  0.9713849  0.5105718 
#>   Evaluate     Create      Share 
#>  1.0941240  2.1476285  1.4107039 
```
