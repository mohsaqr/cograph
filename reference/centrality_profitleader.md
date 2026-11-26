# ProfitLeader

ProfitLeader (Yu et al. 2019) scores a node by the resource it can share
with its neighbors. For a neighbor \\v\\ of \\u\\, the available
resource is one more than the degree of \\u\\ plus the degrees of the
neighbors of \\u\\ that are neither \\v\\ nor neighbors of \\v\\, and
the sharing probability is the Jaccard similarity of the two closed
neighborhoods: \$\$PC(u) = \sum\_{v \in N(u)} \Bigl(k_u + 1 + \sum\_{s
\in N(u) \setminus N\[v\]} k_s\Bigr) \frac{\|N\[u\] \cap
N\[v\]\|}{\|N\[u\] \cup N\[v\]\|}.\$\$

## Usage

``` r
centrality_profitleader(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

The measure is computed on the simple undirected skeleton of the
network, so direction, weights, loops and parallel edges are ignored.
\\N\[u\]\\ is the neighborhood of \\u\\ with \\u\\ added. The one added
to the degree is that of the code the authors released: with it the
implementation reproduces all 40 positions of the top ten nodes the
source lists in its Table 5 for the karate club, dolphin, adjective-noun
and Les Miserables networks, and without it 36. The article itself is
not consistent on this point: its equation (2), and the closed forms
printed with its Fig. 3, have no added one. Isolated nodes score 0.
`normalized = TRUE` divides the scores by their maximum.

## References

Yu, Z., Shao, J., Yang, Q. and Sun, Z. (2019). ProfitLeader: identifying
leaders in networks with profit capacity. World Wide Web, 22(2),
533-553.
[doi:10.1007/s11280-018-0537-6](https://doi.org/10.1007/s11280-018-0537-6)
.

## See also

[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_profitleader(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   38.63492   52.65000   89.03889   53.25556   35.60833   37.62698   20.16667 
#>   Evaluate     Create      Share 
#>   34.51111   59.68056   38.66667 
```
