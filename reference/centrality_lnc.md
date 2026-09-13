# Local neighbor contribution centrality

The local neighbor contribution (LNC) of Dai, Wang, Sheng, Sun, Khawaja,
Ullah, Dejene and Duan multiplies what a node contributes on its own by
what its neighbourhood contributes to it:
\\LNC(i)=d_i^{3}\\(1-1/d_i)^{d_i-1}\\ \bigl(\sum\_{j\in
N(i)}d_j\bigr)/(n-1)\\, with \\0^0=1\\. The first two factors are the
source's *own contribution* \\ownCon(i)=d_i(1-1/d_i)^{d_i-1}\\, the
chance that a node picking one neighbour uniformly at random reaches a
given one and misses the rest, scaled by its degree; the rest is the
*neighbour contribution* \\neiCon(i)=d_i^{2}\sum\_{j\in
N(i)}d_j/(n-1)\\, the source's cluster degree weighted by its
neighbours' degree centralities.

## Usage

``` r
centrality_lnc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

The measure takes no parameters. The source calls this out as a feature,
"Parameter-Free: LNC does not rely on prior knowledge and parameter
adjustments", so none is offered.

**Raw scores are not comparable across graphs of different order.** The
\\1/(n-1)\\ comes from the degree centrality of equation (1), where
\\n\\ is the vertex count of the whole network, not of the node's
component. Adding a disconnected component therefore multiplies every
score by \\(n-1)/(n'-1)\\, leaving the ranking alone and the raw values
not.

**The source's printed equations do not literally give its printed
numbers, and cograph follows the numbers.** Equations (4) and (5) both
sum a term over \\j=1,\dots,k\\, and \\k\\ is described three
incompatible ways: the prose calls it the number of nearest and next
nearest neighbours, Algorithm 1 line 12 sets it to the degree, and
equation (5) taken literally carries one factor of \\d_i\\ too many. The
printed intermediates \\D(v_5)=12\\, \\ownCon(v_5)=1.6875\\ and
\\neiCon(v_5)=19.2\\, together with all eleven Table 1 influences, are
reproduced by exactly one pair of factors, the one above: \\k\\ acts as
\\d_i\\ in (5) and as \\d_i^2\\ in (4). The equally literal split that
moves one \\d_i\\ from the neighbour factor to the own factor gives the
same product, so the measure itself is unambiguous.

**This is not the Centrality Zoo's formula.** Zoo section 2.238 writes
the own contribution as \\d_i\|N^{(\le 2)}(i)\|\sum\_{j\in N^{(\le
2)}(i)}(1/d_j) (1-1/d_j)^{\|N^{(\le 2)}(i)\|-1}\\, replacing the focal
node's own contribution probability \\P(v_i)\\ by each neighbour's
\\P(v_j)\\ and the binomial count \\d_i\\ by the size of the two-hop
neighbourhood; its neighbour factor is right in form but uses that same
two-hop size where the printed numbers need \\d_i^2\\. On the source's
own Figure 1 the Zoo reading reproduces none of the eleven printed
values and inverts the paper's headline ranking, scoring \\v_8\\ 32.23
above \\v_5\\ 28.90 where the paper prints 32.4 for \\v_5\\ and 29.7 for
\\v_8\\, and lifting the degree-two nodes \\v_6, v_7\\ above the
degree-three \\v_9\\. cograph implements the paper. No Zoo variant is
offered.

Uses the simple undirected unweighted skeleton, which is the source
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, mode, cutoff and path-weight inversion are
ignored. Isolates score zero, and so does the single node of a singleton
graph: the source has no value there, since \\P(v_i)=1/0\\ and the
\\n-1\\ denominator vanishes, and zero is a cograph extension chosen
because \\d_i^3\\ and the empty neighbour-degree sum are both zero.
Empty graphs return no scores. The source states no normalization;
`normalized = TRUE` max-scales the finished vector as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).
Nothing overflows: the cubed degree is bounded by \\n^3\\, the
neighbour-degree sum by twice the edge count, and the binomial factor
lies in \\\[1/4, 1\]\\. Cost is one sparse matrix-vector product, O(n +
m).

Numerical verification establishes agreement with the source's printed
Table 1 and printed intermediates, not parity with author software,
which does not exist, and not any claim about spreading performance.

## References

Dai, J., Wang, B., Sheng, J., Sun, Z., Khawaja, F. R., Ullah, A.,
Dejene, D. A. and Duan, G. (2019). Identifying influential nodes in
complex networks based on local neighbor contribution. IEEE Access, 7,
131719-131731. Definitions 1-5, equations (1)-(6) and Algorithm 1,
journal pages 131721-131723, with the Figure 1 graph and Table 1 on page
131720.
[doi:10.1109/ACCESS.2019.2939804](https://doi.org/10.1109/ACCESS.2019.2939804)
.

## See also

[`centrality_semilocal`](https://sonsoles.me/cograph/reference/centrality_semilocal.md)
and
[`centrality_neighbor_distance`](https://sonsoles.me/cograph/reference/centrality_neighbor_distance.md)
for other neighbourhood sums, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# Every node of a ring has degree two and a neighbour-degree sum of four
centrality_lnc(igraph::make_ring(6))
#>   1   2   3   4   5   6 
#> 3.2 3.2 3.2 3.2 3.2 3.2 

# A star: the centre carries the whole neighbourhood
centrality_lnc(igraph::make_star(5, mode = "undirected"))
#>  1  2  3  4  5 
#> 27  1  1  1  1 
```
