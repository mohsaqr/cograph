# Lhc Index

Wang, Yang, Liu and Ma's Lhc index is a semi-local hybrid: it reads a
node's *neighbour information* from degree and its *topological
location* from the share of the network's triangles that sit on it, then
spreads both over a small ball and collects the result one step out. The
influence of a node is \\C(v)=\sum\_{u\in\Phi(v)}k_u(1+TP(u))/d^2(uv)\\,
a sum over the ball \\\Phi(v)\\ of radius `lhc_radius` in which each
member contributes its degree, inflated by its triangle share,
discounted by the square of its distance; and the index itself is
\\Lhc(v)=\sum\_{w\in\tau(v)}C(w)\\, the influence summed over the open
neighbourhood \\\tau(v)=N(v)\\. The triangle share is
\\TP(u)=NTS(u)/TNTS\\, with \\NTS(u)\\ the number of triangles
containing \\u\\ and \\TNTS=\sum_u NTS(u)\\.

## Usage

``` r
centrality_lhc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `lhc_radius`.

## Value

Named numeric vector in input node order.

## Details

**The denominator is \\TNTS\\, not the number of triangles, and the
paper settles it rather than the Zoo.** Immediately after defining
\\TNTS\\ the source writes that "the total number of triangle structure
exists in the network are \\\frac{1}{3}\*TNTS\\", so \\TNTS=3\Delta\\
for \\\Delta\\ distinct triangles and \\TP\\ sums to exactly one over
the nodes – it really is a share. Entry 2.221 of the Centrality Zoo
transcribes the structure of both equations correctly but names the
denominator "\\\Delta\\, the total number of triangular structures in
the network", which read literally is three times too small. The two
readings are not related by a monotone transform in general, and they
differ substantially: on the Krackhardt kite the paper's reading scores
node 1 at \\100.15\\ where the Zoo's literal wording gives \\125.45\\.
cograph follows the paper.

**`lhc_radius` is the source's own parameter, exposed with the source's
default.** The paper writes it \\d\\, states on page 4 that "the
distance ranged \\d\\ is set to be 2, namely, only the nearest neighbors
and the next-nearest neighbors are taken into consideration", and then
sweeps it in section 3 over eleven real networks, reporting that "the
optimal value of \\d\\ is about 2-3" and that the correlation stabilises
beyond 3. It is therefore a genuine modelling knob rather than an
implementation detail, and it is exposed with the paper's 2 as the
default. At `lhc_radius = 1` the ball collapses to the neighbours and
\\C(v)\\ becomes \\\sum\_{u\in N(v)}k_u(1+TP(u))\\; a radius at or above
the graph's diameter takes in everything reachable and the score stops
moving. The domain is a whole number of at least one; anything else is
refused with a `cograph_bad_parameter` error.

**Both neighbourhoods are open, and a node contributes to its own
score.** \\\Phi(v)\\ is \\1\le d(u,v)\le\\ `lhc_radius`: the focal node
is outside it, because \\d^2(vv)=0\\ would divide by zero, and
unreachable nodes fall outside the radius so no infinity arises.
\\\tau(v)\\ is the open neighbourhood. It follows – the paper does not
remark on it, but its equations say so – that \\v\\ does enter its own
\\Lhc(v)\\, since \\v\\ lies in \\\Phi(w)\\ at distance 1 for every
neighbour \\w\\.

**Triangle-free graphs are a cograph decision, taken explicitly.** Every
tree, star, path, even cycle and bipartite graph has \\TNTS=0\\, and
\\TP(u)\\ is then \\0/0\\ everywhere. The source never mentions the
case. Since \\TNTS\\ is a sum of nonnegative counts, it vanishes exactly
when every numerator \\NTS(u)\\ vanishes too, so there is no share to
distribute and no node with a claim on one: \\TP\\ is written as
**zero**, and the index reduces to the pure degree-over-squared-distance
sum, which is the neighbour and location half of the hybrid with the
triangle half contributing nothing. The test is made on \\TNTS\\ before
any division, so no \\0/0\\ is evaluated; `NA` or an error would refuse
every tree, which the source's own construction handles perfectly well.

**Raw scores are not component-local.** \\TNTS\\ is a global sum, so
attaching a disconnected component that carries a triangle rescales
every \\TP\\ and moves every score. Attaching a component with no
triangle – an isolate included – changes nothing, since it changes no
degree, no triangle and no finite distance inside the existing
components. An isolate itself scores zero because \\\tau(v)\\ is empty
and equation (2) is an empty sum; a singleton graph and every node of an
edgeless graph score zero for the same reason, and an empty graph
returns no scores.

Direction, weights, loops and parallel edges are dropped to the simple
undirected skeleton the source defines on: \\k_u\\ is a count, \\d(uv)\\
a hop count and \\NTS(u)\\ a combinatorial quantity, and the paper's
eleven networks are simple and undirected. There is no in/out/all
variant to select, so the measure sits in the no-mode family, and
`cutoff` and `invert_weights` are ignored as well. The source states no
normalization, so `normalized = TRUE` max-scales the finished vector as
elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

**The source prints no numerical example.** There is no toy graph with a
table of scores anywhere in the paper – its Table 1 lists network
statistics and its figures are aggregate SIR and Kendall plots – so
there is no published per-node fixture to reproduce. Verification rests
instead on independent reference implementations and on hand-derived
closed forms for stars, complete graphs, rings and paths.

## References

Wang, X., Yang, Q., Liu, M. and Ma, X. (2021). Comprehensive influence
of topological location and neighbor information on identifying
influential nodes in complex networks. PLoS ONE, 16(5), e0251208.
Equation (1) and its symbol list on page 3, equation (2), the \\d=2\\
statement and Algorithm 1 on page 4, and the \\d\\ sweep on page 7.
[doi:10.1371/journal.pone.0251208](https://doi.org/10.1371/journal.pone.0251208)
.

## See also

[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md)
and
[`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md)
for other degree-and-position hybrids,
[`centrality_neighbor_distance`](https://sonsoles.me/cograph/reference/centrality_neighbor_distance.md)
for another distance-discounted neighbourhood sum, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# The path 1-2-3 is triangle-free, so the triangle share drops out and
# the scores are the hand-derived 2, 4.5, 2.
centrality_lhc(igraph::make_graph(c(1, 2, 2, 3), directed = FALSE))
#>   1   2   3 
#> 2.0 4.5 2.0 

# On a complete graph every node scores (n-1)^3 (n+1) / n; for n = 5
# that is 76.8.
centrality_lhc(igraph::make_full_graph(5))
#>    1    2    3    4    5 
#> 76.8 76.8 76.8 76.8 76.8 

# Widening the ball can only raise the score, and it stops moving once
# the radius reaches the diameter.
ring <- igraph::make_ring(9)
centrality_lhc(ring, lhc_radius = 1)
#> 1 2 3 4 5 6 7 8 9 
#> 8 8 8 8 8 8 8 8 8 
centrality_lhc(ring)
#>  1  2  3  4  5  6  7  8  9 
#> 10 10 10 10 10 10 10 10 10 
centrality_lhc(ring, lhc_radius = 4)
#>        1        2        3        4        5        6        7        8 
#> 11.38889 11.38889 11.38889 11.38889 11.38889 11.38889 11.38889 11.38889 
#>        9 
#> 11.38889 
```
