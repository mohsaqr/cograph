# Randomized Shortest Paths Betweenness Centrality

Kivimaki, Lebichot, Saramaki and Saerens interpolate between
shortest-path betweenness and a random-walk quantity with a single knob.
They place a Boltzmann distribution over the absorbing walks from \\s\\
to \\t\\, tilted by an inverse temperature \\\beta\\ away from the
unbiased random walk and towards low-cost walks, and score a node by the
expected number of visits it receives summed over every ordered
source-target pair:
\\bet_i=\sum\_{s,t}(z\_{si}/z\_{st}-z\_{ti}/z\_{tt})z\_{it}\\, where
\\Z=(I-W)^{-1}\\ is the fundamental matrix of the killed random walk
\\W=(D^{-1}A)\circ\exp(-\beta C)\\. Large `rsp_beta` concentrates the
distribution on shortest paths; `rsp_beta` towards zero relaxes it to
the plain random walk, where the source states the score becomes
proportional to degree on an undirected graph.

## Usage

``` r
centrality_rsp_betweenness(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `rsp_beta` and `rsp_cost`.

## Value

Named numeric vector in input node order.

## Details

**The published closed form is only defined on a strongly connected
graph, and the source says what to do otherwise.** Equation (15) divides
by every entry of \\Z\\, and Algorithm 1 takes "a directed strongly
connected graph" as its input, but the text below equation (9) settles
the general case directly: the derivation "holds only if there exists a
path from \\s\\ to \\t\\. Otherwise, naturally,
\\\bar\eta\_{ij}(s,t)=0\\." cograph applies that zero rule to the whole
term of an unreachable pair and evaluates the closed form masked by
reachability, which reproduces equation (15) to machine precision
whenever the graph is strongly connected and extends it consistently
when it is not. The mask has to reach *both* halves of the term, since
both come from the same \\\bar n_i(s,t)\\; `NetworkToolbox::rspbc()`
masks only the reciprocal and leaves the \\n\\\mathrm{Diag}(Z^{\div})\\
half counting every source, so the two part company on a disconnected
graph and agree exactly on a strongly connected one.

**The consequence is that scores are component-local.** A node's score
depends only on the pairs it can stand between, so two disjoint
triangles score exactly what one triangle scores, and adding a
disconnected component – an isolate included – leaves every existing
score untouched. That is a cograph decision, taken because the source
resolves the unreachable pair rather than because the source discusses
disconnected graphs, which it does not.

**Zero out-degree is a derived zero, not an imputed one.** \\D^{-1}\\ is
undefined at out-degree zero. cograph writes that row of \\P^{ref}\\ as
zero, which is the paper's own killed random walk read at a node where
the walker dies at once; \\Z\\ then has \\z\_{ii}=1\\ and the arithmetic
gives exactly \\1-1=0\\. An isolate, a singleton graph and every node of
an edgeless graph therefore score zero because the formula says so.
`NetworkToolbox::rspbc()` raises an error on such input, and
[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md)
returns `NA` on disconnected input; this measure is able to answer where
those cannot, because \\(I-W)\\ stays nonsingular whatever the
connectivity.

**`rsp_beta` defaults to 0.01, which is not the source's number.** The
paper fixes no default and treats \\\beta\\ as a modelling choice; 0.01
is the value recommended by `NetworkToolbox::rspbc()`, adopted here so
that the two implementations are directly comparable out of the box. It
sits near the high-temperature end, so the default reading is close to
the random-walk limit and far from shortest-path betweenness – raise it,
to 1 or beyond, to move towards shortest paths. The domain is
\\\beta\>0\\; zero and negative values are refused with a
`cograph_bad_parameter` error rather than extended, since \\\beta\le 0\\
is outside the Boltzmann model and can make \\W\\ leave the
substochastic regime the inverse depends on.

**`rsp_cost` chooses how a weight becomes a cost, because the source
leaves \\C\\ free.** Algorithm 1 takes the cost matrix as an input and
never derives it from the weights. `"inverse"`, the default, sets
\\C=1/w\\, reading a weight as an affinity so a heavier edge is cheaper;
this is cograph's usual convention for a weight and the one
`NetworkToolbox::rspbc()` hard-codes. `"weight"` sets \\C=w\\, reading a
weight as a distance. The two coincide on a binary graph, where both
give unit cost per arc, so the choice only bites on genuinely weighted
input. Negative or non-finite weights are refused with a
`cograph_bad_input` error: Algorithm 1 requires a non-negative cost
matrix, and a negative cost makes \\\exp(-\beta C)\>1\\ and the Neumann
series diverge.

Direction is read from the graph, not from `mode`: \\P^{ref}\\
normalises by out-strength and \\Z\\ counts directed walks, so a
directed input is scored as directed and a reversed input generally
scores differently. There is no in/out/all variant to select, so the
measure sits in the no-mode family. Loops are dropped and `cutoff` and
`invert_weights` are ignored; the source discusses none of the three.
The source states no normalization, so `normalized = TRUE` max-scales
the finished vector as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

Marked **costly**: one dense \\n\times n\\ inverse, which the source
itself calls the computational bottleneck at \\O(n^3)\\ time and
\\O(n^2)\\ memory, "because of which the method is currently not
practical with very large networks" (page 7). It is held back from
`centrality(type = "all")` and computed whenever named directly.

Numerical verification establishes agreement with the definition and
with `NetworkToolbox::rspbc()` on strongly connected input after undoing
that function's rounding and shifting, which are its own post-processing
and are nowhere in the paper. The paper prints no table of node scores
on a small graph, so there is no published per-node example to
reproduce; what is checked against the paper instead is the printed
limit claim on page 9, that the score becomes proportional to degree as
\\\beta\to 0^+\\ on an undirected graph.

## References

Kivimaki, I., Lebichot, B., Saramaki, J. and Saerens, M. (2016). Two
betweenness centrality measures based on Randomized Shortest Paths.
Scientific Reports, 6, 19668. Equations (6) and (8) on pages 5-6,
equations (14) and (15) and Algorithm 1 on pages 6-7, and the \\\beta\to
0^+\\ limit on page 9.
[doi:10.1038/srep19668](https://doi.org/10.1038/srep19668) .

## See also

[`centrality_current_flow_betweenness`](https://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md)
and
[`centrality_random_walk`](https://sonsoles.me/cograph/reference/centrality_random_walk.md)
for the random-walk end of the same spectrum,
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
for the shortest-path end, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# A single edge scores exactly 1 at both nodes, for every rsp_beta.
centrality_rsp_betweenness(igraph::make_full_graph(2))
#> 1 2 
#> 1 1 

# A directed cycle scores n (n - 1) / 2 everywhere, independently of
# rsp_beta: every ordered pair is joined by exactly one directed path.
centrality_rsp_betweenness(igraph::make_ring(5, directed = TRUE))
#>  1  2  3  4  5 
#> 10 10 10 10 10 

# Raising rsp_beta moves the reading from the random walk towards
# shortest paths, and can reorder the nodes.
kite <- igraph::make_graph(c(1,2, 1,3, 1,4, 1,6, 2,4, 2,5, 2,7, 3,4, 3,6,
                             4,5, 4,6, 4,7, 5,7, 6,7, 6,8, 7,8, 8,9, 9,10),
                           directed = FALSE)
centrality_rsp_betweenness(kite)
#>         1         2         3         4         5         6         7         8 
#> 143.56014 143.56014 108.07795 215.70532 108.07795 185.66015 185.66015 124.47240 
#>         9        10 
#>  92.22811  47.03370 
centrality_rsp_betweenness(kite, rsp_beta = 1)
#>        1        2        3        4        5        6        7        8 
#> 15.29555 15.29555 12.33183 21.92590 12.33183 30.43398 30.43398 39.33117 
#>        9       10 
#> 26.96457 10.20127 
```
