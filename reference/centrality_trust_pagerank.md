# Trust-PageRank

Sheng, Zhu, Wang, Wang and Hou replace PageRank's uniform split of a
node's score among its neighbors with a *trust-value* that mixes how
similar two nodes are with how large the receiving node's degree is. The
similarity is SimRank restricted to the lines of the graph, the degree
ratio is a node's degree over the total degree of its partner's
neighborhood, and the two are blended and fed to a damped power
iteration: \$\$Rs\_{ij}=\frac{s(i,j)}{\sum\_{k\in N_j}s(j,k)},\qquad
Rd\_{ij}=\frac{d_i}{\sum\_{k\in N_j}d_k},\$\$
\$\$T(i,j)=(1-k)Rs\_{ij}+k\\Rd\_{ij},\qquad
TPR_i^{t}=\frac{1-\alpha}{n}+\alpha\sum\_{j\in
N_i}T(i,j)TPR_j^{t-1},\$\$ with the similarity itself the fixed point of
\\s(a,a)=1\\ and \\s(a,b)=(C/(\|N_a\|\|N_b\|))\sum\_{l\in
N_a}\sum\_{m\in N_b}s(l,m)\\.

## Usage

``` r
centrality_trust_pagerank(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `tpr_alpha`, `tpr_k`, `tpr_decay`, `tpr_tol` and
  `tpr_max_iter`.

## Value

Named numeric vector in input node order, one score per node, summing to
one on a graph with no isolate and no undefined component. `NA` at every
node of a component that has lines but no triangle, accompanied by a
`cograph_undefined_measure` warning. The domain does not depend on
`tpr_k`: the similarity ratio is part of the trust-value at every mixing
weight, and cograph does not switch a measure's domain on the knife-edge
value `tpr_k = 1`.

## Details

**The Centrality Zoo cites the wrong paper for this measure.** Its entry
2.381 attributes Trust-PageRank to Sheng et al.'s *Physica A*
541:123262, which defines the unrelated global-and-local-structure
index. The measure printed there is equations (2), (4), (5), (6) and (7)
of the *Algorithms* paper cited below, which is fully open access. A
reader following the Zoo's reference will land on a different measure.

**Both ratios are normalized over the receiving node's neighborhood, so
the trust matrix is column-stochastic and equation (7) is an ordinary
damped PageRank.** Because \\s\\ is symmetric, \\\sum\_{i\in
N_j}Rs\_{ij}=1\\ and \\\sum\_{i\in N_j}Rd\_{ij}=1\\ whatever \\k\\ is,
so \\\sum\_{i\in N_j}T(i,j)=1\\ and the iteration has a unique fixed
point at which the scores sum to one. The source never fixes an
iteration count and does not need to: the count is a convergence
tolerance, exposed here as `tpr_tol` and `tpr_max_iter`, and a recursion
still moving at the bound raises `cograph_no_converge` rather than
returning a silently unconverged estimate. **Both tests are relative
rather than absolute**: the similarities on one graph span many orders
of magnitude, because the mass reaching a line decays geometrically with
its distance from the nearest triangle, and on a long chain at \\C=0.2\\
the largest similarity is \\2.4\times 10^{-2}\\ while the smallest
positive one is \\2.7\times 10^{-20}\\. An absolute test would stop
while those small entries were still an order of magnitude out, and
equation (2) divides two of them by each other. An isolate emits
nothing, so on a graph with isolates the scores sum to less than one.

**The similarity recursion runs on the lines of the graph only, and this
is what makes it converge.** Algorithm 1's line 4 quantifies over
*connected* pairs and Table 3 marks every non-adjacent cell with a dash,
so the similarity map holds an entry for each line and for the diagonal,
and a non-adjacent pair entering the double sum contributes zero rather
than the \\0.1\\ that initializes the lines. The diagonal \\s(l,l)=1\\
is then the only inhomogeneous term, and it reaches a line \\(a,b)\\
exactly through the common neighbors of \\a\\ and \\b\\ – that is,
through the triangles the line carries. Each row of the linear part sums
to at most \\1-p/(d_ad_b)\\ for a line on \\p\\ triangles, so the
recursion contracts on any block that carries a triangle even at the
source's \\C=1\\. Pinning non-adjacent pairs at \\0.1\\ instead
reproduces neither published fixture; see the batch 51 published audit
in the package's verification directory.

**On a component that has lines but no triangle the measure has no
value, and that whole component is returned as `NA`.** With no triangle
the recursion is homogeneous, its least nonnegative fixed point is
\\s\equiv 0\\, and equation (2) divides zero by zero. An undefined
column makes equation (7) undefined for everything that solves against
it, which is why the `NA` covers the component rather than the one node.
The class is not a corner case: every path, tree, star, even cycle and
complete bipartite graph is in it, and so is the Petersen graph. An
isolate is *not*: it is never a denominator in equation (2), and
equation (7) gives it the bare \\(1-\alpha)/n\\. cograph refuses to name
a value on the rest. The obvious fallback, \\Rs\_{ij}:=1/d_j\\, was
considered and rejected: it is not forced by the vanishing numerators
the way the zero of
[`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md)
and
[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md)
is, since the ratios need only sum to one over \\N_j\\ and nothing in
the source chooses between the ways of doing that; adopting it would
silently turn the measure into a degree-ratio PageRank over the whole
triangle-free class while still calling it Trust-PageRank. This follows
[`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md),
which returns `NA` rather than the finite number its closed form would
otherwise print, and deliberately does not follow
[`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md).
A second reading – start the recursion at the source's \\0.1\\ rather
than at zero – would define the sub-class on which that start is itself
a fixed point (\\K_2\\, \\P_3\\, stars, \\C_4\\, complete bipartite
graphs), where it yields \\Rs\_{ij}=1/d_j\\ independently of the
constant's size. It was rejected because the value is then an artifact
of the initialization being uniform rather than of the graph, because it
leaves the rest of the triangle-free class undefined anyway, and because
separating it from an exponentially decaying zero needs a numerical
threshold where cograph can instead settle the question structurally, by
asking which lines can reach a triangle at all.

**Direction and weights are dropped, because the source excludes them.**
Page 3 sets the paper in an undirected network with \\a(i,j)=1\\, and
every quantity in the five equations is a count or a ratio of counts. A
directed, weighted or multigraph input is projected onto its simple
undirected skeleton – arcs symmetrized, weights and parallel edges
collapsed to a single line, loops dropped – as every other
undirected-domain measure in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
does, so `mode`, `cutoff` and `invert_weights` are ignored. The source
states no normalization, so `normalized = TRUE` max-scales the finished
vector as elsewhere.

**The source's claim that \\C\\ does not matter is false for the
converged recursion, and \\C\\ is exposed rather than hidden.** Page 5
argues that "the value of \\C\\ does not affect the results, since only
the ratio of similarity is calculated". That holds for a homogeneous
recursion, where \\C\\ is an overall scale, but not for this one: the
diagonal makes it affine, so \\C\\ enters the resolvent as well as the
scale. Measured on the Zachary karate club, moving \\C\\ from 1 to 0.5
moves \\Rs\\ by up to 0.141 and the scores by up to \\9.1\times
10^{-4}\\. `tpr_decay` defaults to the source's 1.

**Both published fixtures are reproduced.** Table 3 on page 6 prints
seven similarities of the five-node network of Fig. 3, and Table 5 on
page 10 prints the Trust-PageRank top ten of the Krackhardt kite and of
the Zachary karate club. All seven similarities round to their printed
two decimals and all ten karate positions are recovered in order; the
kite is recovered up to three exact ties forced by its own automorphism.
See the batch 51 published audit.

## References

Sheng, J., Zhu, J., Wang, Y., Wang, B. and Hou, Z. (2020). Identifying
Influential Nodes of Complex Networks Based on Trust-Value. Algorithms,
13(11), 280. Equations (2) and (4) on page 5, equations (5) and (6) on
page 5, equation (7) and Algorithm 1 on page 7, Figure 3 and Table 3 on
page 6, and Table 5 on page 10.
[doi:10.3390/a13110280](https://doi.org/10.3390/a13110280) . The same
construction is restated as equations (1)-(5) by Hajarathaiah, K.,
Enduri, M. K., Anamalamudi, S., Subba Reddy, T. and Tokala, S. (2022).
Computing Influential Nodes Using the Nearest Neighborhood Trust Value
and PageRank in Complex Networks. Entropy, 24(5), 704.
[doi:10.3390/e24050704](https://doi.org/10.3390/e24050704) .

## See also

[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md)
for the uniform split this measure replaces,
[`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md)
and
[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md)
for other triangle-aware scores,
[`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md)
for the other measure that returns `NA` outside its domain, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# The Krackhardt kite, one of the source's two published fixtures. Its
# automorphism forces three exact ties, so the paper's printed order
# 7, 4, 5, 9, 10, 3, 6, 8, 2, 1 is recovered up to those ties.
kite <- igraph::make_graph(
  c(6, 10, 6, 5, 6, 7, 10, 5, 10, 7, 10, 9, 5, 7, 5, 4, 5, 3,
    7, 9, 7, 4, 7, 8, 9, 4, 9, 8, 4, 8, 4, 3, 3, 2, 2, 1),
  directed = FALSE)
centrality_trust_pagerank(kite)
#>          1          2          3          4          5          6          7 
#> 0.02723587 0.05007005 0.08168747 0.14918690 0.14918690 0.07309551 0.18839525 
#>          8          9         10 
#> 0.07309551 0.10402327 0.10402327 

# A complete graph is vertex-transitive, so every node scores 1 / n.
centrality_trust_pagerank(igraph::make_full_graph(5))
#>   1   2   3   4   5 
#> 0.2 0.2 0.2 0.2 0.2 

# The similarity has nothing to work with on a triangle-free graph, so
# the measure declines to score a ring rather than inventing a split.
suppressWarnings(centrality_trust_pagerank(igraph::make_ring(6)))
#>  1  2  3  4  5  6 
#> NA NA NA NA NA NA 
```
