# Degree and Importance of Lines

Liu, Xiong, Shi, Shi and Wang rank a node by its degree plus the share
it can claim of the importance of the lines that touch it. A line
matters when its two endpoints reach far beyond it and when no triangle
offers a way round it, so the importance of the line \\e\_{mn}\\ is
\\I\_{e\_{mn}}=U/\lambda\\ with \\U=(k_m-p-1)(k_n-p-1)\\ and
\\\lambda=p/2+1\\, where \\p\\ is the number of triangles one of whose
edges is \\e\_{mn}\\. That importance is then split between the
endpoints in proportion to their own degrees,
\\W\_{v_iv_j}=I\_{e\_{ij}}(k_i-1)/(k_i+k_j-2)\\, and the score is
\\L\_{v_i}=k_i+\sum\_{v_j\in\Gamma_i}W\_{v_iv_j}\\ over the open
neighborhood \\\Gamma_i\\. The measure is strictly two-hop local: only
the degrees of a node, of its neighbors and the triangles on its
incident lines enter, so it costs \\O(n\langle k\rangle^2)\\ and its raw
scores are component-local.

## Usage

``` r
centrality_dil(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order, one score per node, each at
least the node's degree in the simple undirected skeleton.

## Details

**\\\lambda\\ is \\p/2+1\\, and reading it from a text layer gets it
wrong.** The stacked fraction extracts from the published PDF as
\\\lambda=2p+1\\, in Liu et al.'s original as much as in the Almasi and
Hu (2019) reproduction of it. The page image shows \\p\\ over \\2\\; so
does the paper's own worked example, in printed prose, on page 210: for
the seven-line network of its Fig. 1(b) it writes \\p=1\\, \\U=4\\,
"\\\lambda=1/2+1=1.5\\" and \\I\_{e\_{45}}=4/1.5\approx 2.6667\\. The
wrong reading returns \\4/3\\ there. cograph reproduces \\8/3\\.

**\\U\\ is never negative, so a score never falls below the node's
degree.** For a line \\(i,j)\\, \\j\\ belongs to \\N(i)\\ but to neither
\\N(j)\\ nor the intersection, so \\p=\|N(i)\cap N(j)\|\le k_i-1\\ and
both factors of \\U\\ are at least zero. Since \\\lambda\ge 1\\, every
\\I\\ and every \\W\\ is at least zero and \\L\_{v_i}\ge k_i\\. Equality
is common rather than exceptional: every line of a complete graph, of a
star, or of any network whose lines all touch a degree-one node has
\\U=0\\, so \\K_n\\ scores \\n-1\\ at every node and a star scores its
degree at every node.

**The importance of a line is conserved when it is split.** The two
shares \\(k_i-1)/(k_i+k_j-2)\\ and \\(k_j-1)/(k_i+k_j-2)\\ sum to one,
so \\\sum_i (L\_{v_i}-k_i)=\sum\_{e}I_e\\: the network's total excess
over degree is exactly the total importance of its lines. That identity
is asserted over the package's whole verification collection.

**An isolated \\K_2\\ is the one undefined split, and it is resolved
rather than refused.** The denominator \\k_i+k_j-2\\ vanishes only when
\\k_i=k_j=1\\, since both endpoints of a line have degree at least one –
that is a two-node component – and there \\p=0\\ and
\\U=(1-0-1)(1-0-1)=0\\, so the importance being divided is exactly zero
while the split of it is \\0/0\\. Because \\W\\ is a *share* of \\I\\,
and the two shares sum to one wherever they are defined, every
admissible split of an exactly zero importance gives an exactly zero
contribution: the answer does not depend on resolving the indeterminacy.
cograph therefore writes the share as zero, taking the test before the
division so that no \\0/0\\ is ever evaluated, and both nodes of a
\\K_2\\ score \\1\\. **The source says nothing about this case**; the
choice is cograph's, and it follows the precedent of
[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md),
whose \\0/0\\ on a triangle-free graph is likewise written as zero
because the denominator vanishes exactly where every numerator does. It
deliberately does not follow
[`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md),
which returns `NA` on reducible input: there the closed form returns a
finite number in place of an infinite one, so a value would be wrong,
where here every candidate value is the same value.

**Direction and weights are dropped, because the authors exclude them.**
Page 210 opens the derivation with "we assume that a network \\G=(V,E)\\
is an undirected and unweighted network", and every quantity in the
three equations is a count: a degree, a triangle census, a difference of
integers. A directed, weighted or multigraph input is therefore
projected onto its simple undirected skeleton – arcs symmetrized,
weights and parallel edges collapsed to a single line, loops dropped –
rather than refused, which is the convention every other
undirected-domain measure in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
already follows, and the projection is silent rather than warned for the
same reason. There is no in/out/all reading to choose between, so the
measure sits in the no-mode family and `cutoff` and `invert_weights` are
ignored as well. The source states no normalization, so
`normalized = TRUE` max-scales the finished vector as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

**Isolates, singletons and disconnected input need no special rule.** An
isolate has degree zero and an empty sum, so it scores zero; the single
node of a one-node graph and every node of an edgeless graph score zero
for the same reason, and an empty graph returns no scores. Because
nothing in equations (1)-(3) reaches past a node's second neighbors, the
raw scores are component-local: attaching a disjoint component leaves
every existing score unchanged.

**The source prints three numerical fixtures and all three are
reproduced.** Fig. 1 on page 210 prints \\I\_{e\_{45}}=9\\ at \\p=0\\
and \\8/3\\ at \\p=1\\; Fig. 2 on page 211 prints \\L\_{v_2}=26/9\\ and
\\L\_{v_5}=52/15\\ on a 27-node tree; and Table 3 on page 217 prints a
DIL value for every one of the 21 nodes of the ARPA network, whose
topology is Fig. 6 on the same page. All 21 printed values are
reproduced, and the edge list read off the figure is corroborated
independently by the paper's own degree column. See the batch 50
published audit in the package's verification directory.

## References

Liu, J., Xiong, Q., Shi, W., Shi, X. and Wang, K. (2016). Evaluating the
importance of nodes in complex networks. Physica A: Statistical
Mechanics and its Applications, 452, 209-219. Equation (1) and the
definitions of \\U\\, \\p\\ and \\\lambda\\ on page 210, equations (2)
and (3) on page 211, the complexity claim in Table 4 on page 218, and
the ARPA fixture in Table 3 and Fig. 6 on page 217.
[doi:10.1016/j.physa.2016.02.049](https://doi.org/10.1016/j.physa.2016.02.049)
. The same three equations are reproduced as equations (7)-(9) by
Almasi, S. and Hu, T. (2019). Measuring the importance of vertices in
the weighted human disease network. PLoS ONE, 14(3), e0205936.
[doi:10.1371/journal.pone.0205936](https://doi.org/10.1371/journal.pone.0205936)
.

## See also

[`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md)
and
[`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md)
for other degree-and-triangle hybrids,
[`centrality_bridging`](https://sonsoles.me/cograph/reference/centrality_bridging.md)
for another measure that scores a node by the lines it carries, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# Every line of a complete graph is shortcut by n - 2 triangles, so U is
# zero throughout and the score is the degree.
centrality_dil(igraph::make_full_graph(5))
#> 1 2 3 4 5 
#> 4 4 4 4 4 

# A triangle-free k-regular graph scores k + k(k-1)^2/2 at every node:
# 3 for a ring and 9 for the Petersen graph.
centrality_dil(igraph::make_ring(6))
#> 1 2 3 4 5 6 
#> 3 3 3 3 3 3 

# The path 1-2-3-4-5 scores 1, 2.5, 3, 2.5, 1: a line to a leaf carries
# no importance, and the two interior lines carry one each, split evenly.
centrality_dil(igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5),
                                  directed = FALSE))
#>   1   2   3   4   5 
#> 1.0 2.5 3.0 2.5 1.0 

# A triangle on two degree-three nodes is the case that needs
# lambda = p/2 + 1: I = 1 / 1.5 = 2/3, split evenly, so the two hubs
# score 3 + 1/3. Reading lambda as 2p + 1 would give 3 + 1/6.
centrality_dil(igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 2, 5),
                                  directed = FALSE))
#>        1        2        3        4        5 
#> 3.333333 3.333333 2.000000 1.000000 1.000000 
```
