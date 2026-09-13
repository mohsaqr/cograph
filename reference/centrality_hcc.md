# Hybrid characteristic centrality

Liu and Zheng's hybrid characteristic centrality adds a local and a
global characteristic on the same scale:
\\HCC(u)=k^{ex}(u)/k^{ex}\_{max}+pos(u)/pos\_{max}\\. The local half is
the *extended degree* \\k^{ex}(u)=\delta
k(u)+(1-\delta)\sum\_{v\in\phi(u)}k(v)\\, the node's own degree blended
with its neighbors'; the global half is the *E-shell* position index,
the round in which a repeated minimum-extended-degree peel removes the
node. Both terms are divided by their largest value, so each lies in
\\\[0,1\]\\ and the raw score lies in \\\[0,2\]\\.

## Usage

``` r
centrality_hcc(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  including `hcc_delta`.

## Value

Named numeric vector in input node order.

## Details

The E-shell hierarchy decomposition is not a k-shell decomposition,
although the Centrality Zoo describes it as "a variant of k-shell
decomposition". There is no outer loop over a shell index and no
repeat-until-stable inner loop: each round removes exactly the set of
remaining nodes attaining the current minimum extended degree,
recomputes the extended degrees on what is left, and tags the removed
nodes with the round number. The position indexes therefore run
\\1,\dots,pos\_{max}\\ with every value attained, rather than being
shell numbers, and the two procedures disagree on the source's own
figure 1.

**The source's printed algorithm contains a typo, and cograph implements
the correction its own tables require.** Step 3 of the E-shell procedure
prints \\S_p=\arg\max\_{u\in G_p}\\k^{ex}(u)\\\\ while the same sentence
calls \\S_p\\ "the set of minimum nodes", the preceding paragraph says
"the nodes with minimum extended degree are found and deleted", and the
paper's table 2 heads its column "Minimum extended degree" with the
increasing values 2, 2.5, 3, 4.5, 5, 6. The minimum reading reproduces
every printed row; the literal maximum reading is a different measure.

**The peel recomputes but equation (4) does not.** Step 6 updates the
extended degrees on the residual graph after every removal, which is
what the printed table 2 minima require. The \\k^{ex}(u)\\ of equation
(4), and the \\k^{ex}\_{max}\\ it is divided by, are nevertheless the
**original**-graph values: the paper's own worked line
\\HCC(a)=4.5/11+4/6\\ uses the original 4.5 and the original maximum 11,
and its node \\d\\ settles the question, since its original 9.5 gives
the printed 1.86 while its residual 6 at removal time would give 1.55.

**Raw scores are not component-local.** \\k^{ex}\_{max}\\ and
\\pos\_{max}\\ are single global constants, so adding a disconnected
component – an isolate included – can change every score, and not merely
by a common factor, because the two terms rescale independently. The
source does not discuss disconnected graphs.

**Degenerate cases are cograph decisions, not the source's.** An isolate
has extended degree zero, which for \\\delta\in\[0,1\]\\ is the global
minimum, so it always leaves in the first round with \\pos=1\\. On an
edgeless graph every extended degree is zero and \\k^{ex}\_{max}=0\\,
making the first term \\0/0\\; it is written as **zero**, which leaves
the E-shell term alone. One round then removes everything, so every node
of an edgeless graph – a singleton included – scores exactly 1. Empty
graphs return no scores.

`hcc_delta` defaults to the source's 0.5 and is restricted to the
source's stated domain \\\[0,1\]\\, where \\\delta=1\\ recovers the
classical degree and \\\delta=0\\ drops the node's own degree entirely.
Values outside that interval are refused with a `cograph_bad_parameter`
error rather than extended: they make the extended degree negative on
some graphs, and then equation (4) divides by a nonpositive maximum,
which the source never contemplates.

Uses the simple undirected unweighted skeleton, the source's stated
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, mode, cutoff and path-weight inversion are
ignored, and directed input is symmetrized rather than read as a
directed case, which the source does not define. The source states no
further normalization; `normalized = TRUE` max-scales the finished
vector as elsewhere in
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md), on
top of the two divisions equation (4) already performs. Cost is one
dense matrix-vector product per peeling round, so \\O(n^3)\\ in the
worst case rather than the \\O(n+m)\\ a sparse min-heap would give.

Numerical verification establishes agreement with the definition and
with the values the source prints for its figure 1, not parity with
author software, which does not exist, and not any claim about spreading
performance.

## References

Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
networks based on extended degree and E-shell hierarchy decomposition.
Scientific Reports, 13, 3197. Equations (3) and (4) and the eight-step
E-shell procedure on page 3, with the worked example on page 4.
[doi:10.1038/s41598-023-30308-5](https://doi.org/10.1038/s41598-023-30308-5)
.

## See also

[`centrality_ehcc`](https://sonsoles.me/cograph/reference/centrality_ehcc.md)
for the neighborhood sum of this score,
[`centrality_dkgm`](https://sonsoles.me/cograph/reference/centrality_dkgm.md)
for another shell-and-degree hybrid, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# Every node of a regular graph has the same extended degree, so one
# round removes the whole graph and every node scores 1 + 1 = 2.
centrality_hcc(igraph::make_ring(6))
#> 1 2 3 4 5 6 
#> 2 2 2 2 2 2 

# A star peels its leaves first and its center second.
centrality_hcc(igraph::make_star(6, mode = "undirected"))
#>   1   2   3   4   5   6 
#> 2.0 1.1 1.1 1.1 1.1 1.1 

# delta = 1 is the classical degree in the extended-degree slot.
centrality_hcc(igraph::make_star(6, mode = "undirected"), hcc_delta = 1)
#>   1   2   3   4   5   6 
#> 2.0 0.7 0.7 0.7 0.7 0.7 
```
