# Improved iterative resource allocation (IIRA)

IIRA is
[`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md)
with the receiver's share scaled by how much of a spreading process that
receiver could actually carry: \\a\_{ij}=\[1-(1-\beta)^{k_i}\]\\\theta_i
(\sum\_{u\in\Gamma(j)}\theta_u)^{-1}\\, where \\k_i\\ is the degree of
\\i\\ and \\\beta\\ the spreading rate. The recursion and the initial
condition \\I(0)=(1,\dots,1)\\ are unchanged; there is no \\\alpha\\
exponent, and the denominator keeps the plain masses.

## Usage

``` r
centrality_iira(
  x,
  ira_mass = "coreness",
  iira_beta = 0.2,
  iira_steps = 50,
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ira_mass:

  Node centrality \\\theta\\: `"coreness"` (default, the k-shell index
  the source's worked example uses) or `"degree"`. Shared with
  [`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md).

- iira_beta:

  Spreading rate \\\beta\\, a single number in \\(0,1\]\\; default 0.2,
  the source's worked-example value. The source sweeps \\\beta\\ in its
  experiments and recommends no other default.

- iira_steps:

  Number of iterations \\t\\, a single nonnegative whole number; default
  50, the source's worked-example value. Zero returns \\I(0)\\.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

**The scores are tiny and only their order means anything.** The factor
\\\psi_i=1-(1-\beta)^{k_i}\\ is strictly below one, so every column of
\\A\\ sums to less than one, the spectral radius is below one, and
\\I(t)\to 0\\ geometrically. The source runs exactly \\t=50\\ steps and
prints an \\I(50)\\ of order \\10^{-20}\\; cograph returns that raw
vector, so the printed example is reproducible, and `normalized = TRUE`
max-scales it into \\\[0,1\]\\ for reading. Never compare raw IIRA
scores across connected components: each component decays at its own
rate, so after `iira_steps` steps they sit on different exponential
scales. A large `iira_steps` underflows to zero.

**The Centrality Zoo entry is not this formula.** Section 2.185 prints
\\p\_{ij}=(1-(1-\beta)^{d_i})a\_{ij}c_i/\sum_k a\_{ik}c_k\\, which pairs
the numerator's index with the denominator's own neighborhood; the
source pairs them with opposite sets. As printed, the Zoo's row sums are
\\\psi_i c_i d_i/\sum\_{k\in N(i)}c_k\\, so its matrix is stochastic in
neither direction although the entry calls it stochastic, and it does
not reproduce the source's printed matrix or its printed \\I(50)\\.
cograph implements the source.

Uses the simple undirected unweighted skeleton, which is the source
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, mode, cutoff and path-weight inversion are
ignored. An isolate has an empty neighbor sum and \\\psi=0\\, so it
scores zero from the first step; that is the value of the source's empty
sum, not an accidental zero. `iira_steps = 0` returns the initial
\\I(0)\\, a vector of ones. Empty graphs return no scores. Cost is one
dense \\n^2\\ matrix plus `iira_steps` matrix-vector products.

The version of record was not read: what was read is the author preprint
arXiv:1505.03214v1, whose method section, worked example and figures
carry the definition reproduced here. Numerical verification establishes
agreement with those equations and with every value printed in the
preprint's figure 2 example, not parity with author software, which does
not exist, and not any claim about spreading performance.

## References

Zhong, L.-F., Liu, J.-G. and Shang, M.-S. (2015). Iterative resource
allocation based on propagation feature of node for identifying the
influential nodes. Physics Letters A, 379(38), 2272-2276. Equations 1, 2
and 4 and figure 2 on page 2 of the author preprint arXiv:1505.03214v1,
which is what was read.
[doi:10.1016/j.physleta.2015.05.021](https://doi.org/10.1016/j.physleta.2015.05.021)
.

## See also

[`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md)
for the measure this improves, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# The source's figure 2, whose printed I(50) is
# 8.19e-20, 4.32e-20, 4.32e-20, 6.7e-21, 6.7e-21
fig2 <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 1, 5),
                           directed = FALSE)
centrality_iira(fig2)
#>            1            2            3            4            5 
#> 8.193120e-20 4.317938e-20 4.317938e-20 6.698728e-21 6.698728e-21 

# Only the order carries meaning, so max-scale for reading
centrality_iira(fig2, normalized = TRUE)
#>          1          2          3          4          5 
#> 1.00000000 0.52702007 0.52702007 0.08176041 0.08176041 
```
