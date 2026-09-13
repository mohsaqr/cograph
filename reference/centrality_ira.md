# Iterative resource allocation (IRA)

Every node starts with one unit of resource and hands it to its
neighbours in proportion to the *receiver's* centrality, repeatedly,
until the amounts stop moving. The share node \\j\\ sends to a neighbour
\\i\\ is
\\a\_{ij}=\theta_i^{\alpha}/\sum\_{u\in\Gamma(j)}\theta_u^{\alpha}\\,
the recursion is \\I(t+1)=AI(t)\\ from \\I(0)=(1,\dots,1)\\, and the
steady state \\I\\ ranks the spreaders. Because every non-isolate column
of \\A\\ sums to one, the total resource is conserved: \\\sum_i
I_i(t)=n\\ at every step on a graph with no isolates, and each connected
component keeps its own vertex count.

## Usage

``` r
centrality_ira(
  x,
  ira_mass = "coreness",
  ira_alpha = 1,
  ira_tol = 1e-06,
  ira_max_iter = 1000,
  ...
)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ira_mass:

  Node centrality \\\theta\\: `"coreness"` (default, the k-shell index
  the source's worked example uses) or `"degree"`. The source also
  mentions closeness and betweenness.

- ira_alpha:

  Exponent \\\alpha\\ on the mass, a single finite number; default one,
  the only value the source uses.

- ira_tol:

  Stopping tolerance \\\varepsilon\\ on the largest absolute change
  between successive iterates; default `1e-6`, the source's own value.

- ira_max_iter:

  Iteration bound, a single whole number of at least one; default 1000.
  Reaching it raises `cograph_no_converge`.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

The equilibrium has a closed form. Writing \\s_i=\sum\_{u\in\Gamma(i)}
\theta_u^{\alpha}\\, the limit is \\I_i\propto\theta_i^{\alpha}s_i\\
within each component, scaled so the component's scores sum to its size.
On the source's own figure 1(a) that reproduces the printed \\\[15/8,
5/4, 5/4, 5/16, 5/16\]\\ exactly. cograph nevertheless iterates, because
the iteration is what the source defines and what its table reports, and
because the closed form is a limit that need not exist; see the next
paragraph.

**The iteration does not always converge, and cograph says so.** \\A\\
is the transition matrix of a reversible walk, so on a bipartite
component it has an eigenvalue of exactly \\-1\\. The coefficient of
that eigenvector in \\I(0)=(1,\dots,1)\\ is the difference in size
between the component's two vertex classes, so the iteration settles
into a period-two cycle, never meets `ira_tol`, and returns a value that
depends on the parity of the last step. The three-star alternates for
ever between \\(3,1/3,1/3,1/3)\\ and \\(1,1,1,1)\\, while the four-path,
whose classes are equal, converges to \\(2/3,4/3,4/3,2/3)\\. Neither the
source nor the Centrality Zoo mentions this. cograph runs the source's
own rule, stops at `ira_max_iter`, raises a `cograph_no_converge`
warning naming the largest remaining change, and returns
\\I(\code{ira_max_iter})\\. It does not silently report that iterate as
an equilibrium, and it does not substitute the average of the two
alternating iterates, which would converge but is not the source's rule.
Every graph in the source's own figure 1 carries a triangle and
converges.

The Centrality Zoo (section 2.204) states the transpose,
\\p\_{ij}=a\_{ij}c_j^{\alpha}/\sum_k a\_{ik}c_k^{\alpha}\\, and asks for
the principal left eigenvector of \\P\\. That is the same object up to
scale on a graph where the limit exists, but it is not the source's
finite iteration: it sidesteps the parity problem instead of reporting
it, and it carries no \\\sum_i I_i=n\\ scale.

Uses the simple undirected unweighted skeleton, which is the source
domain: either arc creates one edge, parallel edges count once and loops
are removed. Edge weights, mode, cutoff and path-weight inversion are
ignored. An isolate is in nobody's neighbourhood, so it receives nothing
and its own unit is not passed on: it scores zero from the first step,
which is the value of the source's empty sum and not an accidental zero,
and it is the reason \\\sum_i I_i=n\\ is stated only for graphs with no
isolates. Empty graphs return no scores. Cost is one dense \\n^2\\
matrix plus one matrix-vector product per iteration.

Numerical verification establishes agreement with the source equations
and with every value printed in the source's table 1, not parity with
author software, which does not exist, and not any claim about spreading
performance.

## References

Ren, Z.-M., Zeng, A., Chen, D.-B., Liao, H. and Liu, J.-G. (2014).
Iterative resource allocation for ranking spreaders in complex networks.
EPL (Europhysics Letters), 106(4), 48005. Equations 1-3 on page 2 and
the algorithm i)-iii) on page 3, read in the author postprint recovered
from the Internet Archive.
[doi:10.1209/0295-5075/106/48005](https://doi.org/10.1209/0295-5075/106/48005)
.

## See also

[`centrality_iira`](https://sonsoles.me/cograph/reference/centrality_iira.md)
for the improved variant, and
[`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
for the catalogue.

## Examples

``` r
# The source's figure 1(a): a triangle with two pendants on one corner.
# The printed steady state is 15/8, 5/4, 5/4, 5/16, 5/16.
fig1a <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 1, 5),
                            directed = FALSE)
centrality_ira(fig1a)
#>         1         2         3         4         5 
#> 1.8749997 1.2500001 1.2500001 0.3125001 0.3125001 

# The source's other mass, and a nonlinear exponent
centrality_ira(fig1a, ira_mass = "degree", ira_alpha = 2)
#>         1         2         3         4         5 
#> 2.2727277 1.1363635 1.1363635 0.2272727 0.2272727 
```
