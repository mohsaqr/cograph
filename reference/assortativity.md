# Degree Assortativity Coefficient

Computes the degree assortativity coefficient, measuring the tendency of
nodes to connect to other nodes with similar degree. Positive values
indicate assortative mixing (high-degree nodes connect to high-degree
nodes), negative values indicate disassortative mixing.

## Usage

``` r
assortativity(x, directed = NULL, type = NULL, digits = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- type:

  Character string specifying which degree correlation to compute, or
  NULL (default) to choose automatically: `"out-in"` for directed
  networks and `"degree"` for undirected ones. For a directed network
  the accepted values are `"out-in"`, `"in-in"`, `"out-out"` and
  `"in-out"`; for an undirected network the only accepted value is
  `"degree"`. Any other value raises an error.

- digits:

  Integer or NULL. Round result to this many decimal places. Default
  NULL (no rounding).

- ...:

  Not used. Any argument supplied here raises an `"unused argument"`
  error.

## Value

An object of class `"cograph_assortativity"` with components:

- coefficient:

  Numeric scalar: the assortativity coefficient in \\\[-1, 1\]\\.

- type:

  Character: the degree type used.

- directed:

  Logical: whether the network was treated as directed.

- n_nodes:

  Numeric: number of nodes.

- n_edges:

  Numeric: number of edges.

- network:

  The original input network.

## Details

The degree assortativity coefficient is defined as the Pearson
correlation coefficient between the degrees of nodes at either end of
each edge (Newman 2002):

\$\$r = \frac{\sum\_{jk} jk(e\_{jk} - q_j q_k)}{\sigma_q^2}\$\$

where \\e\_{jk}\\ is the fraction of edges connecting degree-\\j\\ to
degree-\\k\\ vertices, \\q_k\\ is the excess degree distribution, and
\\\sigma_q^2\\ its variance.

Because the Pearson correlation is invariant to subtracting a constant,
the implementation computes the correlation of the raw degrees at the
two ends of each edge, counting every undirected edge in both
orientations. This is numerically identical to the formula above.
Degrees are unweighted counts of edges, so edge weights do not enter the
coefficient.

For directed networks, the coefficient is the Pearson correlation
between the source-end and target-end degrees over each edge in its
stored orientation, with the degree mode at each end chosen by `type`
(Foster et al. 2010).

The coefficient is `NA` when the network has no edges or when either
degree vector has zero variance.

## References

Newman, M.E.J. (2002). Assortative mixing in networks. *Physical Review
Letters*, 89(20), 208701.
[doi:10.1103/PhysRevLett.89.208701](https://doi.org/10.1103/PhysRevLett.89.208701)

Foster, J.G., Foster, D.V., Grassberger, P., & Paczuski, M. (2010). Edge
direction and the structure of networks. *PNAS*, 107(24), 10815-10820.
[doi:10.1073/pnas.0912671107](https://doi.org/10.1073/pnas.0912671107)

## See also

[`assortativity_attribute`](https://sonsoles.me/cograph/reference/assortativity_attribute.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
[`network_summary`](https://sonsoles.me/cograph/reference/network_summary.md)

## Examples

``` r
cograph::assortativity(regulation_net)
#> Assortativity (Degree (out-in))
#> =================================== 
#>   Coefficient: -0.1162 
#>   Interpretation: disassortative 
#>   Nodes: 10   Edges: 30 
#>   Directed: TRUE 
```
