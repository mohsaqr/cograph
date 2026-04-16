# Attribute Assortativity (Homophily)

Computes assortativity with respect to a node attribute, measuring the
tendency of nodes to connect to others with similar attribute values.
For categorical attributes, this computes the modularity-based nominal
assortativity. For numeric attributes, this computes the Pearson
correlation between attribute values at edge endpoints.

## Usage

``` r
assortativity_attribute(x, values, directed = NULL, digits = NULL, ...)

homophily(x, values, directed = NULL, digits = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- values:

  Named vector of attribute values (names must match node names) or an
  unnamed vector in node order.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

- digits:

  Integer or NULL. Round result. Default NULL.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md).

## Value

An object of class `"cograph_assortativity"` with components:

- coefficient:

  Numeric scalar: assortativity coefficient.

- type:

  Character: `"nominal"` or `"scalar"`.

- directed:

  Logical.

- n_nodes:

  Integer.

- n_edges:

  Integer.

- attribute_values:

  The attribute values used.

- network:

  Original input.

## Details

For categorical (nominal) attributes, the coefficient is: \$\$r =
\frac{\text{tr}(\mathbf{e}) - \\\mathbf{e}^2\\}{1 -
\\\mathbf{e}^2\\}\$\$ where \\\mathbf{e}\\ is the mixing matrix with
\\e\_{ij}\\ = fraction of edges connecting type \\i\\ to type \\j\\.

For numeric (scalar) attributes, the coefficient is the Pearson
correlation between attribute values at edge endpoints.

## References

Newman, M.E.J. (2003). Mixing patterns in networks. *Physical Review E*,
67(2), 026126.
[doi:10.1103/PhysRevE.67.026126](https://doi.org/10.1103/PhysRevE.67.026126)

## See also

[`assortativity`](https://sonsoles.me/cograph/reference/assortativity.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)

## Examples

``` r
adj <- matrix(c(0,1,1,0, 1,0,0,0, 1,0,0,1, 0,0,1,0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
groups <- c(A = "x", B = "x", C = "y", D = "y")
cograph::assortativity_attribute(adj, groups)
#> Assortativity (Nominal Attribute)
#> =================================== 
#>   Coefficient: 0.3333 
#>   Interpretation: assortative 
#>   Nodes: 4   Edges: 3 
#>   Directed: FALSE 
```
