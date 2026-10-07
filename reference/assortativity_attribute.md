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

  Named vector of attribute values whose names cover every node name, or
  an unnamed vector of length equal to the number of nodes, in node
  order. A numeric vector is treated as scalar and any other vector as
  nominal.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

- digits:

  Integer or NULL. Round the coefficient to this many decimal places.
  Default NULL (no rounding).

- ...:

  Not used. Any argument supplied here raises an `"unused argument"`
  error.

## Value

An object of class `"cograph_assortativity"` with components:

- coefficient:

  Numeric scalar: assortativity coefficient.

- type:

  Character: `"nominal"` or `"scalar"`.

- directed:

  Logical: whether the network was treated as directed.

- n_nodes:

  Numeric: number of nodes.

- n_edges:

  Numeric: number of edges.

- attribute_values:

  The attribute values used, named by node and in node order.

- network:

  The original input network.

## Details

For categorical (nominal) attributes, the coefficient is: \$\$r =
\frac{\text{tr}(\mathbf{e}) - \\\mathbf{e}^2\\}{1 -
\\\mathbf{e}^2\\}\$\$ where \\\mathbf{e}\\ is the mixing matrix with
\\e\_{ij}\\ = fraction of edges connecting type \\i\\ to type \\j\\.

For numeric (scalar) attributes, the coefficient is the Pearson
correlation between attribute values at edge endpoints (computed over
both orientations of every edge when the network is undirected). Any
non-numeric `values` vector (character or factor) is treated as nominal.

The coefficient is `NA` when the network has no edges, when a nominal
attribute has a single category, or when either value vector has zero
variance.

## References

Newman, M.E.J. (2003). Mixing patterns in networks. *Physical Review E*,
67(2), 026126.
[doi:10.1103/PhysRevE.67.026126](https://doi.org/10.1103/PhysRevE.67.026126)

## See also

[`assortativity`](https://sonsoles.me/cograph/reference/assortativity.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)

## Examples

``` r
cograph::assortativity_attribute(regulation_net, values = rep(c("self", "social"), each = 5))
#> Assortativity (Nominal Attribute)
#> =================================== 
#>   Coefficient: -0.3755 
#>   Interpretation: disassortative 
#>   Nodes: 10   Edges: 30 
#>   Directed: TRUE 
```
