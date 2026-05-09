# Disparity Filter

Extracts the statistically significant backbone of a weighted network
using the disparity filter method (Serrano, Boguna, & Vespignani, 2009).

## Usage

``` r
disparity_filter(x, level = 0.05, ...)

# Default S3 method
disparity_filter(x, level = 0.05, ...)

# S3 method for class 'matrix'
disparity_filter(x, level = 0.05, ...)

# S3 method for class 'tna'
disparity_filter(x, level = 0.05, ...)

# S3 method for class 'cograph_network'
disparity_filter(x, level = 0.05, ...)

# S3 method for class 'igraph'
disparity_filter(x, level = 0.05, ...)
```

## Arguments

- x:

  A weight matrix, tna object, cograph_network, or igraph object.

- level:

  Significance level (default 0.05). Lower values result in a sparser
  backbone (fewer edges retained).

- ...:

  Additional arguments (currently unused).

## Value

For matrices: a binary matrix (0/1) indicating significant edges. For
tna, cograph_network, and igraph objects: a `tna_disparity` object
containing the significance matrix, original weights, filtered weights,
and summary statistics.

## Details

The disparity filter identifies edges that carry a disproportionate
fraction of a node's total weight, based on a null model where weights
are distributed uniformly at random.

For each node \\i\\ with degree \\k_i\\, and each edge \\(i,j)\\ with
normalized weight \\p\_{ij} = w\_{ij} / s_i\\ (where \\s_i\\ is the
node's strength), the p-value is:

\$\$p = (1 - p\_{ij})^{(k_i - 1)}\$\$

Edges are significant if \\p \< level\\ for either endpoint.

## References

Serrano, M. A., Boguna, M., & Vespignani, A. (2009). Extracting the
multiscale backbone of complex weighted networks. Proceedings of the
National Academy of Sciences, 106(16), 6483-6488.

## See also

[`bootstrap`](http://sonsoles.me/tna/reference/bootstrap.md) for
bootstrap-based significance testing

## Examples

``` r
# Create a weighted network
mat <- matrix(c(
  0.0, 0.5, 0.1, 0.0,
  0.3, 0.0, 0.4, 0.1,
  0.1, 0.2, 0.0, 0.5,
  0.0, 0.1, 0.3, 0.0
), nrow = 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

# Extract backbone at 5% significance level
backbone <- disparity_filter(mat, level = 0.05)
backbone
#>   A B C D
#> A 0 0 0 0
#> B 0 0 0 0
#> C 0 0 0 0
#> D 0 0 0 0

# More stringent filter (1% level)
backbone_strict <- disparity_filter(mat, level = 0.01)
```
