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

For a matrix, an integer matrix of the same dimensions and dimnames with
1 for significant edges and 0 otherwise. For tna, cograph_network and
igraph objects, a `tna_disparity` list with elements `significant` (the
0/1 matrix), `weights_orig`, `weights_filtered` (original weights times
the 0/1 matrix), `level`, `n_edges_orig` and `n_edges_filtered`. Any
other input raises an error.

## Details

The disparity filter identifies edges that carry a disproportionate
fraction of a node's total weight, based on a null model where weights
are distributed uniformly at random.

For each node \\i\\ with degree \\k_i\\, and each edge \\(i,j)\\ with
normalized weight \\p\_{ij} = w\_{ij} / s_i\\ (where \\s_i\\ is the
node's strength), the p-value is:

\$\$p = (1 - p\_{ij})^{(k_i - 1)}\$\$

The p-value is computed from the outgoing weights of the source node
(row strength and out-degree) and from the incoming weights of the
target node (column strength and in-degree). An edge is significant if
the smaller of the two p-values is below `level`. Self-loops are never
retained. For igraph input, unweighted edges get weight 1 and multiple
edges are summed before filtering.

## References

Serrano, M. A., Boguna, M., & Vespignani, A. (2009). Extracting the
multiscale backbone of complex weighted networks. Proceedings of the
National Academy of Sciences, 106(16), 6483-6488.

## See also

[`bootstrap`](https://sonsoles.me/tna/reference/bootstrap.html) for
bootstrap-based significance testing

## Examples

``` r
disparity_filter(regulation_net, level = 0.3)
#>            Explore Plan Monitor Adapt Reflect Discuss Synthesize Evaluate
#> Explore          0    0       0     0       0       0          0        0
#> Plan             0    0       0     0       0       0          0        1
#> Monitor          0    0       0     0       0       0          0        0
#> Adapt            1    0       0     0       0       0          0        0
#> Reflect          0    0       1     0       0       0          0        0
#> Discuss          1    0       0     0       0       0          0        0
#> Synthesize       0    0       0     0       1       0          0        0
#> Evaluate         0    0       1     1       0       0          0        0
#> Create           0    0       0     0       0       0          0        1
#> Share            0    0       1     0       0       0          0        0
#>            Create Share
#> Explore         0     0
#> Plan            0     0
#> Monitor         1     0
#> Adapt           0     0
#> Reflect         0     0
#> Discuss         0     0
#> Synthesize      0     0
#> Evaluate        0     0
#> Create          0     0
#> Share           0     0
```
