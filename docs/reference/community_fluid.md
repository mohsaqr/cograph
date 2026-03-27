# Fluid Communities Detection

Simulates fluid dynamics where communities compete for nodes. Requires
specifying the number of communities.

## Usage

``` r
community_fluid(x, no.of.communities, ...)

com_fl(x, no.of.communities, ...)
```

## Arguments

- x:

  Network input

- no.of.communities:

  Number of communities to detect. Required.

- ...:

  Additional arguments passed to
  [`to_igraph`](http://sonsoles.me/cograph/reference/to_igraph.md)

## Value

A `cograph_communities` object

A `cograph_communities` object. See
[`detect_communities`](http://sonsoles.me/cograph/reference/detect_communities.md).

## References

Pares, F., Gasulla, D.G., Vilalta, A., Moreno, J., Ayguade, E., Labarta,
J., Cortes, U., & Suzumura, T. (2018). Fluid communities: A competitive,
scalable and diverse community detection algorithm. *Studies in
Computational Intelligence*, 689, 229-240.

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::make_graph("Zachary")

  # Detect exactly 2 communities
  comm <- community_fluid(g, no.of.communities = 2)
}
if (FALSE) { # \dontrun{
net <- as_cograph(matrix(runif(25), 5, 5))
com_fl(net)
} # }
```
