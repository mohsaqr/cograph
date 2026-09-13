# Plot Community Structure

Visualizes network with community coloring using splot.

## Usage

``` r
# S3 method for class 'cograph_communities'
plot(x, network = NULL, ...)
```

## Arguments

- x:

  A cograph_communities object

- network:

  The original network (required if not stored)

- ...:

  Additional arguments passed to splot

## Value

The value returned by
[`splot`](https://sonsoles.me/cograph/reference/splot.md) (invisibly).
Called for the side effect of drawing the network with nodes grouped by
community.

## Examples

``` r
g <- igraph::make_graph("Zachary")
comm <- community_louvain(g)
mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
plot(comm, network = mat)
```
