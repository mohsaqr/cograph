# Color Nodes by Community

Generate colors for nodes based on community membership. Designed for
direct use with
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) `node_fill`
parameter.

## Usage

``` r
color_communities(x, method = "louvain", palette = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- method:

  Community detection algorithm. See
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)
  for available methods. Default `"louvain"`.

- palette:

  Color palette to use. Can be:

  - `NULL` (default): Uses a colorblind-friendly palette

  - A character vector of colors

  - A function that takes n and returns n colors

  - A palette name: "rainbow", "colorblind", "pastel", "viridis"

- ...:

  Additional arguments passed to
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## Value

A named character vector of colors (one per node), suitable for use with
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) `node_fill`
parameter.

## See also

[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Basic usage with splot
splot(adj, node_fill = color_communities(adj))


# Custom palette
splot(adj, node_fill = color_communities(adj, palette = c("red", "blue")))
```
