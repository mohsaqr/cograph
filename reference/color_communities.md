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

  Any other single string is used as one color for every community.

- ...:

  Additional arguments passed to
  [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md).

## Value

A character vector of colors with one element per node, named by node,
for use as the `node_fill` argument of
[`splot()`](https://sonsoles.me/cograph/reference/splot.md).

## See also

[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md)

## Examples

``` r
color_communities(regulation_net, method = "walktrap")
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  "#000000"  "#E69F00"  "#E69F00"  "#000000"  "#000000"  "#000000"  "#000000" 
#>   Evaluate     Create      Share 
#>  "#E69F00"  "#E69F00"  "#E69F00" 
```
