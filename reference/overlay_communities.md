# Overlay Community Blobs on a Network Plot

Plots a network with
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and overlays
smooth blob shapes that mark node communities.

## Usage

``` r
overlay_communities(
  x,
  communities,
  blob_colors = NULL,
  blob_alpha = 0.25,
  blob_linewidth = 0.7,
  blob_line_alpha = 0.8,
  ...
)
```

## Arguments

- x:

  A network object passed to
  [`splot`](https://sonsoles.me/cograph/reference/splot.md): `tna`,
  matrix, `igraph`, or `cograph_network`.

- communities:

  Community assignments in any of these formats:

  - the name of an igraph `cluster_*` algorithm (e.g., `"walktrap"`,
    `"louvain"`, `"leiden"`, `"edge_betweenness"`), matched partially;
    directed networks are collapsed to undirected before detection;

  - a numeric or factor membership vector in node order (e.g.,
    `c(1, 1, 2, 2, 3)`), or named by node;

  - a named list of character vectors of node names;

  - a `cograph_communities`, igraph `communities` or `tna_communities`
    object.

- blob_colors:

  Character vector of fill colors for blobs. Recycled if shorter than
  the number of communities. Default `NULL` uses the built-in blob
  palette.

- blob_alpha:

  Numeric. Fill transparency (0-1). Default `0.25`.

- blob_linewidth:

  Numeric. Border line width. Default `0.7`.

- blob_line_alpha:

  Numeric. Border line transparency (0-1). Default `0.8`.

- ...:

  Additional arguments passed to
  [`splot`](https://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly, the `cograph_network` object returned by
[`splot`](https://sonsoles.me/cograph/reference/splot.md). Called for
the side effect of plotting.

## Examples

``` r
comm <- cograph::communities(regulation_net, method = "walktrap")
overlay_communities(regulation_net, comm)
```
