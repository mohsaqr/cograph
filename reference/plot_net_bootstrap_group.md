# Plot a Group Bootstrap Result

Plots each cluster's `net_bootstrap` in a grid, routing every panel
through `splot.net_bootstrap` so significance styling (solid vs dashed
edges) is preserved. Earlier versions extracted `bs$original` per
cluster and handed plain netobjects to
[`splot()`](https://sonsoles.me/cograph/reference/splot.md), which
dispatches to `splot.netobject` — that path has no concept of
significance, so every edge rendered identically.

## Usage

``` r
plot_net_bootstrap_group(x, nrow = NULL, ncol = NULL, common_scale = TRUE, ...)

# S3 method for class 'net_bootstrap_group'
plot(x, ...)
```

## Arguments

- x:

  A `net_bootstrap_group` object (list of `net_bootstrap`).

- nrow, ncol:

  Grid dimensions. Defaults to auto-computed square layout.

- common_scale:

  Logical: use the same maximum weight across panels? Default TRUE.

- ...:

  Additional arguments passed to `splot.net_bootstrap` (e.g.
  `display = "significant"`, `show_stars = FALSE`).

## Value

Invisibly returns `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
grp <- Nestimate::cluster_network(data, k = 2)
gbs <- Nestimate::bootstrap_network(grp, iter = 100)
plot_net_bootstrap_group(gbs)
} # }
```
