# Plot Rich Club Results

Two plot types: `"curve"` (default) shows the rich club coefficient
across thresholds with null model bands. `"network"` highlights rich
club members on the network at a given threshold.

## Usage

``` r
# S3 method for class 'cograph_rich_club'
plot(x, type = c("curve", "network"), k = NULL, col = "#E41A1C", ...)
```

## Arguments

- x:

  A `cograph_rich_club` data frame.

- type:

  Character. `"curve"` (default) or `"network"`.

- k:

  Numeric. For `type = "network"`, the degree/strength threshold to
  visualize. If NULL, uses the threshold with the highest phi_norm (or
  phi if not normalized).

- col:

  Line/node color for rich club. Default `"#E41A1C"`.

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) (curve) or
  [`splot`](https://sonsoles.me/cograph/reference/splot.md) (network).

## Value

Invisible `x`.

## Examples

``` r
g <- igraph::sample_pa(50, m = 2, directed = FALSE)
rc <- cograph::rich_club(g)
plot(rc)
```
