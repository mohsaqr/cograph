# Configure a custom multi-panel layout

Sets up a multi-panel device layout for use with cograph plotting
functions called with `combined = FALSE`. Returns a
[`par()`](https://rdrr.io/r/graphics/par.html) snapshot of the previous
device state so the caller can restore it via
`on.exit(graphics::par(old_par))`.

## Usage

``` r
panel_layout(spec, mar = c(2, 2, 3, 1), widths = NULL, heights = NULL)
```

## Arguments

- spec:

  Either a length-2 integer vector `c(nrow, ncol)` for a uniform grid,
  or a numeric matrix of panel positions to pass to
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html).

- mar:

  Numeric vector of length 4 giving panel margins. Default
  `c(2, 2, 3, 1)` matches cograph's multi-panel margin convention.

- widths, heights:

  Optional numeric vectors of column widths and row heights. Only used
  when `spec` is a matrix; passed straight to
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html).

## Value

Invisibly returns a list of previous
[`par()`](https://rdrr.io/r/graphics/par.html) settings that can be
passed back to [`graphics::par()`](https://rdrr.io/r/graphics/par.html)
to restore the prior device state.

## Details

Use `spec = c(nrow, ncol)` for a uniform grid (delegates to
`graphics::par(mfrow = ...)`). Use `spec = <matrix>` for a non-uniform
layout (delegates to
[`graphics::layout()`](https://rdrr.io/r/graphics/layout.html)); the
matrix values name panel cells, so `matrix(c(1, 1, 2, 3), 2, 2)`
produces one wide cell on top and two cells on the bottom row.

## Examples

``` r
mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
colnames(mat) <- rownames(mat) <- c("A", "B", "C")
net1 <- as_cograph(mat)
net2 <- as_cograph(mat * 0.5)

# Uniform 1 x 2 grid
op <- panel_layout(c(1, 2))
splot(net1, combined = FALSE)
splot(net2, combined = FALSE)

graphics::par(op)
```
