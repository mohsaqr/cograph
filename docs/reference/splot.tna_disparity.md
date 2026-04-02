# Plot Disparity Results with splot

Plot Disparity Results with splot

## Usage

``` r
splot.tna_disparity(
  x,
  show = c("styled", "backbone", "full"),
  edge_style_sig = 1,
  edge_style_nonsig = 2,
  alpha_nonsig = 0.3,
  ...
)
```

## Arguments

- x:

  A tna_disparity object.

- show:

  What to display: "styled" (default), "backbone", "full".

- edge_style_sig:

  Line style for backbone edges. Default 1 (solid).

- edge_style_nonsig:

  Line style for non-backbone edges. Default 2 (dashed).

- alpha_nonsig:

  Alpha for non-backbone edges. Default 0.3.

- ...:

  Additional arguments passed to splot.

## Value

Invisibly returns `NULL`. Called for the side effect of producing a
plot.

## Examples

``` r
if (FALSE) { # \dontrun{
mat <- matrix(c(
  0.0, 0.5, 0.1, 0.0,
  0.3, 0.0, 0.4, 0.1,
  0.1, 0.2, 0.0, 0.5,
  0.0, 0.1, 0.3, 0.0
), nrow = 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
disp <- disparity_filter(cograph(mat), level = 0.05)
splot.tna_disparity(disp)
splot.tna_disparity(disp, show = "backbone")
} # }
```
