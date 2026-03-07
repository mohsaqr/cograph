
# cograph <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/sonsoleslp/cograph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sonsoleslp/cograph/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/cograph)](https://CRAN.R-project.org/package=cograph)
[![Codecov](https://app.codecov.io/gh/sonsoleslp/cograph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sonsoleslp/cograph)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**cograph** is a modern R package for network visualization with rich
node styling, multiple layouts, and first-class support for Transition
Network Analysis (TNA).

## Installation

``` r
# Install from CRAN
install.packages("cograph")

# Development version from GitHub
devtools::install_github("sonsoleslp/cograph")
```

## TNA Plot

The primary use case: visualize transition networks from the `tna`
package.

``` r
library(tna)
library(cograph)

# Build a TNA model from sequence data
fit <- tna(engagement)

# One-liner visualization
splot(fit)
```

<img src="man/figures/README-tna-plot-1.jpeg" alt="" width="100%" />

## Simple Network

``` r
library(cograph)

# Create a transition matrix
states <- c("Explore", "Plan", "Monitor", "Adapt", "Reflect")
mat <- matrix(
  c(0.0, 0.4, 0.2, 0.1, 0.3,
    0.3, 0.0, 0.3, 0.2, 0.2,
    0.2, 0.3, 0.0, 0.3, 0.2,
    0.1, 0.2, 0.4, 0.0, 0.3,
    0.2, 0.2, 0.2, 0.4, 0.0),
  nrow = 5, byrow = TRUE,
  dimnames = list(states, states)
)

splot(mat)
```

<img src="man/figures/README-simple-network-1.jpeg" alt="" width="100%" />

## Layouts

``` r
par(mfrow = c(2, 2), mar = c(1, 1, 2, 1))
splot(mat, layout = "oval",   title = "oval")
splot(mat, layout = "circle", title = "circle")
splot(mat, layout = "kk",     title = "kk")
splot(mat, layout = "fr",     title = "fr")
```

<img src="man/figures/README-layouts-1.jpeg" alt="" width="100%" />

## Edge Styling

``` r
splot(mat,
  curvature = 0.3,
  arrow_size = 0.02,
  edge_width = 3
)
```

<img src="man/figures/README-edge-styling-1.jpeg" alt="" width="100%" />

## Node Shapes

``` r
shapes <- c("circle", "square", "hexagon", "diamond", "triangle")

splot(mat,
  node_shape = shapes,
  node_fill = c("#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261"),
  layout = "circle"
)
```

<img src="man/figures/README-node-shapes-1.jpeg" alt="" width="100%" />

## Donuts

Donut nodes show proportional fill with optional polygon shapes.

``` r
fills <- c(0.9, 0.7, 0.5, 0.3, 0.8)

splot(mat,
  donut_fill = fills,
  donut_color = "steelblue",
  donut_shape = c("circle", "hexagon", "square", "diamond", "triangle")
)
```

<img src="man/figures/README-donuts-1.jpeg" alt="" width="100%" />

## Pies

Pie chart nodes with per-node color palettes.

``` r
pie_vals <- list(
  c(0.5, 0.3, 0.2),
  c(0.4, 0.4, 0.2),
  c(0.3, 0.3, 0.4),
  c(0.6, 0.2, 0.2),
  c(0.2, 0.5, 0.3)
)

pie_cols <- list(
  c("#E63946", "#457B9D", "#2A9D8F"),
  c("#264653", "#E9C46A", "#F4A261"),
  c("#F72585", "#7209B7", "#3A0CA3"),
  c("#003049", "#D62828", "#F77F00"),
  c("#606C38", "#283618", "#DDA15E")
)

splot(mat,
  node_shape = "pie",
  pie_values = pie_vals,
  pie_colors = pie_cols,
  layout = "circle"
)
```

<img src="man/figures/README-pies-1.jpeg" alt="" width="100%" />

## Donut + Pie Combo

Combine outer donut ring with inner pie segments.

``` r
splot(mat,
  donut_fill = fills,
  donut_color = "steelblue",
  pie_values = pie_vals,
  pie_colors = c("#E41A1C", "#377EB8", "#4DAF4A")
)
```

<img src="man/figures/README-donut-pie-1.jpeg" alt="" width="100%" />

## Chord Diagram

``` r
plot_chord(mat, title = "Transition Chord Diagram")
```

<img src="man/figures/README-chord-1.jpeg" alt="" width="100%" />

## Heatmap

``` r
plot_heatmap(mat, show_values = TRUE, colors = "viridis",
             value_fontface = "bold", title = "Transition Heatmap")
```

<img src="man/figures/README-heatmap-1.jpeg" alt="" width="100%" />

## Alluvial Flow

``` r
plot_transitions(mat, flow_color_by = "from", flow_alpha = 0.5,
                 from_title = "Source", to_title = "Target")
```

<img src="man/figures/README-alluvial-1.jpeg" alt="" width="100%" />

## License

MIT License.
