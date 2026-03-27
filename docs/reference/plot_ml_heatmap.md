# Multilayer Network Heatmap

Visualizes multiple network layers as heatmaps on tilted 3D-perspective
planes, similar to the plot_mlna network visualization style.

## Usage

``` r
plot_ml_heatmap(
  x,
  layer_list = NULL,
  colors = "viridis",
  layer_spacing = 2.5,
  skew = 0.4,
  compress = 0.6,
  show_connections = FALSE,
  connection_color = "#E63946",
  connection_style = "dashed",
  show_borders = TRUE,
  border_color = "black",
  border_width = 1,
  cell_border_color = "white",
  cell_border_width = 0.2,
  show_labels = TRUE,
  label_size = 5,
  show_legend = TRUE,
  legend_title = "Weight",
  title = NULL,
  limits = NULL,
  na_color = "grey90",
  threshold = 0
)
```

## Arguments

- x:

  A list of matrices (one per layer), a group_tna object,
  cograph_network, or a single matrix with layer_list specified.

- layer_list:

  Optional list defining layers, column name string, or NULL for
  auto-detection from cograph_network nodes.

- colors:

  Color palette: "viridis", "heat", "blues", "reds", "inferno",
  "plasma", or a vector of colors. Default "viridis".

- layer_spacing:

  Vertical spacing between layers. Default 2.5.

- skew:

  Horizontal skew for perspective effect (0-1). Default 0.4.

- compress:

  Vertical compression for perspective (0-1). Default 0.6.

- show_connections:

  Show inter-layer connection lines? Default FALSE.

- connection_color:

  Color for inter-layer connections. Default "#E63946".

- connection_style:

  Line style: "dashed", "solid", "dotted". Default "dashed".

- show_borders:

  Show layer outline borders? Default TRUE.

- border_color:

  Color for layer borders. Default "black".

- border_width:

  Width of layer borders. Default 1.

- cell_border_color:

  Color for cell borders. Default "white".

- cell_border_width:

  Width of cell borders. Default 0.2.

- show_labels:

  Show layer name labels? Default TRUE.

- label_size:

  Size of layer labels. Default 5.

- show_legend:

  Show color legend? Default TRUE.

- legend_title:

  Title for legend. Default "Weight".

- title:

  Plot title. Default NULL.

- limits:

  Color scale limits c(min, max). NULL for auto.

- na_color:

  Color for NA values. Default "grey90".

- threshold:

  Minimum absolute value to display. Cells with `abs(value) < threshold`
  are set to NA (rendered as background). Default 0.

## Value

A ggplot2 object.

## Examples

``` r
# \donttest{
# List of matrices
layers <- list(
  Layer1 = matrix(runif(16), 4, 4),
  Layer2 = matrix(runif(16), 4, 4),
  Layer3 = matrix(runif(16), 4, 4)
)
plot_ml_heatmap(layers)


# With connections
plot_ml_heatmap(layers, show_connections = TRUE)


# Custom styling
plot_ml_heatmap(layers,
  colors = "plasma",
  layer_spacing = 3,
  skew = 0.5
)

# }
```
