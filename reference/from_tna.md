# Convert a tna object to cograph parameters

Extracts the transition matrix, labels, and initial state probabilities
from a `tna` object and plots with cograph. Initial probabilities are
mapped to donut fills.

## Usage

``` r
from_tna(
  tna_object,
  engine = c("splot", "soplot"),
  plot = TRUE,
  weight_digits = NULL,
  show_zero_edges = FALSE,
  ...
)
```

## Arguments

- tna_object:

  A `tna` object from
  [`tna::tna()`](http://sonsoles.me/tna/reference/build_model.md)

- engine:

  Which cograph renderer to use: `"splot"` or `"soplot"`. Default:
  `"splot"`.

- plot:

  Logical. If TRUE (default), immediately plot using the chosen engine.

- weight_digits:

  Number of decimal places to round edge weights to. Default `NULL`,
  which picks the number of digits from the matrix: `0` when every
  non-zero weight is a whole number (counts, as in `ftna`/`ctna` models)
  and `2` otherwise (probabilities). Edges whose weight rounds to zero
  at this precision are dropped unless `show_zero_edges = TRUE`.

- show_zero_edges:

  Logical. Zero is how this representation stores "no edge", so an edge
  whose weight rounds to zero at `weight_digits` is dropped. With `TRUE`
  such an edge is instead drawn at the smallest magnitude
  `weight_digits` can express, carrying its sign; every other weight is
  unchanged. Default: FALSE.

- ...:

  Additional parameters passed to the plotting engine (e.g., `layout`,
  `node_fill`, `donut_color`).

## Value

Invisibly, a named list of cograph parameters that can be passed to
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) or
[`soplot()`](https://sonsoles.me/cograph/reference/soplot.md).

## Details

### Conversion Process

The tna object's transition matrix becomes edge weights, labels become
node labels, and initial state probabilities (`inits`) are mapped to
`donut_fill` values to visualize starting state distributions.

Directedness is read from the tna object when available; otherwise it is
inferred from matrix symmetry. Transition matrices are usually directed,
while symmetric co-occurrence matrices are treated as undirected.

The default `donut_inner_ratio` of 0.8 creates thin rings that
effectively visualize probability values without obscuring node labels.

### Parameter Mapping

The following tna properties are automatically extracted:

- **weights**: Transition matrix `->` edge weights

- **labels**: State labels `->` node labels

- **inits**: Initial probabilities `->` donut_fill (0-1 scale)

### TNA Visual Defaults

The following visual defaults are applied for TNA plots (all can be
overridden via `...`):

- `layout = "oval"`: Oval/elliptical node arrangement

- `node_fill`: Colors from TNA palette (Accent/Set3 based on state
  count)

- `node_size = 7`: Larger nodes for readability

- `arrow_size = 0.61`: Prominent directional arrows for directed
  networks

- `edge_color = "#003355"`: Dark blue edges

- `edge_labels = TRUE`: Show transition weights on edges

- `edge_label_size = 0.4`: Readable edge labels

- `edge_label_position = 0.7`: Labels positioned toward target

- `edge_start_style = "dotted"`: Dotted line at edge source for directed
  networks

- `edge_start_length = 0.2`: 20% of directed edges are dotted

- `edge_label_style = "estimate"` and `edge_label_leading_zero = FALSE`:
  labels show the weight alone, written without a leading zero (`.42`,
  not `0.42`)

- `minimum = 0.01`: transitions weaker than 0.01 are not drawn

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
creating networks from scratch,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for plotting
engines,
[`from_qgraph`](https://sonsoles.me/cograph/reference/from_qgraph.md)
for qgraph object conversion

## Examples

``` r
# Convert and plot a tna object
model <- tna::tna(tna::group_regulation)
from_tna(model)  # Plots with donut rings showing initial probabilities


# Use soplot engine instead
from_tna(model, engine = "soplot")


# Customize the visualization
from_tna(model, layout = "circle", donut_color = c("steelblue", "gray90"))


# Extract parameters without plotting
params <- from_tna(model, plot = FALSE)
# Modify and plot manually
params$node_fill <- "coral"
do.call(splot, params)
```
