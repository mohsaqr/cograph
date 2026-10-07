# Convert a tna object to cograph parameters

Extracts the transition matrix, labels, and initial state probabilities
from a `tna` object and plots the network with a cograph engine. Initial
probabilities are mapped to donut fills.

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
  [`tna::tna()`](https://sonsoles.me/tna/reference/build_model.html)

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

  Logical. A zero weight means that the edge is absent, so an edge whose
  weight rounds to zero at `weight_digits` is dropped. With `TRUE` each
  such edge is plotted at the smallest magnitude that `weight_digits`
  can express, with its original sign. Other weights are unchanged.
  Default: `FALSE`.

- ...:

  Additional parameters passed to the plotting engine (e.g., `layout`,
  `node_fill`, `donut_color`).

## Value

Invisibly, a named list of plotting parameters (the weight matrix `x`,
`labels`, `directed`, the donut settings and the visual defaults above,
after the overrides in `...`). It can be passed to
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) with
[`do.call()`](https://rdrr.io/r/base/do.call.html). An input that is not
a `tna` object raises an error.

## Details

### Conversion Process

The transition matrix (`weights`) supplies the edge weights, the state
labels (`labels`) supply the node labels, and the initial state
probabilities (`inits`) supply the `donut_fill` values. The donuts are
plotted with `donut_inner_ratio = 0.8`.

Directedness is read from the tna object when it is recorded there.
Otherwise a symmetric matrix is treated as undirected and an asymmetric
matrix as directed.

### TNA Visual Defaults

The following defaults are applied. Each can be overridden through
`...`.

- `layout = "oval"`.

- `node_fill`: the RColorBrewer Accent palette for up to 8 states, Set3
  for 9 to 12 states, and a qualitative HCL palette for more.

- `node_size = 7`.

- `edge_color = "#003355"`.

- `edge_labels = TRUE`, with `edge_label_size = 0.4` and
  `edge_label_position = 0.7`.

- `edge_label_style = "estimate"` and `edge_label_leading_zero = FALSE`,
  so a label shows the weight alone without a leading zero (for example
  `.42`).

- `minimum = 0.01`, so transitions weaker than 0.01 are not plotted.

- For directed networks, `arrow_size = 0.61`,
  `edge_start_style = "dotted"` and `edge_start_length = 0.2` (the first
  20 percent of each edge is dotted).

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
from_tna(tna::tna(regulation_net))
```
