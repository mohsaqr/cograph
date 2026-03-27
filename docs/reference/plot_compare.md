# Plot Network Difference

Plots the difference between two networks (x - y) using splot. Positive
differences (x \> y) are shown in green, negative (x \< y) in red.
Optionally displays node-level differences (e.g., initial probabilities)
as donut charts.

## Usage

``` r
plot_compare(
  x,
  y = NULL,
  i = NULL,
  j = NULL,
  pos_color = "#009900",
  neg_color = "#C62828",
  labels = NULL,
  title = NULL,
  inits_x = NULL,
  inits_y = NULL,
  show_inits = NULL,
  donut_inner_ratio = 0.8,
  force = FALSE,
  ...
)
```

## Arguments

- x:

  First network: matrix, CographNetwork, tna, igraph object, OR a
  group_tna object. For group_tna with 2 groups, compares them directly.
  For more groups, plots all pairwise comparisons (or specify i, j).

- y:

  Second network: same type as x. Ignored if x is group_tna.

- i:

  Index/name of first group when x is group_tna. NULL for all pairs.

- j:

  Index/name of second group when x is group_tna. NULL for all pairs.

- pos_color:

  Color for positive differences (x \> y). Default "#009900" (green).

- neg_color:

  Color for negative differences (x \< y). Default "#C62828" (red).

- labels:

  Node labels. NULL uses rownames or defaults.

- title:

  Plot title. NULL for auto-generated title.

- inits_x:

  Node values for x (e.g., initial probabilities). NULL to auto-extract
  from tna.

- inits_y:

  Node values for y. NULL to auto-extract from tna.

- show_inits:

  Logical: show node differences as donuts? Default TRUE if inits
  available.

- donut_inner_ratio:

  Inner radius ratio for donut (0-1). Default 0.8.

- force:

  Logical: force plotting when more than 4 groups (many comparisons).
  Default FALSE.

- ...:

  Additional arguments passed to splot().

## Value

Invisibly returns a list with difference matrix and inits difference.

## Details

The function computes element-wise subtraction of the weight matrices.
Edge colors indicate direction of difference:

- Green edges: x has higher weight than y

- Red edges: y has higher weight than x

When initial probabilities (inits) are provided or extracted from tna
objects, nodes display donut charts showing the absolute difference,
colored by direction:

- Green donut: x has higher initial probability

- Red donut: y has higher initial probability

For lists of networks (e.g., group_tna), specify which elements to
compare using i and j parameters.

## Examples

``` r
# \donttest{
# Compare two adjacency matrices
set.seed(42)
m1 <- matrix(runif(25), 5, 5)
m2 <- matrix(runif(25), 5, 5)
rownames(m1) <- colnames(m1) <- LETTERS[1:5]
rownames(m2) <- colnames(m2) <- LETTERS[1:5]
plot_compare(m1, m2)


# With node-level differences
inits1 <- c(0.3, 0.2, 0.2, 0.15, 0.15)
inits2 <- c(0.1, 0.4, 0.2, 0.2, 0.1)
plot_compare(m1, m2, inits_x = inits1, inits_y = inits2)

# }
```
