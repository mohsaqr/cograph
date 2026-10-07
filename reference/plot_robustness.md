# Plot Network Robustness

Plots the fraction of nodes remaining in the largest connected component
against the fraction of nodes or edges removed, with one line per attack
strategy, in base graphics.

## Usage

``` r
plot_robustness(
  ...,
  x = NULL,
  measures = c("betweenness", "degree", "random"),
  colors = NULL,
  title = "Network Robustness: sequential removal of nodes",
  xlab = "Fraction of removed nodes",
  ylab = "Fraction of remaining nodes",
  lwd = 1.5,
  legend_pos = "topright",
  n_iter = 1000,
  seed = NULL,
  type = "vertex"
)
```

## Arguments

- ...:

  One or more robustness results from
  [`robustness`](https://sonsoles.me/cograph/reference/robustness.md).
  When these are supplied, `x` is ignored.

- x:

  Network on which robustness is computed for each of `measures`. Used
  when `...` is empty.

- measures:

  Character vector of attack strategies to compare when `x` is supplied.
  Default c("betweenness", "degree", "random").

- colors:

  Vector of colors named by measure (`"betweenness"`, `"degree"`,
  `"random"`). Default NULL uses red for betweenness, green for degree
  and blue for random. Unmatched measures are gray.

- title:

  Plot title. Default "Network Robustness: sequential removal of nodes".

- xlab:

  X-axis label. Default "Fraction of removed nodes".

- ylab:

  Y-axis label. Default "Fraction of remaining nodes".

- lwd:

  Line width. Default 1.5.

- legend_pos:

  Legend position. Default "topright".

- n_iter:

  Number of iterations for random removal when `x` is supplied. Default
  1000.

- seed:

  Random seed. Default NULL.

- type:

  Removal type, "vertex" or "edge", when `x` is supplied. Default
  "vertex". With "edge", `measures` must not include "degree".

## Value

Invisibly, a `cograph_robustness` data frame that stacks the plotted
results (columns as in
[`robustness`](https://sonsoles.me/cograph/reference/robustness.md)).

## Examples

``` r
plot_robustness(x = regulation_net, n_iter = 20, seed = 1)
```
