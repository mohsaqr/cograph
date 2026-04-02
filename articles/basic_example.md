# Basic example

This vignette demonstrates the basic usage of the `cograph` package
using a transition probability matrix from a learning analytics study.

``` r
library(cograph)
```

## Create a transition matrix

We use a simulated Markov transition matrix representing student
learning behavior transitions across nine states.

``` r
states <- c("Read", "Watch", "Try", "Ask", "Discuss",
            "Review", "Search", "Reflect", "Submit")

set.seed(42)
mat <- matrix(c(
  0.00, 0.25, 0.15, 0.00, 0.10, 0.00, 0.08, 0.00, 0.00,
  0.10, 0.00, 0.30, 0.00, 0.00, 0.12, 0.00, 0.00, 0.00,
  0.00, 0.10, 0.00, 0.20, 0.00, 0.00, 0.00, 0.15, 0.25,
  0.05, 0.00, 0.10, 0.00, 0.30, 0.00, 0.00, 0.00, 0.00,
  0.00, 0.00, 0.00, 0.15, 0.00, 0.20, 0.00, 0.18, 0.00,
  0.12, 0.08, 0.00, 0.00, 0.00, 0.00, 0.10, 0.00, 0.20,
  0.00, 0.00, 0.15, 0.00, 0.00, 0.10, 0.00, 0.00, 0.12,
  0.00, 0.00, 0.10, 0.00, 0.12, 0.00, 0.00, 0.00, 0.28,
  0.00, 0.00, 0.00, 0.00, 0.00, 0.10, 0.00, 0.05, 0.00
), nrow = 9, byrow = TRUE)
rownames(mat) <- colnames(mat) <- states
```

## Example 1: Default plot

``` r
splot(mat, node_size = 9)
```

![](basic_example_files/figure-html/unnamed-chunk-4-1.png)

## Example 2: Circle layout

``` r
splot(mat, layout = "circle", node_size = 9)
```

![](basic_example_files/figure-html/unnamed-chunk-5-1.png)

## Example 3: With edge labels

``` r
splot(mat, layout = "circle", node_size = 9, edge_labels = TRUE)
```

![](basic_example_files/figure-html/unnamed-chunk-6-1.png)

## Example 4: Themes

``` r
splot(mat, node_size = 9, theme = "dark", title = "Dark")
```

![](basic_example_files/figure-html/unnamed-chunk-7-1.png)

``` r
splot(mat, node_size = 9, theme = "minimal", title = "Minimal")
```

![](basic_example_files/figure-html/unnamed-chunk-7-2.png)

``` r
splot(mat, node_size = 9, theme = "colorblind", title = "Colorblind")
```

![](basic_example_files/figure-html/unnamed-chunk-7-3.png)

## Example 5: Custom node colors

``` r
splot(mat, layout = "circle", node_size = 9,
      node_fill = palette_pastel(9))
```

![](basic_example_files/figure-html/unnamed-chunk-8-1.png)

## Example 6: Node shapes

``` r
splot(mat, layout = "circle", node_size = 9,
      node_shape = c("circle", "square", "triangle", "diamond",
                     "star", "pentagon", "hexagon", "heart", "circle"),
      node_fill = palette_colorblind(9))
```

![](basic_example_files/figure-html/unnamed-chunk-9-1.png)

## Example 7: Dotted edges

``` r
splot(mat, node_size = 9, edge_style = 3)
```

![](basic_example_files/figure-html/unnamed-chunk-10-1.png)

## Example 8: Dashed edge start

``` r
splot(mat, node_size = 9, edge_start_style = "dashed")
```

![](basic_example_files/figure-html/unnamed-chunk-11-1.png)

## Example 9: Edge labels with donuts

``` r
splot(mat, node_size = 9, edge_labels = TRUE,
      edge_label_size = 0.6,
      edge_positive_color = "#1976D2",
      edge_negative_color = "#D32F2F",
      donut_fill = runif(9),
      donut_color = palette_rainbow(9),
      theme = "minimal")
```

![](basic_example_files/figure-html/unnamed-chunk-12-1.png)

## Example 10: Grid graphics with soplot

``` r
soplot(mat, node_size = 9, layout = "oval")
```

![](basic_example_files/figure-html/unnamed-chunk-13-1.png)

## Example 11: Saving

``` r
splot(mat, node_size = 9, filetype = "png",
      filename = file.path(tempdir(), "network"), width = 8, height = 8)
```
