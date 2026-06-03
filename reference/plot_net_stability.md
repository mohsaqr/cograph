# Plot Centrality Stability Results

Visualizes the centrality stability analysis from a `net_stability`
object. Shows how centrality correlations drop as cases are removed.

## Usage

``` r
plot_net_stability(x, ...)
```

## Arguments

- x:

  A `net_stability` object (from
  [`Nestimate::centrality_stability`](https://saqr.me/Nestimate/reference/centrality_stability.html)).

- ...:

  Additional graphical arguments.

## Value

Invisibly returns `x`.

## Examples

``` r
set.seed(1)
seqs <- data.frame(T1 = sample(c("A","B","C"), 30, replace = TRUE),
                   T2 = sample(c("A","B","C"), 30, replace = TRUE))
net <- Nestimate::build_network(seqs, method = "tna")
cs <- Nestimate::centrality_stability(net, iter = 10)
plot_net_stability(cs)
```
