# Entropy Centrality

Entropy centrality, in the form of the centiserve package, removes the
node and measures how evenly reachability is spread over the remaining
network. With \\r_j\\ the number of nodes that node \\j\\ reaches in
\\G - v\\ and \\P\\ half the total of the \\r_j\\, \$\$H(v) = -\sum\_{j}
y_j \log_2 y_j, \qquad y_j = \frac{r_j}{P}.\$\$ Terms with \\y_j = 0\\
are dropped.

## Usage

``` r
centrality_entropy(x, mode = "all", ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- mode:

  For directed networks: `"all"` (default), `"out"` or `"in"`.

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order.

## Details

Edge weights are ignored, and `mode` sets the direction of reachability.
When the network stays strongly connected after the removal of any one
node, every node scores \\2 \log_2((n-1)/2)\\, as on `regulation_net`.

## See also

[`centrality_distance_entropy`](https://sonsoles.me/cograph/reference/centrality_distance_entropy.md),
[`centrality_diversity`](https://sonsoles.me/cograph/reference/centrality_diversity.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_entropy(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>    4.33985    4.33985    4.33985    4.33985    4.33985    4.33985    4.33985 
#>   Evaluate     Create      Share 
#>    4.33985    4.33985    4.33985 
```
