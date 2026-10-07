# Rumor Centrality

Rumor centrality (Shah and Zaman 2010, 2011) is the maximum-likelihood
score for the source of a rumor that has spread to every node under the
susceptible-infected model. On a tree rooted at \\v\\ it counts the
spreading orders that start at \\v\\, \$\$R(v) = \frac{N!}{\prod_u
T^v_u},\$\$ where \\T^v_u\\ is the size of the subtree rooted at \\u\\.
On a general graph \\R\\ is evaluated on the breadth-first tree rooted
at each node.

## Usage

``` r
centrality_rumor(x, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md),
  such as `normalized`.

## Value

A named numeric vector with one score per node, in input node order,
holding \\\log R\\.

## Details

The value is returned as \\\log R(v)\\ with the natural logarithm,
because \\N!\\ overflows beyond 170 nodes. \\N\\ is the size of the
component of the node, so a disconnected network is scored component by
component and an isolated node scores 0. The breadth-first tree attaches
each node to the earliest discovered node of the previous layer,
scanning neighbors in node order. Direction, edge weights and self-loops
are ignored. Higher values mark more plausible origins.

## References

Shah, D., & Zaman, T. (2010). Detecting sources of computer viruses in
networks: theory and experiment. ACM SIGMETRICS, 203-214.

Shah, D., & Zaman, T. (2011). Rumors in a network: Who's the culprit?
IEEE Transactions on Information Theory, 57(8), 5163-5181.

## See also

[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_rumor(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>   10.72239   11.01007   11.41553   11.01007   10.72239   10.72239   10.49924 
#>   Evaluate     Create      Share 
#>   10.72239   11.01007   10.60460 
```
