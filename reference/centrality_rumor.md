# Rumor Centrality

Shah and Zaman's (2010, 2011) maximum-likelihood score for the source of
a rumor that has spread under the susceptible-infected model to every
node. On a tree, \$\$R(v) = \frac{N!}{\prod\_{u} T^v_u},\$\$ where
\\T^v_u\\ is the number of nodes in the subtree rooted at \\u\\ when the
tree is rooted at \\v\\: the number of spreading orders that could have
started at \\v\\. On a general graph the paper evaluates \\R\\ on the
breadth-first tree rooted at each node (its eq. 24). Higher values mark
nodes that are more plausible origins, which in practice are nodes near
the centre of the network.

## Usage

``` r
centrality_rumor(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector, \\\log R\\ per node.

## Details

The value is returned as \\\log R(v)\\ (natural log) because \\N!\\
overflows beyond 170 nodes; rankings and differences are unchanged.
\\N\\ is the size of the node's component, so a disconnected graph is
scored component by component and an isolate scores 0. The breadth-first
tree attaches each node to the earliest discovered node of the previous
layer, scanning neighbours in label order; the paper does not fix a tie
rule, and this one reproduces its Figure 3. Direction and edge weights
are ignored.

Validated on trees against a brute-force count of spreading orders and
against the worked examples in the paper.

## References

Shah, D., & Zaman, T. (2010). Detecting sources of computer viruses in
networks: theory and experiment. ACM SIGMETRICS, 203-214.

Shah, D., & Zaman, T. (2011). Rumors in a network: Who's the culprit?
IEEE Transactions on Information Theory, 57(8), 5163-5181.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]
exp(centrality_rumor(path5))   # spreading orders from each node
#> A B C D E 
#> 1 4 6 4 1 
```
