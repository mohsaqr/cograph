# s-shell Index

The s-shell index (Liu, Tang, Do and Hui 2017) peels the network like
the k-shell decomposition but by a strength computed from the topology.
Each link gets the asymmetric weight \$\$w\_{ij} = 1 + (k_i\\
k^{out}\_j)^a,\$\$ where \\k^{out}\_j\\ counts the neighbors of \\j\\
outside the closed neighborhood of \\i\\, and the strength of \\i\\ is
\\s_i = \sum\_{j \in N(i)} w\_{ij}\\. At each step the nodes at the
minimum remaining strength are removed, the removals cascade, and the
removed nodes receive the next shell index.

## Usage

``` r
centrality_s_shell(x, s_shell_a = 0.5, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- s_shell_a:

  Exponent \\a\\ of the link weights (default 0.5, the value the source
  recommends).

- ...:

  Further arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

A named integer vector with one shell index per node, in input node
order.

## Details

Direction, edge weights and self-loops are ignored. The index is an
ordinal counter starting at 1 for the outermost shell, so values are
comparable within a network only. Higher indices mark more central
nodes. Isolated nodes form shell 1 on their own and shift every other
shell up by one. With \\a = 0\\ the shells are the dense ranks of the
k-core numbers. A `s_shell_a` that is not a single non-negative number
raises an error of class `cograph_bad_parameter`.

## References

Liu, Y., Tang, M., Do, Y., & Hui, P. M. (2017). Accurate ranking of
influential spreaders in networks based on dynamically asymmetric link
weights. Physical Review E, 96(2), 022323.

## See also

[`centrality_coreness`](https://sonsoles.me/cograph/reference/centrality_coreness.md),
[`centrality_weighted_kshell`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_s_shell(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          2          2          2          2          2          2          1 
#>   Evaluate     Create      Share 
#>          2          2          2 
```
