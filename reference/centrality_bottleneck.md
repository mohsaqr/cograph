# Bottleneck Centrality

Bottleneck centrality (Przulj, Wigle and Jurisica 2004) counts the
shortest-path trees in which a node is a bottleneck. For each source
\\s\\, every shortest path from \\s\\ to every reachable node is
enumerated, and a node \\v \ne s\\ scores one for that source when it
lies on more than \\\|T_s\|/4\\ of these paths, \\\|T_s\|\\ the number
of nodes \\s\\ reaches, itself included (cograph 2.7.19 and earlier used
\\n/4\\, which differs where \\s\\ does not reach every node).

## Usage

``` r
centrality_bottleneck(x, mode = "all", ...)
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

A named integer vector with one score per node, in input node order.

## Details

Distances are hop counts, so edge weights are ignored. `mode` sets the
direction in which the paths leave the source. Paths that end at \\v\\
count toward its total. A network with one node gives that node a score
of one.

## References

Przulj, N., Wigle, D. A., & Jurisica, I. (2004). Functional topology in
a network of protein interactions. Bioinformatics, 20(3), 340-348.
[doi:10.1093/bioinformatics/btg415](https://doi.org/10.1093/bioinformatics/btg415)
.

## See also

[`centrality_stress`](https://sonsoles.me/cograph/reference/centrality_stress.md),
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_bottleneck(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          8          7          9          9          9          8          8 
#>   Evaluate     Create      Share 
#>          8          8          7 
```
