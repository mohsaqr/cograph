# Gil-Schmidt Power Index

The Gil-Schmidt power index (Gil and Schmidt 1996) is the mean
reciprocal hop distance from a node to the nodes it reaches, \\R(v)\\:
\$\$GS(v) = \frac{1}{\|R(v)\|} \sum\_{w \in R(v)} \frac{1}{d(v, w)}.\$\$
The score lies between 0 and 1; a node adjacent to every node it reaches
scores 1, and a node that reaches none scores 0.

## Usage

``` r
centrality_gilschmidt(x, mode = "all", ...)
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

Distances are hop counts, so edge weights are ignored and
`invert_weights` has no effect. `mode` sets the direction of the paths.
With `mode = "out"` the values match
[`sna::gilschmidt()`](https://rdrr.io/pkg/sna/man/gilschmidt.html) with
its default settings (`normalize = TRUE`). cograph 2.7.19 and earlier
divided by \\n - 1\\, which differs wherever a node does not reach every
other node.

## See also

[`centrality_harmonic`](https://sonsoles.me/cograph/reference/centrality_harmonic.md),
[`centrality_closeness`](https://sonsoles.me/cograph/reference/centrality_closeness.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Examples

``` r
centrality_gilschmidt(regulation_net)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>  0.7777778  0.8333333  0.8888889  0.8333333  0.7777778  0.7777778  0.7222222 
#>   Evaluate     Create      Share 
#>  0.7777778  0.8333333  0.7777778 
```
