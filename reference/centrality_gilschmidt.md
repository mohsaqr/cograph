# Gil-Schmidt Power Index

The Gil-Schmidt power index sums the reciprocal hop distances from a
node to the nodes it reaches and divides by \\n - 1\\: \$\$GS(v) =
\frac{1}{n - 1} \sum\_{w \ne v} \frac{1}{d(v, w)}.\$\$ Unreachable nodes
contribute 0, so the score lies between 0 and 1, and a node adjacent to
every other node scores 1.

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
its default settings.

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
