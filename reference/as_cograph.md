# Convert to Cograph Network

`as_cograph()` creates a `cograph_network` object from a matrix, an edge
list, or a network object of another package. `to_cograph()` is an
alias. The object is a named list that every cograph function accepts.

## Usage

``` r
as_cograph(x, directed = NULL, simplify = FALSE, ...)

to_cograph(x, directed = NULL, ...)

# S3 method for class 'cograph_network'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("edges", "nodes")
)
```

## Arguments

- x:

  Network input. One of a square numeric weight matrix, a data frame
  edge list, an igraph object, a statnet network object, a qgraph
  object, a tna object, or an existing `cograph_network`, which is
  returned as it is. In an edge list the endpoint columns are found by
  name, ignoring case (`from`, `source`, `src`, `v1`, `node1` or `i`,
  and `to`, `target`, `tgt`, `v2`, `node2` or `j`), and otherwise the
  first two columns are used. An optional weight column is found by the
  names `weight`, `w`, `value` or `strength`. For
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), a
  `cograph_network`.

- directed:

  Logical. Forces a directed or undirected interpretation. `NULL`
  (default) detects it from the input.

- simplify:

  Logical or character. If `FALSE` (default), every transition from tna
  sequence data is a separate edge. If `TRUE` (equivalent to `"sum"`) or
  one of `"sum"`, `"mean"`, `"max"`, `"min"`, duplicate transitions are
  aggregated with that function. Other inputs are not affected.

- ...:

  Passed from `to_cograph()` to `as_cograph()`; otherwise unused.

- row.names:

  `NULL` or a character vector of row names, as for
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Logical, as for
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). It is
  ignored, and the column names are always the documented ones.

- what:

  Which table
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  returns, `"edges"` (default) or `"nodes"`.

## Value

`as_cograph()` and `to_cograph()` return a `cograph_network` object, a
named list with components:

- `nodes`:

  Data frame with `id`, `label`, and optional layout or metadata
  columns.

- `edges`:

  Data frame with integer `from` and `to` columns (row numbers into
  `nodes`), `weight`, and extra columns such as `session` and `time` for
  tna input. Repeated rows of an edge list are kept as separate edges.

- `directed`:

  Logical. Whether the network is directed.

- `weights`:

  The n x n weight matrix for matrix and tna input, or `NULL` (for
  example for an edge list).

- `data`:

  The original estimation data (sequence data, edge list), or `NULL`.

- `meta`:

  Metadata list with `source` (input type), a `tna` entry for tna input
  (type, group name and group index) and, optionally, `splot` (rendering
  hints read by
  [`splot`](https://sonsoles.me/cograph/reference/splot.md)).

- `node_groups`:

  Optional data frame of node groupings.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns a
base data frame. For `what = "edges"` it has one row per edge with
columns `from` and `to` (node labels), `weight`, and any extra edge
columns the network carries, such as `session` or columns computed by
[`mutate_edges`](https://sonsoles.me/cograph/reference/mutate_edges.md).
For `what = "nodes"` it has one row per node with the node table columns
(`id`, `label`, layout coordinates and any custom columns).
[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md)
returns only `from`, `to` and `weight`.

## Details

A `cograph_network` prints a short description of its nodes, edges and
source, [`summary()`](https://rdrr.io/r/base/summary.html) reports
counts and edge weight statistics,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) plots it with
[`sn_render`](https://sonsoles.me/cograph/reference/soplot.md), and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
its edge or node table. The accessor and setter functions are documented
in [`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md).

Producer packages may attach plotting hints under `meta$splot`. The
recognized fields are `renderer` (the cograph renderer to use), `weight`
(the edge column or matrix plotted as `weight`), and `defaults` (a named
list of renderer arguments). Entries in `defaults` are defaults only,
and arguments passed to
[`splot`](https://sonsoles.me/cograph/reference/splot.md) override them.
`renderer` and `weight` define which view is plotted and are not
overridden by plot arguments.

## See also

[`get_nodes`](https://sonsoles.me/cograph/reference/get_nodes.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
[`to_df`](https://sonsoles.me/cograph/reference/to_data_frame.md)

## Examples

``` r
net <- as_cograph(regulation_net)
as.data.frame(net, what = "edges")
#>          from         to weight
#> 1       Adapt    Explore   0.28
#> 2     Reflect    Explore   0.05
#> 3     Discuss    Explore   0.30
#> 4      Create    Explore   0.14
#> 5  Synthesize       Plan   0.11
#> 6       Share       Plan   0.21
#> 7        Plan    Monitor   0.13
#> 8     Reflect    Monitor   0.15
#> 9  Synthesize    Monitor   0.07
#> 10   Evaluate    Monitor   0.33
#> 11     Create    Monitor   0.17
#> 12      Share    Monitor   0.49
#> 13    Monitor      Adapt   0.16
#> 14   Evaluate      Adapt   0.43
#> 15      Share      Adapt   0.39
#> 16    Explore    Reflect   0.35
#> 17    Discuss    Reflect   0.35
#> 18 Synthesize    Reflect   0.42
#> 19   Evaluate    Reflect   0.07
#> 20       Plan    Discuss   0.40
#> 21      Adapt    Discuss   0.34
#> 22      Adapt Synthesize   0.17
#> 23       Plan   Evaluate   0.49
#> 24     Create   Evaluate   0.39
#> 25       Plan     Create   0.20
#> 26    Monitor     Create   0.37
#> 27    Discuss     Create   0.14
#> 28    Explore      Share   0.27
#> 29       Plan      Share   0.36
#> 30     Create      Share   0.23
```
