# Access and Modify a Cograph Network

These functions read and replace the parts of a `cograph_network` object
created by
[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md). The
getters return the node table, the edge table, the node labels, the node
groups, the source type, the stored estimation data, the metadata list,
and the counts of nodes and edges. The setters return a modified copy of
the network.

## Usage

``` r
get_nodes(x)

get_edges(x)

get_labels(x)

get_source(x)

get_data(x)

get_meta(x)

set_nodes(x, nodes_df)

set_edges(x, edges_df)

set_layout(x, layout_df)

set_groups(
  x,
  groups = NULL,
  type = c("group", "cluster", "layer"),
  nodes = NULL,
  layers = NULL,
  clusters = NULL
)

get_groups(x)

nodes(x)

is_directed(x)

n_nodes(x)

n_edges(x)
```

## Arguments

- x:

  A `cograph_network` object. `is_directed()` also accepts a
  [`CographNetwork`](https://sonsoles.me/cograph/reference/CographNetwork.md)
  or an igraph object.

- nodes_df:

  A data frame of node information. A missing `id` column is filled with
  row numbers and a missing `label` column with the ids. The stored
  weight matrix is rebuilt from the new node table.

- edges_df:

  A data frame with columns `from` and `to` (integer row numbers into
  the node table) and an optional `weight` column, which defaults to 1.
  Extra columns are kept. Each edge may appear once, and an undirected
  network counts A-B and B-A as the same edge.

- layout_df:

  A data frame with `x` and `y` columns, or a matrix whose first two
  columns are used, with one row per node.

- groups:

  Node groupings in one of these formats:

  - A character string naming a community detection method of
    [`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)
    (`"louvain"`, `"walktrap"`, `"fast_greedy"`, `"label_prop"`,
    `"infomap"`, `"leiden"`), which requires the igraph package.

  - A named list mapping each group name to a vector of node labels, for
    example `list(A = c("N1", "N2"), B = c("N3", "N4"))`.

  - An unnamed vector with one group assignment per node, in node order.

  - A data frame with a `node` (or `nodes`) column and one of `layer`,
    `cluster` or `group` (plural forms are accepted and normalized to
    the singular).

  - `NULL`, in which case `nodes` and one of `layers` or `clusters`
    supply the grouping.

- type:

  Group type stored by `set_groups()`. One of `"group"` (default),
  `"cluster"` or `"layer"`. It is ignored when `layers` or `clusters` is
  given, since the type then follows from the argument used.

- nodes:

  Character vector of node labels, used with `layers` or `clusters` to
  give groupings as vectors. When `NULL`, the assignments follow the
  node order of the network.

- layers:

  Character or factor vector of layer assignments, the same length as
  `nodes`.

- clusters:

  Character or factor vector of cluster assignments, the same length as
  `nodes`.

## Value

- `get_nodes()`, `nodes()`:

  The node table, with `id` and `label` columns plus layout coordinates
  or other metadata columns when present. `nodes()` is a deprecated
  alias of `get_nodes()`.

- `get_edges()`:

  A data frame with one row per edge and columns `from` and `to`
  (integer row numbers into the node table), `weight`, and any extra
  edge columns. An undirected network stores one row per unordered pair.
  [`as.data.frame()`](https://sonsoles.me/cograph/reference/as_cograph.md)
  returns the same table with node labels as endpoints.

- `get_labels()`:

  A character vector of node labels.

- `get_groups()`:

  A data frame with a `node` column and one of `layer`, `cluster` or
  `group`, or `NULL` when no groups are set.

- `get_source()`:

  A character string naming the input type (for example `"matrix"`,
  `"tna"`, `"igraph"`, `"edgelist"`), or `"unknown"`.

- `get_data()`:

  The original estimation data (for example the sequence data of a tna
  model), or `NULL` when none is stored.

- `get_meta()`:

  A list with component `source` (input type). Networks built from a tna
  model also carry `tna` (type, group name and group index).

- `n_nodes()`, `n_edges()`:

  An integer count. Each undirected edge counts once.

- `is_directed()`:

  A single logical value.

- `set_nodes()`, `set_edges()`, `set_layout()`, `set_groups()`:

  The modified `cograph_network`. `set_layout()` writes the coordinates
  into the `x` and `y` columns of the node table. `set_groups()` stores
  the grouping as `node_groups` for use by the group-aware plot
  functions. It stops with an error when a node is assigned twice, when
  a node is missing from the assignment or unknown to the network, and
  when fewer than two groups result. `set_edges()` raises an error of
  class `cograph_bad_selection` for endpoints outside the node table and
  for duplicated edges.

## See also

[`as_cograph`](https://sonsoles.me/cograph/reference/as_cograph.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)

## Examples

``` r
net <- as_cograph(regulation_net)
get_edges(net)
#>    from to weight
#> 1     4  1   0.28
#> 2     5  1   0.05
#> 3     6  1   0.30
#> 4     9  1   0.14
#> 5     7  2   0.11
#> 6    10  2   0.21
#> 7     2  3   0.13
#> 8     5  3   0.15
#> 9     7  3   0.07
#> 10    8  3   0.33
#> 11    9  3   0.17
#> 12   10  3   0.49
#> 13    3  4   0.16
#> 14    8  4   0.43
#> 15   10  4   0.39
#> 16    1  5   0.35
#> 17    6  5   0.35
#> 18    7  5   0.42
#> 19    8  5   0.07
#> 20    2  6   0.40
#> 21    4  6   0.34
#> 22    4  7   0.17
#> 23    2  8   0.49
#> 24    9  8   0.39
#> 25    2  9   0.20
#> 26    3  9   0.37
#> 27    6  9   0.14
#> 28    1 10   0.27
#> 29    2 10   0.36
#> 30    9 10   0.23
```
