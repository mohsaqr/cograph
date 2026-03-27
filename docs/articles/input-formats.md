# Input Formats

## Overview

cograph accepts network data in all common formats used in R. Pass any
supported object directly to
[`splot()`](http://sonsoles.me/cograph/reference/splot.md) and it will
be automatically parsed.

## Supported Formats

### 1. Adjacency/Weight Matrices

A square numeric matrix where `M[i,j]` represents the edge weight from
node `i` to node `j`.

**Sources:**

- [`cor()`](https://rdrr.io/r/stats/cor.html),
  [`cov()`](https://rdrr.io/r/stats/cor.html) — correlation and
  covariance matrices
- [`qgraph::getWmat()`](https://rdrr.io/pkg/qgraph/man/getWmat.html) —
  extract weights from qgraph objects
- [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html)
  — network estimation output
- `psychonetrics` — structural equation modeling networks
- `markovchain` — transition probability matrices
- `as.matrix(dist())` — distance/dissimilarity matrices

**Auto-detection:**

- Symmetric matrices are treated as undirected (upper triangle only)
- Asymmetric matrices are treated as directed (all entries)
- Node labels extracted from
  [`rownames()`](https://rdrr.io/r/base/colnames.html)/[`colnames()`](https://rdrr.io/r/base/colnames.html)

**Usage:**

``` r

splot(matrix)
splot(matrix, directed = TRUE)  # override auto-detection
```

### 2. Edge List Data Frames

A data frame where each row is an edge with source, target, and optional
weight columns.

**Sources:**

- Database exports (SQL joins on relationship tables)
- CSV files from SNAP, Pajek, KONECT repositories
- `igraph::as_data_frame(g, what = "edges")`
- API responses from social platforms
- Parsed log files

**Column detection (case-insensitive):**

| Purpose | Recognized names                            |
|---------|---------------------------------------------|
| Source  | `from`, `source`, `src`, `v1`, `node1`, `i` |
| Target  | `to`, `target`, `tgt`, `v2`, `node2`, `j`   |
| Weight  | `weight`, `w`, `value`, `strength`          |

Falls back to columns 1 and 2 if no match.

**Usage:**

``` r

splot(data.frame(from = ..., to = ..., weight = ...))
splot(edges, edge_duplicates = "sum")  # handle duplicates
```

### 3. igraph Objects

Objects of class `igraph` from the igraph package.

**Sources:**

- [`graph_from_data_frame()`](https://r.igraph.org/reference/graph_from_data_frame.html),
  [`graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html)
- `make_graph("Zachary")`,
  [`make_ring()`](https://r.igraph.org/reference/make_ring.html),
  [`sample_pa()`](https://r.igraph.org/reference/sample_pa.html), etc.
- [`read_graph()`](https://r.igraph.org/reference/read_graph.html) —
  import from GraphML, GML, Pajek, edge lists
- `intergraph::asIgraph()` — convert from network objects

**Preserved:**

- Vertex attributes: `V(g)$name`, `V(g)$color`, etc.
- Edge attributes: `E(g)$weight`, `E(g)$type`, etc.
- Directedness from `is_directed(g)`

**Usage:**

``` r

splot(g)
splot(g, layout = "fr")  # use igraph layouts: fr, kk, circle, drl, mds
```

### 4. network Objects (statnet)

Objects of class `network` from the network/statnet ecosystem.

**Sources:**

- [`network::network()`](https://rdrr.io/pkg/network/man/network.html) —
  create from matrix or edge list
- `ergm` package — exponential random graph models
- `sna` package — social network analysis
- `intergraph::asNetwork()` — convert from igraph
- Import from Pajek, UCINET formats

**Preserved:**

- Vertex attributes via `%v%` operator
- Edge attributes via `%e%` operator
- Directedness from
  [`is.directed()`](https://r.igraph.org/reference/is.directed.html)

**Usage:**

``` r

splot(net)
```

### 5. qgraph Objects

Objects created by the qgraph package.

**Sources:**

- `qgraph::qgraph(..., DoNotPlot = TRUE)`
- [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html)
  results
- `psychonetrics` model outputs

**Preserved:**

- Layout coordinates from `q$layout`
- Edge weights from `q$Edgelist`
- Node labels from `q$graphAttributes$Nodes`

**Usage:**

``` r

splot(qgraph_object)  # preserves existing layout
```

### 6. TNA Objects

Objects of class `tna` from the tna package (Transition Network
Analysis).

**Sources:**

- [`tna::tna()`](http://sonsoles.me/tna/reference/build_model.md) —
  build from sequence data
- [`tna::group_tna()`](http://sonsoles.me/tna/reference/group_model.md)
  — grouped transition analysis
- Learning analytics, process mining, behavioral sequences

**Extracted:** - Transition matrix from `$weights` - State labels from
`$labels` - Initial probabilities from `$inits`

**Usage:**

``` r

splot(tna_model)
splot(tna_model, node_shape = "donut", donut_fill = tna_model$inits)
splot(group_tna_model, i = 1)  # select group by index or name
```

## Weight Preprocessing

| Parameter           | Effect                                            |
|---------------------|---------------------------------------------------|
| `weight_digits = 2` | Round weights; edges rounding to zero are removed |
| `threshold = 0.3`   | Remove edges with \|weight\| \< threshold         |
| `minimum = 0.3`     | Alias for threshold (qgraph compatibility)        |
| `maximum = 1.0`     | Set reference maximum for edge width scaling      |
| `edge_scale_mode`   | `"linear"`, `"sqrt"`, `"log"`, or `"rank"`        |

## Special Cases

| Feature          | Behavior                                              |
|------------------|-------------------------------------------------------|
| Negative weights | Colored differently (green/red by default)            |
| Self-loops       | Rendered as loops; control angle with `loop_rotation` |
| Reciprocal edges | Curved apart automatically in directed networks       |
| Duplicate edges  | Error by default; use `edge_duplicates` to aggregate  |

## Conversion Functions

cograph provides functions to convert between formats. All conversion
functions accept any supported input type.

### as_cograph() / to_cograph() — Import to cograph

Convert any format to a `cograph_network` object.

**Accepted inputs:**

- `matrix` — Adjacency/weight matrix
- `data.frame` — Edge list with from/to/weight columns
- `igraph` — igraph object
- `network` — statnet network object
- `qgraph` — qgraph object
- `tna` — TNA model object
- `group_tna` — Grouped TNA model object

``` r

net <- as_cograph(matrix)
net <- as_cograph(igraph_obj)
net <- as_cograph(tna_model)

# Override auto-detected directedness
net <- as_cograph(matrix, directed = TRUE)
```

The function is idempotent — passing a `cograph_network` returns it
unchanged.

### to_igraph() — Export to igraph

Convert any format to an igraph object:

``` r

# From matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
g <- to_igraph(adj)

# From cograph_network
g <- to_igraph(net)

# From tna
g <- to_igraph(tna_model)

# Force directed/undirected
g <- to_igraph(adj, directed = TRUE)
g <- to_igraph(adj, directed = FALSE)
```

### to_data_frame() / to_df() — Export to Edge List

Convert any format to an edge list data frame:

``` r

adj <- matrix(c(0, .5, .8,
                .5, 0, .3,
                .8, .3, 0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

df <- to_df(adj)
#   from to weight
# 1    A  B    0.5
# 2    A  C    0.8
# 3    B  C    0.3
```

Output columns: `from`, `to`, `weight`

### to_matrix() — Export to Adjacency Matrix

Convert any format to an adjacency matrix:

``` r

# From cograph_network
net <- as_cograph(adj)
mat <- to_matrix(net)

# From igraph
g <- igraph::make_ring(5)
mat <- to_matrix(g)
```

Returns a square numeric matrix with row/column names preserved.

### to_network() — Export to statnet network

Convert any format to a statnet network object:

``` r

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")

# Convert to statnet network
statnet_net <- to_network(adj)
```

Requires the `network` package. Preserves edge weights and vertex names.

| Function | Output | Use Case |
|----|----|----|
| [`as_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md) / [`to_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md) | `cograph_network` | Import for visualization with splot |
| [`to_igraph()`](http://sonsoles.me/cograph/reference/to_igraph.md) | `igraph` | Use igraph’s analysis functions |
| [`to_df()`](http://sonsoles.me/cograph/reference/to_data_frame.md) | `data.frame` | Export to CSV, database, or other tools |
| [`to_matrix()`](http://sonsoles.me/cograph/reference/to_matrix.md) | `matrix` | Export to adjacency matrix for other packages |
| [`to_network()`](http://sonsoles.me/cograph/reference/to_network.md) | `network` | Use with statnet/ergm ecosystem |

## Summary

| Input | Class | Directed | Weights | Labels |
|----|----|----|----|----|
| Matrix | `matrix` | Symmetry check | Cell values | dimnames |
| Edge list | `data.frame` | Reciprocal check | weight column | Unique nodes |
| igraph | `igraph` | [`is_directed()`](http://sonsoles.me/cograph/reference/is_directed.md) | weight attr | name attr |
| network | `network` | [`is.directed()`](https://r.igraph.org/reference/is.directed.html) | weight attr | vertex.names |
| qgraph | `qgraph` | Edgelist | Edgelist | Node names |
| tna | `tna` | Always TRUE | `$weights` | `$labels` |
