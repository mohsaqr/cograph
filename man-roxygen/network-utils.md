# Network Utility Functions

Simple utility functions for network manipulation and analysis in cograph.

## Naming Convention

All functions follow the `verb_noun()` pattern.

---

## Functions Overview

| Function | Purpose | Returns |
|----------|---------|---------|
| `detect_communities()` | Community detection | data frame |
| `color_communities()` | Colors for plotting | color vector |
| `filter_edges()` | Filter by weight | matrix |
| `filter_nodes()` | Filter by centrality | matrix |
| `to_data_frame()` | Export as edge list | data frame |
| `to_df()` | Alias for above | data frame |
| `to_igraph()` | Convert to igraph | igraph object |

---

## 1. `detect_communities()`

Detect communities in a network. Returns a data frame with node-community assignments.

### Usage

```r
detect_communities(x, method = "louvain", directed = NULL, weights = TRUE)
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | matrix, igraph, network, cograph_network, tna |
| `method` | `"louvain"` | Algorithm: `"louvain"`, `"walktrap"`, `"fast_greedy"`, `"label_prop"`, `"infomap"`, `"leiden"` |
| `directed` | `NULL` | Auto-detect or force TRUE/FALSE |
| `weights` | `TRUE` | Use edge weights |

### Returns

Data frame with columns `node`, `community`

### Example

```r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

detect_communities(adj)
#>   node community
#> 1    A         1
#> 2    B         2
#> 3    C         1
#> 4    D         2

# Different algorithm
detect_communities(adj, method = "walktrap")
```

---

## 2. `color_communities()`

Generate colors for nodes based on community membership. Designed for direct use with `splot()`.

### Usage

```r
color_communities(x, method = "louvain", palette = NULL, ...)
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | Network input |
| `method` | `"louvain"` | Community detection method |
| `palette` | `NULL` | Color palette (NULL = colorblind-friendly) |
| `...` | | Additional arguments passed to `detect_communities()` |

### Palette Options

- `NULL` (default): Colorblind-friendly palette
- Character vector of colors: `c("red", "blue", "green")`
- Palette name: `"rainbow"`, `"colorblind"`, `"pastel"`, `"viridis"`
- Function that takes n and returns n colors

### Returns

Named character vector of colors (one per node)

### Example

```r
# Basic usage with splot
splot(adj, node_fill = color_communities(adj))

# Custom palette
splot(adj, node_fill = color_communities(adj, palette = c("red", "blue")))

# Named palette
splot(adj, node_fill = color_communities(adj, palette = "viridis"))
```

---

## 3. `filter_edges()`

Filter edges using a logical expression on edge weights. Returns a matrix ready for `splot()`.

### Usage

```r
filter_edges(x, expr, directed = NULL)
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | Network input |
| `expr` | required | Logical expression using `weight` |
| `directed` | `NULL` | Auto-detect |

### Available Variables in Expression

- `weight`: Edge weight values

### Returns

Adjacency matrix with non-matching edges set to 0

### Example

```r
# Keep only strong edges
splot(filter_edges(adj, weight > 0.5))

# Keep edges above mean weight
splot(filter_edges(adj, weight >= mean(weight)))

# Combine with color_communities
filtered <- filter_edges(adj, weight > 0.3)
splot(filtered, node_fill = color_communities(filtered))
```

---

## 4. `filter_nodes()`

Filter nodes by centrality measures. Returns a subgraph matrix ready for `splot()`.

### Usage

```r
filter_nodes(x, expr, directed = NULL)
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | Network input |
| `expr` | required | Logical expression using centrality names |
| `directed` | `NULL` | Auto-detect |

### Available Variables in Expression

| Variable | Description |
|----------|-------------|
| `degree` | Total degree |
| `indegree` | In-degree (directed) |
| `outdegree` | Out-degree (directed) |
| `strength` | Weighted degree |
| `instrength` | Weighted in-degree |
| `outstrength` | Weighted out-degree |
| `betweenness` | Betweenness centrality |
| `closeness` | Closeness centrality |
| `eigenvector` | Eigenvector centrality |
| `pagerank` | PageRank |
| `hub` | HITS hub score |
| `authority` | HITS authority score |

### Returns

Subgraph adjacency matrix containing only matching nodes

### Example

```r
# Keep only high-degree nodes
splot(filter_nodes(adj, degree >= 3))

# Keep nodes with high PageRank
splot(filter_nodes(adj, pagerank > 0.2))

# Directed network filters
splot(filter_nodes(adj, indegree > 2 & outdegree > 1))

# Combine with color_communities
hubs <- filter_nodes(adj, degree >= 3)
splot(hubs, node_fill = color_communities(hubs))
```

---

## 5. `to_data_frame()` / `to_df()`

Export network as an edge list data frame.

### Usage

```r
to_data_frame(x, directed = NULL)
to_df(x, directed = NULL)  # alias
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | Network input |
| `directed` | `NULL` | Auto-detect |

### Returns

Data frame with columns `from`, `to`, `weight`

### Example

```r
to_data_frame(adj)
#>   from to weight
#> 1    A  B    0.5
#> 2    A  C    0.8
#> 3    B  C    0.3
#> 4    B  D    0.6
#> 5    C  D    0.4

# Alias works the same
to_df(adj)
```

---

## 6. `to_igraph()`

Convert various network formats to an igraph object.

### Usage

```r
to_igraph(x, directed = NULL)
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `x` | required | matrix, igraph, network, cograph_network, tna |
| `directed` | `NULL` | Auto-detect or force TRUE/FALSE |

### Returns

An igraph object

### Example

```r
# From matrix
g <- to_igraph(adj)

# Force directed
g_dir <- to_igraph(adj, directed = TRUE)
```

---

## Combined Workflow Examples

### Color nodes by community

```r
splot(adj, node_fill = color_communities(adj))
```

### Plot only strong edges

```r
splot(filter_edges(adj, weight > 0.5))
```

### Plot only high-degree nodes

```r
splot(filter_nodes(adj, degree >= 3))
```

### Combine filters with community colors

```r
filtered <- filter_edges(adj, weight > 0.3)
splot(filtered, node_fill = color_communities(filtered))
```

### Full analysis workflow

```r
# Create network
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Detect communities
detect_communities(adj)

# Plot with community colors
splot(adj, node_fill = color_communities(adj))

# Filter and plot
splot(filter_edges(adj, weight > 0.5))
splot(filter_nodes(adj, degree >= 3))

# Export
to_data_frame(adj)
to_igraph(adj)
```
