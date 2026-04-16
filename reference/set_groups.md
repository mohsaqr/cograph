# Set Node Groups

Assigns node groupings to a cograph_network object. Groups are stored as
metadata with a type column ("layer", "cluster", or "group") for use by
specialized plot functions.

## Usage

``` r
set_groups(
  x,
  groups = NULL,
  type = c("group", "cluster", "layer"),
  nodes = NULL,
  layers = NULL,
  clusters = NULL
)
```

## Arguments

- x:

  A cograph_network object.

- groups:

  Node groupings in one of these formats:

  - Character string: Community detection method ("louvain", "walktrap",
    "fast_greedy", "label_prop", "infomap", "leiden")

  - Named list: Group name -\> node vector mapping (e.g.,
    `list(A = c("N1","N2"), B = c("N3","N4"))`)

  - Unnamed vector: Group assignment per node (same order as nodes)

  - Data frame: Must have "node"/"nodes" column plus one of
    "layer"/"layers", "cluster"/"clusters", or "group"/"groups" (plural
    forms are automatically normalized to singular)

  - NULL: Use `nodes` + one of `layers`/`clusters`/`groups` vectors

- type:

  Group type. One of `"group"` (default), `"cluster"`, or `"layer"`.
  Ignored when using vector arguments (`layers`, `clusters`, `groups`)
  since the type is inferred from which argument is provided.

- nodes:

  Character vector of node labels. Use with `layers`, `clusters`, or
  `groups` to specify groupings via vectors instead of a data frame.

- layers:

  Character/factor vector of layer assignments (same length as `nodes`).

- clusters:

  Character/factor vector of cluster assignments (same length as
  `nodes`).

## Value

The modified cograph_network object with `node_groups` set.

## See also

[`get_groups`](https://sonsoles.me/cograph/reference/get_groups.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)

## Examples

``` r
# Create network (symmetric for community detection)
mat <- matrix(runif(100), 10, 10)
mat <- (mat + t(mat)) / 2  # Make symmetric (undirected)
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- paste0("N", 1:10)
net <- as_cograph(mat)

# Using vectors (recommended)
net <- set_groups(net,
  nodes = paste0("N", 1:10),
  layers = c(rep("Macro", 3), rep("Meso", 4), rep("Micro", 3))
)

# Named list -> layers
net <- set_groups(net, list(
  Macro = paste0("N", 1:3),
  Meso = paste0("N", 4:7),
  Micro = paste0("N", 8:10)
), type = "layer")

# Vector -> clusters
net <- set_groups(net, c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C"),
                  type = "cluster")

# Community detection -> groups
net <- set_groups(net, "louvain", type = "group")

# Data frame with explicit columns
df <- data.frame(nodes = paste0("N", 1:10),
                 layers = rep(c("Top", "Bottom"), each = 5))
net <- set_groups(net, df)

# Check groups
get_groups(net)
#>    node  layer
#> 1    N1    Top
#> 2    N2    Top
#> 3    N3    Top
#> 4    N4    Top
#> 5    N5    Top
#> 6    N6 Bottom
#> 7    N7 Bottom
#> 8    N8 Bottom
#> 9    N9 Bottom
#> 10  N10 Bottom
```
