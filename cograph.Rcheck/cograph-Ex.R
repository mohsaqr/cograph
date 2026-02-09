pkgname <- "cograph"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('cograph')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CographLayout")
### * CographLayout

flush(stderr()); flush(stdout())

### Name: CographLayout
### Title: CographLayout R6 Class
### Aliases: CographLayout

### ** Examples

# Create a circular layout
layout <- CographLayout$new("circle")

# Apply to network
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout$compute(net)



cleanEx()
nameEx("CographNetwork")
### * CographNetwork

flush(stderr()); flush(stdout())

### Name: CographNetwork
### Title: CographNetwork R6 Class
### Aliases: CographNetwork

### ** Examples

# Create network from adjacency matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)

# Access properties
net$n_nodes
net$n_edges
net$is_directed



cleanEx()
nameEx("CographTheme")
### * CographTheme

flush(stderr()); flush(stdout())

### Name: CographTheme
### Title: CographTheme R6 Class
### Aliases: CographTheme

### ** Examples

# Create a custom theme
theme <- CographTheme$new(
  background = "white",
  node_fill = "steelblue",
  edge_color = "gray60"
)



cleanEx()
nameEx("aggregate_duplicate_edges")
### * aggregate_duplicate_edges

flush(stderr()); flush(stdout())

### Name: aggregate_duplicate_edges
### Title: Aggregate Duplicate Edges
### Aliases: aggregate_duplicate_edges
### Keywords: internal

### ** Examples

## Not run: 
##D # Create edges with duplicates
##D edges <- data.frame(
##D   from = c(1, 1, 2),
##D   to = c(2, 2, 3),
##D   weight = c(0.5, 0.3, 0.4)
##D )
##D 
##D # Aggregate by sum (0.5 + 0.3 = 0.8)
##D aggregate_duplicate_edges(edges, method = "sum")
##D #   from to weight
##D # 1    1  2    0.8
##D # 2    2  3    0.4
##D 
##D # Aggregate by mean (average: 0.4)
##D aggregate_duplicate_edges(edges, method = "mean")
##D #   from to weight
##D # 1    1  2    0.4
##D # 2    2  3    0.4
##D 
##D # Use custom aggregation function
##D aggregate_duplicate_edges(edges, method = function(x) sqrt(sum(x^2)))
## End(Not run)




cleanEx()
nameEx("aggregate_layers")
### * aggregate_layers

flush(stderr()); flush(stdout())

### Name: aggregate_layers
### Title: Aggregate Layers
### Aliases: aggregate_layers lagg

### ** Examples

# layers <- list(L1 = mat1, L2 = mat2, L3 = mat3)
# aggregate_layers(layers, "sum")           # Total
# aggregate_layers(layers, "mean")          # Average
# aggregate_layers(layers, "union")         # Any edge
# aggregate_layers(layers, "intersection")  # All edges



cleanEx()
nameEx("aggregate_weights")
### * aggregate_weights

flush(stderr()); flush(stdout())

### Name: aggregate_weights
### Title: Aggregate Edge Weights
### Aliases: aggregate_weights wagg

### ** Examples

w <- c(0.5, 0.8, 0.3, 0.9)
aggregate_weights(w, "sum")   # 2.5
aggregate_weights(w, "mean")  # 0.625
aggregate_weights(w, "max")   # 0.9



cleanEx()
nameEx("as_cograph")
### * as_cograph

flush(stderr()); flush(stdout())

### Name: as_cograph
### Title: Convert to Cograph Network
### Aliases: as_cograph to_cograph

### ** Examples

# From adjacency matrix
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)

# Direct $ access to all data
net$from       # edge sources
net$to         # edge targets
net$weight     # edge weights
net$nodes      # nodes data frame
net$directed   # TRUE/FALSE
net$n_nodes    # 3
net$n_edges    # 3

# Getter functions (recommended for programmatic use)
get_nodes(net)   # nodes data frame
get_edges(net)   # edges data frame (from, to, weight)
get_labels(net)  # character vector of labels
n_nodes(net)     # 3
n_edges(net)     # 3
is_directed(net) # FALSE (symmetric matrix)

# Setter functions
net <- set_nodes(net, data.frame(id = 1:3, label = c("A", "B", "C")))
net <- set_edges(net, data.frame(from = c(1,2), to = c(2,3), weight = c(0.5, 0.8)))
net <- set_layout(net, data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))

# Plot it
splot(net)

# From igraph (if installed)
## Not run: 
##D library(igraph)
##D g <- make_ring(10)
##D net <- as_cograph(g)
##D splot(net)
## End(Not run)



cleanEx()
nameEx("cluster_quality")
### * cluster_quality

flush(stderr()); flush(stdout())

### Name: cluster_quality
### Title: Cluster Quality Metrics
### Aliases: cluster_quality cqual

### ** Examples

mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
clusters <- c(1,1,1,2,2,2,3,3,3,3)

q <- cluster_quality(mat, clusters)
q$per_cluster   # Per-cluster metrics
q$global        # Modularity, coverage



cleanEx()
nameEx("cluster_summary")
### * cluster_summary

flush(stderr()); flush(stdout())

### Name: cluster_summary
### Title: Cluster Summary Statistics
### Aliases: cluster_summary csum

### ** Examples

# Create adjacency matrix
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- LETTERS[1:10]

# Define clusters
clusters <- list(
  G1 = c("A", "B", "C"),
  G2 = c("D", "E", "F"),
  G3 = c("G", "H", "I", "J")
)

# Compute summary
result <- cluster_summary(mat, clusters)
result$between  # Between-cluster matrix
result$within   # Within-cluster values



cleanEx()
nameEx("cograph")
### * cograph

flush(stderr()); flush(stdout())

### Name: cograph
### Title: Create a Network Visualization
### Aliases: cograph

### ** Examples

# From adjacency matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
cograph(adj)

# From edge list
edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3))
cograph(edges)

# With customization (pipe-friendly workflow)
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
cograph(adj, layout = "circle") |>
  sn_nodes(fill = "steelblue") |>
  sn_edges(color = "gray50") |>
  splot()

# Weighted network with automatic styling
w_adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.4, -0.3, 0.4, 0), nrow = 3)
cograph(w_adj) |>
  sn_edges(color = "weight", width = "weight") |>
  splot()

# With igraph (if installed)
## Not run: 
##D library(igraph)
##D g <- make_ring(10)
##D cograph(g) |> splot()
## End(Not run)



cleanEx()
nameEx("color_communities")
### * color_communities

flush(stderr()); flush(stdout())

### Name: color_communities
### Title: Color Nodes by Community
### Aliases: color_communities

### ** Examples

adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Basic usage with splot
splot(adj, node_fill = color_communities(adj))

# Custom palette
splot(adj, node_fill = color_communities(adj, palette = c("red", "blue")))



cleanEx()
nameEx("detect_communities")
### * detect_communities

flush(stderr()); flush(stdout())

### Name: detect_communities
### Title: Detect Communities in a Network
### Aliases: detect_communities

### ** Examples

# Basic usage
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
detect_communities(adj)

# Different algorithm
detect_communities(adj, method = "walktrap")



cleanEx()
nameEx("detect_duplicate_edges")
### * detect_duplicate_edges

flush(stderr()); flush(stdout())

### Name: detect_duplicate_edges
### Title: Detect Duplicate Edges in Undirected Network
### Aliases: detect_duplicate_edges
### Keywords: internal

### ** Examples

## Not run: 
##D # Create edges with duplicates
##D edges <- data.frame(
##D   from = c(1, 1, 2, 2, 3),
##D   to = c(2, 2, 3, 1, 1),
##D   weight = c(0.5, 0.3, 0.4, 0.6, 0.2)
##D )
##D 
##D # Detect duplicates (undirected: 1-2 appears 3 times, 1-3 appears 2 times)
##D result <- detect_duplicate_edges(edges)
##D result$has_duplicates
##D # [1] TRUE
##D 
##D # View duplicate details
##D result$info[[1]]
##D # $nodes: 1, 2
##D # $count: 3
##D # $weights: 0.5, 0.3, 0.6
## End(Not run)




cleanEx()
nameEx("filter_edges")
### * filter_edges

flush(stderr()); flush(stdout())

### Name: filter_edges
### Title: Filter Edges by Weight
### Aliases: filter_edges

### ** Examples

adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Keep only strong edges
splot(filter_edges(adj, weight > 0.5))

# Keep edges above mean weight
splot(filter_edges(adj, weight >= mean(weight)))

# Combine with color_communities
filtered <- filter_edges(adj, weight > 0.3)
splot(filtered, node_fill = color_communities(filtered))



cleanEx()
nameEx("filter_nodes")
### * filter_nodes

flush(stderr()); flush(stdout())

### Name: filter_nodes
### Title: Filter Nodes by Centrality
### Aliases: filter_nodes

### ** Examples

adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Keep only high-degree nodes
splot(filter_nodes(adj, degree >= 3))

# Keep nodes with high PageRank
splot(filter_nodes(adj, pagerank > 0.2))

# Combine with color_communities
hubs <- filter_nodes(adj, degree >= 3)
splot(hubs, node_fill = color_communities(hubs))



cleanEx()
nameEx("from_qgraph")
### * from_qgraph

flush(stderr()); flush(stdout())

### Name: from_qgraph
### Title: Convert a qgraph object to cograph parameters
### Aliases: from_qgraph

### ** Examples

## Not run: 
##D # Convert and plot a qgraph object
##D library(qgraph)
##D adj <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
##D q <- qgraph(adj)
##D from_qgraph(q)  # Plots with splot
##D 
##D # Use soplot engine instead
##D from_qgraph(q, engine = "soplot")
##D 
##D # Override extracted parameters
##D from_qgraph(q, node_fill = "steelblue", layout = "circle")
##D 
##D # Extract parameters without plotting
##D params <- from_qgraph(q, plot = FALSE)
##D names(params)  # See what was extracted
##D 
##D # Works with themed qgraph objects
##D q_themed <- qgraph(adj, theme = "colorblind", posCol = "blue")
##D from_qgraph(q_themed)
## End(Not run)




cleanEx()
nameEx("from_tna")
### * from_tna

flush(stderr()); flush(stdout())

### Name: from_tna
### Title: Convert a tna object to cograph parameters
### Aliases: from_tna

### ** Examples

## Not run: 
##D # Convert and plot a tna object
##D library(tna)
##D trans <- tna(transitions)
##D from_tna(trans)  # Plots with donut rings showing initial probabilities
##D 
##D # Use soplot engine instead
##D from_tna(trans, engine = "soplot")
##D 
##D # Customize the visualization
##D from_tna(trans, layout = "circle", donut_color = c("steelblue", "gray90"))
##D 
##D # Extract parameters without plotting
##D params <- from_tna(trans, plot = FALSE)
##D # Modify and plot manually
##D params$node_fill <- "coral"
##D do.call(splot, params)
## End(Not run)




cleanEx()
nameEx("get_edges")
### * get_edges

flush(stderr()); flush(stdout())

### Name: get_edges
### Title: Get Edges from Cograph Network
### Aliases: get_edges

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_edges(net)



cleanEx()
nameEx("get_groups")
### * get_groups

flush(stderr()); flush(stdout())

### Name: get_groups
### Title: Get Node Groups from Cograph Network
### Aliases: get_groups

### ** Examples

mat <- matrix(runif(25), 5, 5)
rownames(mat) <- colnames(mat) <- LETTERS[1:5]
net <- as_cograph(mat)
net <- set_groups(net, list(G1 = c("A", "B"), G2 = c("C", "D", "E")))
get_groups(net)



cleanEx()
nameEx("get_labels")
### * get_labels

flush(stderr()); flush(stdout())

### Name: get_labels
### Title: Get Labels from Cograph Network
### Aliases: get_labels

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_labels(net)



cleanEx()
nameEx("get_layout")
### * get_layout

flush(stderr()); flush(stdout())

### Name: get_layout
### Title: Get a Registered Layout
### Aliases: get_layout

### ** Examples

get_layout("circle")



cleanEx()
nameEx("get_nodes")
### * get_nodes

flush(stderr()); flush(stdout())

### Name: get_nodes
### Title: Get Nodes from Cograph Network
### Aliases: get_nodes

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
get_nodes(net)



cleanEx()
nameEx("get_shape")
### * get_shape

flush(stderr()); flush(stdout())

### Name: get_shape
### Title: Get a Registered Shape
### Aliases: get_shape

### ** Examples

get_shape("circle")



cleanEx()
nameEx("get_theme")
### * get_theme

flush(stderr()); flush(stdout())

### Name: get_theme
### Title: Get a Registered Theme
### Aliases: get_theme

### ** Examples

get_theme("classic")



cleanEx()
nameEx("get_tna_model")
### * get_tna_model

flush(stderr()); flush(stdout())

### Name: get_tna_model
### Title: Get Original TNA Model
### Aliases: get_tna_model

### ** Examples

## Not run: 
##D library(tna)
##D model <- tna(group_regulation)
##D net <- as_cograph(model)
##D 
##D # Get original model
##D original <- get_tna_model(net)
##D class(original)
##D #> "tna"
##D 
##D # Access TNA fields
##D original$weights   # transition matrix
##D original$inits     # initial probabilities
##D original$labels    # state names
##D original$data      # sequence data
##D 
##D # Access attributes
##D attr(original, "type")              # "relative"
##D attr(original$data, "colors")       # state colors
## End(Not run)



cleanEx()
nameEx("is_directed")
### * is_directed

flush(stderr()); flush(stdout())

### Name: is_directed
### Title: Check if Network is Directed
### Aliases: is_directed

### ** Examples

# Symmetric matrix -> undirected
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
is_directed(net)  # FALSE

# Asymmetric matrix -> directed
mat2 <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
net2 <- as_cograph(mat2)
is_directed(net2)  # TRUE



cleanEx()
nameEx("is_tna_network")
### * is_tna_network

flush(stderr()); flush(stdout())

### Name: is_tna_network
### Title: Check if Network is TNA-based
### Aliases: is_tna_network

### ** Examples

## Not run: 
##D library(tna)
##D model <- tna(group_regulation)
##D net <- as_cograph(model)
##D is_tna_network(net)
##D #> TRUE
##D 
##D # Non-TNA network
##D mat <- matrix(runif(25), 5, 5)
##D net2 <- as_cograph(mat)
##D is_tna_network(net2)
##D #> FALSE
## End(Not run)



cleanEx()
nameEx("layer_similarity")
### * layer_similarity

flush(stderr()); flush(stdout())

### Name: layer_similarity
### Title: Layer Similarity
### Aliases: layer_similarity lsim

### ** Examples

A1 <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
A2 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)

layer_similarity(A1, A2, "jaccard")  # Edge overlap
layer_similarity(A1, A2, "cosine")   # Weight similarity



cleanEx()
nameEx("layer_similarity_matrix")
### * layer_similarity_matrix

flush(stderr()); flush(stdout())

### Name: layer_similarity_matrix
### Title: Pairwise Layer Similarities
### Aliases: layer_similarity_matrix lsim_matrix

### ** Examples

# layers <- list(T1 = mat1, T2 = mat2, T3 = mat3)
# layer_similarity_matrix(layers, "cosine")



cleanEx()
nameEx("layout_circle")
### * layout_circle

flush(stderr()); flush(stdout())

### Name: layout_circle
### Title: Circular Layout
### Aliases: layout_circle

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout_circle(net)



cleanEx()
nameEx("layout_gephi_fr")
### * layout_gephi_fr

flush(stderr()); flush(stdout())

### Name: layout_gephi_fr
### Title: Gephi Fruchterman-Reingold Layout
### Aliases: layout_gephi_fr
### Keywords: internal

### ** Examples

## Not run: 
##D library(igraph)
##D g <- make_ring(10)
##D coords <- layout_gephi_fr(g)
##D plot(g, layout = coords)
## End(Not run)




cleanEx()
nameEx("layout_groups")
### * layout_groups

flush(stderr()); flush(stdout())

### Name: layout_groups
### Title: Group-based Layout
### Aliases: layout_groups

### ** Examples

# Create a network with groups
adj <- matrix(0, 9, 9)
adj[1, 2:3] <- 1; adj[2:3, 1] <- 1  # Group 1
adj[4, 5:6] <- 1; adj[5:6, 4] <- 1  # Group 2
adj[7, 8:9] <- 1; adj[8:9, 7] <- 1  # Group 3
net <- CographNetwork$new(adj)
groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
coords <- layout_groups(net, groups)



cleanEx()
nameEx("layout_oval")
### * layout_oval

flush(stderr()); flush(stdout())

### Name: layout_oval
### Title: Oval Layout
### Aliases: layout_oval

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- CographNetwork$new(adj)
coords <- layout_oval(net, ratio = 1.5)



cleanEx()
nameEx("layout_spring")
### * layout_spring

flush(stderr()); flush(stdout())

### Name: layout_spring
### Title: Fruchterman-Reingold Spring Layout
### Aliases: layout_spring

### ** Examples

adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)
net <- CographNetwork$new(adj)
coords <- layout_spring(net, seed = 42)

# For animations: use previous layout as initial with constraints
coords2 <- layout_spring(net, initial = coords, max_displacement = 0.05)



cleanEx()
nameEx("list_layouts")
### * list_layouts

flush(stderr()); flush(stdout())

### Name: list_layouts
### Title: List Available Layouts
### Aliases: list_layouts

### ** Examples

list_layouts()



cleanEx()
nameEx("list_palettes")
### * list_palettes

flush(stderr()); flush(stdout())

### Name: list_palettes
### Title: List Available Color Palettes
### Aliases: list_palettes

### ** Examples

list_palettes()



cleanEx()
nameEx("list_shapes")
### * list_shapes

flush(stderr()); flush(stdout())

### Name: list_shapes
### Title: List Available Shapes
### Aliases: list_shapes

### ** Examples

list_shapes()



cleanEx()
nameEx("list_svg_shapes")
### * list_svg_shapes

flush(stderr()); flush(stdout())

### Name: list_svg_shapes
### Title: List Registered SVG Shapes
### Aliases: list_svg_shapes

### ** Examples

list_svg_shapes()



cleanEx()
nameEx("list_themes")
### * list_themes

flush(stderr()); flush(stdout())

### Name: list_themes
### Title: List Available Themes
### Aliases: list_themes

### ** Examples

list_themes()



cleanEx()
nameEx("n_edges")
### * n_edges

flush(stderr()); flush(stdout())

### Name: n_edges
### Title: Get Number of Edges
### Aliases: n_edges

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
n_edges(net)  # 3



cleanEx()
nameEx("n_nodes")
### * n_nodes

flush(stderr()); flush(stdout())

### Name: n_nodes
### Title: Get Number of Nodes
### Aliases: n_nodes

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
n_nodes(net)  # 3



cleanEx()
nameEx("nodes")
### * nodes

flush(stderr()); flush(stdout())

### Name: nodes
### Title: Get Nodes from Cograph Network (Deprecated)
### Aliases: nodes

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
nodes(net)  # Deprecated, use get_nodes(net) instead



cleanEx()
nameEx("palette_blues")
### * palette_blues

flush(stderr()); flush(stdout())

### Name: palette_blues
### Title: Blues Palette
### Aliases: palette_blues

### ** Examples

palette_blues(5)



cleanEx()
nameEx("palette_colorblind")
### * palette_colorblind

flush(stderr()); flush(stdout())

### Name: palette_colorblind
### Title: Colorblind-friendly Palette
### Aliases: palette_colorblind

### ** Examples

palette_colorblind(5)



cleanEx()
nameEx("palette_diverging")
### * palette_diverging

flush(stderr()); flush(stdout())

### Name: palette_diverging
### Title: Diverging Palette
### Aliases: palette_diverging

### ** Examples

palette_diverging(5)



cleanEx()
nameEx("palette_pastel")
### * palette_pastel

flush(stderr()); flush(stdout())

### Name: palette_pastel
### Title: Pastel Palette
### Aliases: palette_pastel

### ** Examples

palette_pastel(5)



cleanEx()
nameEx("palette_rainbow")
### * palette_rainbow

flush(stderr()); flush(stdout())

### Name: palette_rainbow
### Title: Rainbow Palette
### Aliases: palette_rainbow

### ** Examples

palette_rainbow(5)



cleanEx()
nameEx("palette_reds")
### * palette_reds

flush(stderr()); flush(stdout())

### Name: palette_reds
### Title: Reds Palette
### Aliases: palette_reds

### ** Examples

palette_reds(5)



cleanEx()
nameEx("palette_viridis")
### * palette_viridis

flush(stderr()); flush(stdout())

### Name: palette_viridis
### Title: Viridis Palette
### Aliases: palette_viridis

### ** Examples

palette_viridis(5)



cleanEx()
nameEx("plot.cograph_network")
### * plot.cograph_network

flush(stderr()); flush(stdout())

### Name: plot.cograph_network
### Title: Plot cograph_network Object
### Aliases: plot.cograph_network

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- cograph(adj)
plot(net)



cleanEx()
nameEx("plot_compare")
### * plot_compare

flush(stderr()); flush(stdout())

### Name: plot_compare
### Title: Plot Network Difference
### Aliases: plot_compare

### ** Examples

## Not run: 
##D # Compare two adjacency matrices
##D m1 <- matrix(runif(25), 5, 5)
##D m2 <- matrix(runif(25), 5, 5)
##D plot_compare(m1, m2)
##D 
##D # With node-level differences (e.g., initial probabilities)
##D inits1 <- c(0.3, 0.2, 0.2, 0.15, 0.15)
##D inits2 <- c(0.1, 0.4, 0.2, 0.2, 0.1)
##D plot_compare(m1, m2, inits_x = inits1, inits_y = inits2)
##D 
##D # Compare tna objects (auto-extracts inits)
##D # plot_compare(tna_model1, tna_model2)
##D 
##D # Compare group_tna (auto-detects, plots all pairs)
##D # plot_compare(group_tna_model)
##D # plot_compare(group_tna_model, i = 1, j = 2)  # specific pair
## End(Not run)




cleanEx()
nameEx("plot_comparison_heatmap")
### * plot_comparison_heatmap

flush(stderr()); flush(stdout())

### Name: plot_comparison_heatmap
### Title: Plot Comparison Heatmap
### Aliases: plot_comparison_heatmap

### ** Examples

## Not run: 
##D m1 <- matrix(runif(25), 5, 5)
##D m2 <- matrix(runif(25), 5, 5)
##D rownames(m1) <- colnames(m1) <- LETTERS[1:5]
##D rownames(m2) <- colnames(m2) <- LETTERS[1:5]
##D 
##D # Difference heatmap
##D plot_comparison_heatmap(m1, m2)
##D 
##D # Show just one network
##D plot_comparison_heatmap(m1, type = "x")
##D 
##D # With cell values
##D plot_comparison_heatmap(m1, m2, show_values = TRUE)
## End(Not run)




cleanEx()
nameEx("plot_heatmap")
### * plot_heatmap

flush(stderr()); flush(stdout())

### Name: plot_heatmap
### Title: Plot Network as Heatmap
### Aliases: plot_heatmap

### ** Examples

## Not run: 
##D # Single network
##D m <- matrix(runif(25), 5, 5)
##D rownames(m) <- colnames(m) <- LETTERS[1:5]
##D plot_heatmap(m)
##D 
##D # With clusters
##D clusters <- list(Group1 = c("A", "B"), Group2 = c("C", "D", "E"))
##D plot_heatmap(m, cluster_list = clusters)
##D 
##D # Multi-layer (group_tna)
##D plot_heatmap(group_tna_model)
##D 
##D # Custom colors and legend
##D plot_heatmap(m, colors = "heat", limits = c(0, 1), show_values = TRUE)
## End(Not run)




cleanEx()
nameEx("plot_htna")
### * plot_htna

flush(stderr()); flush(stdout())

### Name: plot_htna
### Title: Plot Heterogeneous TNA Network (Multi-Group Layout)
### Aliases: plot_htna htna

### ** Examples

## Not run: 
##D # Define node groups (2 groups - bipartite)
##D node_types <- list(
##D   Student = c("Wrong", "Retry", "Right", "Attempt", "Instruction", "Skip"),
##D   AI = c("Order", "Correct", "Hint", "Quit", "Clarify", "Question", "Praise")
##D )
##D 
##D # Basic bipartite plot
##D plot_htna(model, node_types)
##D 
##D # With custom jitter
##D plot_htna(model, node_types, jitter_amount = 0.5)
##D 
##D # Triangle layout (3 groups)
##D node_types_3 <- list(
##D   Teacher = c("Explain", "Question", "Feedback"),
##D   Student = c("Answer", "Ask", "Attempt"),
##D   System = c("Hint", "Score", "Progress")
##D )
##D plot_htna(model, node_types_3)  # Auto-detects triangle
##D 
##D # Rectangle layout (4 groups)
##D node_types_4 <- list(
##D   Input = c("Click", "Type", "Scroll"),
##D   Process = c("Validate", "Transform"),
##D   Output = c("Display", "Alert"),
##D   Storage = c("Save", "Load", "Cache")
##D )
##D plot_htna(model, node_types_4)  # Auto-detects rectangle
##D 
##D # Explicit layout selection
##D plot_htna(model, node_types_3, layout = "triangle")
## End(Not run)



cleanEx()
nameEx("plot_mlna")
### * plot_mlna

flush(stderr()); flush(stdout())

### Name: plot_mlna
### Title: Multilevel Network Visualization
### Aliases: plot_mlna mlna

### ** Examples

## Not run: 
##D # Create multilevel network
##D set.seed(42)
##D nodes <- paste0("N", 1:15)
##D m <- matrix(runif(225, 0, 0.3), 15, 15)
##D diag(m) <- 0
##D colnames(m) <- rownames(m) <- nodes
##D 
##D # Define 3 layers
##D layers <- list(
##D   Macro = paste0("N", 1:5),
##D   Meso = paste0("N", 6:10),
##D   Micro = paste0("N", 11:15)
##D )
##D 
##D # Basic usage
##D plot_mlna(m, layers)
##D 
##D # Customized
##D plot_mlna(m, layers,
##D      layer_spacing = 2.5,
##D      layer_width = 5,
##D      between_style = 2,  # dashed
##D      minimum = 0.1)
##D 
##D # Circle layout within layers
##D plot_mlna(m, layers, layout = "circle")
## End(Not run)



cleanEx()
nameEx("plot_mtna")
### * plot_mtna

flush(stderr()); flush(stdout())

### Name: plot_mtna
### Title: Multi-Cluster TNA Network Plot
### Aliases: plot_mtna mtna

### ** Examples

## Not run: 
##D # Create network with 4 clusters
##D nodes <- paste0("N", 1:20)
##D m <- matrix(runif(400, 0, 0.3), 20, 20)
##D diag(m) <- 0
##D colnames(m) <- rownames(m) <- nodes
##D 
##D clusters <- list(
##D   North = paste0("N", 1:5),
##D   East = paste0("N", 6:10),
##D   South = paste0("N", 11:15),
##D   West = paste0("N", 16:20)
##D )
##D 
##D # Summary edges between clusters + individual edges within
##D plot_mtna(m, clusters, summary_edges = TRUE)
##D 
##D # Control spacing and sizes
##D plot_mtna(m, clusters, spacing = 4, shape_size = 1.5, node_spacing = 0.6)
## End(Not run)



cleanEx()
nameEx("plot_tna")
### * plot_tna

flush(stderr()); flush(stdout())

### Name: plot_tna
### Title: TNA-Style Network Plot (qgraph Compatible)
### Aliases: plot_tna tplot

### ** Examples

# Simple usage
m <- matrix(runif(25), 5, 5)
plot_tna(m)

# With qgraph-style parameters
plot_tna(m, vsize = 15, edge.label.cex = 2, layout = "circle")

# With custom colors
plot_tna(m, color = rainbow(5), vsize = 10)




cleanEx()
nameEx("register_layout")
### * register_layout

flush(stderr()); flush(stdout())

### Name: register_layout
### Title: Register a Custom Layout
### Aliases: register_layout

### ** Examples

# Register a simple random layout
register_layout("random", function(network, ...) {
  n <- network$n_nodes
  cbind(x = runif(n), y = runif(n))
})



cleanEx()
nameEx("register_shape")
### * register_shape

flush(stderr()); flush(stdout())

### Name: register_shape
### Title: Register a Custom Shape
### Aliases: register_shape

### ** Examples

# Register a custom hexagon shape
register_shape("hexagon", function(x, y, size, fill, border_color, border_width, ...) {
  angles <- seq(0, 2 * pi, length.out = 7)
  grid::polygonGrob(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
  )
})



cleanEx()
nameEx("register_svg_shape")
### * register_svg_shape

flush(stderr()); flush(stdout())

### Name: register_svg_shape
### Title: Register Custom SVG Shape
### Aliases: register_svg_shape

### ** Examples

## Not run: 
##D # Register from file
##D register_svg_shape("custom_icon", "path/to/icon.svg")
##D 
##D # Register from inline SVG
##D register_svg_shape("simple_star",
##D   '<svg viewBox="0 0 100 100">
##D     <polygon points="50,5 20,99 95,39 5,39 80,99" fill="currentColor"/>
##D   </svg>')
##D 
##D # Use in network
##D cograph(adj) |> sn_nodes(shape = "custom_icon")
## End(Not run)



cleanEx()
nameEx("register_theme")
### * register_theme

flush(stderr()); flush(stdout())

### Name: register_theme
### Title: Register a Custom Theme
### Aliases: register_theme

### ** Examples

# Register a custom theme
register_theme("custom", list(
  background = "white",
  node_fill = "steelblue",
  node_border = "navy",
  edge_color = "gray50"
))



cleanEx()
nameEx("scale_edge_widths")
### * scale_edge_widths

flush(stderr()); flush(stdout())

### Name: scale_edge_widths
### Title: Scale Edge Widths Based on Weights
### Aliases: scale_edge_widths
### Keywords: internal

### ** Examples

## Not run: 
##D weights <- c(0.1, 0.3, 0.5, 0.8, 1.0)
##D 
##D # Linear scaling (default)
##D scale_edge_widths(weights, mode = "linear")
##D 
##D # Log scaling for wide ranges
##D scale_edge_widths(c(0.01, 0.1, 1, 10, 100), mode = "log")
##D 
##D # With two-tier cut
##D scale_edge_widths(weights, cut = 0.5)
##D 
##D # Rank-based (equal visual spacing)
##D scale_edge_widths(weights, mode = "rank", cut = 0)
## End(Not run)




cleanEx()
nameEx("set_edges")
### * set_edges

flush(stderr()); flush(stdout())

### Name: set_edges
### Title: Set Edges in Cograph Network
### Aliases: set_edges

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
net <- set_edges(net, new_edges)
get_edges(net)



cleanEx()
nameEx("set_groups")
### * set_groups

flush(stderr()); flush(stdout())

### Name: set_groups
### Title: Set Node Groups for Auto-Plot Selection
### Aliases: set_groups

### ** Examples

# Create network
mat <- matrix(runif(100), 10, 10)
rownames(mat) <- colnames(mat) <- paste0("N", 1:10)
net <- as_cograph(mat)

# Using vectors (recommended)
net <- set_groups(net,
  nodes = paste0("N", 1:10),
  layers = c(rep("Macro", 3), rep("Meso", 4), rep("Micro", 3))
)

# Named list -> layers (for plot_mlna)
net <- set_groups(net, list(
  Macro = paste0("N", 1:3),
  Meso = paste0("N", 4:7),
  Micro = paste0("N", 8:10)
), type = "layer")

# Vector -> clusters (for plot_mtna)
net <- set_groups(net, c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C"),
                  type = "cluster")

# Community detection -> groups (for plot_htna)
net <- set_groups(net, "louvain", type = "group")

# Data frame with explicit columns
df <- data.frame(nodes = paste0("N", 1:10),
                 layers = rep(c("Top", "Bottom"), each = 5))
net <- set_groups(net, df)

# Check groups
get_groups(net)



cleanEx()
nameEx("set_layout")
### * set_layout

flush(stderr()); flush(stdout())

### Name: set_layout
### Title: Set Layout in Cograph Network
### Aliases: set_layout

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
layout <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
net <- set_layout(net, layout)
get_nodes(net)



cleanEx()
nameEx("set_nodes")
### * set_nodes

flush(stderr()); flush(stdout())

### Name: set_nodes
### Title: Set Nodes in Cograph Network
### Aliases: set_nodes

### ** Examples

mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
new_nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
net <- set_nodes(net, new_nodes)
get_labels(net)



cleanEx()
nameEx("sn_edges")
### * sn_edges

flush(stderr()); flush(stdout())

### Name: sn_edges
### Title: Set Edge Aesthetics
### Aliases: sn_edges

### ** Examples

adj <- matrix(c(0, 1, -0.5, 1, 0, 1, -0.5, 1, 0), nrow = 3)

# Basic: auto-style by weight
cograph(adj) |>
  sn_edges(width = "weight", color = "weight")

# Direct matrix input (auto-converted)
adj |> sn_edges(width = 2, color = "gray50")

# Custom positive/negative colors
cograph(adj) |>
  sn_edges(
    color = "weight",
    edge_positive_color = "darkblue",
    edge_negative_color = "darkred"
  ) |>
  splot()

# Edge labels showing weights
cograph(adj) |>
  sn_edges(labels = TRUE, label_size = 0.8) |>
  splot()

# Statistical output with CI template
# Suppose we have estimates, lower/upper CI bounds
estimates <- c(0.5, -0.3, 0.8)
ci_lo <- c(0.2, -0.6, 0.5)
ci_hi <- c(0.8, -0.1, 1.1)

## Not run: 
##D cograph(adj) |>
##D   sn_edges(
##D     label_template = "{est} [{low}, {up}]",
##D     ci_lower = ci_lo,
##D     ci_upper = ci_hi,
##D     label_digits = 2
##D   ) |>
##D   splot()
## End(Not run)

# Curved edges for reciprocal pairs
cograph(adj) |>
  sn_edges(curves = "mutual", curvature = 0.3) |>
  splot()



cleanEx()
nameEx("sn_ggplot")
### * sn_ggplot

flush(stderr()); flush(stdout())

### Name: sn_ggplot
### Title: Convert Network to ggplot2
### Aliases: sn_ggplot

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
# With cograph()
p <- cograph(adj) |> sn_ggplot()
print(p)

# Direct matrix input
p <- adj |> sn_ggplot()

# Further customization
p + ggplot2::labs(title = "My Network")



cleanEx()
nameEx("sn_layout")
### * sn_layout

flush(stderr()); flush(stdout())

### Name: sn_layout
### Title: Apply Layout to Network
### Aliases: sn_layout

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Built-in layouts
cograph(adj) |> sn_layout("circle") |> splot()
cograph(adj) |> sn_layout("spring") |> splot()

# igraph layouts (if igraph installed)
## Not run: 
##D cograph(adj) |> sn_layout("kk") |> splot()
##D cograph(adj) |> sn_layout("fr") |> splot()
## End(Not run)

# Custom coordinates
coords <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
cograph(adj) |> sn_layout(coords) |> splot()

# Direct matrix input (auto-converts)
adj |> sn_layout("circle")



cleanEx()
nameEx("sn_nodes")
### * sn_nodes

flush(stderr()); flush(stdout())

### Name: sn_nodes
### Title: Set Node Aesthetics
### Aliases: sn_nodes

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Basic usage with cograph()
cograph(adj) |>
  sn_nodes(size = 0.08, fill = "steelblue", shape = "circle")

# Direct matrix input (auto-converted)
adj |> sn_nodes(fill = "coral", size = 0.1)

# Per-node customization with vectors
cograph(adj) |>
  sn_nodes(
    size = c(0.08, 0.06, 0.1),
    fill = c("red", "blue", "green"),
    label_position = c("above", "below", "center")
  ) |>
  splot()

# Donut chart nodes showing proportions
cograph(adj) |>
  sn_nodes(
    donut_fill = c(0.25, 0.75, 0.5),
    donut_color = "steelblue",
    donut_show_value = TRUE,
    donut_value_suffix = "%"
  ) |>
  splot()

# Mixed shapes per node
cograph(adj) |>
  sn_nodes(
    shape = c("circle", "square", "triangle"),
    fill = c("#E41A1C", "#377EB8", "#4DAF4A")
  ) |>
  splot()



cleanEx()
nameEx("sn_palette")
### * sn_palette

flush(stderr()); flush(stdout())

### Name: sn_palette
### Title: Apply Color Palette to Network
### Aliases: sn_palette

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Apply palette to nodes
cograph(adj) |> sn_palette("viridis") |> splot()

# Apply to edges
cograph(adj) |> sn_palette("colorblind", target = "edges") |> splot()

# Apply to both
cograph(adj) |> sn_palette("pastel", target = "both") |> splot()

# Custom palette function
my_pal <- function(n) rainbow(n, s = 0.7)
cograph(adj) |> sn_palette(my_pal) |> splot()

# Direct matrix input
adj |> sn_palette("viridis")



cleanEx()
nameEx("sn_save")
### * sn_save

flush(stderr()); flush(stdout())

### Name: sn_save
### Title: Save Network Visualization
### Aliases: sn_save

### ** Examples

## Not run: 
##D adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
##D # With cograph()
##D net <- cograph(adj)
##D sn_save(net, "network.pdf")
##D 
##D # Direct matrix input
##D sn_save(adj, "network.png", dpi = 300)
## End(Not run)



cleanEx()
nameEx("sn_save_ggplot")
### * sn_save_ggplot

flush(stderr()); flush(stdout())

### Name: sn_save_ggplot
### Title: Save as ggplot2
### Aliases: sn_save_ggplot

### ** Examples

## Not run: 
##D adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
##D net <- cograph(adj)
##D sn_save_ggplot(net, "network.pdf")
## End(Not run)



cleanEx()
nameEx("sn_theme")
### * sn_theme

flush(stderr()); flush(stdout())

### Name: sn_theme
### Title: Apply Theme to Network
### Aliases: sn_theme

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Apply different themes
cograph(adj) |> sn_theme("dark") |> splot()
cograph(adj) |> sn_theme("minimal") |> splot()

# Override specific theme properties
cograph(adj) |> sn_theme("classic", background = "lightgray") |> splot()

# Direct matrix input
adj |> sn_theme("dark")



cleanEx()
nameEx("soplot")
### * soplot

flush(stderr()); flush(stdout())

### Name: soplot
### Title: Plot Cograph Network
### Aliases: soplot sn_render

### ** Examples

adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
# With cograph()
cograph(adj) |> soplot()

# Direct matrix input with all options
adj |> soplot(
  layout = "circle",
  node_fill = "steelblue",
  node_size = 0.08,
  edge_width = 2
)



cleanEx()
nameEx("splot")
### * splot

flush(stderr()); flush(stdout())

### Name: splot
### Title: Base R Graphics Network Plotting
### Aliases: splot

### ** Examples

# Basic network from adjacency matrix
adj <- matrix(c(0, 1, 1, 0,
                0, 0, 1, 1,
                0, 0, 0, 1,
                0, 0, 0, 0), 4, 4, byrow = TRUE)
splot(adj)

# With curved edges
splot(adj, curvature = 0.2)

# Weighted network with colors
w_adj <- matrix(c(0, 0.5, -0.3, 0,
                  0.8, 0, 0.4, -0.2,
                  0, 0, 0, 0.6,
                  0, 0, 0, 0), 4, 4, byrow = TRUE)
splot(w_adj, edge_positive_color = "darkgreen", edge_negative_color = "red")

# Pie chart nodes
splot(adj, pie_values = list(c(1,2,3), c(2,2), c(1,1,1,1), c(3,1)))

# Circle layout with labels
splot(adj, layout = "circle", labels = c("A", "B", "C", "D"))




cleanEx()
nameEx("supra_adjacency")
### * supra_adjacency

flush(stderr()); flush(stdout())

### Name: supra_adjacency
### Title: Supra-Adjacency Matrix
### Aliases: supra_adjacency supra

### ** Examples

# layers <- list(L1 = mat1, L2 = mat2)
# S <- supra_adjacency(layers, omega = 0.5)
# dim(S)  # (2*n) x (2*n)



cleanEx()
nameEx("theme_cograph_classic")
### * theme_cograph_classic

flush(stderr()); flush(stdout())

### Name: theme_cograph_classic
### Title: Classic Theme
### Aliases: theme_cograph_classic

### ** Examples

theme <- theme_cograph_classic()



cleanEx()
nameEx("theme_cograph_colorblind")
### * theme_cograph_colorblind

flush(stderr()); flush(stdout())

### Name: theme_cograph_colorblind
### Title: Colorblind-friendly Theme
### Aliases: theme_cograph_colorblind

### ** Examples

theme <- theme_cograph_colorblind()



cleanEx()
nameEx("theme_cograph_dark")
### * theme_cograph_dark

flush(stderr()); flush(stdout())

### Name: theme_cograph_dark
### Title: Dark Theme
### Aliases: theme_cograph_dark

### ** Examples

theme <- theme_cograph_dark()



cleanEx()
nameEx("theme_cograph_gray")
### * theme_cograph_gray

flush(stderr()); flush(stdout())

### Name: theme_cograph_gray
### Title: Grayscale Theme
### Aliases: theme_cograph_gray

### ** Examples

theme <- theme_cograph_gray()



cleanEx()
nameEx("theme_cograph_minimal")
### * theme_cograph_minimal

flush(stderr()); flush(stdout())

### Name: theme_cograph_minimal
### Title: Minimal Theme
### Aliases: theme_cograph_minimal

### ** Examples

theme <- theme_cograph_minimal()



cleanEx()
nameEx("theme_cograph_nature")
### * theme_cograph_nature

flush(stderr()); flush(stdout())

### Name: theme_cograph_nature
### Title: Nature Theme
### Aliases: theme_cograph_nature

### ** Examples

theme <- theme_cograph_nature()



cleanEx()
nameEx("theme_cograph_viridis")
### * theme_cograph_viridis

flush(stderr()); flush(stdout())

### Name: theme_cograph_viridis
### Title: Viridis Theme
### Aliases: theme_cograph_viridis

### ** Examples

theme <- theme_cograph_viridis()



cleanEx()
nameEx("tna_animate")
### * tna_animate

flush(stderr()); flush(stdout())

### Name: tna_animate
### Title: Create Animated GIF of TNA Network Evolution
### Aliases: tna_animate

### ** Examples

## Not run: 
##D # Create sample sequence data
##D set.seed(123)
##D states <- c("A", "B", "C")
##D data <- data.frame(matrix(
##D   sample(states, 100 * 15, replace = TRUE),
##D   nrow = 100, ncol = 15
##D ))
##D 
##D # Basic animation
##D tna_animate(data, window_size = 5, fps = 2)
##D 
##D # Slower animation with larger windows
##D tna_animate(data,
##D   window_size = 10,
##D   step = 2,
##D   fps = 1,
##D   output = "slow_animation.gif"
##D )
##D 
##D # Customize appearance
##D tna_animate(data,
##D   window_size = 5,
##D   layout = "circle",
##D   node_fill = "steelblue",
##D   edge_color = "gray40"
##D )
## End(Not run)



cleanEx()
nameEx("tna_windows")
### * tna_windows

flush(stderr()); flush(stdout())

### Name: tna_windows
### Title: Generate Windowed TNA Objects from Sequence Data
### Aliases: tna_windows

### ** Examples

## Not run: 
##D # Create sample sequence data
##D set.seed(123)
##D states <- c("A", "B", "C")
##D data <- data.frame(matrix(
##D   sample(states, 100 * 10, replace = TRUE),
##D   nrow = 100, ncol = 10
##D ))
##D 
##D # Generate windowed TNA models
##D result <- tna_windows(data, window_size = 5, step = 1)
##D 
##D # Check how many windows were created
##D length(result$windows)
## End(Not run)



cleanEx()
nameEx("to_data_frame")
### * to_data_frame

flush(stderr()); flush(stdout())

### Name: to_data_frame
### Title: Export Network as Edge List Data Frame
### Aliases: to_data_frame to_df

### ** Examples

adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Convert to edge list
to_data_frame(adj)

# Use alias
to_df(adj)



cleanEx()
nameEx("to_igraph")
### * to_igraph

flush(stderr()); flush(stdout())

### Name: to_igraph
### Title: Convert Network to igraph Object
### Aliases: to_igraph

### ** Examples

# From matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
g <- to_igraph(adj)

# Force directed
g_dir <- to_igraph(adj, directed = TRUE)



cleanEx()
nameEx("to_matrix")
### * to_matrix

flush(stderr()); flush(stdout())

### Name: to_matrix
### Title: Convert Network to Adjacency Matrix
### Aliases: to_matrix

### ** Examples

# From matrix
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
to_matrix(adj)

# From cograph_network
net <- as_cograph(adj)
to_matrix(net)

# From igraph
## Not run: 
##D g <- igraph::make_ring(5)
##D to_matrix(g)
## End(Not run)
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
rownames(mat) <- colnames(mat) <- c("A", "B", "C")
net <- as_cograph(mat)
to_matrix(net)



cleanEx()
nameEx("to_network")
### * to_network

flush(stderr()); flush(stdout())

### Name: to_network
### Title: Convert Network to statnet network Object
### Aliases: to_network

### ** Examples

## Not run: 
##D adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
##D rownames(adj) <- colnames(adj) <- c("A", "B", "C")
##D net <- to_network(adj)
## End(Not run)



cleanEx()
nameEx("unregister_svg_shape")
### * unregister_svg_shape

flush(stderr()); flush(stdout())

### Name: unregister_svg_shape
### Title: Unregister SVG Shape
### Aliases: unregister_svg_shape

### ** Examples

# Attempt to unregister a non-existent shape (returns FALSE)
unregister_svg_shape("nonexistent")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
