# Cluster Metrics for Network Analysis
# Summary measures for between/within clusters and multilayer networks

# ==============================================================================
# 1. Edge Weight Aggregation
# ==============================================================================

#' Aggregate Edge Weights
#'
#' Aggregates a vector of edge weights using various methods.
#' Compatible with igraph's edge.attr.comb parameter.
#'
#' @param w Numeric vector of edge weights
#' @param method Aggregation method: "sum", "mean", "median", "max", "min",
#'   "prod", "density", "geomean"
#' @param n_possible Number of possible edges (for density calculation)
#' @return Single aggregated value
#' @export
#' @examples
#' w <- c(0.5, 0.8, 0.3, 0.9)
#' aggregate_weights(w, "sum")   # 2.5
#' aggregate_weights(w, "mean")  # 0.625
#' aggregate_weights(w, "max")   # 0.9
aggregate_weights <- function(w, method = "sum", n_possible = NULL) {
  # Remove NA and zero weights
  w <- w[!is.na(w) & w != 0]
  if (length(w) == 0) return(0)

  switch(method,
    "sum"     = sum(w),
    "mean"    = mean(w),
    "median"  = stats::median(w),
    "max"     = max(w),
    "min"     = min(w),
    "prod"    = prod(w),
    "density" = if (!is.null(n_possible) && n_possible > 0) {
      sum(w) / n_possible
    } else {
      sum(w) / length(w)
    },
    "geomean" = {
      pos_w <- w[w > 0]
      if (length(pos_w) == 0) 0 else exp(mean(log(pos_w)))
    },
    stop("Unknown method: ", method, call. = FALSE)
  )
}

#' @rdname aggregate_weights
#' @export
wagg <- aggregate_weights

# ==============================================================================
# 2. Cluster Summary (Between/Within Aggregates)
# ==============================================================================

#' Cluster Summary Statistics
#'
#' Computes aggregated edge weights between and within clusters.
#' Results are numerically identical to igraph's contract_vertices + simplify.
#'
#' @param x Adjacency matrix (numeric matrix with node names as row/colnames)
#' @param clusters Either a named list of node vectors, or a membership vector
#'   (integer vector where clusters\[i\] is the cluster of node i)
#' @param method Aggregation method: "sum", "mean", "median", "max", "min",
#'   "density", "geomean"
#' @param directed Logical; if TRUE, treat network as directed
#' @return A `cluster_summary` object with:
#'   \item{between}{Matrix of between-cluster aggregated weights}
#'   \item{within}{Vector of within-cluster aggregated weights}
#'   \item{cluster_sizes}{Number of nodes per cluster}
#'   \item{cluster_names}{Names of clusters}
#' @export
#' @examples
#' # Create adjacency matrix
#' mat <- matrix(runif(100), 10, 10)
#' diag(mat) <- 0
#' rownames(mat) <- colnames(mat) <- LETTERS[1:10]
#'
#' # Define clusters
#' clusters <- list(
#'   G1 = c("A", "B", "C"),
#'   G2 = c("D", "E", "F"),
#'   G3 = c("G", "H", "I", "J")
#' )
#'
#' # Compute summary
#' result <- cluster_summary(mat, clusters)
#' result$between  # Between-cluster matrix
#' result$within   # Within-cluster values
cluster_summary <- function(x,
                            clusters,
                            method = c("sum", "mean", "median", "max",
                                       "min", "density", "geomean"),
                            directed = TRUE) {

  method <- match.arg(method)

  # Validate input matrix
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("x must be a numeric matrix", call. = FALSE)
  }
  if (nrow(x) != ncol(x)) {
    stop("x must be a square matrix", call. = FALSE)
  }

  n <- nrow(x)
  node_names <- rownames(x)
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  # Convert clusters to list format
  cluster_list <- .normalize_clusters(clusters, node_names)
  n_clusters <- length(cluster_list)
  cluster_names <- names(cluster_list)
  if (is.null(cluster_names)) cluster_names <- as.character(seq_len(n_clusters))

  # Get node indices for each cluster
  cluster_indices <- lapply(cluster_list, function(nodes) {
    match(nodes, node_names)
  })

  # Compute cluster sizes
  cluster_sizes <- vapply(cluster_list, length, integer(1))

  # Initialize output matrices
  between <- matrix(0, n_clusters, n_clusters,
                    dimnames = list(cluster_names, cluster_names))
  within <- setNames(numeric(n_clusters), cluster_names)

  # Compute aggregates
  for (i in seq_len(n_clusters)) {
    idx_i <- cluster_indices[[i]]
    n_i <- length(idx_i)

    # Within-cluster (excluding diagonal for density)
    if (n_i > 1) {
      within_weights <- x[idx_i, idx_i]
      diag(within_weights) <- 0  # Exclude self-loops for within
      n_possible_within <- n_i * (n_i - 1)
      if (!directed) n_possible_within <- n_possible_within / 2
      within[i] <- aggregate_weights(as.vector(within_weights), method,
                                     n_possible_within)
    }

    # Between-cluster
    for (j in seq_len(n_clusters)) {
      if (i == j) next

      idx_j <- cluster_indices[[j]]
      n_j <- length(idx_j)

      between_weights <- x[idx_i, idx_j]
      n_possible_between <- n_i * n_j

      between[i, j] <- aggregate_weights(as.vector(between_weights), method,
                                         n_possible_between)
    }
  }

  structure(
    list(
      between = between,
      within = within,
      cluster_sizes = cluster_sizes,
      cluster_names = cluster_names,
      method = method,
      directed = directed
    ),
    class = "cluster_summary"
  )
}

#' @rdname cluster_summary
#' @export
csum <- cluster_summary

#' Normalize cluster specification to list format
#' @keywords internal
.normalize_clusters <- function(clusters, node_names) {
  if (is.list(clusters)) {
    # Already a list - validate node names
    all_nodes <- unlist(clusters)
    if (!all(all_nodes %in% node_names)) {
      missing <- setdiff(all_nodes, node_names)
      stop("Unknown nodes in clusters: ",
           paste(utils::head(missing, 5), collapse = ", "), call. = FALSE)
    }
    return(clusters)
  }

  if (is.vector(clusters) && (is.numeric(clusters) || is.integer(clusters))) {
    # Membership vector
    if (length(clusters) != length(node_names)) {
      stop("Membership vector length (", length(clusters),
           ") must equal number of nodes (", length(node_names), ")",
           call. = FALSE)
    }
    # Convert to list
    unique_clusters <- sort(unique(clusters))
    cluster_list <- lapply(unique_clusters, function(k) {
      node_names[clusters == k]
    })
    names(cluster_list) <- as.character(unique_clusters)
    return(cluster_list)
  }

  if (is.factor(clusters) || is.character(clusters)) {
    # Named membership
    if (length(clusters) != length(node_names)) {
      stop("Membership vector length must equal number of nodes", call. = FALSE)
    }
    clusters <- as.character(clusters)
    unique_clusters <- unique(clusters)
    cluster_list <- lapply(unique_clusters, function(k) {
      node_names[clusters == k]
    })
    names(cluster_list) <- unique_clusters
    return(cluster_list)
  }

  stop("clusters must be a list, numeric vector, or factor", call. = FALSE)
}

# ==============================================================================
# 3. Cluster Quality Metrics
# ==============================================================================

#' Cluster Quality Metrics
#'
#' Computes per-cluster and global quality metrics for network partitioning.
#' Supports both binary and weighted networks.
#'
#' @param x Adjacency matrix
#' @param clusters Cluster specification (list or membership vector)
#' @param weighted Logical; if TRUE, use edge weights; if FALSE, binarize
#' @param directed Logical; if TRUE, treat as directed network
#' @return A `cluster_quality` object with:
#'   \item{per_cluster}{Data frame with per-cluster metrics}
#'   \item{global}{List of global metrics (modularity, coverage)}
#' @export
#' @examples
#' mat <- matrix(runif(100), 10, 10)
#' diag(mat) <- 0
#' clusters <- c(1,1,1,2,2,2,3,3,3,3)
#'
#' q <- cluster_quality(mat, clusters)
#' q$per_cluster   # Per-cluster metrics
#' q$global        # Modularity, coverage
cluster_quality <- function(x,
                            clusters,
                            weighted = TRUE,
                            directed = TRUE) {

  # Validate and prepare
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("x must be a numeric matrix", call. = FALSE)
  }

  n <- nrow(x)
  node_names <- rownames(x)
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  # Normalize clusters
  cluster_list <- .normalize_clusters(clusters, node_names)
  n_clusters <- length(cluster_list)

  # Create membership vector for global metrics
  membership <- integer(n)
  for (k in seq_along(cluster_list)) {
    idx <- match(cluster_list[[k]], node_names)
    membership[idx] <- k
  }

  # Work with weighted or binarized matrix
  if (weighted) {
    A <- x
  } else {
    A <- (x > 0) * 1
  }

  # Total edges/weights
  m_total <- sum(A)
  if (!directed) m_total <- m_total / 2

  # Compute per-cluster metrics
  metrics_list <- lapply(seq_along(cluster_list), function(k) {
    S <- match(cluster_list[[k]], node_names)
    n_S <- length(S)

    if (n_S == 0) {
      return(data.frame(
        cluster = k,
        n_nodes = 0,
        internal_edges = 0,
        cut_edges = 0,
        internal_density = NA_real_,
        avg_internal_degree = NA_real_,
        expansion = NA_real_,
        cut_ratio = NA_real_,
        conductance = NA_real_
      ))
    }

    # Internal edges/weights (within cluster)
    m_S <- sum(A[S, S])
    if (!directed) m_S <- m_S / 2

    # Cut edges/weights (crossing cluster boundary)
    not_S <- setdiff(seq_len(n), S)
    if (directed) {
      c_S <- sum(A[S, not_S]) + sum(A[not_S, S])
    } else {
      c_S <- sum(A[S, not_S])
    }

    # Metrics
    max_internal <- n_S * (n_S - 1)
    if (!directed) max_internal <- max_internal / 2
    internal_density <- if (max_internal > 0) m_S / max_internal else NA_real_

    avg_internal_degree <- if (n_S > 0) 2 * m_S / n_S else NA_real_

    expansion <- if (n_S > 0) c_S / n_S else NA_real_

    max_cut <- n_S * (n - n_S)
    cut_ratio <- if (max_cut > 0) c_S / max_cut else NA_real_

    vol_S <- 2 * m_S + c_S
    conductance <- if (vol_S > 0) c_S / vol_S else NA_real_

    data.frame(
      cluster = k,
      cluster_name = names(cluster_list)[k],
      n_nodes = n_S,
      internal_edges = m_S,
      cut_edges = c_S,
      internal_density = internal_density,
      avg_internal_degree = avg_internal_degree,
      expansion = expansion,
      cut_ratio = cut_ratio,
      conductance = conductance
    )
  })

  per_cluster <- do.call(rbind, metrics_list)
  rownames(per_cluster) <- NULL

  # Global metrics
  total_internal <- sum(per_cluster$internal_edges)
  coverage <- if (m_total > 0) total_internal / m_total else NA_real_

  modularity <- .compute_modularity(A, membership, directed)

  structure(
    list(
      per_cluster = per_cluster,
      global = list(
        modularity = modularity,
        coverage = coverage,
        n_clusters = n_clusters
      )
    ),
    class = "cluster_quality"
  )
}

#' @rdname cluster_quality
#' @export
cqual <- cluster_quality

#' Compute modularity
#' @keywords internal
.compute_modularity <- function(A, membership, directed = TRUE) {
  n <- nrow(A)
  m <- sum(A)
  if (!directed) m <- m / 2
  if (m == 0) return(NA_real_)

  if (directed) {
    k_out <- rowSums(A)
    k_in <- colSums(A)
  } else {
    k <- rowSums(A)
    k_out <- k_in <- k
  }

  Q <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (membership[i] == membership[j]) {
        expected <- k_out[i] * k_in[j] / m
        Q <- Q + (A[i, j] - expected)
      }
    }
  }
  Q / m
}

# ==============================================================================
# 4. Layer Similarity Metrics
# ==============================================================================

#' Layer Similarity
#'
#' Computes similarity between two network layers.
#'
#' @param A1 First adjacency matrix
#' @param A2 Second adjacency matrix
#' @param method Similarity method: "jaccard", "overlap", "hamming", "cosine",
#'   "pearson"
#' @return Numeric similarity value
#' @export
#' @examples
#' A1 <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
#' A2 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
#'
#' layer_similarity(A1, A2, "jaccard")  # Edge overlap
#' layer_similarity(A1, A2, "cosine")   # Weight similarity
layer_similarity <- function(A1, A2,
                             method = c("jaccard", "overlap", "hamming",
                                        "cosine", "pearson")) {
  method <- match.arg(method)

  if (!identical(dim(A1), dim(A2))) {
    stop("Matrices must have identical dimensions", call. = FALSE)
  }

  E1 <- A1 > 0
  E2 <- A2 > 0

  switch(method,
    "jaccard" = {
      intersection <- sum(E1 & E2)
      union <- sum(E1 | E2)
      if (union == 0) NA_real_ else intersection / union
    },
    "overlap" = {
      intersection <- sum(E1 & E2)
      min_size <- min(sum(E1), sum(E2))
      if (min_size == 0) NA_real_ else intersection / min_size
    },
    "hamming" = {
      sum(xor(E1, E2))
    },
    "cosine" = {
      dot_product <- sum(A1 * A2)
      norm1 <- sqrt(sum(A1^2))
      norm2 <- sqrt(sum(A2^2))
      if (norm1 == 0 || norm2 == 0) NA_real_ else dot_product / (norm1 * norm2)
    },
    "pearson" = {
      stats::cor(as.vector(A1), as.vector(A2))
    }
  )
}

#' @rdname layer_similarity
#' @export
lsim <- layer_similarity

#' Pairwise Layer Similarities
#'
#' Computes similarity matrix for all pairs of layers.
#'
#' @param layers List of adjacency matrices (one per layer)
#' @param method Similarity method
#' @return Symmetric matrix of pairwise similarities
#' @export
#' @examples
#' # layers <- list(T1 = mat1, T2 = mat2, T3 = mat3)
#' # layer_similarity_matrix(layers, "cosine")
layer_similarity_matrix <- function(layers,
                                    method = c("jaccard", "overlap", "cosine",
                                               "pearson")) {
  method <- match.arg(method)
  L <- length(layers)

  if (L < 2) {
    stop("Need at least 2 layers for comparison", call. = FALSE)
  }

  layer_names <- names(layers)
  if (is.null(layer_names)) layer_names <- paste0("Layer", seq_len(L))

  sim_matrix <- matrix(NA_real_, L, L,
                       dimnames = list(layer_names, layer_names))

  for (i in seq_len(L)) {
    sim_matrix[i, i] <- 1
    for (j in seq_len(i - 1)) {
      sim <- layer_similarity(layers[[i]], layers[[j]], method)
      sim_matrix[i, j] <- sim
      sim_matrix[j, i] <- sim
    }
  }

  sim_matrix
}

#' @rdname layer_similarity_matrix
#' @export
lsim_matrix <- layer_similarity_matrix

#' Degree Correlation Between Layers
#'
#' Measures hub consistency across layers via degree correlation.
#'
#' @param layers List of adjacency matrices
#' @param mode Degree type: "total", "in", "out"
#' @return Correlation matrix between layer degree sequences
#' @export
layer_degree_correlation <- function(layers, mode = c("total", "in", "out")) {
  mode <- match.arg(mode)
  L <- length(layers)

  degrees <- lapply(layers, function(A) {
    switch(mode,
      "total" = rowSums(A) + colSums(A),
      "in" = colSums(A),
      "out" = rowSums(A)
    )
  })

  degree_matrix <- do.call(cbind, degrees)
  layer_names <- names(layers)
  if (is.null(layer_names)) layer_names <- paste0("Layer", seq_len(L))
  colnames(degree_matrix) <- layer_names

  stats::cor(degree_matrix)
}

#' @rdname layer_degree_correlation
#' @export
ldegcor <- layer_degree_correlation

# ==============================================================================
# 5. Supra-Adjacency Matrix Construction
# ==============================================================================

#' Supra-Adjacency Matrix
#'
#' Builds the supra-adjacency matrix for multilayer networks.
#' Diagonal blocks = intra-layer, off-diagonal = inter-layer.
#'
#' @param layers List of adjacency matrices (same dimensions)
#' @param omega Inter-layer coupling coefficient (scalar or L x L matrix)
#' @param coupling Coupling type: "diagonal", "full", or "custom"
#' @param interlayer_matrices For coupling="custom", list of inter-layer matrices
#' @return Supra-adjacency matrix of dimension (N*L) x (N*L)
#' @export
#' @examples
#' # layers <- list(L1 = mat1, L2 = mat2)
#' # S <- supra_adjacency(layers, omega = 0.5)
#' # dim(S)  # (2*n) x (2*n)
supra_adjacency <- function(layers,
                            omega = 1,
                            coupling = c("diagonal", "full", "custom"),
                            interlayer_matrices = NULL) {

  coupling <- match.arg(coupling)
  L <- length(layers)

  if (L < 1) stop("Need at least 1 layer", call. = FALSE)

  dims <- vapply(layers, function(A) c(nrow(A), ncol(A)), integer(2))
  if (!all(dims[1, ] == dims[1, 1]) || !all(dims[2, ] == dims[2, 1])) {
    stop("All layers must have identical dimensions", call. = FALSE)
  }

  n <- nrow(layers[[1]])
  N <- n * L

  A_supra <- matrix(0, N, N)

  node_names <- rownames(layers[[1]])
  if (is.null(node_names)) node_names <- as.character(seq_len(n))
  layer_names <- names(layers)
  if (is.null(layer_names)) layer_names <- paste0("L", seq_len(L))

  supra_names <- paste0(rep(layer_names, each = n), "_", rep(node_names, L))
  dimnames(A_supra) <- list(supra_names, supra_names)

  # Fill diagonal blocks (intra-layer)
  for (a in seq_len(L)) {
    idx <- ((a - 1) * n + 1):(a * n)
    A_supra[idx, idx] <- layers[[a]]
  }

  # Fill off-diagonal blocks (inter-layer)
  if (L > 1) {
    I <- diag(n)

    omega_matrix <- if (is.matrix(omega)) {
      if (!identical(dim(omega), c(L, L))) {
        stop("omega matrix must be L x L", call. = FALSE)
      }
      omega
    } else {
      matrix(omega, L, L)
    }

    for (a in seq_len(L - 1)) {
      for (b in (a + 1):L) {
        idx_a <- ((a - 1) * n + 1):(a * n)
        idx_b <- ((b - 1) * n + 1):(b * n)

        interlayer <- switch(coupling,
          "diagonal" = omega_matrix[a, b] * I,
          "full" = matrix(omega_matrix[a, b], n, n),
          "custom" = {
            if (is.null(interlayer_matrices)) {
              stop("interlayer_matrices required for custom coupling",
                   call. = FALSE)
            }
            idx_pair <- which(a == seq_len(L - 1) & b == (a + 1))
            if (length(idx_pair) == 1) {
              interlayer_matrices[[idx_pair]]
            } else {
              omega_matrix[a, b] * I
            }
          }
        )

        A_supra[idx_a, idx_b] <- interlayer
        A_supra[idx_b, idx_a] <- t(interlayer)
      }
    }
  }

  structure(
    A_supra,
    n_nodes = n,
    n_layers = L,
    node_names = node_names,
    layer_names = layer_names,
    omega = omega,
    coupling = coupling,
    class = c("supra_adjacency", "matrix")
  )
}

#' @rdname supra_adjacency
#' @export
supra <- supra_adjacency

#' Extract Layer from Supra-Adjacency Matrix
#'
#' @param x Supra-adjacency matrix
#' @param layer Layer index to extract
#' @return Intra-layer adjacency matrix
#' @export
supra_layer <- function(x, layer) {
  n <- attr(x, "n_nodes")
  L <- attr(x, "n_layers")

  if (layer < 1 || layer > L) {
    stop("layer must be between 1 and ", L, call. = FALSE)
  }

  idx <- ((layer - 1) * n + 1):(layer * n)
  A <- x[idx, idx]

  node_names <- attr(x, "node_names")
  dimnames(A) <- list(node_names, node_names)

  A
}

#' @rdname supra_layer
#' @export
extract_layer <- supra_layer

#' Extract Inter-Layer Block
#'
#' @param x Supra-adjacency matrix
#' @param from Source layer index
#' @param to Target layer index
#' @return Inter-layer adjacency matrix
#' @export
supra_interlayer <- function(x, from, to) {
  n <- attr(x, "n_nodes")
  L <- attr(x, "n_layers")

  if (from < 1 || from > L || to < 1 || to > L) {
    stop("layer indices must be between 1 and ", L, call. = FALSE)
  }

  idx_from <- ((from - 1) * n + 1):(from * n)
  idx_to <- ((to - 1) * n + 1):(to * n)

  x[idx_from, idx_to]
}

#' @rdname supra_interlayer
#' @export
extract_interlayer <- supra_interlayer

# ==============================================================================
# 6. Layer Aggregation
# ==============================================================================

#' Aggregate Layers
#'
#' Combines multiple network layers into a single network.
#'
#' @param layers List of adjacency matrices
#' @param method Aggregation: "sum", "mean", "max", "min", "union", "intersection"
#' @param weights Optional layer weights (for weighted sum)
#' @return Aggregated adjacency matrix
#' @export
#' @examples
#' # layers <- list(L1 = mat1, L2 = mat2, L3 = mat3)
#' # aggregate_layers(layers, "sum")           # Total
#' # aggregate_layers(layers, "mean")          # Average
#' # aggregate_layers(layers, "union")         # Any edge
#' # aggregate_layers(layers, "intersection")  # All edges
aggregate_layers <- function(layers,
                             method = c("sum", "mean", "max", "min",
                                        "union", "intersection"),
                             weights = NULL) {
  method <- match.arg(method)
  L <- length(layers)

  if (L == 0) stop("Need at least 1 layer", call. = FALSE)
  if (L == 1) return(layers[[1]])

  n <- nrow(layers[[1]])
  arr <- array(0, dim = c(n, n, L))
  for (l in seq_len(L)) {
    arr[, , l] <- layers[[l]]
  }

  result <- switch(method,
    "sum" = {
      if (!is.null(weights)) {
        if (length(weights) != L) {
          stop("weights must have length equal to number of layers",
               call. = FALSE)
        }
        Reduce(`+`, Map(`*`, layers, weights))
      } else {
        apply(arr, c(1, 2), sum)
      }
    },
    "mean" = apply(arr, c(1, 2), mean),
    "max" = apply(arr, c(1, 2), max),
    "min" = apply(arr, c(1, 2), min),
    "union" = {
      result <- matrix(0, n, n)
      for (l in seq_len(L)) {
        result <- result | (layers[[l]] > 0)
      }
      result * 1
    },
    "intersection" = {
      result <- matrix(1, n, n)
      for (l in seq_len(L)) {
        result <- result & (layers[[l]] > 0)
      }
      result * 1
    }
  )

  dimnames(result) <- dimnames(layers[[1]])
  result
}

#' @rdname aggregate_layers
#' @export
lagg <- aggregate_layers

# ==============================================================================
# 7. Verification (igraph compatibility)
# ==============================================================================

#' Verify Against igraph
#'
#' Confirms numerical match with igraph's contract_vertices + simplify.
#'
#' @param x Adjacency matrix
#' @param clusters Cluster specification
#' @param method Aggregation method
#' @return List with comparison results
#' @export
verify_with_igraph <- function(x, clusters, method = "sum") {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    message("igraph package not available for verification")
    return(NULL)
  }

  our_result <- cluster_summary(x, clusters, method = method, directed = TRUE)

  g <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE, mode = "directed")

  node_names <- rownames(x)
  if (is.null(node_names)) node_names <- as.character(seq_len(nrow(x)))

  cluster_list <- .normalize_clusters(clusters, node_names)
  membership <- integer(nrow(x))
  for (k in seq_along(cluster_list)) {
    idx <- match(cluster_list[[k]], node_names)
    membership[idx] <- k
  }

  g_contracted <- igraph::contract(g, membership,
                                   vertex.attr.comb = list(name = "first"))
  g_simplified <- igraph::simplify(g_contracted,
                                   edge.attr.comb = list(weight = method))

  igraph_result <- igraph::as_adjacency_matrix(g_simplified,
                                               attr = "weight",
                                               sparse = FALSE)

  diag(igraph_result) <- 0

  matches <- all.equal(our_result$between, igraph_result,
                       check.attributes = FALSE, tolerance = 1e-10)

  list(
    our_result = our_result$between,
    igraph_result = igraph_result,
    matches = isTRUE(matches),
    difference = if (!isTRUE(matches)) matches else NULL
  )
}

#' @rdname verify_with_igraph
#' @export
verify_igraph <- verify_with_igraph

# ==============================================================================
# Print Methods
# ==============================================================================

#' @export
print.cluster_summary <- function(x, ...) {
  cat("Cluster Summary (method: ", x$method, ")\n", sep = "")
  cat("Clusters:", length(x$cluster_names), "\n")
  cat("Cluster sizes:", paste(x$cluster_sizes, collapse = ", "), "\n\n")

  cat("Between-cluster matrix:\n")
  print(round(x$between, 4))
  cat("\nWithin-cluster values:\n")
  print(round(x$within, 4))

  invisible(x)
}

#' @export
print.cluster_quality <- function(x, ...) {
  cat("Cluster Quality Metrics\n")
  cat("=======================\n\n")

  cat("Global metrics:\n")
  cat("  Modularity:", round(x$global$modularity, 4), "\n")
  cat("  Coverage:  ", round(x$global$coverage, 4), "\n")
  cat("  Clusters:  ", x$global$n_clusters, "\n\n")

  cat("Per-cluster metrics:\n")
  print(x$per_cluster, row.names = FALSE)

  invisible(x)
}
