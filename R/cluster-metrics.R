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
#' Aggregates node-level network weights to cluster-level summaries. Computes
#' both between-cluster transitions (how clusters connect to each other) and
#' within-cluster transitions (how nodes connect within each cluster).
#'
#' This is the core function for Multi-Cluster Multi-Level (MCML) analysis.
#' Use \code{\link{as_tna}} to convert results to tna objects for further
#' analysis with the tna package.
#'
#' @param x Network input. Accepts multiple formats:
#'   \describe{
#'     \item{matrix}{Numeric adjacency/weight matrix. Row and column names are
#'       used as node labels. Values represent edge weights (e.g., transition
#'       counts, co-occurrence frequencies, or probabilities).}
#'     \item{cograph_network}{A cograph network object. The function extracts
#'       the weight matrix from \code{x$weights} or converts via
#'       \code{to_matrix()}. Clusters can be auto-detected from node attributes.}
#'     \item{tna}{A tna object from the tna package. Extracts \code{x$weights}.}
#'     \item{cluster_summary}{If already a cluster_summary, returns unchanged.}
#'   }
#'
#' @param clusters Cluster/group assignments for nodes. Accepts multiple formats:
#'   \describe{
#'     \item{NULL}{(default) Auto-detect from cograph_network. Looks for columns
#'       named 'clusters', 'cluster', 'groups', or 'group' in \code{x$nodes}.
#'       Throws an error if no cluster column is found.
#'       This option only works when \code{x} is a cograph_network.}
#'     \item{vector}{Cluster membership for each node, in the same order as the

#'       matrix rows/columns. Can be numeric (1, 2, 3) or character ("A", "B").
#'       Cluster names will be derived from unique values.
#'       Example: \code{c(1, 1, 2, 2, 3, 3)} assigns first two nodes to cluster 1.}
#'     \item{named list}{Explicit mapping of cluster names to node labels.
#'       List names become cluster names, values are character vectors of node
#'       labels that must match matrix row/column names.
#'       Example: \code{list(Alpha = c("A", "B"), Beta = c("C", "D"))}}
#'   }
#'
#' @param method Aggregation method for combining edge weights within/between
#'   clusters. Controls how multiple node-to-node edges are summarized:
#'   \describe{
#'     \item{"sum"}{(default) Sum of all edge weights. Best for count data
#'       (e.g., transition frequencies). Preserves total flow.}
#'     \item{"mean"}{Average edge weight. Best when cluster sizes differ and
#'       you want to control for size. Note: when input is already a transition
#'       matrix (rows sum to 1), "mean" avoids size bias.
#'       Example: cluster with 5 nodes won't have 5x the weight of cluster with 1 node.}
#'     \item{"median"}{Median edge weight. Robust to outliers.}
#'     \item{"max"}{Maximum edge weight. Captures strongest connection.}
#'     \item{"min"}{Minimum edge weight. Captures weakest connection.}
#'     \item{"density"}{Sum divided by number of possible edges. Normalizes
#'       by cluster size combinations.}
#'     \item{"geomean"}{Geometric mean of positive weights. Useful for
#'       multiplicative processes.}
#'   }
#'
#' @param type Post-processing applied to aggregated weights. Determines the
#'   interpretation of the resulting matrices:
#'   \describe{
#'     \item{"tna"}{(default) Row-normalize so each row sums to 1. Creates
#'       transition probabilities suitable for Markov chain analysis.
#'       Interpretation: "Given I'm in cluster A, what's the probability
#'       of transitioning to cluster B?"
#'       Required for use with tna package functions.
#'       Diagonal is zero; within-cluster data is in \code{$within}.}
#'     \item{"raw"}{No normalization. Returns aggregated counts/weights as-is.
#'       Use for frequency analysis or when you need raw counts.
#'       Compatible with igraph's contract + simplify output.}
#'     \item{"cooccurrence"}{Symmetrize the matrix: (A + t(A)) / 2.
#'       For undirected co-occurrence analysis.}
#'     \item{"semi_markov"}{Row-normalize with duration weighting.
#'       For semi-Markov process analysis.}
#'   }
#'
#' @param directed Logical. If \code{TRUE} (default), treat network as directed.
#'   A->B and B->A are separate edges. If \code{FALSE}, edges are undirected
#'   and the matrix is symmetrized before processing.
#'
#' @param compute_within Logical. If \code{TRUE} (default), compute within-cluster
#'   transition matrices for each cluster. Each cluster gets its own n_i x n_i
#'   matrix showing internal node-to-node transitions.
#'   Set to \code{FALSE} to skip this computation for better performance when
#'   only between-cluster summary is needed.
#'
#' @return A \code{cluster_summary} object (S3 class) containing:
#'   \describe{
#'     \item{between}{List with two elements:
#'       \describe{
#'         \item{weights}{k x k matrix of cluster-to-cluster weights, where k is
#'           the number of clusters. Row i, column j contains the aggregated
#'           weight from cluster i to cluster j. Diagonal is zero (within-cluster
#'           transitions are in \code{$within}). Processing depends on \code{type}.}
#'         \item{inits}{Numeric vector of length k. Initial state distribution
#'           across clusters, computed from column sums of the original matrix.
#'           Represents the proportion of incoming edges to each cluster.}
#'       }
#'     }
#'     \item{within}{Named list with one element per cluster. Each element contains:
#'       \describe{
#'         \item{weights}{n_i x n_i matrix for nodes within that cluster.
#'           Shows internal transitions between nodes in the same cluster.}
#'         \item{inits}{Initial distribution within the cluster.}
#'       }
#'       NULL if \code{compute_within = FALSE}.}
#'     \item{clusters}{Named list mapping cluster names to their member node labels.
#'       Example: \code{list(A = c("n1", "n2"), B = c("n3", "n4", "n5"))}}
#'     \item{meta}{List of metadata:
#'       \describe{
#'         \item{type}{The \code{type} argument used ("tna", "raw", etc.)}
#'         \item{method}{The \code{method} argument used ("sum", "mean", etc.)}
#'         \item{directed}{Logical, whether network was treated as directed}
#'         \item{n_nodes}{Total number of nodes in original network}
#'         \item{n_clusters}{Number of clusters}
#'         \item{cluster_sizes}{Named vector of cluster sizes}
#'       }
#'     }
#'   }
#'
#' @details
#' ## Workflow
#'
#' Typical MCML analysis workflow:
#' \preformatted{
#' # 1. Create network
#' net <- cograph(edges, nodes = nodes)
#' net$nodes$clusters <- group_assignments
#'
#' # 2. Compute cluster summary
#' cs <- cluster_summary(net, type = "tna")
#'
#' # 3. Convert to tna models
#' tna_models <- as_tna(cs)
#'
#' # 4. Analyze/visualize
#' plot(tna_models$between)
#' tna::centrality(tna_models$between)
#' }
#'
#' ## Between-Cluster Matrix Structure
#'
#' The \code{between$weights} matrix has clusters as both rows and columns:
#' \itemize{
#'   \item Off-diagonal (row i, col j): Aggregated weight from cluster i to cluster j
#'   \item Diagonal (row i, col i): Within-cluster total (sum of internal edges in cluster i)
#' }
#'
#' When \code{type = "tna"}, rows sum to 1 and diagonal values represent
#' "retention rate" - the probability of staying within the same cluster.
#'
#' ## Choosing method and type
#'
#' \tabular{lll}{
#'   \strong{Input data} \tab \strong{Recommended} \tab \strong{Reason} \cr
#'   Edge counts \tab method="sum", type="tna" \tab Preserves total flow, normalizes to probabilities \cr
#'   Transition matrix \tab method="mean", type="tna" \tab Avoids cluster size bias \cr
#'   Frequencies \tab method="sum", type="raw" \tab Keep raw counts for analysis \cr
#'   Correlation matrix \tab method="mean", type="raw" \tab Average correlations \cr
#' }
#'
#' @export
#' @seealso
#'   \code{\link{as_tna}} to convert results to tna objects,
#'   \code{\link{plot_mcml}} for two-layer visualization,
#'   \code{\link{plot_mtna}} for flat cluster visualization
#'
#' @examples
#' # -----------------------------------------------------
#' # Basic usage with matrix and cluster vector
#' # -----------------------------------------------------
#' mat <- matrix(runif(100), 10, 10)
#' diag(mat) <- 0
#' rownames(mat) <- colnames(mat) <- LETTERS[1:10]
#'
#' clusters <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
#' cs <- cluster_summary(mat, clusters)
#'
#' # Access results
#' cs$between$weights    # 3x3 cluster transition matrix
#' cs$between$inits      # Initial distribution
#' cs$within$`1`$weights # Within-cluster 1 transitions
#' cs$meta               # Metadata
#'
#' # -----------------------------------------------------
#' # Named list clusters (more readable)
#' # -----------------------------------------------------
#' clusters <- list(
#'   Alpha = c("A", "B", "C"),
#'   Beta = c("D", "E", "F"),
#'   Gamma = c("G", "H", "I", "J")
#' )
#' cs <- cluster_summary(mat, clusters, type = "tna")
#' cs$between$weights    # Rows/cols named Alpha, Beta, Gamma
#' cs$within$Alpha       # Within Alpha cluster
#'
#' # -----------------------------------------------------
#' # Auto-detect clusters from cograph_network
#' # -----------------------------------------------------
#' net <- as_cograph(mat)
#' net$nodes$clusters <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
#' cs <- cluster_summary(net)  # No clusters argument needed
#'
#' # -----------------------------------------------------
#' # Different aggregation methods
#' # -----------------------------------------------------
#' cs_sum <- cluster_summary(mat, clusters, method = "sum")   # Total flow
#' cs_mean <- cluster_summary(mat, clusters, method = "mean") # Average
#' cs_max <- cluster_summary(mat, clusters, method = "max")   # Strongest
#'
#' # -----------------------------------------------------
#' # Raw counts vs TNA probabilities
#' # -----------------------------------------------------
#' cs_raw <- cluster_summary(mat, clusters, type = "raw")
#' cs_tna <- cluster_summary(mat, clusters, type = "tna")
#'
#' rowSums(cs_raw$between$weights)  # Various sums
#' rowSums(cs_tna$between$weights)  # All equal to 1
#'
#' # -----------------------------------------------------
#' # Skip within-cluster computation for speed
#' # -----------------------------------------------------
#' cs_fast <- cluster_summary(mat, clusters, compute_within = FALSE)
#' cs_fast$within  # NULL
#'
#' # -----------------------------------------------------
#' # Convert to tna objects for tna package
#' # -----------------------------------------------------
#' cs <- cluster_summary(mat, clusters, type = "tna")
#' tna_models <- as_tna(cs)
#' # tna_models$between      # tna object
#' # tna_models$within$Alpha # tna object
cluster_summary <- function(x,
                            clusters = NULL,
                            method = c("sum", "mean", "median", "max",
                                       "min", "density", "geomean"),
                            type = c("tna", "cooccurrence", "semi_markov", "raw"),
                            directed = TRUE,
                            compute_within = TRUE) {

  # If already a cluster_summary, return as-is

  if (inherits(x, "cluster_summary")) {
    return(x)
  }

  type <- match.arg(type)
  method <- match.arg(method)

  # Store original for cluster extraction
  x_orig <- x

  # Extract matrix from various input types
  if (inherits(x, "cograph_network")) {
    # Use stored weights matrix if available, else convert
    mat <- if (!is.null(x$weights)) x$weights else to_matrix(x)
  } else if (inherits(x, "tna")) {
    mat <- x$weights
  } else {
    mat <- x
  }

  # Auto-detect clusters from cograph_network if not provided
  if (is.null(clusters) && inherits(x_orig, "cograph_network")) {
    nodes <- x_orig$nodes
    if (!is.null(nodes)) {
      # Look for cluster column (priority order)
      cluster_cols <- c("clusters", "cluster", "groups", "group")
      for (col in cluster_cols) {
        if (col %in% names(nodes)) {
          clusters <- nodes[[col]]
          break
        }
      }
    }
    # Also check node_groups
    if (is.null(clusters) && !is.null(x_orig$node_groups)) {
      ng <- x_orig$node_groups
      cluster_col <- intersect(c("cluster", "group", "layer"), names(ng))
      if (length(cluster_col) > 0) {
        clusters <- ng[[cluster_col[1]]]
      }
    }
    if (is.null(clusters)) {
      stop("No clusters found in cograph_network. ",
           "Add a 'clusters' column to nodes or provide clusters argument.",
           call. = FALSE)
    }
  } else if (is.null(clusters)) {
    stop("clusters argument is required for matrix input", call. = FALSE)
  }

  # Validate input matrix
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("x must be a cograph_network, tna object, or numeric matrix", call. = FALSE)
  }
  if (nrow(mat) != ncol(mat)) {
    stop("x must be a square matrix", call. = FALSE)
  }

  n <- nrow(mat)
  node_names <- rownames(mat)
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  # Convert clusters to list format
  cluster_list <- .normalize_clusters(clusters, node_names)
  n_clusters <- length(cluster_list)
  cluster_names <- names(cluster_list)
  if (is.null(cluster_names)) cluster_names <- as.character(seq_len(n_clusters))
  names(cluster_list) <- cluster_names

  # Get node indices for each cluster
  cluster_indices <- lapply(cluster_list, function(nodes_vec) {
    match(nodes_vec, node_names)
  })

  # ============================================================================
  # Between-cluster computation (always computed)
  # ============================================================================

  # Aggregate between-cluster weights
  between_raw <- matrix(0, n_clusters, n_clusters,
                        dimnames = list(cluster_names, cluster_names))

  for (i in seq_len(n_clusters)) {
    idx_i <- cluster_indices[[i]]
    n_i <- length(idx_i)

    for (j in seq_len(n_clusters)) {
      idx_j <- cluster_indices[[j]]
      n_j <- length(idx_j)

      if (i == j) {
        # Diagonal stays 0 (no self-loops at cluster level)
        # Within-cluster transitions are captured in $within
      } else {
        # Off-diagonal: between-cluster
        w_ij <- mat[idx_i, idx_j]
        n_possible <- n_i * n_j
        between_raw[i, j] <- aggregate_weights(as.vector(w_ij), method, n_possible)
      }
    }
  }

  # Process based on type
  between_weights <- .process_weights(between_raw, type, directed)

  # Compute inits from column sums
  col_sums <- colSums(between_raw)
  total <- sum(col_sums)
  if (total > 0) {
    between_inits <- col_sums / total
  } else {
    between_inits <- rep(1 / n_clusters, n_clusters)
  }
  names(between_inits) <- cluster_names

  # Build $between
  between <- list(
    weights = between_weights,
    inits = between_inits
  )

  # ============================================================================
  # Within-cluster computation (optional)
  # ============================================================================

  within_data <- NULL
  if (isTRUE(compute_within)) {
    within_data <- lapply(seq_len(n_clusters), function(i) {
      idx_i <- cluster_indices[[i]]
      n_i <- length(idx_i)
      cl_nodes <- cluster_list[[i]]

      if (n_i <= 1) {
        # Single node: 1x1 zero matrix
        within_raw <- matrix(0, 1, 1, dimnames = list(cl_nodes, cl_nodes))
        within_weights_i <- within_raw
        within_inits_i <- setNames(1, cl_nodes)
      } else {
        # Extract within-cluster raw weights
        within_raw <- mat[idx_i, idx_i]
        diag(within_raw) <- 0
        dimnames(within_raw) <- list(cl_nodes, cl_nodes)

        # Process based on type
        within_weights_i <- .process_weights(within_raw, type, directed)

        # Within-cluster inits (handle NAs)
        col_sums_w <- colSums(within_raw, na.rm = TRUE)
        total_w <- sum(col_sums_w, na.rm = TRUE)
        within_inits_i <- if (!is.na(total_w) && total_w > 0) {
          col_sums_w / total_w
        } else {
          rep(1 / n_i, n_i)
        }
        names(within_inits_i) <- cl_nodes
      }

      list(
        weights = within_weights_i,
        inits = within_inits_i
      )
    })
    names(within_data) <- cluster_names
  }

  # ============================================================================
  # Build result
  # ============================================================================

  result <- structure(
    list(
      between = between,
      within = within_data,
      clusters = cluster_list,
      meta = list(
        type = type,
        method = method,
        directed = directed,
        n_nodes = n,
        n_clusters = n_clusters,
        cluster_sizes = vapply(cluster_list, length, integer(1))
      )
    ),
    class = "cluster_summary"
  )

  result
}

#' @rdname cluster_summary
#' @export
csum <- cluster_summary

#' Process weights based on type
#' @keywords internal
.process_weights <- function(raw_weights, type, directed = TRUE) {
  if (type == "raw") {
    return(raw_weights)
  }

  if (type == "cooccurrence") {
    # Symmetrize
    return((raw_weights + t(raw_weights)) / 2)
  }

  if (type == "tna" || type == "semi_markov") {
    # Row-normalize so rows sum to 1
    rs <- rowSums(raw_weights, na.rm = TRUE)
    processed <- raw_weights / ifelse(rs == 0 | is.na(rs), 1, rs)
    processed[is.na(processed)] <- 0
    return(processed)
  }

  # Default: return as-is

  raw_weights
}

#' Convert cluster_summary to tna Objects
#'
#' Converts a \code{cluster_summary} object to proper tna objects that can be
#' used with all functions from the tna package. Creates both a between-cluster
#' tna model (cluster-level transitions) and within-cluster tna models (internal
#' transitions within each cluster).
#'
#' This is the final step in the MCML workflow, enabling full integration with
#' the tna package for centrality analysis, bootstrap validation, permutation
#' tests, and visualization.
#'
#' @param x A \code{cluster_summary} object created by \code{\link{cluster_summary}}.
#'   The cluster_summary should typically be created with \code{type = "tna"} to
#'   ensure row-normalized transition probabilities. If created with
#'   \code{type = "raw"}, the raw counts will be passed to \code{tna::tna()}
#'   which will normalize them.
#'
#' @return A \code{cluster_tna} object (S3 class) containing:
#'   \describe{
#'     \item{between}{A tna object representing cluster-level transitions.
#'       Contains \code{$weights} (k x k transition matrix), \code{$inits}
#'       (initial distribution), and \code{$labels} (cluster names).
#'       Use this for analyzing how learners/entities move between high-level
#'       groups or phases.}
#'     \item{within}{Named list of tna objects, one per cluster. Each tna object
#'       represents internal transitions within that cluster. Contains
#'       \code{$weights} (n_i x n_i matrix), \code{$inits} (initial distribution),
#'       and \code{$labels} (node labels). Clusters with single nodes or zero-row
#'       nodes are excluded (tna requires positive row sums).}
#'   }
#'
#' @details
#' ## Requirements
#'
#' The tna package must be installed. If not available, the function throws
#' an error with installation instructions.
#'
#' ## Workflow
#'
#' \preformatted{
#' # Full MCML workflow
#' net <- cograph(edges, nodes = nodes)
#' net$nodes$clusters <- group_assignments
#' cs <- cluster_summary(net, type = "tna")
#' tna_models <- as_tna(cs)
#'
#' # Now use tna package functions
#' plot(tna_models$between)
#' tna::centrality(tna_models$between)
#' tna::bootstrap(tna_models$between, R = 1000)
#'
#' # Analyze within-cluster patterns
#' plot(tna_models$within$ClusterA)
#' tna::centrality(tna_models$within$ClusterA)
#' }
#'
#' ## Excluded Clusters
#'
#' A within-cluster tna cannot be created when:
#' \itemize{
#'   \item The cluster has only 1 node (no internal transitions possible)
#'   \item Some nodes in the cluster have no outgoing edges (row sums to 0)
#' }
#'
#' These clusters are silently excluded from \code{$within}. The between-cluster
#' model still includes all clusters.
#'
#' @export
#' @seealso
#'   \code{\link{cluster_summary}} to create the input object,
#'   \code{\link{plot_mcml}} for visualization without conversion,
#'   \code{tna::tna} for the underlying tna constructor
#'
#' @examples
#' # -----------------------------------------------------
#' # Basic usage
#' # -----------------------------------------------------
#' mat <- matrix(runif(36), 6, 6)
#' diag(mat) <- 0
#' rownames(mat) <- colnames(mat) <- LETTERS[1:6]
#'
#' clusters <- list(
#'   G1 = c("A", "B"),
#'   G2 = c("C", "D"),
#'   G3 = c("E", "F")
#' )
#'
#' cs <- cluster_summary(mat, clusters, type = "tna")
#' tna_models <- as_tna(cs)
#'
#' # Print summary
#' tna_models
#'
#' # -----------------------------------------------------
#' # Access components
#' # -----------------------------------------------------
#' # Between-cluster tna
#' tna_models$between
#' tna_models$between$weights  # 3x3 transition matrix
#' tna_models$between$inits    # Initial distribution
#' tna_models$between$labels   # c("G1", "G2", "G3")
#'
#' # Within-cluster tnas
#' names(tna_models$within)    # Which clusters have within models
#' tna_models$within$G1        # tna for cluster G1
#' tna_models$within$G1$weights  # 2x2 matrix (A, B)
#'
#' # -----------------------------------------------------
#' # Use with tna package (requires tna)
#' # -----------------------------------------------------
#' \dontrun{
#' # Plot
#' plot(tna_models$between)
#' plot(tna_models$within$G1)
#'
#' # Centrality analysis
#' tna::centrality(tna_models$between)
#'
#' # Bootstrap validation
#' boot <- tna::bootstrap(tna_models$between, R = 1000)
#' summary(boot)
#'
#' # Compare clusters
#' tna::centrality(tna_models$within$G1)
#' tna::centrality(tna_models$within$G2)
#' }
#'
#' # -----------------------------------------------------
#' # Check which within-cluster models were created
#' # -----------------------------------------------------
#' cs <- cluster_summary(mat, clusters, type = "tna")
#' tna_models <- as_tna(cs)
#'
#' # All cluster names
#' names(cs$clusters)
#'
#' # Clusters with valid within-models
#' names(tna_models$within)
#'
#' # Clusters excluded (single node or zero rows)
#' setdiff(names(cs$clusters), names(tna_models$within))
as_tna <- function(x) {
  UseMethod("as_tna")
}

#' @export
as_tna.cluster_summary <- function(x) {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' is required for as_tna()", call. = FALSE)
  }

  # Between-cluster tna
  between_tna <- tna::tna(x$between$weights, inits = x$between$inits)

  # Within-cluster tnas
  within_tnas <- lapply(names(x$within), function(cl) {
    w <- x$within[[cl]]$weights
    inits <- x$within[[cl]]$inits

    # Skip if matrix has rows that sum to 0 (tna requires positive rows)
    if (any(rowSums(w) == 0)) {
      return(NULL)
    }

    tna::tna(w, inits = inits)
  })
  names(within_tnas) <- names(x$within)

  # Remove NULL entries
  within_tnas <- within_tnas[!vapply(within_tnas, is.null, logical(1))]

  structure(
    list(
      between = between_tna,
      within = within_tnas
    ),
    class = "cluster_tna"
  )
}

#' @export
as_tna.default <- function(x) {
 if (inherits(x, "tna")) {
    return(x)
  }
  stop("Cannot convert object of class '", class(x)[1], "' to tna", call. = FALSE)
}

#' @export
print.cluster_tna <- function(x, ...) {
  cat("Cluster TNA Models\n")
  cat("==================\n\n")

  cat("Between-cluster network:\n")
  cat("  Clusters:", paste(x$between$labels, collapse = ", "), "\n")
  cat("  Size:", nrow(x$between$weights), "x", ncol(x$between$weights), "\n\n")

  cat("Within-cluster networks:\n")
  for (cl in names(x$within)) {
    w <- x$within[[cl]]
    cat("  ", cl, ": ", nrow(w$weights), " nodes\n", sep = "")
  }

  if (length(x$within) == 0) {
    cat("  (none - clusters may have single nodes or zero rows)\n")
  }

  invisible(x)
}

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
# 3b. Cluster Significance Testing
# ==============================================================================

#' Test Significance of Community Structure
#'
#' Compares observed modularity against a null model distribution to assess
#' whether the detected community structure is statistically significant.
#'
#' @param x Network input: adjacency matrix, igraph object, or cograph_network.
#' @param communities A communities object (from \code{\link{communities}} or
#'   igraph) or a membership vector (integer vector where \code{communities[i]}
#'   is the community of node i).
#' @param n_random Number of random networks to generate for the null
#'   distribution. Default 100.
#' @param method Null model type:
#'   \describe{
#'     \item{"configuration"}{Preserves degree sequence (default). More
#'       stringent test.}
#'     \item{"gnm"}{Erdos-Renyi model with same number of edges. Tests against
#'       random baseline.}
#'   }
#' @param seed Random seed for reproducibility. Default NULL.
#'
#' @return A \code{cograph_cluster_significance} object with:
#'   \describe{
#'     \item{observed_modularity}{Modularity of the input communities}
#'     \item{null_mean}{Mean modularity of random networks}
#'     \item{null_sd}{Standard deviation of null modularity}
#'     \item{z_score}{Standardized score: (observed - null_mean) / null_sd}
#'     \item{p_value}{One-sided p-value (probability of observing equal or
#'       higher modularity by chance)}
#'     \item{null_values}{Vector of modularity values from null distribution}
#'     \item{method}{Null model method used}
#'     \item{n_random}{Number of random networks generated}
#'   }
#'
#' @details
#' The test works by:
#' \enumerate{
#'   \item Computing the modularity of the provided community structure
#'   \item Generating \code{n_random} random networks using the specified null model
#'   \item For each random network, detecting communities with Louvain and
#'     computing modularity
#'   \item Comparing the observed modularity to this null distribution
#' }
#'
#' A significant result (low p-value) indicates that the community structure
#' is stronger than expected by chance for networks with similar properties.
#'
#' @references
#' Reichardt, J., & Bornholdt, S. (2006).
#' Statistical mechanics of community detection.
#' \emph{Physical Review E}, 74, 016110.
#'
#' @export
#' @seealso \code{\link{communities}}, \code{\link{cluster_quality}}
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'   comm <- community_louvain(g)
#'
#'   # Test significance
#'   sig <- cluster_significance(g, comm, n_random = 100, seed = 123)
#'   print(sig)
#'
#'   # Configuration model (stricter test)
#'   sig2 <- cluster_significance(g, comm, method = "configuration")
#' }
cluster_significance <- function(x,
                                  communities,
                                  n_random = 100,
                                  method = c("configuration", "gnm"),
                                  seed = NULL) {

  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)

  # Convert to igraph
  if (inherits(x, "igraph")) {
    g <- x
  } else if (is.matrix(x)) {
    g <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE, mode = "directed")
  } else if (inherits(x, "cograph_network")) {
    g <- to_igraph(x)
  } else {
    g <- to_igraph(x)
  }

  # Get membership vector
  if (inherits(communities, "communities") ||
      inherits(communities, "cograph_communities")) {
    mem <- igraph::membership(communities)
  } else if (is.numeric(communities) || is.integer(communities)) {
    mem <- as.integer(communities)
  } else {
    stop("communities must be a communities object or membership vector",
         call. = FALSE)
  }

  # Observed modularity
  obs_mod <- igraph::modularity(g, mem)

  # Generate null distribution
  null_mods <- numeric(n_random)
  n_nodes <- igraph::vcount(g)
  n_edges <- igraph::ecount(g)

  for (i in seq_len(n_random)) {
    if (method == "configuration") {
      # Configuration model - preserve degree sequence
      deg <- igraph::degree(g)
      g_null <- tryCatch(
        igraph::sample_degseq(deg, method = "configuration"),
        error = function(e) {
          # Fallback to configuration.simple if configuration fails
          igraph::sample_degseq(deg, method = "configuration.simple")
        }
      )
    } else {
      # G(n,m) model - same number of nodes and edges
      g_null <- igraph::sample_gnm(n_nodes, n_edges, directed = igraph::is_directed(g))
    }

    # Detect communities on null graph (use Louvain for speed)
    comm_null <- tryCatch(
      igraph::cluster_louvain(g_null),
      error = function(e) {
        # If Louvain fails, use fast_greedy
        igraph::cluster_fast_greedy(igraph::as_undirected(g_null))
      }
    )
    null_mods[i] <- igraph::modularity(comm_null)
  }

  # Statistics
  null_mean <- mean(null_mods)
  null_sd <- sd(null_mods)
  z_score <- if (null_sd > 0) (obs_mod - null_mean) / null_sd else NA_real_
  p_value <- if (!is.na(z_score)) pnorm(z_score, lower.tail = FALSE) else NA_real_

  result <- list(
    observed_modularity = obs_mod,
    null_mean = null_mean,
    null_sd = null_sd,
    z_score = z_score,
    p_value = p_value,
    null_values = null_mods,
    method = method,
    n_random = n_random
  )
  class(result) <- "cograph_cluster_significance"
  result
}

#' @rdname cluster_significance
#' @export
csig <- cluster_significance

#' @export
print.cograph_cluster_significance <- function(x, ...) {
  cat("Cluster Significance Test\n")
  cat("=========================\n\n")
  cat("  Null model:          ", x$method, "(n =", x$n_random, ")\n")
  cat("  Observed modularity: ", round(x$observed_modularity, 4), "\n")
  cat("  Null mean:           ", round(x$null_mean, 4), "\n")
  cat("  Null SD:             ", round(x$null_sd, 4), "\n")
  cat("  Z-score:             ", round(x$z_score, 2), "\n")
  cat("  P-value:             ", format.pval(x$p_value), "\n\n")

  if (!is.na(x$p_value)) {
    if (x$p_value < 0.001) {
      cat("  Conclusion: Highly significant community structure (p < 0.001)\n")
    } else if (x$p_value < 0.01) {
      cat("  Conclusion: Very significant community structure (p < 0.01)\n")
    } else if (x$p_value < 0.05) {
      cat("  Conclusion: Significant community structure (p < 0.05)\n")
    } else {
      cat("  Conclusion: No significant community structure (p >= 0.05)\n")
    }
  }

  invisible(x)
}

#' Plot Cluster Significance
#'
#' Creates a histogram of the null distribution with the observed value marked.
#'
#' @param x A \code{cograph_cluster_significance} object
#' @param ... Additional arguments passed to \code{hist}
#' @return Invisibly returns x
#' @export
plot.cograph_cluster_significance <- function(x, ...) {
  # Create histogram of null distribution
  h <- graphics::hist(
    x$null_values,
    main = paste0("Cluster Significance Test (", x$method, ")"),
    xlab = "Modularity",
    col = "lightgray",
    border = "white",
    ...
  )

  # Add observed value line
  graphics::abline(v = x$observed_modularity, col = "#C62828", lwd = 2, lty = 2)

  # Add legend
  graphics::legend(
    "topright",
    legend = c(
      paste0("Observed (Q = ", round(x$observed_modularity, 3), ")"),
      paste0("Null mean (", round(x$null_mean, 3), ")")
    ),
    col = c("#C62828", "black"),
    lwd = c(2, 1),
    lty = c(2, 1),
    bty = "n"
  )

  # Add null mean line
  graphics::abline(v = x$null_mean, col = "black", lwd = 1)

  # Add p-value text
  graphics::mtext(
    paste0("p = ", format.pval(x$p_value)),
    side = 3,
    adj = 1,
    cex = 0.9
  )

  invisible(x)
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
#' @param type Normalization type. Defaults to "raw" for igraph compatibility.
#' @return List with comparison results
#' @export
verify_with_igraph <- function(x, clusters, method = "sum", type = "raw") {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    message("igraph package not available for verification")
    return(NULL)
  }

  # Use type = "raw" by default since igraph contract+simplify gives raw values
  our_result <- cluster_summary(x, clusters, method = method, directed = TRUE,
                                type = type)

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

  matches <- all.equal(our_result$between$weights, igraph_result,
                       check.attributes = FALSE, tolerance = 1e-10)

  list(
    our_result = our_result$between$weights,
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
  cat("Cluster Summary\n")
  cat("---------------\n")

  n_clusters <- x$meta$n_clusters
  n_nodes <- x$meta$n_nodes
  cluster_names <- names(x$clusters)
  cluster_sizes <- x$meta$cluster_sizes

  cat("Type:", x$meta$type, "\n")
  cat("Method:", x$meta$method, "\n")
  cat("Clusters:", n_clusters, "\n")
  cat("Nodes:", n_nodes, "\n")
  cat("Cluster sizes:", paste(cluster_sizes, collapse = ", "), "\n\n")

  # Between-cluster output
  bw <- x$between$weights
  cat("Between-cluster weights (", nrow(bw), "x", ncol(bw), "):\n", sep = "")
  cat("  Inits:", paste(round(x$between$inits, 3), collapse = ", "), "\n")
  if (nrow(bw) <= 6) {
    print(round(bw, 3))
  } else {
    cat("  [showing first 6x6 corner]\n")
    print(round(bw[1:6, 1:6], 3))
  }

  # Within-cluster output
  if (!is.null(x$within)) {
    cat("\nWithin-cluster weights (per-cluster):\n")
    n_show <- min(3, length(x$within))
    for (i in seq_len(n_show)) {
      cl_name <- names(x$within)[i]
      cl_mat <- x$within[[cl_name]]$weights
      cat("  ", cl_name, " (", nrow(cl_mat), " nodes)\n", sep = "")
    }
    if (length(x$within) > 3) {
      cat("  ... and", length(x$within) - 3, "more clusters\n")
    }
  } else {
    cat("\nWithin-cluster: not computed\n")
  }

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

# ==============================================================================
# 8. Summarize Network
# ==============================================================================

#' Summarize Network by Clusters
#'
#' Creates a summary network where each cluster becomes a single node.
#' Edge weights are aggregated from the original network using the specified
#' method. Returns a cograph_network object ready for plotting.
#'
#' @param x A weight matrix, tna object, or cograph_network.
#' @param cluster_list Cluster specification:
#'   \itemize{
#'     \item Named list of node vectors (e.g., \code{list(A = c("n1", "n2"), B = c("n3", "n4"))})
#'     \item String column name from nodes data (e.g., "clusters", "groups")
#'     \item NULL to auto-detect from common column names
#'   }
#' @param method Aggregation method for edge weights: "sum", "mean", "max",
#'   "min", "median", "density", "geomean". Default "sum".
#' @param directed Logical. Treat network as directed. Default TRUE.
#'
#' @return A cograph_network object with:
#'   \itemize{
#'     \item One node per cluster (named by cluster)
#'     \item Edge weights = aggregated between-cluster weights
#'     \item nodes$size = cluster sizes (number of original nodes)
#'   }
#'
#' @export
#' @seealso \code{\link{cluster_summary}}, \code{\link{plot_mcml}}
#'
#' @examples
#' # Create a network with clusters
#' mat <- matrix(runif(100), 10, 10)
#' diag(mat) <- 0
#' rownames(mat) <- colnames(mat) <- LETTERS[1:10]
#'
#' # Define clusters
#' clusters <- list(
#'   Group1 = c("A", "B", "C"),
#'   Group2 = c("D", "E", "F"),
#'   Group3 = c("G", "H", "I", "J")
#' )
#'
#' # Create summary network
#' summary_net <- summarize_network(mat, clusters)
#' splot(summary_net)
#'
#' # With cograph_network (auto-detect clusters column)
#' \dontrun{
#' Net <- cograph(mat)
#' Net$nodes$clusters <- rep(c("A", "B", "C"), c(3, 3, 4))
#' summary_net <- summarize_network(Net)  # Auto-detects 'clusters'
#' }
summarize_network <- function(x,
                               cluster_list = NULL,
                               method = c("sum", "mean", "max", "min",
                                          "median", "density", "geomean"),
                               directed = TRUE) {

  method <- match.arg(method)

  # Extract weight matrix and nodes data
  nodes_df <- NULL
  if (inherits(x, "cograph_network")) {
    mat <- to_matrix(x)
    nodes_df <- get_nodes(x)
    lab <- if (!is.null(nodes_df$label)) nodes_df$label else rownames(mat)
  } else if (inherits(x, "tna")) {
    mat <- x$weights
    lab <- x$labels
    if (is.null(lab)) lab <- rownames(mat)
  } else if (is.matrix(x)) {
    mat <- x
    lab <- rownames(mat)
    if (is.null(lab)) lab <- as.character(seq_len(nrow(mat)))
  } else {
    stop("x must be a cograph_network, tna object, or matrix", call. = FALSE)
  }

  # Handle cluster_list specification
  if (is.character(cluster_list) && length(cluster_list) == 1) {
    # Column name provided
    if (is.null(nodes_df)) {
      stop("To use a column name for cluster_list, x must be a cograph_network",
           call. = FALSE)
    }
    if (!cluster_list %in% names(nodes_df)) {
      stop("Column '", cluster_list, "' not found in nodes. Available: ",
           paste(names(nodes_df), collapse = ", "), call. = FALSE)
    }
    cluster_col <- nodes_df[[cluster_list]]
    cluster_list <- split(lab, cluster_col)
  } else if (is.null(cluster_list) && !is.null(nodes_df)) {
    # Auto-detect from common column names
    cluster_cols <- c("clusters", "cluster", "groups", "group", "community", "module")
    for (col in cluster_cols) {
      if (col %in% names(nodes_df)) {
        cluster_col <- nodes_df[[col]]
        cluster_list <- split(lab, cluster_col)
        message("Using '", col, "' column for clusters")
        break
      }
    }
  }

  if (is.null(cluster_list)) {
    stop("cluster_list required: provide a list, column name, or add a ",
         "'clusters'/'groups' column to nodes", call. = FALSE)
  }

  # Compute cluster summary with raw aggregation (no normalization)
  cs <- cluster_summary(mat, cluster_list, method = method, directed = directed,
                        type = "raw")

  # Create cograph_network from between-cluster matrix
  result <- cograph(cs$between$weights, directed = directed)

  # Add cluster sizes to nodes
  result$nodes$size <- cs$meta$cluster_sizes[match(result$nodes$label, names(cs$clusters))]

  result
}

#' @rdname summarize_network
#' @export
cluster_network <- summarize_network

#' @rdname summarize_network
#' @export
cnet <- summarize_network
