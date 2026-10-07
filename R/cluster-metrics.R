# Cluster Metrics for Network Analysis
# Summary measures for macro/per-cluster networks and multilayer networks

# ==============================================================================
# 1. Edge Weight Aggregation
# ==============================================================================

#' Aggregate Edge Weights
#'
#' Aggregates a vector of edge weights into a single value. The method names
#' follow those of igraph's \code{edge.attr.comb} argument.
#'
#' @param w Numeric vector of edge weights. \code{NA} and zero entries are
#'   dropped before aggregation.
#' @param method Aggregation method: "sum", "mean", "median", "max", "min",
#'   "prod", "density", "geomean". Default "sum". Any other value is an error.
#'   \code{"geomean"} uses only the positive weights.
#' @param n_possible Number of possible edges (used only by
#'   \code{method = "density"}; when NULL or not positive, the number of
#'   surviving weights is used as the denominator instead).
#' @return A single numeric value, or 0 when no non-zero, non-NA weight
#'   remains.
#' @export
#' @examples
#' aggregate_weights(regulation_net, method = "mean")
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
# 2. Cluster Summary (Macro/Cluster Aggregates)
# ==============================================================================

#' Cluster Summary Statistics
#'
#' Aggregates node-level network weights to cluster-level summaries. The
#' result holds the macro (cluster-to-cluster) network and one network of
#' within-cluster connections per cluster.
#'
#' The function is the matrix-based entry point to Multi-Cluster Multi-Level
#' (MCML) analysis. \code{\link{as_tna}} converts the result to tna models.
#'
#' @param x Network input. Accepts multiple formats:
#'   \describe{
#'     \item{matrix}{Numeric adjacency/weight matrix. Row and column names are
#'       used as node labels. Values represent edge weights (e.g., transition
#'       counts, co-occurrence frequencies, or probabilities).}
#'     \item{cograph_network}{A cograph network object. Its weight matrix is
#'       used, and clusters can be auto-detected from node attributes.}
#'     \item{tna}{A tna object from the tna package. Its weight matrix is used
#'       and its sequence data are kept in \code{macro$data}.}
#'     \item{cluster_summary}{Returned unchanged.}
#'   }
#'
#' @param clusters Cluster/group assignments for nodes. Accepts multiple formats:
#'   \describe{
#'     \item{NULL}{(default) Auto-detect from a cograph_network. The first
#'       node column named 'clusters', 'cluster', 'groups' or 'group' is used,
#'       then a 'cluster', 'group' or 'layer' column of the node groups.
#'       An error is raised when none is found, and for any other input
#'       \code{clusters} must be supplied.}
#'     \item{vector}{Cluster membership for each node, in the same order as the
#'       matrix rows/columns. Can be numeric (1, 2, 3) or character ("A", "B").
#'       Cluster names are the unique values, sorted for a numeric vector and
#'       in order of appearance for a character vector or factor.
#'       Example: \code{c(1, 1, 2, 2, 3, 3)} assigns first two nodes to cluster 1.}
#'     \item{data.frame}{A data frame where the first column contains node names
#'       and the second column contains group/cluster names.
#'       Example: \code{data.frame(node = c("A", "B", "C"), group = c("G1", "G1", "G2"))}}
#'     \item{named list}{Explicit mapping of cluster names to node labels.
#'       List names become cluster names, values are character vectors of node
#'       labels that must match matrix row/column names.
#'       Example: \code{list(Alpha = c("A", "B"), Beta = c("C", "D"))}}
#'   }
#'
#' @param method Aggregation method for combining edge weights within/between
#'   clusters. Zero and \code{NA} weights are dropped before aggregation
#'   (see \code{\link{aggregate_weights}}):
#'   \describe{
#'     \item{"sum"}{(default) Sum of the edge weights. Suited to count data
#'       such as transition frequencies, because it preserves the total flow.}
#'     \item{"mean"}{Mean edge weight. Suited to inputs that are already
#'       transition probabilities, because the result does not grow with
#'       cluster size.}
#'     \item{"median"}{Median edge weight. Robust to outliers.}
#'     \item{"max"}{Maximum edge weight. Captures strongest connection.}
#'     \item{"min"}{Minimum edge weight. Captures weakest connection.}
#'     \item{"density"}{Sum divided by the number of possible edges
#'       (\eqn{n_i n_j} for clusters of sizes \eqn{n_i} and \eqn{n_j}).}
#'     \item{"geomean"}{Geometric mean of positive weights. Useful for
#'       multiplicative processes.}
#'   }
#'
#' @param type Post-processing applied to aggregated weights. Determines the
#'   interpretation of the resulting matrices:
#'   \describe{
#'     \item{"tna"}{(default) Row-normalize so each row sums to 1, which gives
#'       transition probabilities. Rows that sum to zero are left at zero.}
#'     \item{"raw"}{No normalization. The aggregated weights are returned
#'       as computed.}
#'     \item{"cooccurrence"}{Symmetrize the matrix as (A + t(A)) / 2.}
#'     \item{"semi_markov"}{Row-normalize, identical to \code{"tna"}.}
#'   }
#'
#' @param directed Logical. Default \code{TRUE}. With \code{FALSE} the
#'   node-level weights are symmetrized as \eqn{(A + A^T) / 2}{(A + t(A)) / 2}
#'   before aggregation, so the direction of a tie is ignored, and
#'   \code{meta$directed} is \code{FALSE}. With \code{type = "cooccurrence"}
#'   the aggregated weights are symmetrized and \code{meta$directed} is
#'   \code{FALSE} whatever this value is.
#'
#' @param compute_within Logical. If \code{TRUE} (default), compute per-cluster
#'   matrices, one \eqn{n_i \times n_i}{n_i x n_i} matrix of internal node-to-node
#'   weights per cluster. \code{FALSE} skips this step when only the macro
#'   summary is needed.
#'
#' @return A \code{cluster_summary} object (S3 class) containing:
#'   \describe{
#'     \item{macro}{A tna object representing the macro (cluster-level) network:
#'       \describe{
#'         \item{weights}{k x k matrix of cluster-to-cluster weights, where k is
#'           the number of clusters. Row i, column j contains the aggregated
#'           weight from cluster i to cluster j. The diagonal contains the
#'           aggregated within-cluster weight, node self-loops included.
#'           Processing depends on \code{type}.}
#'         \item{inits}{Named numeric vector of length k, the column sums of
#'           the aggregated cluster matrix (before \code{type} processing)
#'           divided by their total. It is uniform when all weights are zero.}
#'         \item{labels}{Cluster names.}
#'         \item{data}{Sequence data of a tna input, otherwise \code{NULL}.}
#'       }
#'     }
#'     \item{clusters}{A \code{group_tna} list with one tna object per
#'       cluster, each containing:
#'       \describe{
#'         \item{weights}{n_i x n_i matrix for nodes inside that cluster.
#'           Shows internal transitions between nodes in the same cluster.}
#'         \item{inits}{Column sums of the within-cluster weights divided by
#'           their total.}
#'       }
#'       NULL if \code{compute_within = FALSE}.}
#'     \item{cluster_members}{Named list mapping cluster names to their member node labels.
#'       Example: \code{list(A = c("n1", "n2"), B = c("n3", "n4", "n5"))}}
#'     \item{meta}{List of metadata:
#'       \describe{
#'         \item{type}{The \code{type} argument used ("tna", "raw", etc.)}
#'         \item{method}{The \code{method} argument used ("sum", "mean", etc.)}
#'         \item{directed}{Logical, effective directedness of the stored
#'           weights (\code{FALSE} when \code{type = "cooccurrence"}, which
#'           symmetrizes them)}
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
#' A typical MCML analysis computes the summary, then plots it with
#' \code{\link{plot_mcml}} or converts it to tna models with
#' \code{\link{as_tna}}:
#' \preformatted{
#' cs <- csum(net, clusters = clusters, type = "tna")
#' plot_mcml(cs)
#' as_tna(cs)
#' }
#'
#' ## Between-Cluster Matrix Structure
#'
#' The macro weight matrix has clusters as both rows and columns. An
#' off-diagonal cell (i, j) holds the aggregated weight from cluster i to
#' cluster j. A diagonal cell (i, i) holds the aggregated weight of the edges
#' inside cluster i. When \code{type = "tna"}, rows sum to 1 and the diagonal
#' is the probability of staying inside the same cluster.
#'
#' ## Choosing method and type
#'
#' \tabular{lll}{
#'   Input data \tab Recommended \tab Reason \cr
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
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' csum(regulation_net, clusters = clusters, type = "tna")
csum <- function(x,
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

  # An undirected summary treats A->B and B->A as one tie: symmetrize the
  # node-level weights before they are aggregated.
  if (!isTRUE(directed)) {
    mat <- (mat + t(mat)) / 2
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
  # Macro (cluster-level) computation (always computed)
  # ============================================================================

  # Aggregate macro (cluster-to-cluster) weights
  between_raw <- matrix(0, n_clusters, n_clusters,
                        dimnames = list(cluster_names, cluster_names))

  for (i in seq_len(n_clusters)) {
    idx_i <- cluster_indices[[i]]
    n_i <- length(idx_i)

    for (j in seq_len(n_clusters)) {
      idx_j <- cluster_indices[[j]]
      n_j <- length(idx_j)

      if (i == j) {
        # Diagonal: aggregated intra-cluster weight (retention)
        # Includes node self-loops (A->A) -- these are valid intra-cluster flow
        w_ii <- mat[idx_i, idx_i, drop = FALSE]
        n_possible <- n_i * n_i
        between_raw[i, j] <- aggregate_weights(as.vector(w_ii), method,
                                                n_possible)
      } else {
        # Off-diagonal: inter-cluster transitions
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

  # Preserve original sequence data from tna input (no transformation)
  orig_data <- if (inherits(x_orig, "tna")) x_orig$data

  # Build $macro
  between <- structure(
    list(
      weights = between_weights,
      inits = between_inits,
      labels = cluster_names,
      data = orig_data
    ),
    type = if (type == "tna") "relative" else "frequency",
    scaling = character(0),
    class = "tna"
  )

  # ============================================================================
  # Per-cluster computation (optional)
  # ============================================================================

  cl_data <- NULL
  if (isTRUE(compute_within)) {
    cl_data <- lapply(seq_len(n_clusters), function(i) {
      idx_i <- cluster_indices[[i]]
      n_i <- length(idx_i)
      cl_nodes <- cluster_list[[i]]
      cl_name <- cluster_names[[i]]

      if (n_i <= 1) {
        # Single node: self-loop value preserved
        cl_raw <- mat[idx_i, idx_i, drop = FALSE]
        dimnames(cl_raw) <- list(cl_nodes, cl_nodes)
        cl_weights_i <- .process_weights(cl_raw, type, directed)
        cl_inits_i <- setNames(1, cl_nodes)
      } else {
        # Extract intra-cluster raw weights (self-loops preserved)
        cl_raw <- mat[idx_i, idx_i]
        dimnames(cl_raw) <- list(cl_nodes, cl_nodes)

        # Process based on type
        cl_weights_i <- .process_weights(cl_raw, type, directed)

        # Per-cluster inits (handle NAs)
        col_sums_w <- colSums(cl_raw, na.rm = TRUE)
        total_w <- sum(col_sums_w, na.rm = TRUE)
        cl_inits_i <- if (!is.na(total_w) && total_w > 0) {
          col_sums_w / total_w
        } else {
          rep(1 / n_i, n_i)
        }
        names(cl_inits_i) <- cl_nodes
      }

      structure(
        list(
          weights = cl_weights_i,
          inits = cl_inits_i,
          labels = cl_nodes,
          data = orig_data
        ),
        type = if (type == "tna") "relative" else "frequency",
        scaling = character(0),
        class = "tna"
      )
    })
    names(cl_data) <- cluster_names
    class(cl_data) <- "group_tna"
  }

  # ============================================================================
  # Build result
  # ============================================================================

  result <- structure(
    list(
      macro = between,
      clusters = cl_data,
      cluster_members = cluster_list,
      meta = list(
        type = type,
        method = method,
        # Effective directedness of the stored weights: type =
        # "cooccurrence" symmetrizes regardless of the directed argument.
        directed = isTRUE(directed) && type != "cooccurrence",
        n_nodes = n,
        n_clusters = n_clusters,
        cluster_sizes = vapply(cluster_list, length, integer(1))
      )
    ),
    class = "cluster_summary"
  )

  result
}

# Internal alias: the matrix aggregator was exported as cluster_summary()
# until 2.3.7, when the export was renamed to csum() to end the collision
# with Nestimate::cluster_summary() (a different, aggregation-only verb).
# Namespace-internal callers (plot_mcml, plot_htna_multi, mcml, ...) keep
# using this name; it is no longer exported.
cluster_summary <- csum

# ==============================================================================
# 2b. Build MCML from Raw Transition Data
# ==============================================================================

#' Build MCML from Raw Transition Data
#'
#' Builds a Multi-Cluster Multi-Level (MCML) model from raw transition data
#' (edge lists or sequences) by recoding node labels to cluster labels and
#' counting the observed transitions. The macro network is then the Markov
#' chain over cluster states. Weight matrices are passed to
#' \code{\link{csum}}, which aggregates them.
#'
#' @param x Input data. Accepts multiple formats:
#'   \describe{
#'     \item{data.frame with from/to columns}{Edge list. Columns named
#'       from/source/src/v1/node1/i and to/target/tgt/v2/node2/j are
#'       auto-detected. Optional weight column (weight/w/value/strength).}
#'     \item{data.frame without from/to columns}{Sequence data. Each row is a
#'       sequence, columns are time steps. Consecutive pairs (t, t+1) become
#'       transitions. A data frame with three or more columns that all have
#'       the default names V1, V2, V3, ... is read as sequence data, even
#'       though V1 and V2 are also from/to names.}
#'     \item{tna object}{If \code{x$data} is non-NULL, uses sequence path on
#'       the raw data. Otherwise falls back to \code{\link{csum}}.}
#'     \item{cograph_network}{If \code{x$data} is non-NULL, detects edge list
#'       vs sequence data. Otherwise falls back to \code{\link{csum}}.}
#'     \item{group_tna}{Converted as by \code{\link{as_mcml}}.}
#'     \item{mcml or cluster_summary}{Returned unchanged.}
#'     \item{square numeric matrix}{Falls back to \code{\link{csum}}.}
#'     \item{non-square or character matrix}{Treated as sequence data.}
#'   }
#'
#' @param clusters Cluster/group assignments. Accepts:
#'   \describe{
#'     \item{named list}{Direct mapping. List names = cluster names, values =
#'       character vectors of node labels.
#'       Example: \code{list(A = c("N1","N2"), B = c("N3","N4"))}}
#'     \item{data.frame}{A data frame where the first column contains node names
#'       and the second column contains group/cluster names.
#'       Example: \code{data.frame(node = c("N1","N2","N3"), group = c("A","A","B"))}}
#'     \item{membership vector}{Character or numeric vector. Node names are
#'       extracted from the data.
#'       Example: \code{c("A","A","B","B")}}
#'     \item{column name string}{For edge list data.frames, the name of a
#'       column containing cluster labels. The mapping is built from unique
#'       (node, group) pairs in both from and to columns.}
#'     \item{NULL}{Auto-detect from \code{cograph_network$nodes} or
#'       \code{$node_groups} (same logic as \code{\link{csum}}).}
#'   }
#'
#' @param method Aggregation method for combining edge weights: "sum", "mean",
#'   "median", "max", "min", "density", "geomean". Default "sum".
#' @param type Post-processing: "tna" (row-normalize), "frequency" or "raw"
#'   (no normalization), "cooccurrence" (symmetrize), or "semi_markov"
#'   (row-normalize, identical to "tna"). Default "tna".
#' @param directed Logical. Default \code{TRUE}. With \code{FALSE} the
#'   node-level weights are symmetrized as \eqn{(A + A^T) / 2}{(A + t(A)) / 2}
#'   before aggregation: a matrix input is averaged with its transpose, and
#'   each observed transition counts half in each direction. The value is
#'   recorded in \code{meta$directed}.
#' @param compute_within Logical. Compute within-cluster matrices? Default TRUE.
#'
#' @return Usually an \code{mcml} object. Existing \code{mcml} or
#'   \code{cluster_summary} inputs are returned unchanged. Transition-data
#'   results include \code{meta$source = "transitions"} and are compatible with
#'   \code{\link{plot_mcml}}, \code{\link{as_tna}}, and \code{\link{splot}}.
#'
#' @export
#' @seealso \code{\link{csum}} for matrix-based aggregation,
#'   \code{\link{as_tna}} to convert to tna objects,
#'   \code{\link{plot_mcml}} for visualization
#'
#' @examples
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' summarize_clusters(regulation_net, clusters = clusters, method = "mean")
summarize_clusters <- function(x,
                       clusters = NULL,
                       method = c("sum", "mean", "median", "max",
                                  "min", "density", "geomean"),
                       type = c("tna", "frequency", "cooccurrence",
                                "semi_markov", "raw"),
                       directed = TRUE,
                       compute_within = TRUE) {

  # If already an mcml or cluster_summary, return as-is
  if (inherits(x, c("mcml", "cluster_summary"))) {
    return(x)
  }

  type <- match.arg(type)
  method <- match.arg(method)

  input_type <- .detect_mcml_input(x)

  switch(input_type,
    "group_tna" = .group_tna_to_mcml(x, clusters, method, type,
                                       directed, compute_within),
    "edgelist" = .build_mcml_edgelist(x, clusters, method, type,
                                       directed, compute_within),
    "sequence" = .build_mcml_sequence(x, clusters, method, type,
                                       directed, compute_within),
    "tna_data" = .build_mcml_sequence(.decode_tna_data(x$data), clusters,
                                       method, type, directed, compute_within),
    "tna_matrix" = .as_mcml(cluster_summary(x, clusters, method = method,
                     type = type, directed = directed,
                     compute_within = compute_within)),
    "cograph_data" = {
      data <- x$data
      # Auto-detect clusters from network if not provided
      if (is.null(clusters)) {
        clusters <- .auto_detect_clusters(x)
      }
      sub_type <- .detect_mcml_input(data)
      if (sub_type == "edgelist") {
        .build_mcml_edgelist(data, clusters, method, type,
                              directed, compute_within)
      } else {
        .build_mcml_sequence(data, clusters, method, type,
                              directed, compute_within)
      }
    },
    "cograph_matrix" = {
      if (is.null(clusters)) {
        clusters <- .auto_detect_clusters(x)
      }
      .as_mcml(cluster_summary(x, clusters, method = method, type = type,
                                directed = directed,
                                compute_within = compute_within))
    },
    "matrix" = .as_mcml(cluster_summary(x, clusters, method = method,
                          type = type, directed = directed,
                          compute_within = compute_within)),
    stop("Cannot build MCML from input of class '", class(x)[1], "'",
         call. = FALSE)
  )
}

#' Convert a cluster_summary to mcml (strip tna classes)
#' @keywords internal
#' @noRd
.as_mcml <- function(cs) {
  # Strip tna class from macro
  m <- unclass(cs$macro)
  attributes(m) <- NULL
  names(m) <- c("weights", "inits", "labels", "data")
  if (length(m) >= 4) {
    names(m) <- c("weights", "inits", "labels", "data")[seq_along(m)]
  }

  # Strip tna/group_tna from clusters
  cl <- NULL
  if (!is.null(cs$clusters)) {
    cl <- lapply(cs$clusters, function(obj) {
      o <- unclass(obj)
      attributes(o) <- NULL
      names(o) <- c("weights", "inits", "labels", "data")[seq_along(o)]
      o
    })
    names(cl) <- names(cs$clusters)
  }

  structure(
    list(
      macro = m,
      clusters = cl,
      cluster_members = cs$cluster_members,
      meta = cs$meta
    ),
    class = "mcml"
  )
}

#' Decode numeric tna_seq_data back to character labels
#' @keywords internal
#' @noRd
.decode_tna_data <- function(data) {
  if (is.null(data)) return(NULL)
  tna_labels <- attr(data, "labels")
  if (is.null(tna_labels) || !is.numeric(data)) return(data)
  decoded <- as.data.frame(
    matrix(tna_labels[data], nrow = nrow(data)),
    stringsAsFactors = FALSE
  )
  if (!is.null(colnames(data))) colnames(decoded) <- colnames(data)
  decoded
}

#' Convert a group_tna to mcml
#'
#' Two modes depending on whether \code{clusters} is provided:
#' \describe{
#'   \item{With clusters (row-level)}{For group_tna from
#'     \code{tna::group_model(cluster_data(...))}. \code{clusters} is the
#'     row-to-group assignments. Per-cluster tnas are taken as-is.
#'     Macro data is the assignments vector.}
#'   \item{Without clusters (node-level)}{For group_tna from
#'     \code{as_tna(cluster_summary(...))}. Cluster membership inferred
#'     from each tna's labels. Macro rebuilt from original data.}
#' }
#' @keywords internal
#' @noRd
.group_tna_to_mcml <- function(x, clusters = NULL, method = "sum",
                                type = "tna", directed = TRUE,
                                compute_within = TRUE) {
  nms <- names(x)

  # ------------------------------------------------------------------
  # Case 1: clusters provided -> row-level grouping (from group_model)
  # ------------------------------------------------------------------
  if (!is.null(clusters)) {
    cluster_nms <- names(x)

    # Strip tna classes, preserve data
    cl_data <- lapply(cluster_nms, function(nm) {
      obj <- x[[nm]]
      list(
        weights = obj$weights,
        inits = obj$inits,
        labels = obj$labels,
        data = obj$data
      )
    })
    names(cl_data) <- cluster_nms

    # Macro data = the assignments
    macro_data <- clusters

    # All groups share the same labels (states)
    all_labels <- x[[1]]$labels
    n_groups <- length(cluster_nms)

    return(structure(
      list(
        macro = list(
          weights = NULL,
          inits = NULL,
          labels = cluster_nms,
          data = macro_data
        ),
        clusters = cl_data,
        cluster_members = NULL,
        meta = list(
          type = type,
          method = method,
          directed = directed,
          n_nodes = length(all_labels),
          n_clusters = n_groups,
          cluster_sizes = vapply(cl_data, function(cl) {
            nrow_data <- if (!is.null(cl$data)) nrow(cl$data) else 0L
            nrow_data
          }, integer(1)),
          source = "group_tna"
        )
      ),
      class = "mcml"
    ))
  }

  # ------------------------------------------------------------------
  # Case 2: no clusters -> node-level grouping (from as_tna)
  # ------------------------------------------------------------------
  cluster_nms <- setdiff(nms, "macro")
  if (length(cluster_nms) == 0) cluster_nms <- nms # nocov

  # Check if labels differ across groups (node-level) or are same (row-level)
  label_sets <- lapply(cluster_nms, function(nm) sort(x[[nm]]$labels))
  all_same <- length(unique(label_sets)) == 1
  if (all_same && length(cluster_nms) > 1) {
    stop("All groups have the same labels -- this is a row-level group_tna. ",
         "Provide clusters argument with row-to-group assignments.",
         call. = FALSE)
  }

  # Node-level: membership from each tna's labels
  cluster_members <- lapply(cluster_nms, function(nm) x[[nm]]$labels)
  names(cluster_members) <- cluster_nms

  # Recover dropped clusters from macro labels
  if ("macro" %in% nms) {
    missing <- setdiff(x[["macro"]]$labels, cluster_nms)
    if (length(missing) > 0) { # nocov start
      assigned <- unlist(cluster_members, use.names = FALSE)
      orig <- NULL
      for (nm in nms) {
        if (!is.null(x[[nm]]$data)) { orig <- x[[nm]]$data; break }
      }
      if (!is.null(orig)) {
        decoded <- .decode_tna_data(orig)
        if (is.data.frame(decoded)) {
          all_nodes <- sort(unique(unlist(decoded, use.names = FALSE)))
          all_nodes <- all_nodes[!is.na(all_nodes)]
          unassigned <- setdiff(all_nodes, assigned)
          if (length(missing) == 1 && length(unassigned) > 0) {
            cluster_members[[missing]] <- unassigned
          }
        }
      }
    } # nocov end
  }

  # Rebuild from data if available
  orig_data <- NULL
  for (nm in nms) {
    if (!is.null(x[[nm]]$data)) { orig_data <- x[[nm]]$data; break } # nocov
  }

  if (!is.null(orig_data)) { # nocov start
    .build_mcml_sequence(.decode_tna_data(orig_data), cluster_members,
                          method, type, directed, compute_within) # nocov end
  } else {
    # No data: reconstruct from weight matrices
    all_nodes <- unlist(cluster_members, use.names = FALSE)
    n <- length(all_nodes)
    mat <- matrix(0, n, n, dimnames = list(all_nodes, all_nodes))
    lapply(cluster_nms, function(nm) {
      w <- x[[nm]]$weights
      labs <- x[[nm]]$labels
      mat[labs, labs] <<- w
    })
    .as_mcml(cluster_summary(mat, cluster_members, method = method,
                              type = type, directed = directed,
                              compute_within = compute_within))
  }
}

#' Detect input type for summarize_clusters
#' @keywords internal
#' @noRd
.detect_mcml_input <- function(x) {
  if (inherits(x, "group_tna")) return("group_tna")

  if (inherits(x, "tna")) {
    if (!is.null(x$data)) return("tna_data")
    return("tna_matrix")
  }

  if (inherits(x, "cograph_network")) {
    if (!is.null(x$data)) return("cograph_data")
    return("cograph_matrix")
  }

  if (is.data.frame(x)) {
    col_names <- tolower(names(x))
    # Default data.frame names V1, V2, ... mark time steps of sequence data
    # (as.data.frame() of a sequence matrix), not from/to columns.
    # Two such columns read the same either way, so only 3+ are switched.
    if (length(col_names) >= 3L && all(grepl("^v[0-9]+$", col_names))) {
      return("sequence")
    }
    from_cols <- c("from", "source", "src", "v1", "node1", "i")
    to_cols <- c("to", "target", "tgt", "v2", "node2", "j")
    has_from <- any(from_cols %in% col_names)
    has_to <- any(to_cols %in% col_names)
    if (has_from && has_to) return("edgelist")
    return("sequence")
  }

  if (is.matrix(x)) {
    if (is.numeric(x) && nrow(x) == ncol(x)) return("matrix")
    return("sequence")
  }

  "unknown"
}

#' Auto-detect clusters from cograph_network
#' @keywords internal
#' @noRd
.auto_detect_clusters <- function(x) {
  clusters <- NULL
  if (!is.null(x$nodes)) {
    cluster_cols <- c("clusters", "cluster", "groups", "group")
    for (col in cluster_cols) {
      if (col %in% names(x$nodes)) {
        clusters <- x$nodes[[col]]
        break
      }
    }
  }
  if (is.null(clusters) && !is.null(x$node_groups)) {
    ng <- x$node_groups
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
  clusters
}

#' Build node-to-cluster lookup from cluster specification
#' @keywords internal
#' @noRd
.build_cluster_lookup <- function(clusters, all_nodes) {
  if (is.data.frame(clusters)) { # nocov start
    # Defensive: .normalize_clusters converts df to list before this is called
    stopifnot(ncol(clusters) >= 2)
    nodes <- as.character(clusters[[1]])
    groups <- as.character(clusters[[2]])
    clusters <- split(nodes, groups)
  } # nocov end

  if (is.list(clusters) && !is.data.frame(clusters)) {
    # Named list: cluster_name -> node vector
    lookup <- character(0)
    for (cl_name in names(clusters)) {
      nodes <- clusters[[cl_name]]
      lookup[nodes] <- cl_name
    }
    # Verify all nodes are mapped
    unmapped <- setdiff(all_nodes, names(lookup))
    if (length(unmapped) > 0) {
      stop("Unmapped nodes: ",
           paste(utils::head(unmapped, 5), collapse = ", "),
           call. = FALSE)
    }
    return(lookup)
  }

  if (is.character(clusters) || is.factor(clusters)) {
    clusters <- as.character(clusters)
    if (length(clusters) != length(all_nodes)) {
      stop("Membership vector length (", length(clusters),
           ") must equal number of unique nodes (", length(all_nodes), ")",
           call. = FALSE)
    }
    lookup <- setNames(clusters, all_nodes)
    return(lookup)
  }

  if (is.numeric(clusters) || is.integer(clusters)) {
    if (length(clusters) != length(all_nodes)) {
      stop("Membership vector length (", length(clusters),
           ") must equal number of unique nodes (", length(all_nodes), ")",
           call. = FALSE)
    }
    lookup <- setNames(as.character(clusters), all_nodes)
    return(lookup)
  }

  stop("clusters must be a named list, character/numeric vector, or column name",
       call. = FALSE)
}

#' Build cluster_summary from transition vectors
#' @keywords internal
#' @noRd
.build_from_transitions <- function(from_nodes, to_nodes, weights,
                                     cluster_lookup, cluster_list,
                                     method, type, directed,
                                     compute_within, data = NULL) {

  # Sort clusters alphabetically (TNA convention)
  cluster_list <- cluster_list[order(names(cluster_list))]
  cluster_names <- names(cluster_list)
  n_clusters <- length(cluster_names)

  # The edges table reports the transitions as observed.
  edges_from <- from_nodes
  edges_to <- to_nodes
  edges_weight <- weights

  # Undirected: each transition counts half in each direction, which is the
  # node-level symmetrization (A + t(A)) / 2 applied before aggregation.
  if (!isTRUE(directed)) {
    from_nodes <- c(edges_from, edges_to)
    to_nodes <- c(edges_to, edges_from)
    weights <- c(edges_weight, edges_weight) / 2
  }

  # Recode to cluster labels
  from_clusters <- cluster_lookup[from_nodes]
  to_clusters <- cluster_lookup[to_nodes]

  # ---- Macro (cluster-level) matrix (includes diagonal = per-cluster loops) ----
  between_raw <- matrix(0, n_clusters, n_clusters,
                        dimnames = list(cluster_names, cluster_names))

  # Include ALL transitions -- node-level self-loops (A->A) are valid
  # cluster-level self-loops (e.g. discuss->discuss = Social->Social)
  b_from <- from_clusters
  b_to <- to_clusters
  b_w <- weights

  if (length(b_from) > 0) {
    # Build pair keys and aggregate
    pair_keys <- paste(b_from, b_to, sep = "\t")
    names(b_w) <- pair_keys
    agg_vals <- tapply(b_w, pair_keys, function(w) {
      n_possible <- NULL
      if (method == "density") {
        parts <- strsplit(names(w)[1], "\t")[[1]]
        n_i <- length(cluster_list[[parts[1]]])
        n_j <- length(cluster_list[[parts[2]]])
        n_possible <- n_i * n_j
      }
      aggregate_weights(w, method, n_possible)
    })

    for (key in names(agg_vals)) {
      parts <- strsplit(key, "\t")[[1]]
      between_raw[parts[1], parts[2]] <- agg_vals[[key]]
    }
  }

  # Process macro weights
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

  # Preserve original data as-is (no recoding or filtering)
  between <- list(
    weights = between_weights,
    inits = between_inits,
    labels = cluster_names,
    data = data
  )

  # ---- Per-cluster matrices ----
  cl_data <- NULL
  if (isTRUE(compute_within)) {
    # Filter intra-cluster transitions (same cluster, including self-loops)
    is_intra <- from_clusters == to_clusters
    w_from <- from_nodes[is_intra]
    w_to <- to_nodes[is_intra]
    w_w <- weights[is_intra]

    cl_data <- lapply(seq_len(n_clusters), function(i) {
      cl_name <- cluster_names[i]
      cl_nodes <- cluster_list[[cl_name]]
      n_i <- length(cl_nodes)

      if (n_i <= 1) {
        # Single node: build matrix from self-loop transitions
        in_cluster <- w_from %in% cl_nodes & w_to %in% cl_nodes
        self_w <- w_w[in_cluster]
        self_val <- if (length(self_w) > 0) {
          aggregate_weights(self_w, method)
        } else {
          0
        }
        cl_raw <- matrix(self_val, 1, 1,
                          dimnames = list(cl_nodes, cl_nodes))
        cl_weights_i <- .process_weights(cl_raw, type, directed)
        cl_inits_i <- setNames(1, cl_nodes)
      } else {
        # Filter transitions for this cluster (self-loops preserved)
        in_cluster <- w_from %in% cl_nodes & w_to %in% cl_nodes

        keep <- in_cluster
        cf <- w_from[keep]
        ct <- w_to[keep]
        cw <- w_w[keep]

        cl_raw <- matrix(0, n_i, n_i,
                          dimnames = list(cl_nodes, cl_nodes))

        if (length(cf) > 0) {
          pair_keys <- paste(cf, ct, sep = "\t")
          agg_vals <- tapply(cw, pair_keys, function(w) {
            aggregate_weights(w, method)
          })
          for (key in names(agg_vals)) {
            parts <- strsplit(key, "\t")[[1]]
            cl_raw[parts[1], parts[2]] <- agg_vals[[key]]
          }
        }

        cl_weights_i <- .process_weights(cl_raw, type, directed)

        col_sums_w <- colSums(cl_raw, na.rm = TRUE)
        total_w <- sum(col_sums_w, na.rm = TRUE)
        cl_inits_i <- if (!is.na(total_w) && total_w > 0) {
          col_sums_w / total_w
        } else {
          rep(1 / n_i, n_i)
        }
        names(cl_inits_i) <- cl_nodes
      }

      list(
        weights = cl_weights_i,
        inits = cl_inits_i,
        labels = cl_nodes,
        data = data
      )
    })
    names(cl_data) <- cluster_names
  }

  # ---- Edges data.frame ----
  edges_cl_from <- unname(cluster_lookup[edges_from])
  edges_cl_to <- unname(cluster_lookup[edges_to])
  edge_type <- ifelse(edges_cl_from == edges_cl_to, "within", "between")
  edges <- data.frame(
    from = edges_from,
    to = edges_to,
    weight = edges_weight,
    cluster_from = edges_cl_from,
    cluster_to = edges_cl_to,
    type = edge_type,
    stringsAsFactors = FALSE
  )

  # ---- Assemble result ----
  all_nodes <- sort(unique(c(from_nodes, to_nodes)))
  n_nodes <- length(all_nodes)

  structure(
    list(
      macro = between,
      clusters = cl_data,
      edges = edges,
      data = data,
      cluster_members = cluster_list,
      meta = list(
        type = type,
        method = method,
        # Effective directedness of the stored weights: type =
        # "cooccurrence" symmetrizes regardless of the directed argument.
        directed = isTRUE(directed) && type != "cooccurrence",
        n_nodes = n_nodes,
        n_clusters = n_clusters,
        cluster_sizes = vapply(cluster_list, length, integer(1)),
        source = "transitions"
      )
    ),
    class = "mcml"
  )
}

#' Build MCML from edge list data.frame
#' @keywords internal
#' @noRd
.build_mcml_edgelist <- function(df, clusters, method, type,
                                  directed, compute_within) {

  col_names <- tolower(names(df))

  # Detect from/to columns
  from_col <- which(col_names %in% c("from", "source", "src",
                                       "v1", "node1", "i"))[1]
  if (is.na(from_col)) from_col <- 1L

  to_col <- which(col_names %in% c("to", "target", "tgt",
                                     "v2", "node2", "j"))[1]
  if (is.na(to_col)) to_col <- 2L

  # Detect weight column
  weight_col <- which(col_names %in% c("weight", "w", "value", "strength"))[1]
  has_weight <- !is.na(weight_col)

  from_vals <- as.character(df[[from_col]])
  to_vals <- as.character(df[[to_col]])
  weights <- if (has_weight) as.numeric(df[[weight_col]]) else rep(1, nrow(df))

  # Remove rows with NA in from/to
  valid <- !is.na(from_vals) & !is.na(to_vals)
  from_vals <- from_vals[valid]
  to_vals <- to_vals[valid]
  weights <- weights[valid]

  all_nodes <- sort(unique(c(from_vals, to_vals)))

  # Handle clusters parameter
  if (is.character(clusters) && length(clusters) == 1 &&
      clusters %in% names(df)) {
    # Column name: build lookup from both from+group and to+group
    group_col <- df[[clusters]]
    group_col <- as.character(group_col[valid])

    # Build mapping from from-side
    from_map <- setNames(group_col, from_vals)
    # Build mapping from to-side
    to_map <- setNames(group_col, to_vals)
    # Merge (from takes priority if conflicting, but shouldn't)
    full_map <- c(to_map, from_map)
    # Keep unique node -> cluster mapping
    full_map <- full_map[!duplicated(names(full_map))]

    # Build cluster_list
    cluster_list <- split(names(full_map), unname(full_map))
    cluster_list <- lapply(cluster_list, sort)
    cluster_lookup <- full_map

    # Re-derive all_nodes from the lookup
    all_nodes <- sort(names(cluster_lookup))
  } else {
    # List or vector clusters
    if (is.null(clusters)) {
      stop("clusters argument is required for data.frame input", call. = FALSE)
    }

    if (is.list(clusters) && !is.data.frame(clusters)) {
      cluster_list <- clusters
    } else {
      # Membership vector
      cluster_list <- .normalize_clusters(clusters, all_nodes)
    }

    cluster_lookup <- .build_cluster_lookup(cluster_list, all_nodes)
  }

  .build_from_transitions(from_vals, to_vals, weights,
                            cluster_lookup, cluster_list,
                            method, type, directed, compute_within,
                            data = df)
}

#' Build MCML from sequence data.frame
#' @keywords internal
#' @noRd
.build_mcml_sequence <- function(df, clusters, method, type,
                                  directed, compute_within) {

  if (is.matrix(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)

  stopifnot(is.data.frame(df))

  nc <- ncol(df)
  if (nc < 2) {
    stop("Sequence data must have at least 2 columns (time steps)",
         call. = FALSE)
  }

  # Extract consecutive pairs: (col[t], col[t+1]) for all rows
  pairs <- lapply(seq_len(nc - 1), function(t) {
    from_t <- as.character(df[[t]])
    to_t <- as.character(df[[t + 1]])
    data.frame(from = from_t, to = to_t, stringsAsFactors = FALSE)
  })
  pairs <- do.call(rbind, pairs)

  # Remove NA pairs
  valid <- !is.na(pairs$from) & !is.na(pairs$to)
  from_vals <- pairs$from[valid]
  to_vals <- pairs$to[valid]
  weights <- rep(1, length(from_vals))

  all_nodes <- sort(unique(c(from_vals, to_vals)))

  if (is.null(clusters)) {
    stop("clusters argument is required for sequence data", call. = FALSE)
  }

  if (is.list(clusters) && !is.data.frame(clusters)) {
    cluster_list <- clusters
  } else {
    cluster_list <- .normalize_clusters(clusters, all_nodes)
  }

  cluster_lookup <- .build_cluster_lookup(cluster_list, all_nodes)

  .build_from_transitions(from_vals, to_vals, weights,
                            cluster_lookup, cluster_list,
                            method, type, directed, compute_within,
                            data = df)
}

#' Process weights based on type
#' @keywords internal
#' @noRd
.process_weights <- function(raw_weights, type, directed = TRUE) {
  if (type == "raw" || type == "frequency") {
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
  raw_weights # nocov
}

#' Convert cluster_summary to tna Objects
#'
#' Converts a \code{cluster_summary} or \code{mcml} object to tna models that
#' can be used with the functions of the tna package. The result holds a macro
#' (cluster-level) model and one model of the internal transitions of each
#' cluster, as a flat \code{group_tna} object.
#'
#' @param x A \code{cluster_summary} object created by \code{\link{csum}}, or
#'   an \code{mcml} object created by \code{\link{summarize_clusters}}. A tna
#'   object is returned unchanged, and any other input is an error. The
#'   weights are passed to \code{tna::tna()}, which row-normalizes them, so a
#'   summary computed with \code{type = "raw"} gives the same transition
#'   probabilities as one computed with \code{type = "tna"}.
#'
#' @return A \code{group_tna} object, a flat named list of tna objects. The
#'   first element is named \code{"macro"} and holds the cluster-level
#'   transitions. The remaining elements are named by cluster and hold the
#'   internal transitions of each cluster.
#'   \describe{
#'     \item{macro}{A tna object of cluster-level transitions, with
#'       \code{weights} (k x k transition matrix), \code{inits} (initial
#'       distribution) and \code{labels} (cluster names).}
#'     \item{<cluster_name>}{One tna object per cluster, with \code{weights}
#'       (n_i x n_i matrix), \code{inits} (initial distribution) and
#'       \code{labels} (node labels). A cluster that cannot become a tna
#'       model is left out with a warning (see Excluded Clusters).}
#'   }
#'
#' @details
#' ## Requirements
#'
#' The tna package must be installed. Without it, the function raises an
#' error.
#'
#' ## Excluded Clusters
#'
#' A per-cluster tna cannot be created when:
#' \itemize{
#'   \item The cluster has only 1 node (no internal transitions possible)
#'   \item Some nodes in the cluster have no outgoing edges (row sums to 0)
#' }
#'
#' These clusters are left out of the result with a warning of class
#' \code{cograph_cluster_dropped}, which names each cluster and the nodes
#' that have no transition within it. The macro (cluster-level) model still
#' includes all clusters.
#'
#' @export
#' @seealso
#'   \code{\link{csum}} to create the input object,
#'   \code{\link{plot_mcml}} for visualization without conversion,
#'   \code{tna::tna} for the underlying tna constructor
#'
#' @examplesIf requireNamespace("tna", quietly = TRUE)
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' as_tna(csum(regulation_net, clusters = clusters, type = "tna"))
as_tna <- function(x) {
  UseMethod("as_tna")
}

#' @rdname as_tna
#' @return A \code{group_tna} object (flat list of tna objects: macro + per-cluster).
#' @export
as_tna.cluster_summary <- function(x) {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' is required for as_tna()", call. = FALSE) # nocov
  }

  # Macro (cluster-level) tna
  between_tna <- tna::tna(x$macro$weights, inits = x$macro$inits)
  between_tna$data <- x$macro$data

  within_tnas <- .within_cluster_tnas(x$clusters)

  # Combine macro + all cluster tnas into flat group_tna
  all_tnas <- c(list(macro = between_tna), within_tnas)
  class(all_tnas) <- "group_tna"
  all_tnas
}

#' @rdname as_tna
#' @return A \code{group_tna} object (flat list of tna objects: macro + per-cluster).
#' @export
as_tna.mcml <- function(x) {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' is required for as_tna()", call. = FALSE) # nocov
  }

  # Macro (cluster-level) tna
  between_tna <- tna::tna(x$macro$weights, inits = x$macro$inits)
  between_tna$data <- x$macro$data

  within_tnas <- .within_cluster_tnas(x$clusters)

  all_tnas <- c(list(macro = between_tna), within_tnas)
  class(all_tnas) <- "group_tna"
  all_tnas
}

# Build one tna model per cluster from its within-cluster weights. tna::tna()
# needs every row to have a positive sum, so a cluster in which some node has
# no transition to another node of the same cluster (every one-node cluster
# included) cannot become a model. Those clusters are left out, and a single
# warning of class `cograph_cluster_dropped` names each one and its nodes.
.within_cluster_tnas <- function(clusters) {
  if (is.null(clusters)) return(list())
  zero_nodes <- lapply(clusters, function(cl) {
    w <- cl$weights
    nodes <- rownames(w) %||% as.character(seq_len(nrow(w)))
    nodes[!(rowSums(w) > 0)]
  })
  dropped <- lengths(zero_nodes) > 0
  if (any(dropped)) {
    quote_list <- function(v) {
      v <- sprintf("\"%s\"", v)
      if (length(v) == 1L) return(v)
      paste(paste(v[-length(v)], collapse = ", "), "and", v[length(v)])
    }
    detail <- vapply(names(clusters)[dropped], function(cl) {
      nodes <- zero_nodes[[cl]]
      if (nrow(clusters[[cl]]$weights) == 1L) {
        return(sprintf("\"%s\": single-node cluster (%s).", cl,
                       quote_list(nodes)))
      }
      sprintf("\"%s\": no outgoing within-cluster transitions from %s.",
              cl, quote_list(nodes))
    }, character(1))
    n_dropped <- sum(dropped)
    warning(warningCondition(
      paste0(
        if (n_dropped == 1L) {
          "The within-cluster TNA model for 1 cluster was not estimated"
        } else {
          sprintf("Within-cluster TNA models for %d clusters were not estimated",
                  n_dropped)
        },
        ": transition probabilities are undefined for a node with no ",
        "outgoing within-cluster transitions.\n",
        paste0("  * ", detail, collapse = "\n")
      ),
      class = "cograph_cluster_dropped", call = NULL
    ))
  }
  lapply(clusters[!dropped], function(cl) {
    obj <- tna::tna(cl$weights, inits = cl$inits)
    obj$data <- cl$data
    obj
  })
}

#' @rdname as_tna
#' @return A \code{tna} object constructed from the input.
#' @export
as_tna.default <- function(x) {
 if (inherits(x, "tna")) {
    return(x)
  }
  stop("Cannot convert object of class '", class(x)[1], "' to tna", call. = FALSE)
}

# print.cluster_tna removed -- as_tna() now returns group_tna directly,
# which has its own print method via the tna package.

# ==============================================================================
# 8. as_mcml -- Convert to mcml
# ==============================================================================

#' Convert to mcml
#'
#' Converts an object to the \code{mcml} class, a representation of a
#' multi-cluster network that does not depend on the tna package.
#'
#' @param x A \code{cluster_summary}, \code{group_tna} or \code{mcml} object.
#'   Any other input is an error; \code{\link{summarize_clusters}} builds an
#'   \code{mcml} object from raw data.
#' @param ... Additional arguments passed to methods.
#' @return An \code{mcml} object with components \code{macro}, \code{clusters},
#'   \code{cluster_members}, and \code{meta}.
#' @seealso \code{\link{summarize_clusters}}, \code{\link{as_tna}}
#' @export
#'
#' @examples
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' as_mcml(csum(regulation_net, clusters = clusters, type = "tna"))
as_mcml <- function(x, ...) {
  UseMethod("as_mcml")
}

#' @rdname as_mcml
#' @return An \code{mcml} object.
#' @export
as_mcml.cluster_summary <- function(x, ...) {
  .as_mcml(x)
}

#' @rdname as_mcml
#' @param clusters Integer or character vector of row-to-group assignments.
#'   Required when the \code{group_tna} has the same labels across all groups
#'   (row-level clustering from \code{tna::group_model(cluster_data(...))}).
#' @param method Aggregation method for macro weights (default \code{"sum"}).
#' @param type Transition type (default \code{"tna"}).
#' @param directed Logical; whether the network is directed (default \code{TRUE}).
#' @return An \code{mcml} object. When \code{clusters} is provided,
#'   \code{macro$data} contains the cluster assignments and \code{macro$weights}
#'   is \code{NULL}.
#' @export
as_mcml.group_tna <- function(x, clusters = NULL, method = "sum",
                               type = "tna", directed = TRUE, ...) {
  .group_tna_to_mcml(x, clusters = clusters, method = method,
                      type = type, directed = directed,
                      compute_within = TRUE)
}

#' @rdname as_mcml
#' @return The input \code{mcml} object unchanged.
#' @export
as_mcml.mcml <- function(x, ...) {
  x
}

#' @rdname as_mcml
#' @export
as_mcml.default <- function(x, ...) {
  if (inherits(x, "mcml")) return(x) # nocov
  stop("Cannot convert object of class '", class(x)[1], "' to mcml. ",
       "Use summarize_clusters() for raw data inputs.", call. = FALSE)
}

#' Normalize cluster specification to list format
#' @keywords internal
#' @noRd
.normalize_clusters <- function(clusters, node_names) {
  if (is.data.frame(clusters)) {
    # Data frame with node and group columns
    stopifnot(ncol(clusters) >= 2)
    nodes <- as.character(clusters[[1]])
    groups <- as.character(clusters[[2]])
    clusters <- split(nodes, groups)
  }

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
#' @param x Adjacency matrix (numeric)
#' @param clusters Cluster specification (named list, data frame, or membership
#'   vector; see \code{\link{csum}})
#' @param weighted Logical; if TRUE (default), use edge weights; if FALSE,
#'   binarize the matrix first
#' @param directed Logical; if TRUE (default), treat as directed network
#' @return A `cluster_quality` object (a list) with:
#'   \item{per_cluster}{Data frame, one row per cluster, with columns
#'     \code{cluster} (index), \code{cluster_name}, \code{n_nodes},
#'     \code{internal_edges} (within-cluster weight), \code{cut_edges}
#'     (boundary-crossing weight), \code{internal_density},
#'     \code{avg_internal_degree}, \code{expansion}, \code{cut_ratio} and
#'     \code{conductance}.}
#'   \item{global}{List with \code{modularity} (Newman-Girvan, computed on the
#'     weighted or binarized matrix), \code{coverage} (share of total weight
#'     that is internal to some cluster) and \code{n_clusters}.}
#' @export
#' @examples
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' cluster_quality(regulation_net, clusters = clusters)
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

    if (n_S == 0) { # nocov start
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
    } # nocov end

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
    # Density counts node pairs, so self-loops stay out of the numerator
    # just as they are out of the n_S * (n_S - 1) denominator.
    m_S_pairs <- sum(A[S, S]) - sum(diag(A)[S])
    if (!directed) m_S_pairs <- m_S_pairs / 2
    max_internal <- n_S * (n_S - 1)
    if (!directed) max_internal <- max_internal / 2
    internal_density <- if (max_internal > 0) m_S_pairs / max_internal else NA_real_

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
#' @return See \code{\link{cluster_quality}}.
#' @export
cqual <- cluster_quality

#' Compute modularity
#' @keywords internal
#' @noRd
.compute_modularity <- function(A, membership, directed = TRUE) {
  # Both directed and undirected use m_total = sum(A). For symmetric A the
  # double-sum counts each edge twice, making this algebraically equivalent
  # to the standard Newman-Girvan 1/(2m) formulation.
  m_total <- sum(A)
  if (m_total == 0) return(NA_real_)

  k_out <- rowSums(A)
  k_in <- if (directed) colSums(A) else k_out

  # Cluster-wise sum: O(sum of cluster_size^2), no n*n matrix allocation
  Q <- sum(vapply(unique(membership), function(c) {
    idx <- which(membership == c)
    sum(A[idx, idx]) - sum(k_out[idx]) * sum(k_in[idx]) / m_total
  }, numeric(1)))

  Q / m_total
}

# ==============================================================================
# 3b. Cluster Significance Testing
# ==============================================================================

#' Test Significance of Community Structure
#'
#' Compares observed modularity against a null model distribution to assess
#' whether the detected community structure is statistically significant.
#'
#' @importFrom stats sd pnorm
#'
#' @param x Network input: adjacency matrix, igraph object, or cograph_network.
#' @param communities A communities object (from \code{\link{communities}} or
#'   igraph) or a membership vector (integer vector where \code{communities[i]}
#'   is the community of node i).
#' @param n_random Number of random networks to generate for the null
#'   distribution. Default 100.
#' @param method Null model type:
#'   \describe{
#'     \item{"configuration"}{(default) Undirected configuration model
#'       that preserves the total degree of each node.}
#'     \item{"gnm"}{Erdos-Renyi G(n, m) model with the same number of nodes
#'       and edges and the same directedness.}
#'   }
#' @param null Which null question to answer. Default \code{"detect"}:
#'   \describe{
#'     \item{"detect"}{The null value is the modularity of the partition
#'       found by community detection on each null graph.}
#'     \item{"fixed"}{The null value is the modularity of the supplied
#'       \code{communities} membership evaluated on each null graph.}
#'   }
#' @param seed Random seed for reproducibility. Default NULL.
#'
#' @return A \code{cograph_cluster_significance} object with:
#'   \describe{
#'     \item{observed_modularity}{Modularity of the input communities}
#'     \item{null_mean}{Mean modularity of random networks}
#'     \item{null_sd}{Standard deviation of null modularity}
#'     \item{z_score}{Standardized score (observed - null_mean) / null_sd,
#'       or \code{NA} when \code{null_sd} is zero}
#'     \item{p_value}{One-sided upper-tail p-value of \code{z_score} under
#'       the standard normal distribution; \code{NA} when \code{null_sd} is
#'       zero}
#'     \item{null_values}{Vector of modularity values from null distribution}
#'     \item{method}{Null model method used}
#'     \item{null}{Which null question was asked ("detect" or "fixed")}
#'     \item{n_random}{Number of random networks generated}
#'   }
#'
#' @details
#' The function generates \code{n_random} random networks from the null model.
#' With \code{null = "detect"}, community detection (Louvain, or fast greedy
#' when Louvain fails) is run on each null network and its modularity is
#' recorded. A low p-value then indicates that the observed partition is
#' stronger than the partitions detection recovers on random networks. With
#' \code{null = "fixed"}, the supplied membership is evaluated on each null
#' network. A low p-value then indicates that the partition explains more
#' structure in the observed network than in random networks, independently
#' of any detection algorithm.
#'
#' The observed modularity is computed with the edge weights of \code{x}.
#' When \code{x} is weighted, each null network receives the observed edge
#' weights, randomly reassigned to its edges, so the observed and null
#' modularity are on the same scale.
#'
#' @references
#' Reichardt, J., & Bornholdt, S. (2006).
#' Statistical mechanics of community detection.
#' \emph{Physical Review E}, 74, 016110.
#'
#' @export
#' @seealso \code{\link{communities}}, \code{\link{cluster_quality}}
#'
#' @section Printing and plotting:
#' Printing the result shows the null model, the observed and null modularity,
#' the z-score and the p-value. \code{plot()} on the result plots a histogram
#' of the null modularity values with the observed value marked.
#'
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' comm <- communities(regulation_net, method = "walktrap")
#' cluster_significance(regulation_net, comm, n_random = 20, seed = 1)
cluster_significance <- function(x,
                                  communities,
                                  n_random = 100,
                                  method = c("configuration", "gnm"),
                                  null = c("detect", "fixed"),
                                  seed = NULL) {

  method <- match.arg(method)
  null <- match.arg(null)
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

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
    mem <- membership(communities)
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
  # Observed edge weights; each null graph receives them in random order so
  # observed and null modularity are on the same (weighted) scale.
  obs_weights <- igraph::E(g)$weight

  for (i in seq_len(n_random)) {
    if (method == "configuration") {
      # Configuration model - preserve degree sequence
      deg <- igraph::degree(g)
      g_null <- tryCatch(
        igraph::sample_degseq(deg, method = "configuration"),
        error = function(e) { # nocov start
          # Fallback to configuration.simple if configuration fails
          igraph::sample_degseq(deg, method = "configuration.simple")
        } # nocov end
      )
    } else {
      # G(n,m) model - same number of nodes and edges
      g_null <- igraph::sample_gnm(n_nodes, n_edges, directed = igraph::is_directed(g))
    }
    if (!is.null(obs_weights)) {
      m_null <- igraph::ecount(g_null)
      igraph::E(g_null)$weight <- if (m_null == length(obs_weights)) {
        obs_weights[sample.int(length(obs_weights))]
      } else { # nocov start
        obs_weights[sample.int(length(obs_weights), m_null, replace = TRUE)]
      } # nocov end
    }

    if (null == "detect") {
      # Historical default: re-run detection on the null graph and record the
      # best-partition modularity. Compares observed structure to what
      # detection would recover on similar random graphs.
      comm_null <- tryCatch(
        igraph::cluster_louvain(g_null),
        error = function(e) { # nocov start
          igraph::cluster_fast_greedy(igraph::as_undirected(g_null))
        } # nocov end
      )
      null_mods[i] <- igraph::modularity(comm_null)
    } else {
      # null == "fixed": evaluate the SUPPLIED partition on the null graph.
      # Isolates the partition's quality from detector behavior.
      null_mods[i] <- tryCatch(
        igraph::modularity(g_null, mem),
        error = function(e) NA_real_
      )
    }
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
    null = null,
    n_random = n_random
  )
  class(result) <- "cograph_cluster_significance"
  result
}

#' @rdname cluster_significance
#' @export
csig <- cluster_significance

#' @noRd
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

#' @noRd
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
#' @param A2 Second adjacency matrix, with the same dimensions as \code{A1}
#' @param method Comparison method: "jaccard" (default), "overlap", "hamming",
#'   "cosine" or "pearson"
#' @return A single numeric value. All methods except \code{"hamming"} return a
#'   similarity, where higher values mean more alike layers. \code{"hamming"}
#'   returns a distance, the number of matrix cells whose edge presence
#'   differs between the two layers. Lower values then mean more alike layers,
#'   and the value is not bounded by 1. \code{NA} is returned when the
#'   denominator is undefined
#'   (\code{"jaccard"} with no edges in either layer, \code{"overlap"} with an
#'   empty layer, \code{"cosine"} with an all-zero layer).
#'
#' @details
#' \code{"jaccard"}, \code{"overlap"} and \code{"hamming"} compare edge
#' presence (\code{A > 0}) and ignore weights. \code{"cosine"} and
#' \code{"pearson"} are computed on the cell values, diagonal included.
#' Matrices of different dimensions raise an error.
#' @export
#' @examples
#' layer_similarity(regulation_net, t(regulation_net), method = "cosine")
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
#' Computes the similarity of every pair of layers with
#' \code{\link{layer_similarity}}.
#'
#' @param layers Named list of adjacency matrices (one per layer); at least two
#'   are required.
#' @param method Comparison method: "jaccard" (default), "overlap", "cosine" or
#'   "pearson". The \code{"hamming"} distance is not accepted.
#' @return A symmetric L x L matrix of pairwise similarities with 1 on the
#'   diagonal. The dimnames are the layer names, or \code{"Layer1"},
#'   \code{"Layer2"}, ... for an unnamed list.
#' @export
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' layer_similarity_matrix(layers, method = "cosine")
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
#' Measures the consistency of hubs across layers as the Pearson correlation
#' of node degrees between layers.
#'
#' @param layers List of adjacency matrices of the same dimensions
#' @param mode Degree type: "total" (default, row plus column sums), "in"
#'   (column sums) or "out" (row sums). The sums use the edge weights, so on
#'   a weighted layer the degree is the node strength.
#' @return An L x L Pearson correlation matrix of the layer degree sequences,
#'   with the layer names (or \code{"Layer1"}, \code{"Layer2"}, ...) as
#'   dimnames.
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' layer_degree_correlation(layers, mode = "total")
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
#' Builds the supra-adjacency matrix of a multilayer network. The diagonal
#' blocks hold the intra-layer adjacencies and the off-diagonal blocks the
#' inter-layer coupling.
#'
#' @param layers List of adjacency matrices (same dimensions)
#' @param omega Inter-layer coupling coefficient, a scalar or an L x L matrix.
#'   Default 1. For a matrix, entry \code{[a, b]} with \code{a < b} sets the
#'   coupling of layers a and b.
#' @param coupling Coupling type. \code{"diagonal"} (default) couples each
#'   node to its own copy in the other layers with weight \code{omega}.
#'   \code{"full"} couples every node to every node of the other layers with
#'   weight \code{omega}. \code{"custom"} uses \code{interlayer_matrices}.
#' @param interlayer_matrices For \code{coupling = "custom"}, a list of
#'   inter-layer matrices. Accepted shapes:
#'   \itemize{
#'     \item Named list with keys \code{"a_b"} (integer layer indices) or
#'       \code{"<layer_name_a>_<layer_name_b>"}; either order works.
#'     \item Unnamed list of length \code{choose(L, 2)} giving every pair
#'       in upper-triangle row-major order: \code{(1,2), (1,3), ..., (1,L),
#'       (2,3), ..., (L-1,L)}.
#'     \item Unnamed list of length \code{L-1} giving adjacent pairs only.
#'       Entry \code{i} is the coupling for \code{(i, i+1)}.
#'   }
#'   The block of layers b and a is the transpose of the block of a and b.
#'   A pair with no matching entry receives the diagonal coupling
#'   \code{omega[a,b] * I} with a warning. A \code{NULL} value with
#'   \code{coupling = "custom"} is an error.
#' @return A supra-adjacency matrix of dimension (N*L) x (N*L) with class
#'   \code{c("supra_adjacency", "matrix")}. Diagonal N x N blocks hold the
#'   intra-layer adjacencies and off-diagonal blocks the inter-layer coupling.
#'   The attributes \code{"n_nodes"}, \code{"n_layers"}, \code{"node_names"},
#'   \code{"layer_names"}, \code{"omega"} and \code{"coupling"} record the
#'   construction and are read back by \code{\link{supra_layer}()} and
#'   \code{\link{supra_interlayer}()}.
#' @export
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' supra_adjacency(layers, omega = 0.5)
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

    # Pre-compute an upper-triangle → list-position lookup once per call so
    # the hot inner loop just does table reads (formerly ran a full which()
    # per pair and silently returned the diagonal default for non-adjacent
    # (a, b) — see docs for the accepted interlayer_matrices shapes).
    n_pairs_full <- L * (L - 1L) / 2L
    lookup_pair <- function(a, b) {
      if (is.null(interlayer_matrices)) return(NULL)
      nms <- names(interlayer_matrices)
      if (!is.null(nms)) {
        keys <- c(paste0(a, "_", b), paste0(b, "_", a),
                  paste0(layer_names[a], "_", layer_names[b]),
                  paste0(layer_names[b], "_", layer_names[a]))
        for (k in keys) {
          if (k %in% nms) return(interlayer_matrices[[k]])
        }
      }
      if (length(interlayer_matrices) == n_pairs_full) {
        # upper-tri row-major index
        pos <- (a - 1L) * (L - a / 2L) + (b - a)
        return(interlayer_matrices[[as.integer(pos)]])
      }
      if (length(interlayer_matrices) == L - 1L && b == a + 1L) {
        # legacy adjacent-chain layout
        return(interlayer_matrices[[a]])
      }
      NULL
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
            mat_ab <- lookup_pair(a, b)
            if (is.null(mat_ab)) {
              warning(sprintf(
                "supra_adjacency: no custom interlayer matrix for pair (%d, %d); using omega diagonal fallback",
                a, b), call. = FALSE)
              mat_ab <- omega_matrix[a, b] * I
            }
            mat_ab
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
#' @param x Supra-adjacency matrix from \code{\link{supra_adjacency}}
#' @param layer Integer index of the layer to extract
#' @return The N x N intra-layer adjacency matrix, with the node names as
#'   dimnames. An index outside \code{1:L} raises an error.
#' @export
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' supra <- supra_adjacency(layers, omega = 0.5)
#' supra_layer(supra, layer = 2)
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
#' @param x Supra-adjacency matrix from \code{\link{supra_adjacency}}
#' @param from Integer index of the source layer
#' @param to Integer index of the target layer
#' @return The N x N inter-layer block, with the supra-matrix labels
#'   (\code{"<layer>_<node>"}) as dimnames. An index outside \code{1:L}
#'   raises an error.
#' @export
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' supra <- supra_adjacency(layers, omega = 0.5)
#' supra_interlayer(supra, from = 1, to = 2)
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
#' @param layers List of adjacency matrices of the same dimensions
#' @param method Aggregation: "sum" (default), "mean", "max", "min", "union"
#'   or "intersection". \code{"union"} and \code{"intersection"} return a
#'   binary matrix of the cells with a positive weight in any or in every
#'   layer.
#' @param weights Optional numeric vector of layer weights, one per layer,
#'   used only by \code{method = "sum"} to compute a weighted sum.
#' @return The aggregated adjacency matrix, with the dimnames of the first
#'   layer. A list with a single layer is aggregated the same way, so
#'   \code{"union"} and \code{"intersection"} binarize it and \code{"sum"}
#'   multiplies it by its layer weight.
#' @export
#' @examples
#' layers <- list(forward = regulation_net, backward = t(regulation_net))
#' aggregate_layers(layers, method = "mean")
aggregate_layers <- function(layers,
                             method = c("sum", "mean", "max", "min",
                                        "union", "intersection"),
                             weights = NULL) {
  method <- match.arg(method)
  L <- length(layers)

  if (L == 0) stop("Need at least 1 layer", call. = FALSE)

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
        rowSums(arr, dims = 2)
      }
    },
    "mean" = rowMeans(arr, dims = 2),
    "max" = apply(arr, c(1, 2), max),
    "min" = apply(arr, c(1, 2), min),
    "union" = (rowSums(arr > 0, dims = 2) > 0) * 1,
    "intersection" = (rowSums(arr > 0, dims = 2) == L) * 1
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
#' Compares the macro weights of \code{\link{csum}} with the result of
#' contracting the clusters in igraph (\code{igraph::contract()} followed by
#' \code{igraph::simplify()}).
#'
#' @param x Adjacency matrix
#' @param clusters Cluster specification (see \code{\link{csum}})
#' @param method Aggregation method. Default "sum".
#' @param type Normalization type passed to \code{\link{csum}}. Default
#'   "raw", the only type whose values igraph reproduces.
#' @return A list with components \code{our_result} (cograph's macro weight
#'   matrix), \code{igraph_result} (igraph's
#'   \code{contract()} + \code{simplify()} matrix, diagonal set to zero),
#'   \code{matches} (logical, whether the off-diagonal cells agree to within
#'   1e-10) and \code{difference} (the
#'   \code{all.equal()} report when they do not, otherwise NULL). Returns
#'   \code{NULL} with a message if igraph is not installed.
#' @export
verify_with_igraph <- function(x, clusters, method = "sum", type = "raw") {

  if (!requireNamespace("igraph", quietly = TRUE)) { # nocov start
    message("igraph package not available for verification")
    return(NULL)
  } # nocov end

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

  # Compare off-diagonal only (diagonal = intra-cluster retention,
  # not comparable to igraph's contract+simplify)
  our_offdiag <- our_result$macro$weights
  diag(our_offdiag) <- 0
  matches <- all.equal(our_offdiag, igraph_result,
                       check.attributes = FALSE, tolerance = 1e-10)

  list(
    our_result = our_result$macro$weights,
    igraph_result = igraph_result,
    matches = isTRUE(matches),
    difference = if (!isTRUE(matches)) matches else NULL
  )
}

#' @rdname verify_with_igraph
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' verify_with_igraph(regulation_net, clusters = clusters)
verify_igraph <- verify_with_igraph

# ==============================================================================
# Print Methods
# ==============================================================================

#' @noRd
#' @export
print.cluster_summary <- function(x, ...) {
  cat("Cluster Summary\n")
  cat("---------------\n")

  n_clusters <- x$meta$n_clusters
  n_nodes <- x$meta$n_nodes
  cluster_names <- names(x$cluster_members)
  cluster_sizes <- x$meta$cluster_sizes

  cat("Type:", x$meta$type, "\n")
  cat("Method:", x$meta$method, "\n")
  cat("Clusters:", n_clusters, "\n")
  cat("Nodes:", n_nodes, "\n")
  cat("Cluster sizes:", paste(cluster_sizes, collapse = ", "), "\n\n")

  # Macro (cluster-level) output
  bw <- x$macro$weights
  cat("Macro (cluster-level) weights (", nrow(bw), "x", ncol(bw), "):\n", sep = "")
  cat("  Inits:", paste(round(x$macro$inits, 3), collapse = ", "), "\n")
  if (nrow(bw) <= 6) {
    print(round(bw, 3))
  } else {
    cat("  [showing first 6x6 corner]\n")
    print(round(bw[1:6, 1:6], 3))
  }

  # Per-cluster output
  if (!is.null(x$clusters)) {
    cat("\nPer-cluster weights:\n")
    n_show <- min(3, length(x$clusters))
    for (i in seq_len(n_show)) {
      cl_name <- names(x$clusters)[i]
      cl_mat <- x$clusters[[cl_name]]$weights
      cat("  ", cl_name, " (", nrow(cl_mat), " nodes)\n", sep = "")
    }
    if (length(x$clusters) > 3) {
      cat("  ... and", length(x$clusters) - 3, "more clusters\n")
    }
  } else {
    cat("\nPer-cluster: not computed\n")
  }

  invisible(x)
}

# print.mcml lives in Nestimate, which owns the mcml class. Registering a
# second method here made the printed form depend on which package was loaded
# last.

#' @noRd
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
#' method, without normalization.
#'
#' @param x A weight matrix, tna object, or cograph_network.
#' @param cluster_list Cluster specification:
#'   \itemize{
#'     \item Named list of node vectors (e.g., \code{list(A = c("n1", "n2"), B = c("n3", "n4"))})
#'     \item A membership vector or a data frame, as in \code{\link{csum}}
#'     \item A single string naming a column of the node table of a
#'       cograph_network (e.g., "clusters", "groups")
#'     \item NULL (default) to use the first node column named "clusters",
#'       "cluster", "groups", "group", "community" or "module" of a
#'       cograph_network, with a message naming the column
#'   }
#' @param method Aggregation method for edge weights: "sum", "mean", "max",
#'   "min", "median", "density", "geomean". Default "sum".
#' @param directed Logical. Whether the summary network is directed.
#'   Default TRUE.
#'
#' @return A cograph_network object with one node per cluster, labelled by
#'   cluster name. The edge weights are the aggregated between-cluster
#'   weights, and the diagonal holds the aggregated within-cluster weights.
#'   The node table has a \code{size} column with the number of original
#'   nodes in each cluster.
#'
#' @export
#' @seealso \code{\link{csum}}, \code{\link{plot_mcml}}
#'
#' @examples
#' clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
#'                  C2 = c("Plan", "Create", "Share"),
#'                  C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
#' summarize_network(regulation_net, cluster_list = clusters)
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

  # Create cograph_network from macro (cluster-level) matrix
  result <- cograph(cs$macro$weights, directed = directed)

  # Add cluster sizes to nodes
  result$nodes$size <- cs$meta$cluster_sizes[match(result$nodes$label, names(cs$cluster_members))]

  result
}

#' @rdname summarize_network
#' @return See \code{\link{summarize_network}}.
#' @export
cnet <- summarize_network
