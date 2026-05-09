#' Detect Core-Periphery Structure
#'
#' Identifies core-periphery structure in a network using either continuous
#' (Borgatti-Everett) or discrete methods. Core nodes are densely interconnected,
#' while periphery nodes connect primarily to the core.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param method Character string; either "continuous" (default, Borgatti-Everett
#'   model) or "discrete" (binary core/periphery assignment).
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param iter Integer; maximum number of iterations for the continuous algorithm.
#'   Default 100.
#' @param digits Integer or NULL. Round numeric outputs to this many decimal
#'   places. Default NULL (no rounding).
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A data frame with class \code{"cograph_core_periphery"} and columns
#'   \code{node}, \code{role}, and \code{coreness}. Fitness, core density,
#'   periphery density, and the original network are stored as attributes.
#'
#' @details
#' \strong{Continuous method (Borgatti-Everett):}
#' Finds a coreness vector \code{c} (values 0-1) that maximizes the correlation
#' between the adjacency matrix and the ideal rank-1 pattern matrix
#' (the outer product of the coreness vector with itself). The algorithm
#' initializes from eigenvector centrality and iteratively refines via
#' power iteration until convergence.
#'
#' \strong{Discrete method:}
#' Produces a binary core (1) / periphery (0) assignment. Starts from the
#' continuous solution, thresholds at the median, then greedily swaps node
#' assignments to maximize fitness. Discrete fitness is defined by high density
#' within the core and low density within the periphery.
#'
#' @references
#' Borgatti, S.P. & Everett, M.G. (2000). Models of core/periphery structures.
#' \emph{Social Networks}, 21(4), 375-395.
#' \doi{10.1016/S0378-8733(99)00019-2}
#'
#' @export
#' @examples
#' # Core-periphery in a simple network
#' adj <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 1, 1, 0,
#'   1, 1, 0, 1, 1,
#'   1, 1, 1, 0, 1,
#'   0, 0, 1, 1, 0
#' ), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' cp <- cograph::core_periphery(adj)
#' cp
#'
#' # Discrete assignment
#' cp_disc <- cograph::core_periphery(adj, method = "discrete")
#' cp_disc$assignment
#'
#' @seealso \code{\link{centrality}}, \code{\link{network_summary}}
core_periphery <- function(x,
                           method = c("continuous", "discrete"),
                           directed = NULL,
                           iter = 100,
                           digits = NULL,
                           ...) {

  method <- match.arg(method)
  stopifnot(
    is.numeric(iter),
    length(iter) == 1L,
    iter >= 1L
  )
  iter <- as.integer(iter)

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for core_periphery()", call. = FALSE)
  }

  # Convert input to igraph
  g <- to_igraph(x, directed = directed, ...)

  n <- igraph::vcount(g)
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) {
    node_names <- as.character(seq_len(n))
  }

  # Get adjacency matrix (binary for structure detection)
  adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

  # Compute continuous coreness scores via power iteration
  scores <- .cp_continuous(adj, iter)
  names(scores) <- node_names

  # Compute fitness: correlation between A and outer(c, c)
  # Use full off-diagonal for directed, lower.tri for undirected
  diag(adj) <- 0  # exclude self-loops
  ideal <- outer(scores, scores)
  is_sym <- isSymmetric(unname(adj))
  mask <- if (is_sym) lower.tri(adj) else row(adj) != col(adj)
  fitness <- tryCatch(stats::cor(adj[mask], ideal[mask]), warning = function(w) NA)
  if (is.na(fitness)) fitness <- 0  # constant vectors = no structure

  # Derive discrete assignment from continuous scores
  assignment <- ifelse(scores >= stats::median(scores), "core", "periphery")
  names(assignment) <- node_names

  # If discrete method requested, refine via greedy swaps

  if (method == "discrete") {
    assignment <- .cp_discrete_refine(adj, assignment, node_names)
    # Recompute fitness for discrete case
    core_vec <- as.numeric(assignment == "core")
    ideal_disc <- outer(core_vec, core_vec)
    fitness <- tryCatch(stats::cor(adj[mask], ideal_disc[mask]),
                        warning = function(w) NA)
    if (is.na(fitness)) fitness <- 0
  }

  # Compute subgroup densities
  core_nodes <- which(assignment == "core")
  periphery_nodes <- which(assignment == "periphery")
  core_density <- .subgraph_density(adj, core_nodes)
  periphery_density <- .subgraph_density(adj, periphery_nodes)

  if (!is.null(digits)) {
    scores <- round(scores, digits)
    fitness <- round(fitness, digits)
    core_density <- round(core_density, digits)
    periphery_density <- round(periphery_density, digits)
  }

  df <- data.frame(
    node = node_names,
    role = assignment,
    coreness = unname(scores),
    stringsAsFactors = FALSE
  )

  attr(df, "fitness") <- fitness
  attr(df, "core_density") <- core_density
  attr(df, "periphery_density") <- periphery_density
  attr(df, "network") <- x
  class(df) <- c("cograph_core_periphery", "data.frame")
  df
}


# =============================================================================
# Internal Helpers
# =============================================================================

#' Power iteration for continuous core-periphery scores
#' @param adj Adjacency matrix (numeric)
#' @param max_iter Maximum iterations
#' @param tol Convergence tolerance
#' @return Numeric vector of coreness scores scaled to 0-1 range
#' @keywords internal
#' @noRd
.cp_continuous <- function(adj, max_iter = 100, tol = 1e-6) {
  n <- nrow(adj)
  is_sym <- isSymmetric(unname(adj))

  # Initialize with eigenvector centrality (dominant eigenvector)
  eig <- eigen(adj, symmetric = is_sym)
  c_vec <- abs(Re(eig$vectors[, 1]))

  # Normalize to [0, 1] with tolerance to avoid floating-point noise
  .rescale01 <- function(v) {
    vr <- range(v)
    span <- vr[2] - vr[1]
    if (span < tol) return(rep(1, length(v)))  # all equal = all core
    (v - vr[1]) / span
  }

  c_vec <- .rescale01(c_vec)

  # Power iteration: c_new = A %*% c_old, then rescale to [0,1]
  step <- 0L
  while (step < max_iter) {
    step <- step + 1L
    c_new <- .rescale01(as.numeric(adj %*% c_vec))

    # Check convergence
    if (max(abs(c_new - c_vec)) < tol) break
    c_vec <- c_new
  }

  c_vec
}


#' Greedy refinement for discrete core-periphery assignment
#' @param adj Adjacency matrix
#' @param assignment Character vector of "core" / "periphery"
#' @param node_names Node name labels
#' @return Refined assignment character vector
#' @keywords internal
#' @noRd
.cp_discrete_refine <- function(adj, assignment, node_names) {
  n <- length(assignment)
  current <- assignment

  # Compute initial discrete fitness
  best_fitness <- .cp_discrete_fitness(adj, current)

  improved <- TRUE
  while (improved) {
    improved <- FALSE
    # Try swapping each node's assignment
    swap_results <- vapply(seq_len(n), function(i) {
      candidate <- current
      candidate[i] <- if (candidate[i] == "core") "periphery" else "core"
      .cp_discrete_fitness(adj, candidate)
    }, numeric(1))

    best_swap <- which.max(swap_results)
    if (swap_results[best_swap] > best_fitness) {
      current[best_swap] <- if (current[best_swap] == "core") {
        "periphery"
      } else {
        "core"
      }
      best_fitness <- swap_results[best_swap]
      improved <- TRUE
    }
  }

  names(current) <- node_names
  current
}


#' Compute discrete core-periphery fitness
#'
#' Fitness = density(core-core) - density(periphery-periphery)
#' @param adj Adjacency matrix
#' @param assignment Character vector of "core" / "periphery"
#' @return Numeric fitness score
#' @keywords internal
#' @noRd
.cp_discrete_fitness <- function(adj, assignment) {
  core_idx <- which(assignment == "core")
  peri_idx <- which(assignment == "periphery")

  core_dens <- .subgraph_density(adj, core_idx)
  peri_dens <- .subgraph_density(adj, peri_idx)

  core_dens - peri_dens
}


#' Compute edge density of a subgraph defined by node indices
#' @param adj Full adjacency matrix
#' @param idx Integer vector of node indices
#' @return Numeric density (0-1), or 0 if fewer than 2 nodes
#' @keywords internal
#' @noRd
.subgraph_density <- function(adj, idx) {
  if (length(idx) < 2L) return(0)
  sub <- adj[idx, idx, drop = FALSE]
  diag(sub) <- 0  # exclude self-loops
  n_sub <- length(idx)
  n_possible <- n_sub * (n_sub - 1L)
  if (n_possible == 0L) return(0)
  sum(sub != 0) / n_possible
}


# =============================================================================
# Plot Method
# =============================================================================

#' Plot Core-Periphery Structure
#'
#' Visualizes the network with core nodes highlighted (larger, red) and
#' periphery nodes de-emphasized (smaller, blue).
#'
#' @param x A \code{cograph_core_periphery} object from
#'   \code{\link{core_periphery}}.
#' @param core_color Color for core nodes. Default \code{"#E41A1C"}.
#' @param periphery_color Color for periphery nodes. Default \code{"#377EB8"}.
#' @param core_size Numeric size for core nodes. Default 12.
#' @param periphery_size Numeric size for periphery nodes. Default 6.
#' @param ... Additional arguments passed to \code{\link{splot}}.
#'
#' @return Invisible \code{x}.
#' @method plot cograph_core_periphery
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,1,0, 1,0,1,1,0, 1,1,0,1,1,
#'                 1,1,1,0,1, 0,0,1,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' cp <- cograph::core_periphery(adj)
#' plot(cp)
plot.cograph_core_periphery <- function(x,
                                        core_color = "#E41A1C",
                                        periphery_color = "#377EB8",
                                        core_size = 12,
                                        periphery_size = 6,
                                        ...) {
  is_core <- x$role == "core"
  node_cols <- ifelse(is_core, core_color, periphery_color)
  node_sizes <- ifelse(is_core, core_size, periphery_size)

  n_core <- sum(is_core)
  n_peri <- sum(!is_core)
  fitness <- attr(x, "fitness") %||% 0
  title <- sprintf("Core-Periphery (core: %d, periphery: %d, fitness: %.2f)",
                   n_core, n_peri, fitness)

  network <- attr(x, "network")
  if (is.null(network)) stop("No network stored", call. = FALSE)
  splot(network, node_color = node_cols, node_size = node_sizes,
        title = title, ...)
  invisible(x)
}

# =============================================================================
# Print Method
# =============================================================================

#' @method print cograph_core_periphery
#' @export
#' @noRd
print.cograph_core_periphery <- function(x, ...) {
  fitness <- attr(x, "fitness") %||% 0
  cd <- attr(x, "core_density") %||% 0
  pd <- attr(x, "periphery_density") %||% 0
  n_core <- sum(x$role == "core")
  n_peri <- sum(x$role == "periphery")
  cat(sprintf("Core-Periphery | Core: %d  Periphery: %d  Fitness: %.3f\n",
              n_core, n_peri, fitness))
  cat(sprintf("Core density: %.3f | Periphery density: %.3f\n\n",
              cd, pd))
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}
