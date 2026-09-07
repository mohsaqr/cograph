#' Canonical global eigenvector feature for MCGM
#' @keywords internal
#' @noRd
.cg_mcgm_eigenvector <- function(a) {
  n <- nrow(a)
  labels <- .cg_component_labels(a)
  groups <- split(seq_len(n), labels)
  pairs <- lapply(groups, function(ids) {
    if (length(ids) == 1L) return(list(root = 0, vector = 1))
    e <- eigen(a[ids, ids, drop = FALSE], symmetric = TRUE)
    v <- e$vectors[, 1L]
    v <- v * sign(v[which.max(abs(v))])
    if (any(v <= 0)) {
      stop("mcgm positive eigenvector is unresolved", call. = FALSE)
    }
    list(root = e$values[1L], vector = v)
  })
  roots <- vapply(pairs, `[[`, numeric(1), "root")
  top <- max(roots)
  tolerance <- 64 * .Machine$double.eps * n * max(1, top)
  result <- numeric(n)
  for (j in which(top - roots <= tolerance)) {
    v <- pairs[[j]]$vector
    # Orthogonal projection of the all-ones vector onto the top eigenspace.
    result[groups[[j]]] <- v * sum(v)
  }
  result / max(result)
}

#' Calculate the multi-characteristics gravity model
#' @keywords internal
#' @noRd
calculate_mcgm <- function(g, mcgm_radius = 2, mcgm_alpha = NULL,
                           normalized = FALSE) {
  if (is.null(mcgm_radius)) mcgm_radius <- Inf
  if (!is.numeric(mcgm_radius) || length(mcgm_radius) != 1L ||
        is.na(mcgm_radius) || mcgm_radius < 0) {
    stop("mcgm_radius must be a nonnegative number, Inf or NULL",
         call. = FALSE)
  }
  if (!is.null(mcgm_alpha) &&
        (!is.numeric(mcgm_alpha) || length(mcgm_alpha) != 1L ||
           !is.finite(mcgm_alpha) || mcgm_alpha < 0)) {
    stop("mcgm_alpha must be NULL or a finite nonnegative number",
         call. = FALSE)
  }
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  n <- nrow(a)
  degree <- rowSums(a)
  if (!any(degree > 0) || mcgm_radius < 1) return(numeric(n))
  core <- .cg_mdd(a, lambda = 0)
  degree <- degree / max(degree)
  core <- core / max(core)
  ev <- .cg_mcgm_eigenvector(a)
  if (is.null(mcgm_alpha)) {
    if (stats::median(core) == 0) {
      stop("mcgm automatic alpha is undefined when median coreness is zero; ",
           "supply an explicit mcgm_alpha", call. = FALSE)
    }
    mcgm_alpha <- max(stats::median(degree), stats::median(ev)) /
      stats::median(core)
  }
  scale <- max(1, mcgm_alpha)
  mass <- degree / scale + (mcgm_alpha / scale) * core + ev / scale
  d <- .cg_distances(a)
  kernel <- matrix(0, n, n)
  use <- is.finite(d) & d > 0 & d <= mcgm_radius
  kernel[use] <- 1 / d[use]^2
  score <- mass * as.numeric(kernel %*% mass)
  if (normalized) return(score / max(score))
  score <- (score * scale) * scale
  if (any(!is.finite(score))) {
    stop("mcgm raw scores exceed double precision; use normalized = TRUE",
         call. = FALSE)
  }
  score
}

#' Multi-characteristics gravity model
#'
#' Li and Huang's MCGM combines degree k, core number s and eigenvector
#' centrality x in node masses. Write K, S and X for these features divided
#' by their respective global maxima. Equations 17 and 18 define
#' \eqn{\alpha=\max\{\operatorname{median}(K),
#' \operatorname{median}(X)\}/\operatorname{median}(S)},
#' \eqn{m_i=K_i+\alpha S_i+X_i}, and
#' \eqn{MCGM_i=\sum_{j:0<d(i,j)\le R}m_i m_j/d(i,j)^2}.
#' The default radius two is the paper's recommended practical setting.
#' All features refer to the original graph, not each node's neighborhood.
#'
#' The source domain is simple undirected unweighted graphs. Other inputs
#' use their simple undirected skeleton: either arc creates one edge,
#' parallel edges count once and loops are removed. Weights, mode, cutoff,
#' gravity_mass, gravity_radius and path-weight inversion are ignored.
#' These input projections are cograph conventions.
#'
#' On connected graphs with edges, X is the unique positive Perron vector,
#' scaled to maximum one. For disconnected graphs the paper does not specify
#' an eigenvector selection. This implementation projects the all-ones vector
#' onto the global dominant eigenspace and then scales to maximum one.
#' Equivalently, it selects the limit of identity-shifted power iteration
#' initialized uniformly. Components below the largest spectral radius
#' have eigenvector feature zero; tied components share the projection.
#' Component roots within 64 times machine epsilon times n times
#' max(1, spectral radius) are treated as tied. All feature maxima and
#' medians remain global. Adding a disconnected component can change scores.
#'
#' When edges exist but median coreness is zero, the source's automatic
#' alpha is undefined and an error requests an explicit \code{mcgm_alpha}.
#' This override is an extension of the published adaptive rule; setting
#' it to one recovers equation 16. It is never silently inferred from a
#' different subset of nodes. Isolates score zero when the mass rule is
#' defined. Edgeless graphs and radii below one return zero by an explicit
#' empty-interaction convention, including a singleton; empty graphs return
#' no scores. NULL or infinite radius includes all reachable partners.
#'
#' Raw scores preserve equation 18's scale. Optional maximum normalization
#' occurs after all gravity contributions and can handle very large explicit
#' alpha values whose raw scores overflow. Dense spectral calculations and
#' all-pairs distances require O(n cubed) time and O(n squared) memory.
#' Unresolved positive eigenvectors or overflowing raw scores raise errors.
#' The published nine-node numerical example is reproduced at its printed
#' precision. This establishes numerical agreement, not a universal guarantee
#' of spreading prediction or parity with unreleased author software.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mcgm_radius Nonnegative hop-distance cutoff, default two. NULL
#'   or infinity includes every reachable partner. Fractional cutoffs
#'   include exactly the integer hop distances not exceeding them.
#' @param mcgm_alpha NULL uses the published median-based coefficient.
#'   A finite nonnegative scalar explicitly overrides it.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Li, Z. and Huang, X. (2022). Identifying influential spreaders
#'   by gravity model considering multi-characteristics of nodes.
#'   Scientific Reports, 12, 9879. Equations 17-18, Algorithm 1, Tables 1-2.
#'   \doi{10.1038/s41598-022-14005-3}.
#' @export
#' @examples
#' centrality_mcgm(igraph::make_ring(6))
#' centrality_mcgm(igraph::make_star(6), mcgm_radius = 3)
centrality_mcgm <- function(x, mcgm_radius = 2, mcgm_alpha = NULL, ...) {
  df <- centrality(x, measures = "mcgm", mcgm_radius = mcgm_radius,
                   mcgm_alpha = mcgm_alpha, ...)
  stats::setNames(df$mcgm, df$node)
}
