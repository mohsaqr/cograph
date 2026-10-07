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
calculate_mcgm <- function(cg, mcgm_radius = 2, mcgm_alpha = NULL,
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
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
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

#' Multi-Characteristics Gravity Model Centrality
#'
#' MCGM (Li and Huang 2022) is a gravity model whose node mass combines
#' degree \eqn{K}{K}, core number \eqn{S}{S} and eigenvector centrality
#' \eqn{X}{X}, each divided by its maximum over the network:
#' \deqn{MCGM_i = \sum_{j : 0 < d(i,j) \le R} \frac{m_i m_j}{d(i,j)^2},
#'   \qquad m_i = K_i + \alpha S_i + X_i.}{
#'   MCGM_i = sum_{j: 0 < d(i,j) <= R} m_i m_j / d(i,j)^2,
#'   m_i = K_i + alpha S_i + X_i.}
#' By default \eqn{\alpha}{alpha} is the larger of the medians of
#' \eqn{K}{K} and \eqn{X}{X} divided by the median of \eqn{S}{S}
#' (equation 17).
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored,
#' and \code{gravity_mass} and \code{gravity_radius} have no effect. On a
#' disconnected network the eigenvector feature is the projection of the
#' all-ones vector onto the dominant eigenspace, so a component with a
#' smaller spectral radius has \eqn{X = 0}{X = 0}. When the median core
#' number is zero the automatic \eqn{\alpha}{alpha} is undefined, and an
#' error asks for \code{mcgm_alpha}. \code{mcgm_alpha = 1} gives equation
#' 16 of the paper. Isolated nodes score zero, and an edgeless network or
#' a radius below one gives zero scores.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mcgm_radius Hop-distance cutoff \eqn{R}{R}. Default 2, the
#'   setting recommended in the paper. \code{NULL} or \code{Inf} includes
#'   every reachable node.
#' @param mcgm_alpha \code{NULL} (default) uses the median-based
#'   \eqn{\alpha}{alpha}. A finite nonnegative number replaces it.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, Z. and Huang, X. (2022). Identifying influential spreaders by gravity
#'   model considering multi-characteristics of nodes. Scientific Reports, 12,
#'   9879. \doi{10.1038/s41598-022-14005-3}.
#' @seealso \code{\link{centrality_gravity}},
#'   \code{\link{centrality_mixed_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcgm(regulation_net)
centrality_mcgm <- function(x, mcgm_radius = 2, mcgm_alpha = NULL, ...) {
  df <- centrality(x, measures = "mcgm", mcgm_radius = mcgm_radius,
                   mcgm_alpha = mcgm_alpha, ...)
  stats::setNames(df$mcgm, df$node)
}
