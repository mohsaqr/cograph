#' SpectralRank and its diagonal-prior family
#' @keywords internal
#' @noRd
calculate_spectralrank <- function(cg, weights = NULL, sr_prior = 0) {
  n <- cg$n
  if (!is.numeric(sr_prior) || !(length(sr_prior) %in% c(1L, n)) ||
        any(!is.finite(sr_prior)) || any(sr_prior < 0)) {
    .cg_stop_bad_parameter("sr_prior must be a finite nonnegative scalar or one value per node")
  }
  if (length(sr_prior) == n && !is.null(names(sr_prior)) && n > 0) {
    labels <- cg$labels
    if (!cg$has_names || anyDuplicated(names(sr_prior)) ||
          !setequal(names(sr_prior), labels)) {
      .cg_stop_bad_parameter("sr_prior names must match node names exactly")
    }
    sr_prior <- sr_prior[match(labels, names(sr_prior))]
  }
  prior <- rep_len(as.numeric(sr_prior), n)
  a <- .cg_candidate_adjacency(cg, weights, "spectralrank")
  diag(a) <- 0
  if (n == 0L) return(numeric())
  if (n == 1L) return(1)
  if (!any(a > 0) && length(unique(prior)) == 1L) {
    # The Perron definition exists even when the paper's unshifted power
    # iteration alternates. For uniform priors the two-class system is exact.
    p <- prior[1]
    value <- if (p >= n - 1) 1 else
      p / (2 * n) + sqrt((p / (2 * n))^2 + 1 / n)
    return(rep(value, n))
  }
  b <- rbind(cbind(a, 1), c(rep(1, n), 0))
  diag(b) <- c(prior, 0)
  positive <- b > 0
  scale <- max(b)
  b <- b / scale
  if (any(b[positive] == 0)) {
    stop("spectralrank weight/prior range exceeds double precision",
         call. = FALSE)
  }
  eig <- eigen(b, symmetric = isSymmetric(b, tol = 0,
                                          check.attributes = FALSE))
  index <- which.max(Re(eig$values))
  root <- eig$values[index]
  vector <- eig$vectors[, index]
  if (abs(Im(root)) > 1e-12 || max(abs(Im(vector))) > 1e-12) {
    stop("spectralrank Perron eigenpair is unresolved", call. = FALSE)
  }
  vector <- Re(vector)
  vector <- vector * sign(vector[which.max(abs(vector))])
  if (any(!is.finite(vector)) || any(vector <= 0)) {
    stop("spectralrank positive eigenvector is unresolved; ",
         "reduce weight/prior range",
         call. = FALSE)
  }
  ratios <- as.numeric(b %*% vector) / vector
  if (max(abs(ratios - Re(root))) > 1e-10 * max(1, Re(root))) {
    stop("spectralrank eigenpair residual exceeds numerical precision",
         call. = FALSE)
  }
  (vector / max(vector))[seq_len(n)]
}

#' SpectralRank Centrality
#'
#' SpectralRank (Xu et al. 2019) is the positive eigenvector of the largest
#' eigenvalue of the adjacency matrix augmented by a ground node, which is
#' linked in both directions to every node with unit weight:
#' \deqn{B = \left(\begin{smallmatrix} A + P & \mathbf{1} \\
#'   \mathbf{1}^T & 0 \end{smallmatrix}\right).}{
#'   B = [A + P, 1; t(1), 0].}
#' The diagonal prior \eqn{P}{P} is zero for ordinary SpectralRank, and a
#' nonnegative prior gives the weighted SpectralRank family of the paper.
#'
#' @details
#' The eigenvector is divided by its maximum over all nodes including the
#' ground node, which is then dropped, so the largest returned score can be
#' below one. \code{normalized = TRUE} further divides by the maximum over
#' the original nodes. An arc from \eqn{i}{i} to \eqn{j}{j} contributes the
#' score of \eqn{j}{j} to \eqn{i}{i}. Edge weights must be finite and
#' nonnegative, \code{weighted = FALSE} gives every edge weight one, and
#' loops are removed. Every node, isolated nodes included, scores
#' positive, and without edges or priors each of \eqn{n}{n} nodes scores
#' \eqn{1/\sqrt{n}}{1/sqrt(n)}. The update line of Algorithm 1 in the paper
#' omits \eqn{P}{P}, and the implementation follows section III-A2 with
#' \eqn{B = \tilde{A} + P}{B = A~ + P}. An invalid \code{sr_prior} or an
#' unresolved eigenvector raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param sr_prior Diagonal prior \eqn{P}{P}, a nonnegative scalar or one
#'   value per node. A named vector is matched to the node names. Default 0,
#'   which gives ordinary SpectralRank.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Xu, S., Wang, P., Zhang, C.-X. and Lu, J. (2019). Spectral Learning
#'   Algorithm Reveals Propagation Capability of Complex Networks. IEEE
#'   Transactions on Cybernetics, 49(12), 4253-4261.
#'   \doi{10.1109/TCYB.2018.2861568}.
#' @seealso \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_leaderrank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_spectralrank(regulation_net)
centrality_spectralrank <- function(x, sr_prior = 0, ...) {
  df <- centrality(x, measures = "spectralrank", sr_prior = sr_prior, ...)
  stats::setNames(df$spectralrank, df$node)
}
