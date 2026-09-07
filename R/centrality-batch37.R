#' SpectralRank and its diagonal-prior family
#' @keywords internal
#' @noRd
calculate_spectralrank <- function(g, weights = NULL, sr_prior = 0) {
  n <- igraph::vcount(g)
  if (!is.numeric(sr_prior) || !(length(sr_prior) %in% c(1L, n)) ||
        any(!is.finite(sr_prior)) || any(sr_prior < 0)) {
    stop("sr_prior must be a finite nonnegative scalar or one value per node",
         call. = FALSE)
  }
  if (length(sr_prior) == n && !is.null(names(sr_prior)) && n > 0) {
    labels <- igraph::V(g)$name
    if (is.null(labels) || anyDuplicated(names(sr_prior)) ||
          !setequal(names(sr_prior), labels)) {
      stop("sr_prior names must match node names exactly", call. = FALSE)
    }
    sr_prior <- sr_prior[match(labels, names(sr_prior))]
  }
  prior <- rep_len(as.numeric(sr_prior), n)
  a <- .cg_candidate_adjacency(g, weights, "spectralrank")
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

#' SpectralRank with optional diagonal prior information
#'
#' Xu et al.'s SpectralRank is the positive right eigenvector belonging to
#' the largest real eigenvalue of the augmented adjacency
#' \eqn{B = \left(\begin{smallmatrix}A+P&\mathbf{1}\\
#' \mathbf{1}^T&0\end{smallmatrix}\right)}.
#' A ground node connects bidirectionally to every original node with unit
#' edge weight. The diagonal P is zero for ordinary SpectralRank; a
#' nonnegative prior gives the paper's weighted SpectralRank family.
#'
#' Raw scores are scaled by the maximum over ALL nodes, including the
#' ground node, which is then omitted from the output. Its score is not
#' redistributed. Consequently the largest returned score can be below one.
#' Optional \code{normalized = TRUE} additionally divides by the maximum
#' over original nodes, changing this source-defined scale.
#'
#' The paper uses binary adjacency and outgoing neighbors: an edge i to j
#' contributes j's score to i. The function preserves this orientation;
#' transpose the graph to use incoming neighbors. Finite nonnegative edge
#' weights extend the same matrix definition; they are interaction weights,
#' separate from the diagonal-prior meaning of weighted SpectralRank.
#' Unit ground edges stay fixed, so scaling original edge weights generally
#' changes scores. For tiny asymmetric matrix weights, set
#' \code{directed = TRUE} or use a directed igraph object because the shared
#' parser otherwise uses approximate symmetry detection.
#'
#' Loops are removed and remaining parallel edges sum after the generic
#' simplify rule; unweighted remaining edges count once each. Zero weights
#' are absent. Mode, path-weight inversion and cutoff are ignored.
#' Named vector priors are matched to node names. Scalar priors broadcast;
#' the ground prior is always zero. Supply externally computed degree,
#' H-index or coreness scores as a vector to select those prior families.
#'
#' All nodes, including isolates, receive positive spectral scores because
#' of the ground links. Without edges or priors, each of n original nodes
#' scores \eqn{1/\sqrt{n}}. For an edgeless graph the paper's unshifted power
#' iteration oscillates, although the Perron eigenvector is unique. This
#' function explicitly uses that eigenvector definition, without claiming
#' convergence of the published iteration. A singleton scores one; an empty
#' graph returns no scores. Adding disconnected nodes generally changes
#' other scores because all share the ground node.
#'
#' For nonzero priors the implementation follows section III-A2's
#' \eqn{B=\widetilde A+P}. Algorithm 1 constructs that matrix but its update
#' line prints \eqn{\widetilde A}, omitting P; this inconsistency is retained
#' in the verification audit. No author-software parity is claimed.
#'
#' Dense eigendecomposition takes O(n cubed) time and O(n squared) memory.
#' Extreme weight/prior ranges or unresolved positive eigenpairs raise
#' errors. The score defines a spectral ranking, not a spreading probability
#' or a general guarantee of predictive performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param sr_prior Nonnegative finite scalar or one value per original node.
#'   Default zero selects SpectralRank; one selects a uniform unit prior.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Xu, S., Wang, P., Zhang, C.-X. and Lu, J. (2019; online2018).
#'   Spectral Learning Algorithm Reveals Propagation Capability of Complex
#'   Networks. IEEE Transactions on Cybernetics, 49(12), 4253-4261.
#'   Section III-A, equations 4-8 and Algorithm 1.
#'   \doi{10.1109/TCYB.2018.2861568}.
#' @export
#' @examples
#' centrality_spectralrank(igraph::make_ring(5))
#' centrality_spectralrank(igraph::make_star(5), sr_prior = 1)
centrality_spectralrank <- function(x, sr_prior = 0, ...) {
  df <- centrality(x, measures = "spectralrank", sr_prior = sr_prior, ...)
  stats::setNames(df$spectralrank, df$node)
}
