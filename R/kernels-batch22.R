#' Weighted LeaderRank via elimination of the ground-node score
#' @param b Binary loop-free adjacency matrix, row to column direction.
#' @param alpha Finite exponent on original in-degree.
#' @return Steady-state scores with total mass n+1 before ground removal.
#' @keywords internal
#' @noRd
.cg_weighted_leaderrank <- function(b, alpha = 1) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha)) {
    stop("wlr_alpha must be a finite number", call. = FALSE)
  }
  n <- nrow(b)
  if (!n) return(numeric(0))
  degree <- colSums(b)
  if (alpha < 0 && any(degree == 0)) {
    stop("negative wlr_alpha requires positive in-degree at every node",
         call. = FALSE)
  }
  if (alpha == 0) {
    q <- rep(1 / n, n)
  } else {
    positive <- degree > 0
    if (!any(positive)) return(rep(NaN, n))
    logs <- log(degree[positive])
    shift <- if (alpha > 0) max(logs) else min(logs)
    q <- numeric(n)
    # Subtract before multiplying: even extreme finite exponents work.
    q[positive] <- exp(alpha * (logs - shift))
    q <- q / sum(q)
  }
  # Ordinary nodes give one unit of edge weight to the ground. The
  # submatrix is strictly substochastic, so I - t(p) is nonsingular.
  p <- b / (rowSums(b) + 1)
  y <- as.numeric(solve(diag(1, n, n) - t(p), q))
  if (any(!is.finite(y)) || any(y < -1e-10)) {
    stop("weighted LeaderRank stationary solve failed", call. = FALSE)
  }
  y <- pmax(y, 0)
  (n + 1) * y / (1 + sum(y))
}
