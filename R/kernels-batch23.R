#' Open-neighborhood H-index for adaptive LeaderRank
#' @param b Binary loop-free adjacency matrix.
#' @param mode H-index topology: all, out or in.
#' @return Original node H-indices, before adding the ground node.
#' @keywords internal
#' @noRd
.cg_alr_hindex <- function(b, mode = "all") {
  mode <- match.arg(mode, c("all", "out", "in"))
  view <- switch(mode, all = .cg_undirected_view(b), out = b, `in` = t(b))
  degree <- rowSums(view)
  vapply(seq_len(nrow(b)), function(i) {
    .cg_hindex(degree[view[i, ] != 0])
  }, numeric(1))
}

#' Adaptive LeaderRank with original H-index destination weights
#' @param b Binary loop-free adjacency matrix.
#' @param h_mode H-index topology: all, out or in.
#' @return Stationary node scores, retaining total augmented mass n.
#' @keywords internal
#' @noRd
.cg_adaptive_leaderrank <- function(b, h_mode = "all") {
  h <- .cg_alr_hindex(b, h_mode)
  n <- nrow(b)
  if (!n) return(numeric(0))
  if (!any(h > 0)) return(rep(NaN, n))
  w <- sweep(b, 2, h, "*")
  p <- w / (rowSums(w) + 1)
  q <- h / sum(h)
  # The ground edge has weight one in each original row, making p
  # strictly substochastic. Eliminate ground mass before solving.
  y <- as.numeric(solve(diag(1, n, n) - t(p), q))
  if (any(!is.finite(y)) || any(y < -1e-10)) {
    stop("adaptive LeaderRank stationary solve failed", call. = FALSE)
  }
  y <- pmax(y, 0)
  n * y / (1 + sum(y))
}
