#' Exogenous contribution to three unnormalized base centralities
#' @param b Binary adjacency, already mode-adjusted, with zero diagonal.
#' @param base Degree, betweenness or reverse_closeness.
#' @param directed Whether ordered paths should count separately.
#' @return Numeric vector, with signed betweenness contributions retained.
#' @keywords internal
#' @noRd
.cg_exogenous <- function(b, base = "reverse_closeness", directed = FALSE) {
  choices <- c("degree", "betweenness", "reverse_closeness")
  if (!is.character(base) || length(base) != 1L || is.na(base) ||
        !base %in% choices) {
    stop("exogenous_base must be degree, betweenness or reverse_closeness",
         call. = FALSE)
  }
  n <- nrow(b)
  if (n <= 1L) return(numeric(n))
  # Base is outgoing degree in this effective adjacency. Its exogenous
  # contribution is incoming degree, reversing the base direction.
  if (base == "degree") return(colSums(b))
  d <- .cg_distances(b, "out")
  if (base == "betweenness") {
    endogenous <- .cg_betweenness(b, n, directed)
    if (any(!is.finite(endogenous))) {
      stop("exogenous betweenness exceeds numerical path-count range",
           call. = FALSE)
    }
    # Every path of length d contributes d-1 across its internal nodes;
    # sharing credit among equal shortest paths preserves this total.
    invariant <- function(d) {
      sum(d[is.finite(d) & d > 0] - 1) / if (directed) 1 else 2
    }
    full <- invariant(d)
  } else {
    proximity <- pmax(n - d, 0)
    diag(proximity) <- 0
    endogenous <- rowSums(proximity)
    full <- sum(endogenous)
    # n remains the ORIGINAL size; unreachable pairs contribute zero.
    invariant <- function(d) {
      proximity <- pmax(n - d, 0)
      diag(proximity) <- 0
      sum(proximity)
    }
  }
  vapply(seq_len(n), function(i) {
    reduced <- .cg_distances(b[-i, -i, drop = FALSE], "out")
    full - endogenous[i] - invariant(reduced)
  }, numeric(1))
}
