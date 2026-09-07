#' Improved closeness from distances and log shortest-path counts
#' @param b Symmetric binary zero-diagonal adjacency matrix.
#' @param alpha Shortest-path multiplicity exponent, between zero and one.
#' @return Improved closeness; disconnected graphs and singletons score zero.
#' @keywords internal
#' @noRd
.cg_improved_closeness <- function(b, alpha = 0.2) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0 || alpha > 1) {
    stop("icc_alpha must be a finite number between 0 and 1", call. = FALSE)
  }
  n <- nrow(b)
  out <- numeric(n)
  if (n <= 1L || .cg_n_components(b) > 1L) return(out)
  neighbors <- lapply(seq_len(n), function(i) which(b[i, ] != 0))
  for (source in seq_len(n)) {
    distance <- rep(-1L, n)
    log_count <- rep(-Inf, n)
    distance[source] <- 0L
    log_count[source] <- 0
    queue <- integer(n)
    queue[1L] <- source
    first <- 1L
    last <- 1L
    while (first <= last) {
      v <- queue[first]
      first <- first + 1L
      for (u in neighbors[[v]]) {
        if (distance[u] < 0L) {
          distance[u] <- distance[v] + 1L
          last <- last + 1L
          queue[last] <- u
        }
        if (distance[u] == distance[v] + 1L) {
          high <- max(log_count[u], log_count[v])
          low <- min(log_count[u], log_count[v])
          log_count[u] <- high + log1p(exp(low - high))
        }
      }
    }
    # Direct neighbors always contribute one, even when distant terms
    # underflow to zero; a connected nontrivial graph has a positive sum.
    denominator <- sum(distance * exp(-alpha * log_count))
    out[source] <- (n - 1) / denominator
  }
  out
}
