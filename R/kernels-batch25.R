#' Discounted first arrivals with target-specific absorbing systems
#' @param a Nonnegative weighted adjacency, retaining direction and loops.
#' @param decay Discount factor in [0,1).
#' @param mass Nonnegative starting weight for each node.
#' @param normalized Whether to scale mass before summing to avoid overflow.
#' @return Weighted first-arrival scores.
#' @keywords internal
#' @noRd
.cg_random_walk_decay <- function(a, decay, mass, normalized = FALSE) {
  if (!is.numeric(decay) || length(decay) != 1L || !is.finite(decay) ||
        decay < 0 || decay >= 1) {
    stop("rwd_decay must be a finite number in [0,1)", call. = FALSE)
  }
  n <- nrow(a)
  if (!n) return(numeric(0))
  if (normalized && max(mass) > 0) mass <- mass / max(mass)
  if (decay == 0 || !any(mass > 0)) return(mass)
  p <- a
  for (i in seq_len(n)) {
    scale <- max(a[i, ])
    if (scale == 0) next
    row <- a[i, ] / scale
    if (any(a[i, ] > 0 & row == 0)) {
      stop("random_walk_decay transition range exceeds double precision",
           call. = FALSE)
    }
    p[i, ] <- row / sum(row)
    if (any(a[i, ] > 0 & p[i, ] == 0)) {
      stop("random_walk_decay transition range exceeds double precision",
           call. = FALSE)
    }
  }
  reachable <- is.finite(.cg_distances(1 * (a > 0), "out"))
  out <- mass
  for (target in seq_len(n)) {
    from <- which(reachable[, target] & seq_len(n) != target)
    if (!length(from)) next
    # Reaching the target ends the walk. Its outgoing edges are absent
    # from the system, making lack of self-impact explicit. Unreachable
    # vertices have hitting probability zero, including separate classes.
    q <- p[from, from, drop = FALSE]
    discounted <- decay * q
    if (any(q > 0 & discounted == 0)) {
      stop("random_walk_decay discounted transition range exceeds precision",
           call. = FALSE)
    }
    system <- diag(1, length(from)) - discounted
    hit <- tryCatch(solve(system, decay * p[from, target]),
                    error = function(e) NULL)
    if (is.null(hit) || any(!is.finite(hit)) ||
          any(hit < -1e-10 | hit > 1 + 1e-10)) {
      stop("random_walk_decay first-arrival system is numerically unstable",
           call. = FALSE)
    }
    hit <- pmin(1, pmax(0, hit))
    out[target] <- mass[target] + sum(mass[from] * hit)
    lost <- hit == 0 & mass[from] > 0
    if (any(lost)) {
      # A probability can underflow before a large starting mass rescales
      # it. Propagate only those starting masses forward, then accumulate
      # their discounted incoming flux at the absorbing target in log space.
      starting <- mass[from]
      starting[!lost] <- 0
      visits <- tryCatch(solve(t(system), starting), error = function(e) NULL)
      if (is.null(visits) || any(!is.finite(visits)) || any(visits < 0)) {
        stop("random_walk_decay forward-mass system is numerically unstable",
             call. = FALSE)
      }
      flux <- exp(log(decay) + log(p[from, target]) + log(visits))
      out[target] <- out[target] + sum(flux)
    }
  }
  if (any(!is.finite(out))) {
    stop("random_walk_decay raw scores overflow; use normalized = TRUE",
         call. = FALSE)
  }
  out
}
