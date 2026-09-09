#' Multiply nonnegative walk masses with explicit range checks
#' @keywords internal
#' @noRd
.cg_bridge_product <- function(a, b) {
  result <- a %*% b
  if (any(!is.finite(result))) {
    stop("bridging_capital walk masses exceed double precision", call. = FALSE)
  }
  if (any(a > 0) && any(b > 0) &&
        log(min(a[a > 0])) + log(min(b[b > 0])) < -700) {
    reachable <- (a > 0) %*% (b > 0) > 0
    if (any(reachable & result == 0)) {
      stop("bridging_capital walk masses underflow double precision",
           call. = FALSE)
    }
  }
  result
}

#' Bridging capital via walks that have used a selected arc
#' @keywords internal
#' @noRd
calculate_bridging_capital <- function(cg, weights = NULL, steps = 2,
                                       values = NULL, normalized = FALSE) {
  if (!is.numeric(steps) || length(steps) != 1L || !is.finite(steps) ||
        steps < 0 || steps != floor(steps) || steps > .Machine$integer.max) {
    stop("bridging_steps must be a nonnegative integer", call. = FALSE)
  }
  p <- .cg_candidate_adjacency(cg, weights, "bridging_capital")
  if (any(p > 1)) {
    stop("bridging_capital transmission probabilities must be in [0,1]",
         call. = FALSE)
  }
  n <- nrow(p)
  if (is.null(values)) values <- matrix(1, n, n)
  if (!is.matrix(values) || !is.numeric(values) ||
        !identical(dim(values), c(n, n)) ||
        any(!is.finite(values)) || any(values < 0)) {
    stop("bridging_values must be a finite nonnegative n by n matrix",
         call. = FALSE)
  }
  if (!is.null(rownames(values)) || !is.null(colnames(values))) {
    nodes <- cg$labels
    valid <- function(labels) {
      !is.null(labels) && !anyDuplicated(labels) && setequal(labels, nodes)
    }
    if (!valid(rownames(values)) || !valid(colnames(values))) {
      stop("bridging_values row and column names must match node labels",
           call. = FALSE)
    }
    values <- values[nodes, nodes, drop = FALSE]
  }
  out <- numeric(n)
  if (!n || steps == 0 || !any(p > 0) || !any(values > 0)) return(out)
  if (normalized) {
    positive <- values > 0
    values <- values / max(values)
    if (any(values[positive] == 0)) {
      stop("bridging_capital value range exceeds double precision",
           call. = FALSE)
    }
  }
  arcs <- which(p > 0, arr.ind = TRUE)
  for (e in seq_len(nrow(arcs))) {
    i <- arcs[e, 1L]
    j <- arcs[e, 2L]
    without <- p
    without[i, j] <- 0
    unused <- diag(1, n)
    used <- matrix(0, n, n)
    criticality <- 0
    for (step in seq_len(steps)) {
      first_use <- unused[, i] * p[i, j]
      if (any(unused[, i] > 0 & first_use == 0)) {
        stop("bridging_capital first-use mass underflows double precision",
             call. = FALSE)
      }
      used <- .cg_bridge_product(used, p)
      used[, j] <- used[, j] + first_use
      unused <- .cg_bridge_product(unused, without)
      contribution <- used * values
      if (any(used > 0 & values > 0 & contribution == 0)) {
        stop("bridging_capital valued mass underflows double precision",
             call. = FALSE)
      }
      criticality <- criticality + sum(contribution)
      if (!is.finite(criticality)) {
        stop("bridging_capital raw scores overflow; try normalized = TRUE",
             call. = FALSE)
      }
    }
    out[i] <- out[i] + criticality
  }
  if (any(!is.finite(out))) {
    stop("bridging_capital raw scores overflow; try normalized = TRUE",
         call. = FALSE)
  }
  out
}

#' Bridging capital from lost information walks
#'
#' Implements Jackson's section 3.3 definition:
#' \deqn{Brid_i=\sum_j\sum_{s,t}v_{st}\sum_{h=1}^T
#'                 [P^h-(P-P_{ij}E_{ij})^h]_{st}.}
#' P contains per-contact transmission probabilities between zero and one.
#' Rows need
#' not sum to one: this is broadcast information flow, not a Markov chain.
#' \code{bridging_steps} is the finite horizon T, default two, with zero
#' giving an empty sum. Input edge weights supply P; unweighted edges use
#' probability one. Finite nonnegative pair values v_st default to one,
#' including diagonal entries. Named value matrices are reordered by labels.
#'
#' The source explicitly deletes one matrix entry P_ij and credits its
#' criticality to i. On undirected input, opposite entries are therefore
#' tested separately; deleting one leaves the reverse entry present. This
#' is not simultaneous deletion of an undirected edge or of a whole node.
#' Walks can repeat nodes and edges. A walk using the selected entry several
#' times contributes once to that entry's deletion loss, not once per use.
#'
#' Direction and loops are retained, as allowed by the source's formal
#' definitions. Generic loops/simplify apply first. Remaining parallel
#' weights sum into one matrix entry and must still be at most one; removal
#' deletes that aggregate entry. Zero weights are absent. Mode, inversion
#' and shortest-path cutoff do not affect results. Isolates score zero,
#' empty inputs return no scores, and all-zero values or zero horizon give
#' zeros. No renormalization follows entry removal.
#'
#' The native implementation tracks walks that have and have not used the
#' selected entry, avoiding cancellation in matrix-power subtraction. Dense
#' cost is O(m T n cubed) time and O(n squared) memory, where m is the number
#' of positive directed matrix entries. Request this costly measure explicitly.
#' Nonrepresentable intermediate walk masses raise errors, even if a final
#' rescaled result might exist. Raw valued-score overflow may be avoided by
#' \code{normalized=TRUE}, which scales values first then divides final node
#' scores by their maximum. This implements expected walk counts EInf, not
#' the source's alternative probability-of-ever-hearing measure PInf.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param bridging_steps Nonnegative integer horizon, default two.
#' @param bridging_values Optional nonnegative n by n source-destination
#'   information-value matrix; NULL uses ones. Both dimensions may be named.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Jackson, M. O. (2020). A typology of social capital and
#'   associated network measures. Social Choice and Welfare, 54, 311-336.
#'   \doi{10.1007/s00355-019-01189-3}. Definition read in author preprint
#'   arXiv:1711.09504v3 (2019), section 3.3, page 18; transmission model
#'   section 3.1 and formal graph conventions section 2.
#' @export
#' @examples
#' centrality_bridging_capital(igraph::make_ring(4), bridging_steps = 2)
centrality_bridging_capital <- function(x, bridging_steps = 2,
                                        bridging_values = NULL, ...) {
  df <- centrality(x, measures = "bridging_capital",
                   bridging_steps = bridging_steps,
                   bridging_values = bridging_values, ...)
  stats::setNames(df$bridging_capital, df$node)
}
