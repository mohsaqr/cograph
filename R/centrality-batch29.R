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

#' Bridging Capital
#'
#' Bridging capital (Jackson 2020, section 3.3) credits node \eqn{i}{i}
#' with the expected number of walks of length at most \eqn{T}{T} that are
#' lost when one entry \eqn{P_{ij}}{P_ij} of the transmission matrix is
#' deleted, summed over \eqn{j}{j} and weighted by source-destination
#' values \eqn{v_{st}}{v_st}:
#' \deqn{Brid_i = \sum_j \sum_{s,t} v_{st} \sum_{h=1}^{T}
#'   \left[P^h - (P - P_{ij} E_{ij})^h\right]_{st}.}{
#'   Brid_i = sum_j sum_{s,t} v_st sum_{h=1}^T [P^h - (P - P_ij E_ij)^h]_st.}
#'
#' @details
#' Edge weights are the transmission probabilities in \eqn{P}{P} and must
#' lie between zero and one, and an unweighted edge has probability one. A
#' weight above one raises an error. Direction and loops are kept, and the
#' two entries of an undirected edge are deleted separately. Walks may
#' repeat nodes and edges, and a walk that uses the deleted entry several
#' times is counted once. Isolated nodes score zero, and
#' \code{bridging_steps = 0} gives zero scores. The function computes the
#' expected walk count EInf of the source paper.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param bridging_steps Horizon \eqn{T}{T}, a nonnegative integer. Default
#'   2.
#' @param bridging_values Nonnegative n by n matrix of source-destination
#'   values \eqn{v}{v}. \code{NULL} (default) sets every value to one. Row
#'   and column names, when present, are matched to the node names.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Jackson, M. O. (2020). A typology of social capital and associated network
#'   measures. Social Choice and Welfare, 54, 311-336.
#'   \doi{10.1007/s00355-019-01189-3}.
#' @seealso \code{\link{centrality_bridging}},
#'   \code{\link{centrality_diffusion_centrality}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_bridging_capital(regulation_net)
centrality_bridging_capital <- function(x, bridging_steps = 2,
                                        bridging_values = NULL, ...) {
  df <- centrality(x, measures = "bridging_capital",
                   bridging_steps = bridging_steps,
                   bridging_values = bridging_values, ...)
  stats::setNames(df$bridging_capital, df$node)
}
