#' Assemble weighted transitions and starting masses for random walk decay
#' @keywords internal
#' @noRd
calculate_random_walk_decay <- function(cg, weights = NULL, decay = 0.5,
                                        node_weights = NULL,
                                        normalized = FALSE) {
  n <- cg$n
  mass <- node_weights
  if (is.null(mass)) mass <- rep(1, n)
  if (!is.numeric(mass) || length(mass) != n ||
        any(!is.finite(mass)) || any(mass < 0)) {
    stop("rwd_node_weights must contain one finite nonnegative number per node",
         call. = FALSE)
  }
  if (!is.null(names(mass))) {
    labels <- cg$labels
    if (anyDuplicated(names(mass)) || !setequal(names(mass), labels)) {
      stop("rwd_node_weights names must match node names exactly",
           call. = FALSE)
    }
    mass <- mass[match(labels, names(mass))]
  }
  a <- .cg_candidate_adjacency(cg, weights, "random_walk_decay")
  .cg_random_walk_decay(a, decay, unname(mass), normalized)
}

#' Random walk decay centrality
#'
#' Was, Rahwan and Skibski's random walk decay centrality sums discounted
#' first-arrival probabilities:
#' \eqn{RWD_v=\sum_u b_u E_u[a^{T_v};T_v<\infty]}, where \eqn{T_v} is
#' the first time the walk reaches v and a is \code{rwd_decay}. The walk
#' follows outgoing edges in proportion to their nonnegative weights.
#' Sinks lead to a terminal state outside the graph; there is no restart
#' or redistribution of their probability. The start counts as an arrival
#' at time zero, so each node contributes its own starting weight b to its
#' score. Later returns to the same target make no additional contribution.
#'
#' The paper defines a in (0,1). The default 0.5 is an explicit cograph
#' choice; zero is supported as the continuous limit, returning b.
#' \code{rwd_node_weights = NULL} sets all starting weights to one. These
#' weights are not normalized into a probability distribution in the final
#' score. All-zero starting weights return zero by linear extension.
#' Isolates score their own starting weight; empty input returns no scores.
#' Disconnected components are independent before optional normalization.
#'
#' Retains input direction and loops. Undirected edges become opposite
#' transitions; an undirected self-loop is one stay transition. Remaining
#' parallel edges contribute their combined weight, or their multiplicity
#' when \code{weighted = FALSE}. Generic \code{simplify} is applied first;
#' use \code{simplify = FALSE} to preserve unweighted parallel multiplicity.
#' Use \code{loops = FALSE} to remove loops explicitly. Node weights and
#' edge weights are distinct. \code{weighted = FALSE} ignores edge weights
#' but retains supplied node weights. Generic \code{mode}, shortest-path
#' inversion and cutoff do not affect this measure.
#'
#' Removing any target's outgoing edges cannot change its own raw score:
#' those edges can only be traversed after first arrival. Other nodes'
#' scores may change. Global maximum normalization need not preserve this
#' property. The published Example 3 has internally inconsistent numerical
#' values; the implementation follows Definition 1, equation 6. Independent
#' first-arrival calculations and the separate Example 4 and 5 tables verify it.
#'
#' Native absorbing systems are solved separately for each target, using
#' only vertices that can reach it. Worst-case runtime is O(n to the fourth)
#' with O(n squared) memory, so this measure must be requested explicitly.
#' Row scaling avoids overflow of total outgoing weights. Unresolvable
#' transition ranges, unstable solves and raw score overflow raise errors.
#' If a first-arrival probability underflows, a forward-mass solve and
#' log-space incoming flux recover its contribution where representable.
#' Unrepresentably small final contributions can still underflow to zero.
#' \code{normalized = TRUE} supports overflowing raw mass sums by scaling
#' starting weights first; tiny normalized contributions may underflow.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param rwd_decay Finite discount factor in [0,1), default 0.5.
#' @param rwd_node_weights Nonnegative finite starting weights, one per
#'   input node. NULL means ones. Unnamed vectors follow input node order;
#'   named vectors must match every node name exactly and are reordered.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Was, T., Rahwan, T., & Skibski, O. (2019). Random Walk Decay
#'   Centrality. Proceedings of the AAAI Conference on Artificial
#'   Intelligence, 33(01), 2197-2204. Definition 1, equation 6; transition
#'   equation 3 and terminal-sink convention.
#'   \doi{10.1609/aaai.v33i01.33012197}.
#' @export
#' @examples
#' centrality_random_walk_decay(igraph::make_ring(4), rwd_decay = 0.8)
centrality_random_walk_decay <- function(x, rwd_decay = 0.5,
                                         rwd_node_weights = NULL, ...) {
  df <- centrality(x, measures = "random_walk_decay", rwd_decay = rwd_decay,
                   rwd_node_weights = rwd_node_weights, ...)
  stats::setNames(df$random_walk_decay, df$node)
}
