#' LineRank from directed or ordinary undirected line-graph walks
#' @keywords internal
#' @noRd
calculate_linerank <- function(cg, weights = NULL, damping = 0.85,
                               aggregation = "probability",
                               normalized = FALSE) {
  aggregation <- match.arg(aggregation, c("probability", "weight"))
  if (!is.numeric(damping) || length(damping) != 1L ||
        !is.finite(damping) || damping < 0 || damping >= 1) {
    stop("LineRank damping must be finite and in [0,1)", call. = FALSE)
  }
  w <- if (is.null(weights)) rep(1, nrow(cg$edges)) else weights
  if (any(!is.finite(w)) || any(w < 0)) {
    stop("linerank requires finite nonnegative edge weights", call. = FALSE)
  }
  edges <- cg$edges[w > 0, , drop = FALSE]
  w <- w[w > 0]
  n <- cg$n
  m <- length(w)
  out <- numeric(n)
  if (!m) return(out)
  source <- edges[, 1L]
  target <- edges[, 2L]
  if (cg$directed) {
    adjacent <- outer(target, source, "==")
  } else {
    adjacent <- outer(source, source, "==") | outer(source, target, "==") |
      outer(target, source, "==") | outer(target, target, "==")
    diag(adjacent) <- FALSE
  }
  transition <- matrix(0, m, m)
  for (i in seq_len(m)) {
    neighbors <- which(adjacent[i, ])
    if (!length(neighbors)) {
      transition[i, ] <- 1 / m
    } else {
      # The source edge weight cancels from w_i*w_j after row normalization.
      scaled <- w[neighbors] / max(w[neighbors])
      probability <- scaled / sum(scaled)
      if (any(probability == 0)) {
        stop("linerank transition range exceeds double precision",
             call. = FALSE)
      }
      transition[i, neighbors] <- probability
    }
  }
  system <- diag(1, m) - damping * t(transition)
  rhs <- rep((1 - damping) / m, m)
  system[m, ] <- 1
  rhs[m] <- 1
  stationary <- tryCatch(solve(system, rhs), error = function(e) NULL)
  if (is.null(stationary) || any(!is.finite(stationary)) ||
        any(stationary < -1e-10)) {
    stop("linerank stationary system is numerically unstable", call. = FALSE)
  }
  stationary <- pmax(stationary, 0)
  stationary <- stationary / sum(stationary)
  if (aggregation == "weight") {
    edge_weight <- if (normalized) w / max(w) else w
    stationary <- stationary * edge_weight
  }
  for (i in seq_len(m)) {
    out[source[i]] <- out[source[i]] + stationary[i]
    out[target[i]] <- out[target[i]] + stationary[i]
  }
  if (any(!is.finite(out))) {
    stop("linerank raw scores overflow; use normalized = TRUE", call. = FALSE)
  }
  out
}

#' LineRank centrality
#'
#' Computes PageRank probabilities on the graph whose vertices represent
#' input edges, then aggregates them at the original endpoints. On directed
#' inputs, edge e can lead to f when the target of e is the source of f.
#' On undirected inputs, distinct edge states are adjacent when they share
#' an endpoint, following Kosa et al.'s clarification. This uses one state
#' per undirected edge. A pair sharing both endpoints is adjacent once.
#'
#' Line-graph transition weights are products of original edge weights.
#' Row normalization cancels the starting edge weight, so products need not
#' be formed. Uniform teleportation uses probability 1-damping; a dangling
#' edge state also redistributes uniformly. The latter is an explicit
#' cograph PageRank convention because the source does not pin dangling
#' behavior. Damping accepts [0,1), default 0.85; zero is a limit extension.
#'
#' Default \code{linerank_aggregation = "probability"} sums stationary edge
#' probabilities, following the definition's prose and the later study.
#' Raw scores then sum to two on a graph with edges. \code{"weight"}
#' additionally multiplies each probability by its original edge weight,
#' matching the weighted incidence aggregation in Kang et al.'s Algorithm2.
#' These conventions differ for weighted inputs and are not interchangeable.
#' The original pseudocode also has inconsistent row/column normalization;
#' this implementation follows its random-walk definition, corroborated by
#' the later paper, rather than claiming literal pseudocode equivalence.
#'
#' Retains direction, loops and remaining parallel edges as distinct states.
#' A directed loop can transition to itself. Undirected line graphs exclude
#' self transitions. Both aggregation choices count endpoint incidences, so
#' an original loop contributes twice at its node. These loop conventions
#' are explicit extensions. Generic \code{loops} and \code{simplify} apply
#' first. Finite nonnegative weights are supported; zero-weight edges are
#' absent. \code{weighted = FALSE} uses unit edge weights. Generic mode,
#' shortest-path inversion and cutoff do not affect the result. Isolates
#' score zero, edgeless inputs return zeros, and empty inputs return no scores.
#'
#' The native dense line-graph solve costs O(m cubed) time and O(m squared)
#' memory for m retained edges; this is not the authors' distributed
#' large-graph implementation. The measure must be requested explicitly.
#' Unresolvable transition ranges, unstable systems and overflowing raw
#' weighted aggregation raise errors. Maximum normalization supports raw
#' weight overflow by scaling weights first; tiny ratios can underflow.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param damping Edge-walk continuation probability in [0,1), default0.85.
#' @param linerank_aggregation Either probability (default) or weight.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Kang, U., Papadimitriou, S., Sun, J., & Tong, H. (2011).
#'   Centralities in Large Networks: Algorithms and Observations.
#'   SDM, 119-130. Definitions2-4, Algorithm2.
#'   \doi{10.1137/1.9781611972818.11}.
#'   Kosa, B., Balassi, M., Englert, P., & Kiss, A. (2015).
#'   Betweenness versus Linerank. Computer Science and Information Systems,
#'   12(1), 33-48, section4. \doi{10.2298/CSIS141101092K}.
#' @export
#' @examples
#' centrality_linerank(igraph::make_ring(4))
centrality_linerank <- function(x, damping = 0.85,
                                linerank_aggregation = "probability", ...) {
  df <- centrality(x, measures = "linerank", damping = damping,
                   linerank_aggregation = linerank_aggregation, ...)
  stats::setNames(df$linerank, df$node)
}
