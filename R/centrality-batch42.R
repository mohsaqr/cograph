#' Neighborhood (neighbor distance) centrality
#' @keywords internal
#' @noRd
calculate_neighbor_distance <- function(g, order = 2, decay = 0.2,
                                        mass = "degree") {
  if (!is.numeric(order) || length(order) != 1L || !is.finite(order) ||
        order < 0 || order != trunc(order)) {
    stop("nd_order must be a single nonnegative whole number", call. = FALSE)
  }
  if (!is.numeric(decay) || length(decay) != 1L || !is.finite(decay)) {
    stop("nd_decay must be a single finite number", call. = FALSE)
  }
  mass <- match.arg(mass, c("degree", "coreness"))
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- if (identical(mass, "degree")) {
    rowSums(a)
  } else {
    .cg_coreness(a, n)
  }
  steps <- .cg_nb_walk_sums(a, theta, order)
  if (!length(steps)) return(theta)
  theta + Reduce(`+`, Map(function(s, k) decay^k * s, steps, seq_along(steps)))
}

#' Neighborhood centrality, and its neighbor distance special case
#'
#' Neighborhood centrality adds to a node's own benchmark centrality the
#' benchmark centrality of the nodes its walks reach, discounted once per
#' step:
#' \eqn{C^n_i(\theta)=\theta_i+a\sum_{j\in\Gamma_i}\theta_j
#' +a^2\sum_{l\in\Gamma_j\setminus i}\theta_l+\dots
#' +a^n\sum_{s\in\Gamma_{s-1}\setminus x}\theta_s}.
#' The sums are nested and each level excludes only the node the walk just
#' came from, so the \eqn{k}-th term sums \eqn{\theta} over the endpoints of
#' the **non-backtracking walks of length \eqn{k}** that start at \eqn{i},
#' once per walk. A walk may revisit a node it passed earlier, including
#' \eqn{i} itself; only immediate backtracking is barred. The Zoo calls the
#' setting \code{nd_mass = "degree"}, \code{nd_order = 2},
#' \code{nd_decay = 0.2} the *neighbor distance centrality*, and that is the
#' default here; it is the configuration the source recommends.
#'
#' \strong{This is not the same as summing over distance shells.} The
#' Centrality Zoo (section 2.279, equation 2.1) paraphrases the measure with
#' sums over \eqn{N^{(k)}(i)}, "the set of \eqn{k}-hop neighbors", which
#' visits each node at most once per level and never revisits a closer one.
#' The two readings agree on trees and disagree on any graph carrying a
#' triangle or a cycle of length at most \eqn{2n}, and the difference is a
#' per-node offset, not a rescaling. On the triangle-plus-pendant
#' \code{A-B, A-C, B-C, A-D} with the defaults, the walk sums of the source
#' give \code{4.16, 3.24, 3.24, 1.76} while distance shells would give
#' \code{4.00, 3.04, 3.04, 1.76}. cograph implements the source equation.
#' No shell variant is offered: the shell form appears only in a secondary
#' paraphrase, which also attributes the measure to a different paper whose
#' text does not contain it.
#'
#' The source states no normalization, so raw scores grow with
#' \code{nd_decay} and \code{nd_order}; \code{normalized = TRUE} max-scales
#' the finished vector and is a cograph convention. \code{nd_decay} is
#' \eqn{a\in[0,1]} in the source, which sweeps 0.1 to 0.5; cograph accepts
#' any finite value, and a negative or larger one leaves the source's
#' domain. \code{nd_order = 0} drops every sum and returns \eqn{\theta}
#' itself, which is what the source says \eqn{a=0} does.
#'
#' Uses the simple undirected unweighted skeleton, which is the source
#' domain: either arc creates one edge, parallel edges count once, and loops
#' are removed, since a loop would make "the node the walk just came from"
#' ambiguous. Edge weights, mode, cutoff and path-weight inversion are
#' ignored. Isolates have every sum empty and score \eqn{\theta_i}, which is
#' zero for both benchmarks; walks never leave a component, so the raw score
#' of a node is unchanged by adding a disconnected component. Empty graphs
#' return no scores. Core numbers follow \code{\link{centrality}}'s
#' \code{"coreness"}, so an isolate sits in the zero-shell. Cost is
#' \code{nd_order} dense matrix-vector products, O(n^2) each. Walk counts
#' grow geometrically in \code{nd_order}, so a large order overflows to
#' infinity; the source considers one to four steps.
#'
#' Numerical verification establishes agreement with the source equation as
#' printed in the author preprint, not parity with author software, which
#' does not exist, and not any claim about spreading performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param nd_order Number of steps \eqn{n}, a single nonnegative whole
#'   number; default two, the source's recommended setting. The source
#'   studies one to four steps. Zero returns the benchmark centrality.
#' @param nd_decay Per-step decay \eqn{a}, a single finite number; default
#'   0.2, the source's own value. The source's domain is \eqn{[0,1]}.
#' @param nd_mass Benchmark centrality \eqn{\theta}: \code{"degree"}
#'   (default) or \code{"coreness"}. These are the two the source uses.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Liu, Y., Tang, M., Zhou, T. and Do, Y. (2016). Identify
#'   influential spreaders in complex networks, the role of neighborhood.
#'   Physica A: Statistical Mechanics and its Applications, 452, 289-298.
#'   Section 2.3, equation 1, read in the author preprint arXiv:1511.00441v1
#'   page 4. \doi{10.1016/j.physa.2016.02.028}.
#' @seealso \code{\link{centrality_semilocal}} and
#'   \code{\link{centrality_extended_coreness}} for other neighborhood sums,
#'   and \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # Neighbor distance centrality: degree benchmark, two steps, a = 0.2
#' centrality_neighbor_distance(igraph::make_ring(6))
#'
#' # The source's other benchmark, and a wider neighborhood
#' centrality_neighbor_distance(igraph::make_star(7, mode = "undirected"),
#'                              nd_order = 3, nd_mass = "coreness")
centrality_neighbor_distance <- function(x, nd_order = 2, nd_decay = 0.2,
                                         nd_mass = "degree", ...) {
  df <- centrality(x, measures = "neighbor_distance", nd_order = nd_order,
                   nd_decay = nd_decay, nd_mass = nd_mass, ...)
  stats::setNames(df$neighbor_distance, df$node)
}
