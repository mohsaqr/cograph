#' Focal shortest-path credit in a simple undirected ego graph
#' @keywords internal
#' @noRd
.cg_focal_ego_credit <- function(b) {
  n <- nrow(b)
  neighbors <- .cg_adjlist(b, directed = FALSE)
  credit <- 0
  # Focal vertex is first. Propagate counts of shortest paths that have
  # passed through it, alongside total shortest-path counts from each source.
  for (s in seq.int(2L, n)) {
    distance <- rep(-1L, n)
    distance[s] <- 0L
    count <- through <- numeric(n)
    count[s] <- 1
    queue <- integer(n)
    queue[1L] <- s
    head <- tail <- 1L
    while (head <= tail) {
      u <- queue[head]
      head <- head + 1L
      for (v in neighbors[[u]]) {
        if (distance[v] < 0L) {
          distance[v] <- distance[u] + 1L
          tail <- tail + 1L
          queue[tail] <- v
        }
        if (distance[v] == distance[u] + 1L) {
          count[v] <- count[v] + count[u]
          through[v] <- through[v] + if (v == 1L) count[u] else through[u]
        }
      }
    }
    if (any(!is.finite(count))) {
      stop("Ego shortest-path counts exceed numeric range.", call. = FALSE)
    }
    targets <- which(count > 0 & seq_len(n) != 1L)
    credit <- credit + sum(through[targets] / count[targets])
  }
  credit / 2
}

#' Source-defined one-hop and two-hop localized bridging
#' @keywords internal
#' @noRd
calculate_localized_bridging <- function(g, radius = 1L) {
  b <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(b) <- 0
  n <- nrow(b)
  result <- numeric(n)
  neighbors <- .cg_adjlist(b, directed = FALSE)
  coefficient <- .cg_local_candidates(b, "bridging_coefficient")
  for (v in seq_len(n)) {
    first <- neighbors[[v]]
    if (length(first) < 2L) next
    if (radius == 1L) {
      # Every nonadjacent alter pair has the ego as one common neighbor.
      alters <- b[first, first, drop = FALSE]
      paths <- 1 + alters %*% alters
      ego <- sum(1 / paths[upper.tri(alters) & alters == 0])
    } else {
      ids <- c(v, setdiff(unique(c(first, unlist(neighbors[first]))), v))
      ego <- .cg_focal_ego_credit(b[ids, ids, drop = FALSE])
    }
    result[v] <- ego * coefficient[v]
  }
  result
}

#' Localized bridging centrality from ego betweenness
#'
#' Nanda and Kotz's localized bridging centrality is the product of a node's
#' unnormalized betweenness in its induced one-hop ego network and its
#' bridging coefficient. The coefficient is reciprocal focal degree divided
#' by the sum of reciprocal neighbor degrees, all measured in the original
#' graph. It is not computed from degrees truncated to the ego network.
#'
#' Each unordered pair of other ego-network vertices contributes the fraction
#' of its shortest paths that pass through the focal vertex. Endpoints are
#' excluded. Uses a simple unweighted undirected skeleton: either arc direction
#' creates an edge, loops are removed and parallel edges count once. Weights,
#' mode, inversion and cutoff are ignored. This projection is an explicit
#' cograph convention, not a directed or weighted generalization of LBC.
#'
#' Isolates and leaves score zero; the isolate value extends the undefined
#' bridging coefficient by zero. Complete graphs score zero. Disconnected
#' components are evaluated independently before optional maximum scaling.
#' Empty graphs return no scores. The one-hop calculation uses the
#' Everett-Borgatti common-neighbor shortcut in each ego network, with
#' worst-case O(n to the fourth power) time and O(n squared) memory for
#' dense matrix multiplication across all nodes.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides final scores by their maximum;
#'   all-zero scores remain zero. Ego betweenness is never scaled by ego size.
#' @return Named numeric vector in input node order.
#' @references Nanda, S. and Kotz, D. (2012). Localized Bridging Centrality.
#'   Handbook of Optimization in Complex Networks, pp. 197-224,
#'   equations 7.7-7.8. \doi{10.1007/978-1-4614-0857-4_7}.
#'   This author chapter restates their 2008 LBC definition.
#' @seealso \code{\link{centrality_extended_local_bridging}} for two-hop
#'   ego networks. \code{\link{centrality_local_bridging}} retains the
#'   distinct legacy score, inverse degree times bridging coefficient.
#' @export
#' @examples
#' centrality_localized_bridging(igraph::make_graph("Zachary"))
centrality_localized_bridging <- function(x, ...) {
  df <- centrality(x, measures = "localized_bridging", ...)
  stats::setNames(df$localized_bridging, df$node)
}

#' Extended local bridging centrality
#'
#' Macker's two-hop localized bridging centrality multiplies betweenness of
#' the focal node in its induced closed two-hop neighborhood by its bridging
#' coefficient. Degrees for that coefficient come from the original graph.
#' The ego network includes every edge between the selected vertices.
#' Its shortest paths can be up to four edges long; this is not global
#' betweenness with a path-length cutoff of two. Betweenness uses unordered
#' pairs, excludes endpoints, and is not normalized by ego-network size.
#'
#' Uses the same simple undirected unweighted projection and zero conventions
#' as \code{\link{centrality_localized_bridging}}. Macker's separate weighted
#' model uses link quality for degree and costs for paths; that model is
#' outside this implementation. Native breadth-first path counts cost
#' O(sum over ego networks of n_ego times (n_ego + m_ego)), at worst
#' O(n to the fourth power), with O(n squared) memory. This measure is
#' marked costly and must be selected explicitly or through \code{include}.
#'
#' @inheritParams centrality_localized_bridging
#' @return Named numeric vector in input node order.
#' @references Macker, J. P. (2016). An improved local bridging centrality
#'   model for distributed network analytics. MILCOM, pp. 600-605,
#'   sections IV-V, equation 5 and Table I.
#'   \doi{10.1109/MILCOM.2016.7795393}.
#' @export
#' @examples
#' centrality_extended_local_bridging(igraph::make_graph("Zachary"))
centrality_extended_local_bridging <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "extended_local_bridging", ...)
  stats::setNames(df$extended_local_bridging, df$node)
}
