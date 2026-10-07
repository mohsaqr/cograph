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
calculate_localized_bridging <- function(cg, radius = 1L) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
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

#' Localized Bridging Centrality
#'
#' Localized bridging centrality (Nanda and Kotz 2012) is the product of a
#' node's betweenness \eqn{B^{ego}_i}{B_ego(i)} in its one-hop ego network
#' and its bridging coefficient, the reciprocal of its degree divided by
#' the sum of the reciprocal degrees of its neighbors:
#' \deqn{LBC_i = B^{ego}_i \, \frac{1/d_i}{\sum_{j \in N(i)} 1/d_j}.}{
#'   LBC_i = B_ego(i) (1/d_i) / sum_{j in N(i)} (1/d_j).}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Degrees are taken from the whole network. Ego betweenness counts
#' unordered pairs of the other ego-network nodes, excludes endpoints and
#' is not normalized. Isolated nodes, leaves and every node of a complete
#' graph score zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Nanda, S. and Kotz, D. (2012). Localized Bridging Centrality. In Handbook
#'   of Optimization in Complex Networks, pp. 197-224.
#'   \doi{10.1007/978-1-4614-0857-4_7}.
#' @seealso \code{\link{centrality_extended_local_bridging}},
#'   \code{\link{centrality_local_bridging}},
#'   \code{\link{centrality_ego_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_localized_bridging(regulation_net)
centrality_localized_bridging <- function(x, ...) {
  df <- centrality(x, measures = "localized_bridging", ...)
  stats::setNames(df$localized_bridging, df$node)
}

#' Extended Local Bridging Centrality
#'
#' Extended local bridging centrality (Macker 2016) multiplies a node's
#' betweenness in its two-hop ego network by its bridging coefficient, the
#' reciprocal of its degree divided by the sum of the reciprocal degrees of
#' its neighbors. The two-hop ego network contains every node within two
#' hops and every edge among them, so its shortest paths can have up to
#' four edges.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Degrees are taken from the whole network. Ego betweenness counts
#' unordered pairs, excludes endpoints and is not normalized. Isolated
#' nodes, leaves and every node of a complete graph score zero. The
#' weighted model of Macker (2016), with link quality and path costs, is
#' not implemented.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Macker, J. P. (2016). An improved local bridging centrality model for
#'   distributed network analytics. MILCOM, pp. 600-605.
#'   \doi{10.1109/MILCOM.2016.7795393}.
#' @seealso \code{\link{centrality_localized_bridging}},
#'   \code{\link{centrality_bridging}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_extended_local_bridging(regulation_net)
centrality_extended_local_bridging <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "extended_local_bridging", ...)
  stats::setNames(df$extended_local_bridging, df$node)
}
