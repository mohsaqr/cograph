# ===========================================================================
# Batch 7 — Centrality Zoo comparison batch
#
# igraph-facing calculators (thin glue over the base-R kernels in
# R/kernels-batch7.R) and the exported one-measure verbs.
# ===========================================================================

#' Distance entropy calculator
#' @keywords internal
#' @noRd
calculate_distance_entropy <- function(g, mode = "all", hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(g, mode)
  .cg_distance_entropy(hop_mat)
}

#' Local dimension calculator
#' @keywords internal
#' @noRd
calculate_local_dimension <- function(g, mode = "all", hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(g, mode)
  .cg_local_dimension(hop_mat)
}

#' Local information dimensionality calculator
#' @keywords internal
#' @noRd
calculate_local_information_dimension <- function(g, mode = "all",
                                                  hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(g, mode)
  .cg_local_information_dimension(hop_mat, n_total = igraph::vcount(g))
}

#' Modularity vitality calculator
#'
#' Follows the community-measure convention: a missing partition warns and
#' returns `NA`; a partition of the wrong length is a contract violation.
#' @keywords internal
#' @noRd
calculate_modularity_vitality <- function(g, weights = NULL,
                                          membership = NULL) {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  if (is.null(membership)) {
    warning("modularity_vitality requires membership; returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  .cg_modularity_vitality(.cg_path_matrix(g, weights), membership)
}

#' Neighborhood connectivity calculator
#' @keywords internal
#' @noRd
calculate_neighborhood_connectivity <- function(g, mode = "all") {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_neighborhood_connectivity(.cg_path_matrix(g, NULL), mode)
}

# ---------------------------------------------------------------------------
# Exported one-measure verbs
# ---------------------------------------------------------------------------

#' Distance Entropy
#'
#' Shannon entropy of the distribution of hop distances from a node to every
#' node it can reach (Stella & De Domenico 2018), normalised so that a
#' uniform spread over the node's distance range scores 1:
#' \deqn{h(i) = -\frac{1}{\log(M_i - m_i + 1)} \sum_{k = m_i}^{M_i}
#'   p_k^{(i)} \log p_k^{(i)}, \qquad p_k^{(i)} = n_k^{(i)} / R_i,}
#' where \eqn{n_k^{(i)}} is the number of nodes at distance \eqn{k} from
#' \eqn{i}, \eqn{R_i} the number of reachable nodes, and \eqn{m_i, M_i} the
#' minimum and maximum distance. High values mark nodes whose reach is
#' spread evenly across many network layers; a node whose reachable nodes
#' all sit at one distance scores 0. Closeness summarises the mean of the
#' same distribution; distance entropy summarises its spread.
#'
#' Distances are hop counts (edge weights are ignored). The original paper
#' normalises by \eqn{\log(M_i - m_i)}, which is undefined when only two
#' distinct distances occur; \eqn{\log(M_i - m_i + 1)} is used here so the
#' index is bounded by 1 for a uniform distribution.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   (distances along out-edges), or \code{"in"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node, in \[0, 1\].
#'   \code{NaN} for a node that reaches no other node.
#'
#' @references Stella, M., & De Domenico, M. (2018). Distance entropy
#'   cartography characterises centrality in complex networks. Entropy,
#'   20(4), 268.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_local_dimension}} for the growth-rate view of the
#'   same distance profile.
#'
#' @export
#' @examples
#' path4 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
#' rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")
#' centrality_distance_entropy(path4)
centrality_distance_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "distance_entropy", mode = mode, ...)
  stats::setNames(df[[paste0("distance_entropy_", mode)]], df$node)
}

#' Local Dimension
#'
#' Growth exponent of the ball around a node (Silva & Costa 2013; Pu et al.
#' 2014). Let \eqn{B_i(r)} be the number of nodes within \eqn{r} hops of
#' \eqn{i}, the node itself included. The local dimension is the slope of
#' \eqn{\ln B_i(r)} on \eqn{\ln r} over \eqn{r = 1, \ldots, d_{\max}(i)}:
#' \deqn{D_i = \frac{d \ln B_i(r)}{d \ln r}.}
#' A node that reaches most of the network in a few hops has a small
#' exponent, so **lower values mark more influential nodes**. When a node
#' has a single radius (it reaches every other node in one hop) the
#' regression is undefined and the discretised derivative
#' \eqn{r\, n_i(r) / B_i(r)} at \eqn{r = 1} is reported, where
#' \eqn{n_i(r)} counts the nodes at distance exactly \eqn{r}.
#'
#' The implementation reproduces the worked example in Wen & Deng (2019),
#' which reports 0.9231 for ring sizes 4, 5, 4, 4. Distances are hop counts;
#' edge weights are ignored.
#'
#' @inheritParams centrality_distance_entropy
#'
#' @return Named numeric vector, one value per node. \code{NaN} for a node
#'   that reaches no other node.
#'
#' @references
#' Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
#'   networks. arXiv:1209.2476.
#'
#' Pu, J., Chen, X., Wei, D., Liu, Q., & Deng, Y. (2014). Identifying
#'   influential nodes based on local dimension. EPL, 107(1), 10010.
#'
#' Wen, T., & Deng, Y. (2019). Identifying influential nodes based on fuzzy
#'   local dimension in complex networks. Chaos, Solitons & Fractals, 119,
#'   332-342.
#'
#' @seealso \code{\link{centrality_local_information_dimension}} for the
#'   entropy-weighted variant, \code{\link{centrality_distance_entropy}}.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_local_dimension(star5)
centrality_local_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_dimension", mode = mode, ...)
  stats::setNames(df[[paste0("local_dimension_", mode)]], df$node)
}

#' Local Information Dimensionality
#'
#' Entropy-weighted local dimension (Wen & Deng 2020). With
#' \eqn{p_i(l) = B_i(l) / N} the share of the network inside the box of
#' \eqn{l} hops around \eqn{i} (node included), the box information is
#' \eqn{I_i(l) = -p_i(l) \ln p_i(l)} and
#' \deqn{D^I_i = -\frac{d I_i(l)}{d \ln l},}
#' estimated as minus the least-squares slope of \eqn{I_i(l)} on
#' \eqn{\ln l} for \eqn{l = 1, \ldots, \lceil d_{\max}(i) / 2 \rceil}.
#' **Higher values mark more influential nodes.** When only one box size is
#' available the discretised derivative of the source paper,
#' \eqn{l (1 + \ln p_i(l))\, n_i(l) / N}, is reported.
#'
#' Distances are hop counts; edge weights are ignored.
#'
#' @inheritParams centrality_distance_entropy
#'
#' @return Named numeric vector, one value per node. \code{NaN} for a node
#'   that reaches no other node.
#'
#' @references Wen, T., & Deng, Y. (2020). Identification of influencers in
#'   complex networks by local information dimensionality. Information
#'   Sciences, 512, 549-562.
#'
#' @seealso \code{\link{centrality_local_dimension}}.
#'
#' @export
#' @examples
#' path5 <- matrix(0, 5, 5)
#' path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
#' rownames(path5) <- colnames(path5) <- LETTERS[1:5]
#' centrality_local_information_dimension(path5)
centrality_local_information_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_information_dimension", mode = mode,
                   ...)
  stats::setNames(df[[paste0("local_information_dimension_", mode)]],
                  df$node)
}

#' Modularity Vitality
#'
#' Contribution of a node to the modularity of a fixed partition
#' (Magelinski, Bartulovic & Carley 2021):
#' \deqn{V_Q(i) = Q(G, C) - Q(G - i,\; C \setminus \{i\}),}
#' the drop in Newman modularity when node \eqn{i} is deleted and the
#' remaining nodes keep their communities. Positive values mark community
#' hubs (removing them weakens the modular structure); negative values mark
#' bridges (removing them sharpens it). Weighted graphs use edge weights;
#' directed graphs use the Leicht-Newman directed modularity, as igraph
#' does.
#'
#' All \eqn{n} vitalities are computed in closed form from one matrix
#' product, without recomputing modularity \eqn{n} times.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param membership Community labels, one per node (integer, factor, or
#'   character). Required; without it the function warns and returns
#'   \code{NA}. Obtain one from \code{\link{detect_communities}}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node. \code{NaN} where
#'   deleting the node leaves a graph with no edges.
#'
#' @section Conditions:
#' Raises an error of class \code{cograph_bad_membership} when
#' \code{membership} is not one non-missing label per node.
#'
#' @references Magelinski, T., Bartulovic, M., & Carley, K. M. (2021).
#'   Measuring node contribution to community structure with modularity
#'   vitality. IEEE Transactions on Network Science and Engineering, 8(1),
#'   707-723.
#'
#' @seealso \code{\link{centrality_participation}},
#'   \code{\link{centrality_within_module_z}},
#'   \code{\link{detect_communities}}.
#'
#' @export
#' @examples
#' # Two triangles joined by one bridge edge (C -- D)
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_modularity_vitality(adj, membership = c(1, 1, 1, 2, 2, 2))
centrality_modularity_vitality <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "modularity_vitality",
                   membership = membership, ...)
  stats::setNames(df$modularity_vitality, df$node)
}

#' Neighborhood Connectivity
#'
#' Mean degree of a node's neighbours (Maslov & Sneppen 2002), the
#' "average neighbour degree" reported by Cytoscape:
#' \deqn{C_{NC}(i) = \frac{1}{k_i} \sum_{j \in N(i)} k_j.}
#' High values mark nodes attached to hubs. Isolates score 0. Under
#' \code{mode = "out"} the out-neighbours' out-degrees are averaged, under
#' \code{"in"} the in-neighbours' in-degrees.
#'
#' @inheritParams centrality_distance_entropy
#'
#' @return Named numeric vector, one value per node.
#'
#' @references Maslov, S., & Sneppen, K. (2002). Specificity and stability
#'   in topology of protein networks. Science, 296(5569), 910-913.
#'
#' @seealso \code{\link{centrality_degree}}, and \code{igraph::knn()} for
#'   the Barrat weighted generalisation.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_neighborhood_connectivity(star5)
centrality_neighborhood_connectivity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "neighborhood_connectivity", mode = mode, ...)
  stats::setNames(df[[paste0("neighborhood_connectivity_", mode)]], df$node)
}
