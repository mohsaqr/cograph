#' Calculate the batch 68 measures
#'
#' PhysarumSpreader; `NA` with a warning without `physarum_steps`
#' @keywords internal
#' @noRd
calculate_physarum_spreader <- function(cg, weights = NULL, steps = NULL) {
  .cg_check_physarum_steps(steps)
  w <- .cg_candidate_adjacency(cg, weights, "physarum_spreader")
  n <- nrow(w)
  if (n == 0L) return(numeric(0))
  if (is.null(steps)) {
    .cg_warn_undefined("`physarum_spreader` has no value on this input, so ",
                       "its column is NA: it needs the number of updates ",
                       "`physarum_steps`, which has no default because the ",
                       "source gives no stopping rule.")
    return(rep(NA_real_, n))
  }
  .cg_physarum_spreader(w, steps)
}

#' ArticleRank; `NA` with a warning where the iteration diverges
#' @keywords internal
#' @noRd
calculate_articlerank <- function(cg, references = NULL, damping = 0.85) {
  references <- .cg_check_ar_references(references, cg$labels)
  out <- .cg_articlerank(.cg_path_matrix(cg, NULL), references, damping)
  if (identical(out$status, "diverges")) {
    .cg_warn_undefined(sprintf(paste0(
      "`articlerank` has no value on this input, so its column is NA: the ",
      "spectral radius of the map of equation (2) is %.4g, not below 1, so ",
      "the iteration diverges. Citation networks are acyclic; on a cyclic ",
      "network a smaller `damping` or larger `ar_references` may help."),
      out$radius))
  }
  out$value
}

#' Entropy-based gravity model; `NA` with a warning off its domain
#' @keywords internal
#' @noRd
calculate_entropy_gravity <- function(cg, radius = 3, damping = 0.85) {
  .cg_check_radius(radius, "entropy_gravity_radius")
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  reason <- if (n < 2L) {
    "the network has one node, and K = 1 / ln N in equation (3) is 1 / 0"
  } else if (.cg_n_components(b) > 1L) {
    "closeness (1 / sum of distances) has no value on a disconnected network"
  }
  if (is.null(reason)) {
    out <- .cg_entropy_gravity(b, radius, damping)
    if (identical(out$status, "ok")) return(out$value)
    reason <- paste("one of the attributes (node H-index, closeness,",
                    "betweenness, PageRank) takes the same value at every",
                    "node, so its min-max scaling in equation (2) is 0 / 0")
  }
  .cg_warn_undefined("`entropy_gravity` has no value on this input, so its ",
                     "column is NA: ", reason, ".")
  rep(NA_real_, n)
}

#' Influence capability; `NA` with a warning on a network without edges
#' @keywords internal
#' @noRd
calculate_influence_capability <- function(cg) {
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  if (!any(b > 0)) {
    .cg_warn_undefined("`influence_capability` has no value on this input, ",
                       "so its column is NA: the network has no edge, so ",
                       "ICP and ICN are 0 at every node and equation (5) is ",
                       "0 / 0.")
    return(rep(NA_real_, n))
  }
  .cg_influence_capability(b)
}

#' @keywords internal
#' @noRd
calculate_ehc <- function(cg, p = 0.8, r = 100) {
  .cg_check_ehc_args(p, r)
  .cg_ehc(.cg_skeleton(cg), p, r)
}

#' @keywords internal
#' @noRd
calculate_erm <- function(cg) .cg_erm(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_shell_clustering <- function(cg) {
  .cg_shell_clustering(.cg_skeleton(cg))
}

#' @keywords internal
#' @noRd
calculate_ecrm <- function(cg) .cg_ecrm(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_weight_neighborhood <- function(cg, benchmark = "degree",
                                          alpha = 1) {
  .cg_check_wnc_args(benchmark, alpha)
  .cg_weight_neighborhood(.cg_skeleton(cg), benchmark, alpha)
}

#' PhysarumSpreader
#'
#' PhysarumSpreader (Wang et al. 2015) lets a resource flow over the
#' network as in the Physarum model, through a ground node linked both ways
#' to every node, with the conductivity of each arc adapting to its flux,
#' and scores a node by the resource it holds after \eqn{t} updates:
#' \deqn{S_i = I_i(t) + \frac{I_g(t)}{N}, \qquad I_i(t+1) = \sum_j Q_{ji}(t) I_j(t), \qquad Q_{ij}(t) = \frac{w_{ij} D_{ij}(t)}{\sum_l w_{il} D_{il}(t)}, \qquad D_{ij}(t+1) = \frac{Q_{ij}(t) + D_{ij}(t)}{2},}{S_i = I_i(t) + I_g(t) / N,  I_i(t+1) = sum_j Q_ji(t) I_j(t),  Q_ij(t) = w_ij D_ij(t) / sum_l w_il D_il(t),  D_ij(t+1) = (Q_ij(t) + D_ij(t)) / 2,}
#' with \eqn{I(0) = 1} at every node and 0 at the ground node \eqn{g},
#' \eqn{D(0) = 1}, \eqn{w_{ij}}{w_ij} the weight of the arc \eqn{i \to j}{i
#' -> j}, \eqn{w_{ig}}{w_ig} the mean weight of the arcs leaving \eqn{i} (1
#' when there is none), \eqn{w_{gi} = 1}{w_gi = 1} and \eqn{N} the number
#' of nodes.
#'
#' @details
#' The measure uses the direction and the weights of the arcs; an
#' undirected tie is an arc each way. The source reads a weight as a
#' strength; for a cost, give its reciprocal. Loops are ignored. The
#' conductivities of the ground links stay at 1, as the source's equation
#' (10) adapts only the arcs between nodes. \code{physarum_steps}, the
#' number of updates \eqn{t}, has no default, because the source gives no
#' stopping rule: as \eqn{t} grows every score tends to 1, so a stopping
#' rule by tolerance would return a constant (owner decision 2026-11-26).
#' Without \code{physarum_steps} the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. On four test
#' networks, with \code{physarum_steps = 52}, the score ranks nodes closest
#' to \code{\link{centrality_beta_measure}} (mean Kendall correlation
#' 0.987, 0.984 at the least) and \code{leverage} (0.96); with
#' \code{\link{centrality_pagerank}} the correlation is 0.84 to 0.91. On
#' the karate club the ranking after 52 updates has a Kendall correlation
#' of 0.989 with the ranking after 10000, while the range of the scores
#' shrinks from 0.115 to 0.0006. The scores the source prints for its
#' four-node example are those after 52 updates. With \code{physarum_steps
#' = 52} the implementation reproduces the printed scores of nodes 3 and 4
#' (1.0002 and 0.9801) and gives 1.0097 to node 1 and 1.0100 to node 2,
#' where the source prints them the other way round. The source audit found
#' no update order consistent with the source's printed first flux matrix
#' that gives node 1 the larger score after the fifth update, so the
#' exchange is declared here, not resolved. The printed first flux matrix
#' is reproduced. A \code{physarum_steps} that is not a whole number of at
#' least one raises a \code{cograph_bad_parameter} error, and negative
#' weights a \code{cograph_negative_weights} error. \code{normalized =
#' TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{physarum_steps} (the number of updates, required) and the
#' edge weights (\code{weighted}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, H., Zhang, Y., Zhang, Z., Mahadevan, S. and Deng, Y. (2015).
#' PhysarumSpreader: a new bio-inspired methodology for identifying
#' influential spreaders in complex networks. PLoS ONE, 10(12), e0145028.
#' \doi{10.1371/journal.pone.0145028}.
#' @seealso \code{\link{centrality_pagerank}},
#' \code{\link{centrality_leaderrank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_physarum_spreader(regulation_net, physarum_steps = 52)
#' centrality_physarum_spreader(regulation_net, physarum_steps = 52, weighted = FALSE)
centrality_physarum_spreader <- function(x, ...) {
  df <- centrality(x, measures = "physarum_spreader", ...)
  stats::setNames(df$physarum_spreader, df$node)
}

#' ArticleRank
#'
#' ArticleRank (Li and Willett 2009) is a PageRank-style score for citation
#' networks in which a citing node passes on its score in a share set by
#' its number of references:
#' \deqn{AR(A) = 1 - d + d\,\overline{NR} \sum_{P \to A} \frac{AR(P)}{\overline{NR} + NR(P)},}{AR(A) = 1 - d + d NRbar sum_{P -> A} AR(P) / (NRbar + NR(P)),}
#' with \eqn{P \to A}{P -> A} when \eqn{P} cites \eqn{A}, \eqn{NR(P)} the
#' number of references of \eqn{P}, \eqn{\overline{NR}}{NRbar} its mean
#' over the nodes and \eqn{d} the damping factor.
#'
#' @details
#' The measure uses the direction of the arcs and ignores weights; an
#' undirected tie is an arc each way, and loops are ignored. This is
#' equation (2) of the source, with the mean number of references as a
#' multiplier; the Centrality Zoo prints the form without it, which the
#' source's printed numbers rule out. The source counts all the references
#' of a paper, also those outside the network, so \code{ar_references}
#' takes one count per node (in node order, or named by node); \code{NULL},
#' the default, takes the out-degree (owner decision 2026-11-26).
#' \code{damping} is \eqn{d}, 0.85 by default as in the source. The scores
#' are not normalised; a node nobody cites scores \eqn{1 - d}. The source
#' iterates the equation from \eqn{1 - d} to a precision of
#' \eqn{10^{-10}}{1e-10}; that iteration converges, to the solution of the
#' linear system computed here, exactly when the spectral radius of its
#' linear map is below 1. A citation network without cycles has radius 0. A
#' cyclic network can have radius 1 or more, an undirected one as soon as
#' its degrees are large (an undirected network where every node has degree
#' \eqn{k} and the references are the degrees has radius \eqn{dk/2}); the
#' score then has no value, and the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA} rather than
#' switching to another form. The implementation reproduces the 13 terms
#' \eqn{AR(P)/(\overline{NR} + NR(P))}{AR(P) / (NRbar + NR(P))} of the
#' source's Table 5 whose citing paper is not cited itself, and, on a
#' citation network built so that the 46 citing papers of Table 5 have
#' their printed scores, the printed difference between the scores of
#' papers P004 and P005 (0.49716); with the same citing scores, the form
#' without the multiplier gives a difference 35.6 times smaller. On the
#' four undirected test networks of the other measures' screens the score
#' has no value; oriented from lower to higher node number, the four have
#' Kendall correlations of at most 0.24 in absolute value between this
#' score and any other measure. An \code{ar_references} that is not one
#' finite nonnegative number per node, or whose names do not match the
#' nodes, raises a \code{cograph_bad_parameter} error, as do references
#' that are all zero on a network with arcs. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ar_references} (the number of references of each node,
#' default the out-degree) and \code{damping} (default 0.85).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, J. and Willett, P. (2009). ArticleRank: a PageRank-based alternative
#' to numbers of citations for analysing citation networks. Aslib
#' Proceedings, 61(6), 605-618. \doi{10.1108/00012530911005544}.
#' @seealso \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' # Four papers: B, C and D cite A, and D cites B.
#' citations <- matrix(c(0, 0, 0, 0,  1, 0, 0, 0,  1, 0, 0, 0,  1, 1, 0, 0), 4, byrow = TRUE,
#'                     dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' centrality_articlerank(citations)
#' centrality_articlerank(citations, ar_references = c(A = 12, B = 30, C = 8, D = 45))
#' centrality_articlerank(regulation_net, damping = 0.5)
centrality_articlerank <- function(x, ...) {
  df <- centrality(x, measures = "articlerank", ...)
  stats::setNames(df$articlerank, df$node)
}

#' Entropy-Based Gravity Model
#'
#' The entropy-based gravity model (Yan et al. 2020) combines the node
#' H-index, closeness, betweenness and PageRank of a node into one mass
#' with entropy weights, and adds the gravity between the node and the
#' nodes within \eqn{r} steps:
#' \deqn{G(i) = \sum_{j:\, 1 \le d_{ij} \le r} \frac{C_i C_j}{d_{ij}^2}, \qquad C_i = \sum_{k=1}^{4} w_k b_{ik}, \qquad w_k = \frac{1 - S_k}{4 - \sum_l S_l},}{G(i) = sum_{j: 1 <= d_ij <= r} C_i C_j / d_ij^2,  C_i = sum_k w_k b_ik,  w_k = (1 - S_k) / (4 - sum_l S_l),}
#' with \eqn{d_{ij}}{d_ij} the distance, \eqn{b_{ik}}{b_ik} the \eqn{k}-th
#' attribute of node \eqn{i} min-max scaled over the nodes, and \eqn{S_k}
#' the entropy of the shares \eqn{b_{ik} / \sum_i b_{ik}}{b_ik / sum_i
#' b_ik} divided by \eqn{\ln N}{ln N}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source prints its equation (3) with the shares taken over the
#' attributes of one node and its equation (5) with the entropies \eqn{S_k}
#' in place of the weights \eqn{w_k}. Its text describes the entropy of
#' each attribute across the nodes, a lower entropy giving a larger weight,
#' and a combination by weighted averaging, which is what is computed
#' (owner decision 2026-11-26); read literally, equation (3) gives a
#' negative weight on the kite network. As in the source's equation (3), an
#' attribute at which only one node is above the minimum takes the shares
#' \eqn{(1 + b_{ik}) / \sum_i (1 + b_{ik})}{(1 + b_ik) / sum_i (1 + b_ik)}
#' instead. The attributes are those of the source's chosen combination:
#' the node H-index (\code{\link{centrality_lobby}}), closeness as the
#' reciprocal of the sum of distances, betweenness, and PageRank
#' (\code{\link{centrality_pagerank}}) with \code{damping}, which at
#' \code{damping = 1} is the degree share, the stationary distribution of
#' the random walk; min-max scaling removes their scale.
#' \code{entropy_gravity_radius} is \eqn{r}, 3 by default as in the source
#' (the Centrality Zoo says 2); \code{Inf} admits every node. Scaled values
#' within \eqn{10^{-9}}{1e-9} of 0 count as the minimum, so that nodes
#' equal in exact arithmetic are not separated by rounding. An attribute
#' with the same value at every node, as on any regular network, makes the
#' min-max scaling 0/0; the measure then warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. It does the
#' same on a disconnected or a one-node network, where closeness has no
#' value. The source prints no value, so the implementation is checked
#' against the definition only. On four test networks the score ranks nodes
#' closest to \code{multi_evidence} and \code{hybrid} (mean Kendall
#' correlation 0.90); with closeness the correlation is 0.77 to 0.92. A
#' \code{entropy_gravity_radius} that is not a whole number of at least one
#' or \code{Inf} raises a \code{cograph_bad_parameter} error.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{entropy_gravity_radius} (default 3) and \code{damping} (the
#' PageRank damping factor, default 0.85).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yan, X.-L., Cui, Y.-P. and Ni, S.-J. (2020). Identifying influential
#' spreaders in complex networks based on entropy weight method and gravity
#' law. Chinese Physics B, 29(4), 048902. \doi{10.1088/1674-1056/ab77fe}.
#' @seealso \code{\link{centrality_gravity}}, \code{\link{centrality_egm}},
#' \code{\link{centrality_lobby}}, \code{\link{centrality_pagerank}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_entropy_gravity(to_undirected(regulation_net))
#' centrality_entropy_gravity(to_undirected(regulation_net), entropy_gravity_radius = 2)
centrality_entropy_gravity <- function(x, ...) {
  df <- centrality(x, measures = "entropy_gravity", ...)
  stats::setNames(df$entropy_gravity, df$node)
}

#' Influence Capability
#'
#' Influence capability (IC; Wang et al. 2017) adds a position term from
#' the k-shell decomposition and a neighborhood term, each divided by its
#' total and weighted by the entropy weight method:
#' \deqn{IC(i) = w_1 \frac{ICP(i)}{\sum_l ICP(l)} + w_2 \frac{ICN(i)}{\sum_l ICN(l)}, \qquad ICP(i) = k_i - |\Gamma(i)| + \sum_{j \in \Gamma(i)} \tfrac{2}{\pi} \arctan Iter(j)^{1/3}, \qquad ICN(i) = \sum_{j \in N(i)} \sum_{l \in N(j)} k_l,}{IC(i) = w_1 ICP(i) / sum_l ICP(l) + w_2 ICN(i) / sum_l ICN(l),  ICP(i) = k_i - |Gamma(i)| + sum_{j in Gamma(i)} (2 / pi) atan(Iter(j)^(1/3)),  ICN(i) = sum_{j in N(i)} sum_{l in N(j)} k_l,}
#' with \eqn{k} the degree, \eqn{Iter(j)} the iteration of the k-shell
#' decomposition in which \eqn{j} is removed, counted across the shells
#' from 1, \eqn{\Gamma(i)}{Gamma(i)} the neighbors of \eqn{i} removed in an
#' earlier iteration, and \eqn{w_x = (1 - H_x) / (2 - H_1 - H_2)}{w_x = (1
#' - H_x) / (2 - H_1 - H_2)}, \eqn{H_x} the entropy of the shares of
#' attribute \eqn{x} divided by \eqn{\ln n}{ln n}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. An
#' iteration removes at once every remaining node whose degree is at most
#' the current level, which starts at 1; a check that removes nothing
#' raises the level and does not count. An isolated node is removed in the
#' first iteration, with the leaves. The source's equation (4) weights
#' "normalized" attributes without saying how; they are divided by their
#' totals, the normalisation of its equation (5) (owner decision
#' 2026-11-26). Its equation (7) is printed with a leading minus sign,
#' which its positive weights rule out. When both attributes take one value
#' at every node, as on a regular network, equation (7) is 0/0; the weights
#' are then one half each, which does not change the scores. The
#' implementation reproduces the iteration numbers, position terms and
#' neighborhood terms printed in the source's worked example, the 14 rank
#' groups of the IC column of its Table 2, the ranking of its Table 1 (once
#' three labels of its Fig. 3, which the table numbers differently, are
#' exchanged), and the monotonicity of its Table 3 on the dolphins network
#' (0.9958) and the karate club (0.9542, which is also the largest value an
#' exact structural measure can reach there). The eight entropy weights the
#' source prints for these four networks do not follow from its equations
#' (they differ by 0.003 to 0.042); no printed ranking depends on them. On
#' a network without edges equation (5) is 0/0, and the measure warns
#' (class \code{cograph_undefined_measure}) and returns \code{NA}. On four
#' test networks the score ranks nodes closest to
#' \code{\link{centrality_lhc}} (mean Kendall correlation 0.95) and
#' \code{eskh} (0.93); with the degree the correlation is 0.79 to 0.91.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, Z., Du, C., Fan, J. and Xing, Y. (2017). Ranking influential nodes
#' in social networks based on node position and neighborhood.
#' Neurocomputing, 260, 466-477. \doi{10.1016/j.neucom.2017.04.064}.
#' @seealso \code{\link{centrality_coreness}},
#' \code{\link{centrality_ks_if}}, \code{\link{centrality_degree}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_influence_capability(regulation_net)
centrality_influence_capability <- function(x, ...) {
  df <- centrality(x, measures = "influence_capability", ...)
  stats::setNames(df$influence_capability, df$node)
}

#' Extended H-Index Centrality
#'
#' The extended H-index centrality (EHC; Zareie and Sheikhahmadi 2019) adds
#' over a node's neighbors a weighted count of the degrees of their own
#' neighbors:
#' \deqn{EHC(i) = \sum_{j \in N(i)} CMC(j), \qquad CMC(j) = \sum_{k=1}^{h} p^{1 + kp/r} c_k(j),}{EHC(i) = sum_{j in N(i)} CMC(j),  CMC(j) = sum_{k=1}^{h} p^(1 + k p / r) c_k(j),}
#' with \eqn{c_k(j)} the number of neighbors of \eqn{j} of degree at least
#' \eqn{k} and \eqn{h} the largest degree of the network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{ehc_p = 0.8} and \code{ehc_r = 100} are the authors' values (their
#' Section 4.3). The source gives \eqn{[0, 1]} as the range of both
#' parameters but uses \eqn{r} from 1 to 10000; \code{ehc_p} must lie in
#' \eqn{(0, 1]} and \code{ehc_r} be above 0. Since \eqn{\sum_k
#' c_k(j)}{sum_k c_k(j)} is the sum of the degrees of the neighbors of
#' \eqn{j}, CMC is that sum with the units of degree weighted by the slowly
#' decreasing \eqn{p^{1 + kp/r}}{p^(1 + k p / r)}; at \code{ehc_p = 1} EHC
#' is the sum, over the walks of two steps from the node, of the degree of
#' the node reached. Despite its name the measure ranks nodes far from the
#' h-index family. An isolated node scores 0. With \code{ehc_p = 0.5} and
#' \code{ehc_r = 10} the implementation reproduces the worked example on
#' the source's Fig. 1: the largest degree 7, the two printed cumulative
#' vectors, the 12 printed CMC values and \eqn{EHC(1) = 21.616},
#' \eqn{EHC(3) = 42.106}. The source prints a monotonicity of 0.9612 on the
#' karate club, above 0.9542, the largest value an exact structural measure
#' can reach there, which is the value of the implementation; its
#' monotonicity on the dolphins network (0.9979) is reproduced and is also
#' the largest possible there. On four test networks the score ranks nodes
#' closest to \code{\link{centrality_ninl}} (mean Kendall correlation
#' 0.985, 0.997 on one network) and
#' \code{\link{centrality_diffusion_centrality}} (0.983); with
#' \code{\link{centrality_semilocal}} the correlation is 0.91 to 0.92. A
#' \code{ehc_p} outside \eqn{(0, 1]} or a \code{ehc_r} that is not a finite
#' number above 0 raises a \code{cograph_bad_parameter} error.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ehc_p} (default 0.8) and \code{ehc_r} (default 100).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A. and Sheikhahmadi, A. (2019). EHC: extended H-index centrality
#' measure for identification of users' spreading influence in complex
#' networks. Physica A: Statistical Mechanics and its Applications, 514,
#' 141-155. \doi{10.1016/j.physa.2018.09.064}.
#' @seealso \code{\link{centrality_lobby}},
#' \code{\link{centrality_diffusion_centrality}},
#' \code{\link{centrality_ninl}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ehc(regulation_net)
#' centrality_ehc(regulation_net, ehc_p = 0.5, ehc_r = 10)
centrality_ehc <- function(x, ...) {
  df <- centrality(x, measures = "ehc", ...)
  stats::setNames(df$ehc, df$node)
}

#' Entropy-Based Ranking Measure
#'
#' The entropy-based ranking measure (ERM; Zareie et al. 2017) gives each
#' node an entropy centrality from the spread of its neighbors' degrees and
#' of their degree sums, and adds it over the walks of two steps:
#' \deqn{ERM(i) = \sum_{j \in N(i)} \sum_{l \in N(j)} EC(l), \qquad EC(i) = E_1(i) + \lambda_i E_2(i), \qquad \lambda_i = \frac{k^{(2)}_i}{\max_l k^{(2)}_l},}{ERM(i) = sum_{j in N(i)} sum_{l in N(j)} EC(l),  EC(i) = E_1(i) + lambda_i E_2(i),  lambda_i = k2_i / max_l k2_l,}
#' with \eqn{E_1(i)} the entropy of the shares \eqn{k_j / k^{(1)}_i}{k_j /
#' k1_i} over the neighbors \eqn{j} of \eqn{i}, \eqn{E_2(i)} that of the
#' shares \eqn{k^{(1)}_j / k^{(2)}_i}{k1_j / k2_i}, \eqn{k} the degree,
#' \eqn{k^{(1)}_i}{k1_i} the sum of the neighbors' degrees and
#' \eqn{k^{(2)}_i}{k2_i} the sum of the neighbors' \eqn{k^{(1)}}{k1}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source's equation (4) is printed with \eqn{v_j} on its right side;
#' the sentence that introduces it, and the same authors' restatement
#' (Zareie and Sheikhahmadi 2019, equation 9), have \eqn{v_i}. The source
#' gives no base for the logarithm; the natural logarithm is used (owner
#' decision 2026-11-26), and another base multiplies every score by one
#' constant. A leaf has \eqn{EC = 0}; a node scores through the nodes its
#' walks of two steps reach, so a leaf of a star scores 0, and so does an
#' isolated node. The source prints no score. The implementation reproduces
#' the share of distinct ranks and the monotonicity of its 21-node example
#' (0.8095 and 0.9623) and of the dolphins network (0.9677 and 0.9979), and
#' the monotonicity Zareie et al. (2020) print for the adjective-noun
#' network (0.9997); each of these is also the largest value an exact
#' structural measure can reach on its network, so they show only that the
#' measure separates every pair of nodes the network does not make
#' equivalent. On the karate club the source prints 0.8529 and 0.9612,
#' above the largest values an exact structural measure can reach there
#' (0.7941 and 0.9542, the values of the implementation). On four test
#' networks the score ranks nodes closest to \code{\link{centrality_ninl}}
#' (mean Kendall correlation 0.984) and
#' \code{\link{centrality_diffusion_centrality}} (0.978); with
#' \code{\link{centrality_eigenvector}} the correlation is 0.92 to 0.99.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A. and Fatemi, A. (2017). Influential nodes
#' ranking in complex networks: an entropy-based approach. Chaos, Solitons
#' and Fractals, 104, 485-494. \doi{10.1016/j.chaos.2017.09.010}.
#'
#' Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
#' Finding influential nodes in social networks based on neighborhood
#' correlation coefficient. Knowledge-Based Systems, 194, 105580.
#' \doi{10.1016/j.knosys.2020.105580}.
#' @seealso \code{\link{centrality_ehc}}, \code{\link{centrality_ecrm}},
#' \code{\link{centrality_entropy}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_erm(regulation_net)
centrality_erm <- function(x, ...) {
  df <- centrality(x, measures = "erm", ...)
  stats::setNames(df$erm, df$node)
}

#' Shell Clustering Coefficient
#'
#' The shell clustering coefficient (SCC; Zareie et al. 2020) adds over a
#' node's neighbors one term that is large when their positions in the
#' network's peeling order differ and one that grows with the neighbor's
#' degree:
#' \deqn{SCC_i = \sum_{j \in N(i)} \Bigl[(2 - C_{ij}) + \Bigl(2 \frac{k_j}{\max_l k_l} + 1\Bigr)\Bigr],}{SCC_i = sum_{j in N(i)} [(2 - C_ij) + (2 k_j / max_l k_l + 1)],}
#' with \eqn{k} the degree and \eqn{C_{ij}}{C_ij} the Pearson correlation
#' of the shell vectors of \eqn{i} and \eqn{j}. The shell vector of a node
#' counts its neighbors in each hierarchy, the hierarchy of a node being
#' the round in which it is removed when every round removes all remaining
#' nodes of the lowest degree.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The hierarchies follow the text of the source's Section 3 and are the
#' values of \code{\link{centrality_iksd}}. The source's Algorithm 1 says
#' only "the k-shell algorithm"; the usual peeling with a rising threshold
#' gives other hierarchies on some networks (3 of the 34 nodes of the
#' karate club, 30 of the 62 of the dolphins network). When a shell vector
#' takes one value in every hierarchy, as when every node of a regular
#' network leaves in the first round, the correlation is 0/0; it is taken
#' as 0 (owner decision 2026-11-26). The two brackets are added, as in the
#' source's Algorithm 1. An isolated node, and every node of a network
#' without edges, scores 0. The source prints no score; the implementation
#' reproduces the hierarchies of the 17 nodes of its Fig. 1, its three
#' printed shell vectors and its correlation \eqn{C_{8,9} \approx
#' 0.75}{C_8,9 = 0.75 (approximately)}. \code{\link{centrality_ecrm}} is
#' the source's ranking measure built on this one. On four test networks
#' the score ranks nodes closest to \code{graph_regularization},
#' \code{current_flow_closeness} and \code{information} (mean Kendall
#' correlation 0.89); with the degree the correlation is 0.83 to 0.93.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
#' Finding influential nodes in social networks based on neighborhood
#' correlation coefficient. Knowledge-Based Systems, 194, 105580.
#' \doi{10.1016/j.knosys.2020.105580}.
#' @seealso \code{\link{centrality_ecrm}}, \code{\link{centrality_iksd}},
#' \code{\link{centrality_transitivity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_shell_clustering(regulation_net)
centrality_shell_clustering <- function(x, ...) {
  df <- centrality(x, measures = "shell_clustering", ...)
  stats::setNames(df$shell_clustering, df$node)
}

#' Extended Cluster Coefficient Ranking Measure
#'
#' The extended cluster coefficient ranking measure (ECRM; Zareie et al.
#' 2020) adds the shell clustering coefficient over the walks of two steps
#' from a node:
#' \deqn{ECRM_i = \sum_{j \in N(i)} CRM_j, \qquad CRM_j = \sum_{l \in N(j)} SCC_l,}{ECRM_i = sum_{j in N(i)} CRM_j,  CRM_j = sum_{l in N(j)} SCC_l,}
#' with \eqn{SCC} the shell clustering coefficient of
#' \code{\link{centrality_shell_clustering}}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' These are equations (5) and (6) of the source and lines 15 to 26 of its
#' Algorithm 1; the node itself is reached once for each of its neighbors.
#' The Centrality Zoo's entry for ECRM gives no source; it is this one. The
#' conventions are those of \code{\link{centrality_shell_clustering}}:
#' hierarchies by the lowest-degree peeling, and a correlation that is 0/0
#' taken as 0. An isolated node scores 0. The source prints no score. The
#' implementation reproduces the monotonicity of the source's Table 2 on
#' the dolphins network (0.9979) and on the adjective-noun network of David
#' Copperfield (0.9997); each of these is also the largest value an exact
#' structural measure can reach on its network, so they show only that the
#' measure separates every pair of nodes the network does not make
#' equivalent. On four test networks the score ranks nodes closest to
#' \code{\link{centrality_diffusion_centrality}} (mean Kendall correlation
#' 0.967) and \code{\link{centrality_ninl}} (0.964); with
#' \code{\link{centrality_eigenvector}} the correlation is 0.93 to 0.95.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A., Jalili, M. and Fasaei, M. S. K. (2020).
#' Finding influential nodes in social networks based on neighborhood
#' correlation coefficient. Knowledge-Based Systems, 194, 105580.
#' \doi{10.1016/j.knosys.2020.105580}.
#' @seealso \code{\link{centrality_shell_clustering}},
#' \code{\link{centrality_erm}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ecrm(regulation_net)
centrality_ecrm <- function(x, ...) {
  df <- centrality(x, measures = "ecrm", ...)
  stats::setNames(df$ecrm, df$node)
}

#' Weight Neighborhood Centrality
#'
#' Weight neighborhood centrality (Wang et al. 2017) adds to a benchmark
#' centrality of a node those of its neighbors, each weighted by the
#' importance of the edge relative to the average edge:
#' \deqn{C_i = \phi_i + \sum_{j \in N(i)} \frac{w_{ij}}{\langle w \rangle} \phi_j, \qquad w_{ij} = (k_i k_j)^{\alpha},}{C_i = phi_i + sum_{j in N(i)} (w_ij / <w>) phi_j,  w_ij = (k_i k_j)^alpha,}
#' with \eqn{k} the degree, \eqn{\langle w \rangle}{<w>} the mean of
#' \eqn{w} over the edges and \eqn{\phi}{phi} the degree or the coreness.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source defines the measure for any benchmark centrality and reports
#' the degree and the coreness side by side; \code{wnc_benchmark} chooses,
#' \code{"degree"} by default (owner decision 2026-11-26). \code{wnc_alpha
#' = 1} is the authors' value. \eqn{\langle w \rangle}{<w>} is the mean
#' over the edges, the reading the source's Table 1 fixes, and is taken
#' over the whole network, so adding a component changes every score. On a
#' network without edges the score is the benchmark, 0. The implementation
#' reproduces the 20 values \eqn{C(k)} and \eqn{C(k_s)}{C(ks)} of the
#' source's Table 1 for ten nodes of its Fig. 2. The top of that figure is
#' cut off in the copy read: five of the 27 edges were inferred from the
#' other columns of Table 1 (the source audit gives the reasoning), and the
#' nodes they add are not among the ten. It also reproduces the
#' monotonicity of both variants on the dolphins network (0.9905, Table 3),
#' which is below the largest value possible there. On four test networks,
#' with the degree as benchmark the score ranks nodes closest to
#' \code{diffusion} and the two-step expected influence (mean Kendall
#' correlation 0.95) and \code{\link{centrality_laplacian}} (0.94). With
#' the coreness it ranks them closest to \code{\link{centrality_laplacian}}
#' (0.97), \code{\link{centrality_ked}} and \code{wksdn} (0.96), with
#' correlations of 0.99 or more on single networks: 0.997 with
#' \code{laplacian}, 0.996 with \code{wksdn} and 0.995 with \code{lnc} on
#' one, and 1 with \code{ksgc} on a network with a single k-shell, where
#' the coreness is the same at every node. A \code{wnc_benchmark} other
#' than \code{"degree"} and \code{"coreness"}, or a \code{wnc_alpha} that
#' is not a single finite number, raises a \code{cograph_bad_parameter}
#' error. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{wnc_benchmark} (\code{\"degree\"} or \code{\"coreness\"}) and
#' \code{wnc_alpha} (default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, J., Hou, X., Li, K. and Ding, Y. (2017). A novel weight
#' neighborhood centrality algorithm for identifying influential spreaders
#' in complex networks. Physica A: Statistical Mechanics and its
#' Applications, 475, 88-105. \doi{10.1016/j.physa.2017.02.007}.
#' @seealso \code{\link{centrality_degree}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality_wd}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_weight_neighborhood(regulation_net)
#' centrality_weight_neighborhood(regulation_net, wnc_benchmark = "coreness")
centrality_weight_neighborhood <- function(x, ...) {
  df <- centrality(x, measures = "weight_neighborhood", ...)
  stats::setNames(df$weight_neighborhood, df$node)
}
