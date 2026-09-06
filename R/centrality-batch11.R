# ===========================================================================
# Batch 11 — parameterised members of families cograph already had
#
# igraph-facing calculators over R/kernels-batch11.R and the exported verbs.
# ===========================================================================

#' @keywords internal
#' @noRd
calculate_length_scaled_betweenness <- function(g, weights = NULL) {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  directed <- igraph::is_directed(g)
  w <- .cg_mode_weights(.cg_path_matrix(g, weights),
                        if (directed) "out" else "all")
  .cg_length_scaled_betweenness(w, n, directed)
}

#' @keywords internal
#' @noRd
calculate_delta_betweenness <- function(g, weights = NULL, delta = 1) {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  directed <- igraph::is_directed(g)
  w <- .cg_mode_weights(.cg_path_matrix(g, weights),
                        if (directed) "out" else "all")
  .cg_delta_betweenness(w, n, directed, delta = delta)
}

#' @keywords internal
#' @noRd
calculate_ego_betweenness <- function(g) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_ego_betweenness(.cg_edge_indicator(.cg_path_matrix(g, NULL)),
                      igraph::is_directed(g))
}

#' @keywords internal
#' @noRd
calculate_delta_closeness <- function(g, mode = "all", delta = 1,
                                      dist_mat = NULL, weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  d <- dist_mat %||% .cg_distances(.cg_path_matrix(g, weights), mode)
  .cg_delta_closeness(d, delta = delta)
}

#' Mass vectors for the gravity family
#' @keywords internal
#' @noRd
.cg_gravity_mass <- function(g, mass, mode) {
  deg <- igraph::degree(g, mode = mode)
  ks <- igraph::coreness(g, mode = mode)
  switch(mass,
         degree = list(i = deg, j = deg),
         kshell = list(i = ks, j = ks),
         # cograph's pre-2.4.8 form: no mass on the focal node, and the
         # product of degree and k-shell on its partners. No published
         # source; kept so earlier results stay reproducible.
         legacy = list(i = rep(1, igraph::vcount(g)), j = deg * ks))
}

#' @keywords internal
#' @noRd
calculate_gravity <- function(g, mode = "all", mass = "kshell",
                              radius = 3, exponent = 2) {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  d <- .cg_hop_distances(g, mode)
  if (identical(radius, "auto")) radius <- .cg_gravity_auto_radius(d)
  m <- .cg_gravity_mass(g, mass, mode)
  .cg_gravity(d, m$i, m$j, radius = radius, exponent = exponent)
}

# ---------------------------------------------------------------------------
# Exported verbs
# ---------------------------------------------------------------------------

#' Betweenness and closeness variants that carry a tuning parameter
#'
#' Four measures that reweight, rescope or re-tune a measure
#' \code{\link{centrality}} already computes. Each is a thin wrapper on
#' \code{centrality()}.
#'
#' \describe{
#'   \item{\code{length_scaled_betweenness} (Borgatti & Everett 2006;
#'     Brandes 2008, Algorithm 5)}{Betweenness with each separated pair
#'     weighted by \eqn{1 / d(s,t)}, so brokering between nearby nodes
#'     counts for more than brokering across the graph.}
#'   \item{\code{delta_betweenness} (Agneessens, Borgatti & Everett
#'     2017)}{Betweenness with the pair weight \eqn{(d(s,t) - 1)^{-\delta}}
#'     (\code{betweenness_delta}, default 1). At \eqn{\delta = 0} it is
#'     ordinary betweenness; raising it concentrates the score on locally
#'     brokered pairs.}
#'   \item{\code{ego_betweenness} (Everett & Borgatti 2005)}{Betweenness
#'     computed inside the node's own ego network rather than the whole
#'     graph. A node with fewer than two neighbours scores 0. It is close
#'     to, but not a function of, \code{effective_size}.}
#'   \item{\code{delta_closeness} (Agneessens, Borgatti & Everett 2017,
#'     eq. 2)}{\eqn{\sum_j d_{ij}^{-\delta} / (n-1)}
#'     (\code{closeness_delta}, default 1). One exponent spans the
#'     closeness family: \eqn{\delta = 1} is \code{harmonic} over
#'     \eqn{n-1}, \eqn{\delta = 2} is \code{harary} over \eqn{n-1}, a large
#'     \eqn{\delta} approaches degree, and \eqn{\delta = 0} counts the
#'     reachable set.}
#' }
#'
#' Bounded-distance betweenness, which the Centrality Zoo lists as
#' "k-betweenness", needs no separate measure: it is
#' \code{centrality(x, measures = "betweenness", cutoff = k)}.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param mode Direction: \code{"all"}, \code{"out"} or \code{"in"}.
#' @param betweenness_delta Decay exponent for
#'   \code{centrality_delta_betweenness}. Default 1.
#' @param closeness_delta Distance exponent for
#'   \code{centrality_delta_closeness}. Default 1.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @references
#' Agneessens, F., Borgatti, S. P., & Everett, M. G. (2017). Geodesic based
#'   centrality: Unifying the local and the global. Social Networks, 49,
#'   12-26.
#'
#' Brandes, U. (2008). On variants of shortest-path betweenness centrality
#'   and their generic computation. Social Networks, 30(2), 136-145.
#'
#' Everett, M., & Borgatti, S. P. (2005). Ego network betweenness. Social
#'   Networks, 27(1), 31-38.
#'
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality_gravity}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_length_scaled_betweenness(adj)
#' centrality_delta_betweenness(adj, betweenness_delta = 2)
#' centrality_ego_betweenness(adj)
#' centrality_delta_closeness(adj, closeness_delta = 2)
centrality_length_scaled_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "length_scaled_betweenness", ...)
  stats::setNames(df$length_scaled_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @export
centrality_delta_betweenness <- function(x, betweenness_delta = 1, ...) {
  df <- centrality(x, measures = "delta_betweenness",
                   betweenness_delta = betweenness_delta, ...)
  stats::setNames(df$delta_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @export
centrality_ego_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "ego_betweenness", ...)
  stats::setNames(df$ego_betweenness, df$node)
}

#' @rdname centrality_length_scaled_betweenness
#' @export
centrality_delta_closeness <- function(x, mode = "all", closeness_delta = 1,
                                       ...) {
  df <- centrality(x, measures = "delta_closeness", mode = mode,
                   closeness_delta = closeness_delta, ...)
  stats::setNames(df[[paste0("delta_closeness_", mode)]], df$node)
}

#' Gravity centrality
#'
#' \eqn{G(i) = \sum_j m_i m_j / d_{ij}^{2}}, optionally truncated at
#' \code{gravity_radius}. The published members of the family differ only in
#' the mass and the reach:
#'
#' \describe{
#'   \item{Gravity centrality (Ma, Ma, Zhang & Wang 2016)}{k-shell mass,
#'     radius 3 -- the default.}
#'   \item{Gravity model (Li, Ren, Ma, Liu, Zhang & Zhou 2019, eq. 1)}{
#'     \code{gravity_mass = "degree"}, \code{gravity_radius = NULL}.}
#'   \item{Local gravity model (same paper, eq. 2)}{
#'     \code{gravity_mass = "degree"}, \code{gravity_radius = "auto"},
#'     which is half the mean distance as their eq. 5 recommends.}
#' }
#'
#' @section Change in 2.4.8:
#' Before 2.4.8 this measure computed \eqn{\sum_j k_j s_j / d_{ij}^2}: the
#' product of degree and k-shell on the partner, no mass at all on the focal
#' node, and no truncation. That is not the formula of Li et al. (2019) that
#' its help page cited, and dropping the focal mass changes the ranking
#' rather than the scale. The default is now Ma et al. (2016).
#' \code{gravity_mass = "legacy"} with \code{gravity_radius = NULL}
#' reproduces the earlier values exactly.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param mode Direction: \code{"all"}, \code{"out"} or \code{"in"}.
#' @param gravity_mass \code{"kshell"} (default), \code{"degree"}, or
#'   \code{"legacy"}.
#' @param gravity_radius Largest distance to include: a number,
#'   \code{"auto"} for half the mean distance, or \code{NULL} for the whole
#'   graph. Default 3.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @references
#' Ma, L.-L., Ma, C., Zhang, H.-F., & Wang, B.-H. (2016). Identifying
#'   influential spreaders in complex networks based on gravity formula.
#'   Physica A, 451, 205-212.
#'
#' Li, Z., Ren, T., Ma, X., Liu, S., Zhang, Y., & Zhou, T. (2019).
#'   Identifying influential spreaders by gravity model. Scientific
#'   Reports, 9, 8387.
#'
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_kreach}}, \code{\link{centrality}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_gravity(adj)
#' centrality_gravity(adj, gravity_mass = "degree", gravity_radius = NULL)
centrality_gravity <- function(x, mode = "all", gravity_mass = "kshell",
                               gravity_radius = 3, ...) {
  df <- centrality(x, measures = "gravity", mode = mode,
                   gravity_mass = gravity_mass,
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df[[paste0("gravity_", mode)]], df$node)
}
