#' Coleman-Theil hierarchy using Burt's mutual investment weights
#' @keywords internal
#' @noRd
calculate_coleman_theil <- function(cg, weights = NULL) {
  a <- .cg_candidate_adjacency(cg, weights, "coleman_theil")
  diag(a) <- 0
  n <- nrow(a)
  if (!n || !any(a > 0)) return(numeric(n))
  positive <- a > 0
  a <- a / max(a)
  if (any(a[positive] == 0)) {
    stop("coleman_theil weight range exceeds double precision", call. = FALSE)
  }
  mutual <- a + t(a)
  strength <- rowSums(mutual)
  p <- mutual / ifelse(strength > 0, strength, 1)
  if (any(p[mutual > 0] == 0)) {
    stop("coleman_theil investment range exceeds double precision",
         call. = FALSE)
  }
  indirect <- p %*% p
  vapply(seq_len(n), function(i) {
    neighbors <- which(mutual[i, ] > 0)
    size <- length(neighbors)
    if (size == 0L) return(0)
    if (size == 1L) return(1)
    local <- (p[i, neighbors] + indirect[i, neighbors])^2
    deviation <- (local - mean(local)) / mean(local)
    # Treat indistinguishable local constraints as uniform before max scaling.
    if (max(abs(deviation)) <= 16 * .Machine$double.eps) return(0)
    # r log(r) - (r-1), whose linear terms sum to zero. The series avoids
    # cancellation around r=1; its omitted term is below double precision.
    term <- numeric(size)
    small <- abs(deviation) < 1e-3
    u <- deviation[small]
    polynomial <- -1 / 20 + u * (1 / 30 - u / 42)
    polynomial <- -1 / 6 + u * (1 / 12 + u * polynomial)
    term[small] <- u^2 * (1 / 2 + u * polynomial)
    ordinary <- !small & deviation > -1
    u <- deviation[ordinary]
    term[ordinary] <- (1 + u) * log1p(u) - u
    term[deviation <= -1] <- 1
    max(0, min(1, sum(term) / (size * log(size))))
  }, numeric(1))
}

#' Coleman-Theil hierarchy index
#'
#' Measures concentration of Burt's dyadic constraints over a node's contacts.
#' Let mutual tie strength be z_ij+z_ji, and p_ij its proportion of all mutual
#' strength incident to i. With organizational weights fixed at one, define
#' \deqn{c_{ij}=(p_{ij}+\sum_q p_{iq}p_{qj})^2,\quad
#' r_{ij}=c_{ij}/\operatorname{mean}_{k\in N(i)}c_{ik}.}
#' The index is \eqn{\sum_{j\in N(i)}r_{ij}\log(r_{ij})/(d_i\log(d_i))}.
#' Contacts are distinct nodes with positive mutual strength. Investment
#' proportions use the full supplied graph, including alters' outside ties.
#'
#' Follows Burt's STRUCTURE 4.2 manual (pages 181-183): isolates score zero
#' and nodes with one contact score one. The general formula is undefined
#' in these two cases; these are the author's explicit conventions. JUNG's
#' documented implementation instead returns NaN for isolates. Values range
#' from zero for equal constraints to one for complete concentration. Input
#' organizational/oligopoly multipliers from STRUCTURE are not implemented;
#' they are fixed at one, as in the Zoo's formula.
#'
#' Finite nonnegative weights are supported. Zero-weight ties are absent,
#' loops are removed, and remaining parallel edges sum after generic
#' simplification. Directed ties are combined by summing both directions;
#' \code{weighted=FALSE} assigns unit weight to each retained edge before
#' combining them, so reciprocity can affect mutual investment. Generic mode,
#' shortest-path inversion and cutoff do not affect the result. Empty input
#' returns no scores. Components are independent before global normalization.
#'
#' The default output is already the unit-interval hierarchy index.
#' \code{normalized=TRUE} additionally divides by the largest node score;
#' an all-zero vector remains zero. Dense native arithmetic costs O(n cubed)
#' time and O(n squared) memory. Global weight scaling precedes mutual sums.
#' Unrepresentable positive weight or investment ranges raise an error;
#' tiny squared constraints may underflow and use the zero-log-zero limit.
#' Relative deviations of local constraints within 16 machine epsilons are
#' treated as uniform; a series stabilizes the entropy near uniformity.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Burt, R. S. (1991). STRUCTURE, version 4.2, Reference Manual,
#'   Columbia University, pages 181-183. These pages reproduce the hierarchy
#'   definition attributed to equation 2.9 in Burt (1992), Structural Holes:
#'   The Social Structure of Competition, Harvard University Press.
#' @export
#' @examples
#' centrality_coleman_theil(igraph::make_star(5, mode = "undirected"))
centrality_coleman_theil <- function(x, ...) {
  df <- centrality(x, measures = "coleman_theil", ...)
  stats::setNames(df$coleman_theil, df$node)
}
