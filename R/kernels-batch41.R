#' Constituent indexes of the relative-entropy integrated evaluation
#'
#' The vocabulary Chen, Wang and Luo (2016) actually pin down: the four
#' "distinctiveness" indexes their Kite study integrates (section 4.1) and
#' the two "destructiveness" indexes their second integration adds
#' (section 4.2). Every one of them has both a printed formula or a printed
#' definition and a printed evaluating direction, so nothing here is
#' inferred. Equation (2) clustering and equation (5) eigenvector are
#' defined in section 3.2 but never used and never declared positive or
#' negative, and equation (7) average path length is infinite whenever the
#' residual graph is disconnected, so none of the three is offered.
#'
#' @return Character vector of index names.
#' @keywords internal
#' @noRd
.cg_re_vocabulary <- function() {
  c("degree", "closeness", "betweenness", "constraint",
    "n_components", "largest_component")
}

#' Indexes the source declares negative
#'
#' Section 3.3 declares degree, closeness and betweenness positive; section
#' 4.1 declares the network constraint coefficient negative; section 4.2
#' declares the number of connected components positive and the largest
#' connected component size negative.
#'
#' @return Character vector of index names.
#' @keywords internal
#' @noRd
.cg_re_negative_default <- function() {
  c("constraint", "largest_component")
}

#' Shared context for the relative-entropy indexes
#'
#' The indexes are read off one simple undirected unweighted skeleton, built
#' once so that a six-index request does not rebuild it six times.
#'
#' @param a Symmetric binary adjacency matrix with a zero diagonal.
#' @return List with the matrix `a`, its igraph view `graph` and the node
#'   count `n`.
#' @keywords internal
#' @noRd
.cg_re_context <- function(a) {
  list(a = a, n = nrow(a),
       graph = igraph::graph_from_adjacency_matrix(a, mode = "undirected",
                                                   diag = FALSE))
}

#' Network constraint coefficient of Chen, Wang and Luo (2016), equation (6)
#'
#' `C_CO(i) = sum_{j != i} (P_ij + sum_{q != i, j} P_iq P_qj)^2`, where
#' `P_ij` is `i`'s proportional investment in `j`, here `a_ij / k_i` on the
#' unweighted skeleton. Two readings differ from Burt's usual constraint and
#' from `igraph::constraint()`:
#'
#' * The outer sum runs over every other node, not only over `i`'s
#'   neighbours, so a node two steps away contributes through `P^2` alone.
#'   On the paper's Kite this is what gives node 1 the printed 1.25 where
#'   the neighbour-restricted sum gives 1.
#' * The printed outer limit is `j = 1 ... |V|`, which would add the term
#'   `(sum_{q != i} P_iq P_qi)^2` at `j = i` and raise node 1 to 1.5.
#'   Excluding `j = i` is the only reading that reproduces Table 1, and it
#'   is the reading Burt's constraint uses; cograph follows it.
#'
#' Because `P_ii` and `P_jj` are both zero on a loopless graph, dropping
#' `q = i` and `q = j` from the inner sum removes nothing, so the inner term
#' is exactly the matrix square of `P`.
#'
#' An isolate invests nowhere. `P` would be `0/0`; cograph reads the row as
#' all zeros, giving the isolate constraint zero. That is a cograph
#' convention, not the paper's.
#'
#' @param a Symmetric binary adjacency matrix with a zero diagonal.
#' @return Numeric vector, one constraint value per node.
#' @keywords internal
#' @noRd
.cg_re_constraint <- function(a) {
  k <- rowSums(a)
  denominator <- k
  denominator[!(denominator > 0)] <- 1
  p <- a / denominator
  contribution <- (p + p %*% p)^2
  diag(contribution) <- 0
  rowSums(contribution)
}

#' Residual connectivity after deleting each node
#'
#' The two destructiveness indexes of section 4.2: the number of connected
#' components of `G - i` and the size of its largest component. Deleting the
#' only node of a singleton graph leaves nothing, which cograph reports as
#' zero components of size zero.
#'
#' @param ctx Context from `.cg_re_context()`.
#' @param largest `TRUE` for the largest component size, `FALSE` for the
#'   component count.
#' @return Numeric vector, one value per node.
#' @keywords internal
#' @noRd
.cg_re_destructiveness <- function(ctx, largest) {
  vapply(seq_len(ctx$n), function(i) {
    sizes <- igraph::components(
      igraph::delete_vertices(ctx$graph, i)
    )$csize
    if (!length(sizes)) return(0)
    if (largest) max(sizes) else length(sizes)
  }, numeric(1))
}

#' One constituent index of the relative-entropy evaluation
#'
#' Closeness is equation (3), `1 / sum_j l_ij`, with no `|V| - 1` factor.
#' The paper's sum runs over all of `V`, which is infinite as soon as the
#' graph is disconnected and leaves the index identically zero. cograph sums
#' over the reachable partners instead, which agrees with equation (3)
#' exactly on a connected graph and is the convention the rest of the
#' package uses; on a disconnected graph it is a cograph extension outside
#' the source's stated domain. An isolate reaches nobody, so its reciprocal
#' is undefined; cograph gives it closeness zero, also an extension.
#'
#' Betweenness is equation (4), summed over ordered pairs `j != i != k`, so
#' it is twice the usual unnormalised undirected betweenness. Equation (8)
#' divides the index by its own total, so the factor of two cancels; it is
#' kept because the paper's Table 1 prints the doubled values.
#'
#' @param ctx Context from `.cg_re_context()`.
#' @param name One of `.cg_re_vocabulary()`.
#' @return Numeric vector, one value per node.
#' @keywords internal
#' @noRd
.cg_re_index <- function(ctx, name) {
  switch(name,
    "degree" = rowSums(ctx$a),
    "closeness" = {
      d <- .cg_distances(ctx$a, "all")
      d[!is.finite(d)] <- 0
      total <- rowSums(d)
      out <- numeric(ctx$n)
      reachable <- total > 0
      out[reachable] <- 1 / total[reachable]
      out
    },
    "betweenness" = 2 * igraph::betweenness(ctx$graph, directed = FALSE),
    "constraint" = .cg_re_constraint(ctx$a),
    "n_components" = .cg_re_destructiveness(ctx, largest = FALSE),
    "largest_component" = .cg_re_destructiveness(ctx, largest = TRUE)
  )
}

#' Map one index to a discrete distribution, equations (8) and (9)
#'
#' A positive index becomes `C(i) / sum_j C(j)`; a negative index becomes
#' `(1 - C(i)/sum_j C(j))` renormalised over the nodes. The result is
#' returned in logarithms because equation (11) multiplies the `m`
#' distributions together, and a product of small shares underflows long
#' before its geometric mean does. An exact zero maps to `-Inf`, whose
#' exponential is the exact zero the paper's Table 2 prints for nodes 1, 6
#' and 8; it is deliberately not smoothed.
#'
#' Both maps divide by a total the paper never discusses. An index that is
#' zero on every node leaves equation (8) at `0/0`, and equation (9)'s
#' denominator is `|V| - 1`, which vanishes on a singleton graph. Neither
#' case has a value, so both raise `cograph_undefined_index` rather than
#' returning zeros.
#'
#' @param value Numeric index vector.
#' @param name Index name, used in the condition message.
#' @param negative `TRUE` to apply equation (9), `FALSE` for equation (8).
#' @return Numeric vector of logarithms, one per node.
#' @keywords internal
#' @noRd
.cg_re_distribution <- function(value, name, negative) {
  total <- sum(value)
  if (!isTRUE(total > 0)) {
    text <- paste(
      "The `%s` index is zero at every node, so equation 8 of Chen, Wang",
      "and Luo (2016) divides by zero and `relative_entropy` has no value",
      "on this graph. Choose a different `re_indexes` set."
    )
    stop(errorCondition(sprintf(text, name),
                        class = "cograph_undefined_index", call = NULL))
  }
  share <- value / total
  if (!negative) return(log(share))
  complement <- 1 - share
  denominator <- sum(complement)
  if (!isTRUE(denominator > 0)) {
    text <- paste(
      "The negative `%s` index leaves equation 9 of Chen, Wang and Luo",
      "(2016) with a zero denominator, which happens only when the graph",
      "has a single node. `relative_entropy` has no value there."
    )
    stop(errorCondition(sprintf(text, name),
                        class = "cograph_undefined_index", call = NULL))
  }
  log(complement / denominator)
}
