#' @title Group-based Layout
#' @description Arrange nodes in groups, with each group in a circular arrangement.
#' @name layout-groups
#' @keywords internal
#' @noRd
NULL

#' Group-based Layout
#'
#' Places the nodes by group membership. The group centers lie on a circle
#' around (0.5, 0.5), starting at the top and proceeding counterclockwise in
#' the order of the group levels. A single group is centered at (0.5, 0.5).
#' The nodes of each group lie on a circle around their group center, and a
#' group with one node is placed at its center.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @param groups Vector of group memberships, one per node (numeric,
#'   character, or factor). A length different from the number of nodes
#'   raises an error.
#' @param group_positions Optional list or data frame with columns \code{x}
#'   and \code{y} giving the group centers, one row per group level.
#' @param inner_radius Radius of the circle of nodes within each group.
#'   Default 0.15.
#' @param outer_radius Radius of the circle of group centers. Default 0.35.
#' @return A data frame with columns \code{x} and \code{y} and one row per
#'   node, in node order.
#'
#' @examples
#' layout_groups(CographNetwork$new(regulation_net),
#'   groups = rep(c("A", "B"), each = 5))
#'
#' @export
layout_groups <- function(network, groups, group_positions = NULL,
                          inner_radius = 0.15, outer_radius = 0.35) {

  n <- if (inherits(network, "cograph_network") && !inherits(network, "CographNetwork")) {
    n_nodes(network)
  } else {
    network$n_nodes
  }

  if (n == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  # Validate groups
  if (length(groups) != n) {
    stop("groups must have length equal to number of nodes", call. = FALSE)
  }

  # Convert to factor
  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  # Calculate group center positions
  if (is.null(group_positions)) {
    if (n_groups == 1) {
      # Single group: center
      group_centers <- data.frame(x = 0.5, y = 0.5)
    } else {
      # Multiple groups: arrange in circle
      angles <- seq(pi/2, pi/2 + 2 * pi * (1 - 1/n_groups),
                    length.out = n_groups)
      group_centers <- data.frame(
        x = 0.5 + outer_radius * cos(angles),
        y = 0.5 + outer_radius * sin(angles)
      )
    }
    rownames(group_centers) <- group_levels
  } else {
    if (is.data.frame(group_positions)) {
      group_centers <- group_positions
    } else {
      group_centers <- as.data.frame(group_positions)
    }
  }

  # Initialize coordinates
  coords <- data.frame(x = numeric(n), y = numeric(n))

  # Position nodes within each group
  for (g in group_levels) {
    # Get nodes in this group
    node_idx <- which(groups == g)
    n_in_group <- length(node_idx)

    if (n_in_group == 0) next

    # Group center
    g_idx <- match(g, group_levels)
    cx <- group_centers$x[g_idx]
    cy <- group_centers$y[g_idx]

    if (n_in_group == 1) {
      # Single node: at center
      coords$x[node_idx] <- cx
      coords$y[node_idx] <- cy
    } else {
      # Multiple nodes: arrange in circle
      angles <- seq(pi/2, pi/2 + 2 * pi * (1 - 1/n_in_group),
                    length.out = n_in_group)
      coords$x[node_idx] <- cx + inner_radius * cos(angles)
      coords$y[node_idx] <- cy + inner_radius * sin(angles)
    }
  }

  coords
}
