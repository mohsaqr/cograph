#' @title Group-based Layout
#' @description Arrange nodes in groups, with each group in a circular arrangement.
#' @name layout-groups
NULL

#' Group-based Layout
#'
#' Arrange nodes based on group membership. Groups are positioned in a
#' circular arrangement around the center, with nodes within each group
#' also arranged in a circle.
#'
#' @param network A CographNetwork object.
#' @param groups Vector specifying group membership for each node.
#'   Can be numeric, character, or factor.
#' @param group_positions Optional list or data frame with x, y coordinates
#'   for each group center.
#' @param inner_radius Radius of nodes within each group (default: 0.15).
#' @param outer_radius Radius for positioning group centers (default: 0.35).
#' @return Data frame with x, y coordinates.
#' @export
#'
#' @examples
#' # Create a network with groups
#' adj <- matrix(0, 9, 9)
#' adj[1, 2:3] <- 1; adj[2:3, 1] <- 1  # Group 1
#' adj[4, 5:6] <- 1; adj[5:6, 4] <- 1  # Group 2
#' adj[7, 8:9] <- 1; adj[8:9, 7] <- 1  # Group 3
#' net <- CographNetwork$new(adj)
#' groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' coords <- layout_groups(net, groups)
layout_groups <- function(network, groups, group_positions = NULL,
                          inner_radius = 0.15, outer_radius = 0.35) {

  n <- network$n_nodes

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
