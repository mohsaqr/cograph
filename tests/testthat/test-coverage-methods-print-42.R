# Test file: test-coverage-methods-print-42.R
# Additional tests for S3 print methods in methods-print.R
# Target: Cover uncovered branches and edge cases

# ==============================================================================
# Tests for print.cograph_network - R6 Wrapper Format (lines 68-87)
# This is the main uncovered code path
# ==============================================================================

test_that("print.cograph_network handles R6 wrapper format with network", {
  # Create an R6 CographNetwork object
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  # Wrap it in the old wrapper format
  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Cograph Network", output)))
  expect_true(any(grepl("Nodes:", output)))
  expect_true(any(grepl("Edges:", output)))
})

test_that("print.cograph_network shows R6 wrapper node count", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Nodes:.*3", output)))
})

test_that("print.cograph_network shows R6 wrapper edge count", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Edges:", output)))
})

test_that("print.cograph_network shows R6 wrapper directed status", {
  # Create a directed network
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj, directed = TRUE)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Directed:", output)))
})

test_that("print.cograph_network shows R6 wrapper weighted status", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Weighted:", output)))
})

test_that("print.cograph_network shows R6 wrapper layout not computed", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  # Do NOT set layout

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Layout:", output)))
  expect_true(any(grepl("not computed", output)))
})

test_that("print.cograph_network shows R6 wrapper layout computed", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  # Set layout coordinates
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  r6_net$set_layout_coords(coords)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Layout:", output)))
  expect_true(any(grepl("computed", output)))
})

test_that("print.cograph_network shows R6 wrapper theme none", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  # Do NOT set theme

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Theme:", output)))
  expect_true(any(grepl("none", output)))
})

test_that("print.cograph_network shows R6 wrapper with theme name", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  # Set theme with a name
  r6_net$set_theme(list(name = "dark"))

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Theme:.*dark", output)))
})

test_that("print.cograph_network shows R6 wrapper usage hints", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("plot\\(\\)|sn_render\\(\\)", output)))
  expect_true(any(grepl("sn_ggplot\\(\\)", output)))
})

test_that("print.cograph_network returns invisible x for R6 wrapper format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  result <- print(wrapper)
  expect_identical(result, wrapper)
})

test_that("print.cograph_network shows R6 header separator", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("=", output)))  # Header separator
})

# ==============================================================================
# Tests for print.cograph_network - Edge Cases for List Format
# ==============================================================================

test_that("print.cograph_network handles list format with NULL nodes_df", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = NULL,  # NULL nodes
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network handles list format with nodes missing x column", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C")),  # No x column
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network handles list format with NULL weight", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = NULL  # NULL weight
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  # Should not show Weights line
  expect_false(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles list format with directed NULL", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = NULL,  # NULL directed - should show "undirected"
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("undirected", output)))
})

test_that("print.cograph_network handles list format with edges but no weight", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = TRUE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C"))
    # No weight element at all
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_false(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles weight range with NA values", {
  net <- list(
    n_nodes = 3,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B", "A"), to = c("B", "C", "C")),
    weight = c(0.2, NA, 0.8)  # NA in weights
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

# ==============================================================================
# Tests for print.cograph_network - Edge Cases for Attr Format
# ==============================================================================

test_that("print.cograph_network handles attr format with NULL nodes_df", {
  net <- list(
    edges = data.frame(from = "A", to = "B"),
    weight = c(1)
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- NULL  # NULL nodes
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network handles attr format with nodes missing x column", {
  net <- list(
    edges = data.frame(from = "A", to = "B"),
    weight = c(1)
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"))  # No x column
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network handles attr format with zero edges", {
  net <- list(
    edges = data.frame(from = character(0), to = character(0)),
    weight = numeric(0)
  )
  attr(net, "n_nodes") <- 3
  attr(net, "n_edges") <- 0
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("0.*edges", output)))
  expect_false(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles attr format with NULL weight", {
  net <- list(
    edges = data.frame(from = "A", to = "B")
    # weight is missing
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_false(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles attr format with directed NULL", {
  net <- list(
    edges = data.frame(from = "A", to = "B"),
    weight = 1
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- NULL  # NULL directed
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("undirected", output)))
})

# ==============================================================================
# Tests for Large Network Display
# ==============================================================================

test_that("print.cograph_network handles large network in list format", {
  n <- 100
  net <- list(
    n_nodes = n,
    n_edges = n * 2,
    directed = TRUE,
    nodes = data.frame(
      name = paste0("node_", 1:n),
      x = runif(n),
      y = runif(n)
    ),
    edges = data.frame(
      from = paste0("node_", sample(n, n * 2, replace = TRUE)),
      to = paste0("node_", sample(n, n * 2, replace = TRUE))
    ),
    weight = runif(n * 2)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("100.*nodes", output)))
  expect_true(any(grepl("200.*edges", output)))
})

test_that("print.cograph_network handles single node network", {
  net <- list(
    n_nodes = 1,
    n_edges = 0,
    directed = FALSE,
    nodes = data.frame(name = "A", x = 0.5, y = 0.5),
    edges = data.frame(from = character(0), to = character(0)),
    weight = numeric(0)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("1.*nodes", output)))
  expect_true(any(grepl("0.*edges", output)))
})

# ==============================================================================
# Tests for Special Weight Values
# ==============================================================================

test_that("print.cograph_network handles negative weights", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(-0.5, 0.3)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("-0\\.5", output)))
})

test_that("print.cograph_network handles very small weights", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(0.0001, 0.0002)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles very large weights", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1000, 5000)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("1000", output)))
})

test_that("print.cograph_network handles Inf weights", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(Inf, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

# ==============================================================================
# Tests for print.cograph_network - Fallback Edge Cases
# ==============================================================================

test_that("print.cograph_network fallback with NULL network element", {
  net <- list(network = NULL, other = "data")
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network object", output)))
})

test_that("print.cograph_network fallback with non-CographNetwork network element", {
  net <- list(network = list(not_r6 = TRUE))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network object", output)))
})

test_that("print.cograph_network fallback with empty list", {
  net <- list()
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network object", output)))
})

# ==============================================================================
# Tests for R6 Wrapper with Various Network Configurations
# ==============================================================================

test_that("print.cograph_network R6 wrapper with directed network", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj, directed = TRUE)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Directed:.*TRUE", output)))
})

test_that("print.cograph_network R6 wrapper with undirected network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj, directed = FALSE)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Directed:.*FALSE", output)))
})

test_that("print.cograph_network R6 wrapper with unweighted network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Weighted:", output)))
})

test_that("print.cograph_network R6 wrapper with weighted network", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Weighted:.*TRUE", output)))
})

test_that("print.cograph_network R6 wrapper with empty network", {
  adj <- matrix(0, nrow = 3, ncol = 3)
  r6_net <- CographNetwork$new(adj)

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))
  expect_true(any(grepl("Nodes:", output)))
  expect_true(any(grepl("Edges:", output)))
})

# ==============================================================================
# Tests for print.cograph_network - Mixed Scenarios
# ==============================================================================

test_that("print.cograph_network prefers list n_nodes over attr n_nodes", {
  # Create an object that has BOTH list element and attr
  net <- list(
    n_nodes = 5,  # List element takes precedence
    n_edges = 4,
    directed = TRUE,
    nodes = data.frame(name = paste0("N", 1:5), x = 1:5, y = 1:5),
    edges = data.frame(from = 1:4, to = 2:5),
    weight = rep(1, 4)
  )
  attr(net, "n_nodes") <- 10  # Should be ignored
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("5.*nodes", output)))
  expect_false(any(grepl("10.*nodes", output)))
})

test_that("print.cograph_network handles partial x/y coordinates", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(
      name = c("A", "B", "C"),
      x = c(0, NA, 0.5),  # Partial NAs
      y = c(0, 0, 1)
    ),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  # Should show "set" because not ALL are NA
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network handles all-NA coordinates as no layout", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(
      name = c("A", "B", "C"),
      x = c(NA, NA, NA),
      y = c(NA, NA, NA)
    ),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

# ==============================================================================
# Tests for print.cograph_network - Output Structure
# ==============================================================================

test_that("print.cograph_network output order is consistent for list format", {
  net <- list(
    n_nodes = 3,
    n_edges = 3,
    directed = TRUE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B", "A"), to = c("B", "C", "C")),
    weight = c(0.2, 0.5, 0.8)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))

  # First line should contain "Cograph network:"
  expect_true(grepl("Cograph network:", output[1]))
  # Second line should contain "Weights:"
  expect_true(grepl("Weights:", output[2]))
  # Third line should contain "Layout:"
  expect_true(grepl("Layout:", output[3]))
})

test_that("print.cograph_network output order for R6 format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  r6_net$set_theme(list(name = "default"))
  r6_net$set_layout_coords(data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))

  wrapper <- list(network = r6_net)
  class(wrapper) <- c("cograph_network", "list")

  output <- capture.output(print(wrapper))

  # Check structure - header, separator, details
  header_line <- grep("Cograph Network", output)
  separator_line <- grep("=", output)
  nodes_line <- grep("Nodes:", output)
  edges_line <- grep("Edges:", output)

  expect_true(length(header_line) > 0)
  expect_true(length(separator_line) > 0)
  expect_true(length(nodes_line) > 0)
  expect_true(length(edges_line) > 0)
})

# ==============================================================================
# Tests for Edge Cases with Real cograph() Output
# ==============================================================================

test_that("print works with cograph() from matrix input", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() from symmetric matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() from asymmetric matrix", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() from edgelist", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(1, 2, 3)
  )
  net <- cograph(edges)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
})

# ==============================================================================
# Tests for Numeric Precision in Weight Display
# ==============================================================================

test_that("print.cograph_network rounds weights to 3 decimal places", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(0.123456789, 0.987654321)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  combined <- paste(output, collapse = " ")

  # Should show 3 decimal places
  expect_true(grepl("0\\.123", combined))
  expect_true(grepl("0\\.988", combined))  # Rounded
})

# ==============================================================================
# Tests Ensuring All Print Paths Return Properly
# ==============================================================================

test_that("print.cograph_network returns invisibly for all code paths", {
  # List format
  net1 <- list(
    n_nodes = 3, n_edges = 2, directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net1) <- c("cograph_network", "list")
  result1 <- print(net1)
  expect_identical(result1, net1)

  # Attr format
  net2 <- list(edges = data.frame(from = "A", to = "B"), weight = 1)
  attr(net2, "n_nodes") <- 2
  attr(net2, "n_edges") <- 1
  attr(net2, "directed") <- FALSE
  attr(net2, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net2) <- c("cograph_network", "list")
  result2 <- print(net2)
  expect_identical(result2, net2)

  # R6 wrapper format
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6_net <- CographNetwork$new(adj)
  net3 <- list(network = r6_net)
  class(net3) <- c("cograph_network", "list")
  result3 <- print(net3)
  expect_identical(result3, net3)

  # Fallback format
  net4 <- list(data = "something")
  class(net4) <- c("cograph_network", "list")
  result4 <- print(net4)
  expect_identical(result4, net4)
})
