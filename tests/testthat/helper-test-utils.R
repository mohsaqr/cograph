# Test helper utilities for CRAN testing suite
# Provides common test setup functions and custom expectations

# ============================================
# Test Data Generators
# ============================================

#' Create a test adjacency matrix
#' @param n Number of nodes
#' @param density Edge density (0-1)
#' @param weighted Logical: include weights?
#' @param symmetric Logical: make symmetric (undirected)?
#' @param seed Random seed for reproducibility
#' @return n x n matrix
create_test_matrix <- function(n = 5, density = 0.5, weighted = FALSE,
                               symmetric = TRUE, seed = 42) {

  set.seed(seed)
  mat <- matrix(0, n, n)
  n_possible <- n * (n - 1)
  if (symmetric) n_possible <- n_possible / 2
  n_edges <- round(n_possible * density)

  if (symmetric) {
    # Fill upper triangle
    upper_idx <- which(upper.tri(mat))
    selected <- sample(upper_idx, min(n_edges, length(upper_idx)))
    if (weighted) {
      mat[selected] <- runif(length(selected), 0.1, 1)
    } else {
      mat[selected] <- 1
    }
    mat <- mat + t(mat)
  } else {
    # Fill anywhere except diagonal
    all_idx <- which(row(mat) != col(mat))
    selected <- sample(all_idx, min(n_edges, length(all_idx)))
    if (weighted) {
      mat[selected] <- runif(length(selected), -1, 1)
    } else {
      mat[selected] <- 1
    }
  }

  mat
}

#' Create a test edge list data frame
#' @param n_edges Number of edges
#' @param n_nodes Number of unique nodes
#' @param weighted Include weight column?
#' @param char_nodes Use character node names?
#' @param seed Random seed
#' @return data.frame with from, to, (weight) columns
create_test_edgelist <- function(n_edges = 10, n_nodes = 5, weighted = FALSE,
                                  char_nodes = FALSE, seed = 42) {
  set.seed(seed)

  from <- sample(1:n_nodes, n_edges, replace = TRUE)
  to <- sample(1:n_nodes, n_edges, replace = TRUE)

  # Remove self-loops
  self_loops <- from == to
  if (any(self_loops)) {
    to[self_loops] <- ((to[self_loops]) %% n_nodes) + 1
  }

  if (char_nodes) {
    node_names <- LETTERS[1:n_nodes]
    from <- node_names[from]
    to <- node_names[to]
  }

  df <- data.frame(from = from, to = to, stringsAsFactors = FALSE)

  if (weighted) {
    df$weight <- runif(n_edges, -1, 1)
  }

  df
}

#' Create test network with specific topology
#' @param type One of "complete", "star", "ring", "path", "disconnected"
#' @param n Number of nodes
#' @return Adjacency matrix
create_test_topology <- function(type = "complete", n = 5) {
  mat <- matrix(0, n, n)

  switch(type,
    complete = {
      mat <- matrix(1, n, n)
      diag(mat) <- 0
    },
    star = {
      # Node 1 connects to all others
      mat[1, 2:n] <- 1
      mat[2:n, 1] <- 1
    },
    ring = {
      for (i in 1:(n-1)) {
        mat[i, i+1] <- 1
        mat[i+1, i] <- 1
      }
      mat[n, 1] <- 1
      mat[1, n] <- 1
    },
    path = {
      for (i in 1:(n-1)) {
        mat[i, i+1] <- 1
        mat[i+1, i] <- 1
      }
    },
    disconnected = {
      # Two disconnected components
      half <- floor(n/2)
      if (half > 1) {
        mat[1:half, 1:half] <- 1
        diag(mat[1:half, 1:half]) <- 0
      }
      if (n - half > 1) {
        mat[(half+1):n, (half+1):n] <- 1
        diag(mat[(half+1):n, (half+1):n]) <- 0
      }
    }
  )

  mat
}

# ============================================
# Skip Helpers
# ============================================

#' Skip test if igraph is not available
skip_if_no_igraph <- function() {
  skip_if_not_installed("igraph")
}

#' Skip test if running without display (for graphical tests)
skip_if_no_display <- function() {
  # On CI/CRAN, skip tests that require interactive graphics
  skip_on_cran()
  if (identical(Sys.getenv("CI"), "true")) {
    skip("Skipping graphical test on CI")
  }
}

#' Skip tests that require qgraph
skip_if_no_qgraph <- function() {
  skip_if_not_installed("qgraph")
}

#' Skip tests that require tna
skip_if_no_tna <- function() {
  skip_if_not_installed("tna")
}

# ============================================
# Custom Expectations
# ============================================

#' Expect valid color values
#' @param x Character vector to check
expect_valid_colors <- function(x) {
  # Check that colors can be parsed by R
  for (col in x) {
    result <- tryCatch(
      grDevices::col2rgb(col),
      error = function(e) NULL
    )
    expect_false(is.null(result),
                 info = paste("Invalid color:", col))
  }
  invisible(TRUE)
}

#' Expect file was created
#' @param path File path to check
expect_file_created <- function(path) {
  expect_true(file.exists(path),
              info = paste("File not created:", path))
}

#' Expect file has minimum size
#' @param path File path
#' @param min_bytes Minimum file size in bytes
expect_file_size <- function(path, min_bytes = 100) {
  expect_true(file.exists(path), info = paste("File missing:", path))
  expect_true(file.size(path) >= min_bytes,
              info = paste("File too small:", path, "size:", file.size(path)))
}

#' Expect numeric vector within range
#' @param x Numeric vector
#' @param min Minimum value
#' @param max Maximum value
expect_in_range <- function(x, min, max) {
  expect_true(all(x >= min & x <= max, na.rm = TRUE),
              info = paste("Values outside range [", min, ",", max, "]: ",
                          paste(range(x, na.rm = TRUE), collapse = "-")))
}

#' Expect sonnet_network object
#' @param x Object to check
expect_sonnet_network <- function(x) {
  expect_s3_class(x, "sonnet_network")
  expect_true(!is.null(x$network))
  expect_true(x$network$n_nodes >= 0)
}

# ============================================
# Temp File Helpers
# ============================================

#' Run code with temporary PNG device
#' @param expr Expression to evaluate
#' @param ... Arguments passed to png()
#' @return Result of expression (invisibly)
with_temp_png <- function(expr, width = 200, height = 200, ...) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp, width = width, height = height, ...)
  result <- tryCatch(
    force(expr),
    finally = grDevices::dev.off()
  )

  invisible(result)
}

#' Run code with temporary PDF device
#' @param expr Expression to evaluate
#' @param ... Arguments passed to pdf()
#' @return Result of expression (invisibly)
with_temp_pdf <- function(expr, width = 7, height = 7, ...) {
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::pdf(tmp, width = width, height = height, ...)
  result <- tryCatch(
    force(expr),
    finally = grDevices::dev.off()
  )

  invisible(result)
}

#' Create temporary directory for test outputs
#' @return Path to temporary directory
create_temp_dir <- function() {
  dir <- tempfile(pattern = "sonnet_test_")
  dir.create(dir)
  dir
}

# ============================================
# Graphical Test Helpers
# ============================================

#' Safely run a plotting function (captures errors)
#' @param expr Expression that produces a plot
#' @return List with success flag and any error message
safe_plot <- function(expr) {
  result <- list(success = FALSE, error = NULL)

  tryCatch({
    with_temp_png({
      force(expr)
    })
    result$success <- TRUE
  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  result
}

#' Test that splot() runs without error
#' @param ... Arguments to pass to splot()
expect_splot_works <- function(...) {
  result <- safe_plot(splot(...))
  expect_true(result$success, info = paste("splot() failed:", result$error))
}

#' Test that soplot() runs without error
#' @param ... Arguments to pass to soplot()
expect_soplot_works <- function(...) {
  skip_if_not_installed("grid")
  result <- safe_plot(soplot(...))
  expect_true(result$success, info = paste("soplot() failed:", result$error))
}

# ============================================
# Network Validation Helpers
# ============================================

#' Validate network structure
#' @param net sonnet_network object
#' @param expected_nodes Expected node count
#' @param expected_edges Expected edge count (approximate)
validate_network <- function(net, expected_nodes = NULL, expected_edges = NULL) {
  expect_sonnet_network(net)

  if (!is.null(expected_nodes)) {
    expect_equal(net$network$n_nodes, expected_nodes)
  }

  if (!is.null(expected_edges)) {
    expect_equal(net$network$n_edges, expected_edges)
  }

  # Validate layout exists
  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), net$network$n_nodes)
  expect_true(all(c("x", "y") %in% names(layout)))
}
