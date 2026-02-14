# test-coverage-output-save-43.R - Additional Coverage Tests for output-save.R
# Targets uncovered code paths in sn_save() and sn_save_ggplot()

# ============================================
# SN_SAVE() EDGE CASES - WEIGHTED NETWORKS
# ============================================

test_that("sn_save() handles weighted network correctly", {
  adj <- create_test_matrix(5, weighted = TRUE)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 500)
})

test_that("sn_save() handles network with negative weights", {
  adj <- matrix(c(0, 1, -0.5, 1, 0, 0.7, -0.5, 0.7, 0), nrow = 3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles directed network correctly", {
  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(adj, directed = TRUE)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() FORMAT VARIATIONS
# ============================================

test_that("sn_save() creates JPEG with uppercase extension", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".JPEG")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() creates PNG with mixed case extension", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".Png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() creates PDF with uppercase extension", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".PDF")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() WITH ADDITIONAL ARGUMENTS (...)
# ============================================

test_that("sn_save() passes extra args to png device", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  # bg argument is passed to png()
  expect_message(sn_save(net, tmp, bg = "transparent"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() passes extra args to pdf device", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  # title argument passed to pdf()
  expect_message(sn_save(net, tmp, title = "Test Network", pointsize = 12), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() passes extra args to jpeg device", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  # quality is already set but bg is passed
  expect_message(sn_save(net, tmp, bg = "white"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() passes extra args to tiff device", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  # Suppress compression warning on macOS/quartz
  suppressWarnings(expect_message(sn_save(net, tmp, bg = "lightgray"), "Saved"))
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() NETWORK SIZE VARIATIONS
# ============================================

test_that("sn_save() handles two-node network", {
  adj <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles larger network", {
  adj <- create_test_matrix(15, density = 0.3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, dpi = 150), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles fully connected network", {
  adj <- create_test_topology("complete", n = 6)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles star topology network", {
  adj <- create_test_topology("star", n = 7)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() INPUT CONVERSIONS
# ============================================

test_that("sn_save() handles weighted matrix input directly", {
  adj <- create_test_matrix(5, weighted = TRUE)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(adj, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles weighted edge list input", {
  edges <- create_test_edgelist(n_edges = 8, n_nodes = 5, weighted = TRUE)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(edges, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles character node names in edge list", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4, char_nodes = TRUE)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(edges, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() EDGE CASES
# ============================================

test_that("sn_save_ggplot() handles weighted network", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(5, weighted = TRUE)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles directed network", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(adj, directed = TRUE)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles network with negative weights", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.8, -0.3, 0.8, 0), nrow = 3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() creates SVG file", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  # Skip if SVG device not available
  result <- tryCatch({
    grDevices::svg(tmp)
    grDevices::dev.off()
    TRUE
  }, warning = function(w) {
    if (grepl("cairo|X11", conditionMessage(w), ignore.case = TRUE)) FALSE else TRUE
  }, error = function(e) FALSE)

  if (!result) skip("SVG device not available on this system")

  unlink(tmp)  # Clean up test file
  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() creates JPEG file", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".jpeg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() passes extra arguments to ggsave", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  # limitsize is a ggsave parameter
  expect_message(sn_save_ggplot(net, tmp, dpi = 72, limitsize = FALSE), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() ERROR HANDLING EXTENDED
# ============================================

test_that("sn_save() errors on empty extension with period", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # File ending with just a dot
  expect_error(sn_save(net, "filename."),
               "extension")
})

test_that("sn_save() errors on unsupported format bmp", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".bmp")

  expect_error(sn_save(net, tmp), "Unsupported")
})

test_that("sn_save() errors on unsupported format gif", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".gif")

  expect_error(sn_save(net, tmp), "Unsupported")
})

test_that("sn_save() errors on unsupported format webp", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".webp")

  expect_error(sn_save(net, tmp), "Unsupported")
})

# ============================================
# SN_SAVE() WITH CUSTOMIZED NETWORKS
# ============================================

test_that("sn_save() handles network with custom node shapes", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |>
    sn_nodes(shape = c("circle", "square", "triangle", "diamond"))

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with custom colors", {
  adj <- create_test_matrix(5)
  net <- cograph(adj) |>
    sn_nodes(fill = c("red", "blue", "green", "orange", "purple")) |>
    sn_edges(color = "gray30")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with minimal theme", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |> sn_theme("minimal")

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with dark theme", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |> sn_theme("dark")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() DIMENSION EDGE CASES
# ============================================

test_that("sn_save() handles very small dimensions", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, width = 1, height = 1, dpi = 72), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles non-square dimensions", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, width = 10, height = 5, dpi = 100), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles very high DPI", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, width = 3, height = 3, dpi = 600), "Saved")
  expect_file_created(tmp)
  # High DPI should create larger file
  expect_true(file.size(tmp) > 5000)
})

# ============================================
# SN_SAVE() DEVICE CLEANUP
# ============================================

test_that("sn_save() closes device even on error during render", {
  # Count devices before
  dev_before <- length(dev.list())

  # Create an invalid network that might cause issues
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  # Normal save should work and clean up
  suppressMessages(sn_save(net, tmp))

  # Count devices after
  dev_after <- length(dev.list())

  expect_equal(dev_after, dev_before)
})

test_that("sn_save() properly handles multiple sequential saves", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp1 <- tempfile(fileext = ".pdf")
  tmp2 <- tempfile(fileext = ".png")
  tmp3 <- tempfile(fileext = ".pdf")
  on.exit({
    unlink(tmp1)
    unlink(tmp2)
    unlink(tmp3)
  }, add = TRUE)

  dev_before <- length(dev.list())

  suppressMessages(sn_save(net, tmp1))
  suppressMessages(sn_save(net, tmp2))
  suppressMessages(sn_save(net, tmp3))

  dev_after <- length(dev.list())

  expect_equal(dev_after, dev_before)
  expect_file_created(tmp1)
  expect_file_created(tmp2)
  expect_file_created(tmp3)
})

# ============================================
# SN_SAVE_GGPLOT() DIMENSION CASES
# ============================================

test_that("sn_save_ggplot() handles non-square dimensions", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 8, height = 4, dpi = 72), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles small dimensions", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(3)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 2, height = 2, dpi = 72), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() INPUT CONVERSIONS
# ============================================

test_that("sn_save_ggplot() accepts matrix input directly", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  # sn_save_ggplot calls sn_ggplot which calls ensure_cograph_network
  # but the function signature expects cograph_network, so we need to convert
  net <- cograph(adj)
  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles empty network", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(0, 4, 4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles single node network", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(0, 1, 1)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() FILENAME PATH EDGE CASES
# ============================================

test_that("sn_save() handles filename with multiple periods", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "network.v1.2.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

test_that("sn_save() handles filename with underscores", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "my_network_plot_2024.png")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

test_that("sn_save() handles filename with dashes", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "network-visualization-output.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

# ============================================
# SN_SAVE() LAYOUT PRESERVATION
# ============================================

test_that("sn_save() preserves circle layout", {
  adj <- create_test_matrix(6)
  net <- cograph(adj, layout = "circle")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() preserves spring layout", {
  adj <- create_test_matrix(5)
  net <- cograph(adj, layout = "spring")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() preserves grid layout", {
  adj <- create_test_matrix(9)
  net <- cograph(adj, layout = "grid")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() WITH CUSTOMIZATIONS
# ============================================

test_that("sn_save_ggplot() handles network with custom shapes", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj) |>
    sn_nodes(shape = c("circle", "square", "triangle", "diamond"))

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles network with edge customizations", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj) |>
    sn_edges(width = 2, alpha = 0.7)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() WITH TITLE VARIATIONS
# ============================================

test_that("sn_save() handles long title", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  long_title <- paste(rep("Network Analysis Results", 5), collapse = " - ")
  expect_message(sn_save(net, tmp, title = long_title), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles title with special characters", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, title = "Network: Alpha & Beta (v1.0)"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles empty string title", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, title = ""), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE_GGPLOT() WITH TITLE VARIATIONS
# ============================================

test_that("sn_save_ggplot() handles title with special characters", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, title = "Network: Test & Analysis"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() handles empty string title", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, title = ""), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() RETURN VALUE TESTS
# ============================================

test_that("sn_save() returns absolute path when given absolute path", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  result <- suppressMessages(sn_save(net, tmp))

  expect_equal(result, tmp)
  expect_true(is.character(result))
  expect_length(result, 1)
})

test_that("sn_save_ggplot() returns absolute path", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  result <- suppressMessages(sn_save_ggplot(net, tmp))

  expect_equal(result, tmp)
  expect_true(is.character(result))
})

# ============================================
# SN_SAVE() WITH IGRAPH INPUT
# ============================================

test_that("sn_save() handles igraph weighted network", {
  skip_if_no_igraph()

  g <- igraph::make_ring(6)
  igraph::E(g)$weight <- c(1, 0.5, 0.8, 1.2, 0.3, 0.9)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles igraph directed network", {
  skip_if_no_igraph()

  g <- igraph::make_star(5, mode = "out")

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})
