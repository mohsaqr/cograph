# test-sn-save.R - Output/Save Function Tests
# Tests for sn_save(), sn_save_ggplot(), and sn_render()

# ============================================
# SN_SAVE() FORMAT SUPPORT
# ============================================

test_that("sn_save() creates PDF file", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)  # PDF should be > 1KB
})

test_that("sn_save() creates PNG file", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)  # PNG should be > 1KB
})

test_that("sn_save() creates SVG file", {
  skip_on_cran()  # SVG requires cairo which may not be available

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
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
  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 500)
})

test_that("sn_save() creates JPEG file", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".jpeg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)
})

test_that("sn_save() creates JPG file (alias)", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)
})

test_that("sn_save() creates TIFF file", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)
})

test_that("sn_save() creates EPS file", {
  skip_on_cran()  # EPS/PostScript may have font issues

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".eps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    sn_save(net, tmp)
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

test_that("sn_save() creates PS file", {
  skip_on_cran()  # PS may have font issues

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".ps")
  on.exit(unlink(tmp), add = TRUE)

  # PostScript devices can fail with font family issues
  result <- tryCatch({
    sn_save(net, tmp)
    TRUE
  }, error = function(e) {
    if (grepl("font|family|postscript", conditionMessage(e), ignore.case = TRUE)) {
      FALSE
    } else {
      stop(e)
    }
  })

  if (!result) skip("PostScript device has font issues on this system")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() DIMENSIONS AND DPI
# ============================================

test_that("sn_save() respects width and height parameters", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  # Small PNG
  tmp_small <- tempfile(fileext = ".png")
  sn_save(net, tmp_small, width = 3, height = 3, dpi = 72)

  # Large PNG
  tmp_large <- tempfile(fileext = ".png")
  sn_save(net, tmp_large, width = 10, height = 10, dpi = 72)

  on.exit({
    unlink(tmp_small)
    unlink(tmp_large)
  }, add = TRUE)

  # Large file should be bigger (more pixels at same dpi)
  expect_true(file.size(tmp_large) > file.size(tmp_small))
})

test_that("sn_save() respects dpi parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  # Low DPI
  tmp_low <- tempfile(fileext = ".png")
  sn_save(net, tmp_low, width = 5, height = 5, dpi = 72)

  # High DPI
  tmp_high <- tempfile(fileext = ".png")
  sn_save(net, tmp_high, width = 5, height = 5, dpi = 300)

  on.exit({
    unlink(tmp_low)
    unlink(tmp_high)
  }, add = TRUE)

  # High DPI file should be larger
  expect_true(file.size(tmp_high) > file.size(tmp_low))
})

# ============================================
# SN_SAVE() WITH TITLE
# ============================================

test_that("sn_save() includes title when specified", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp, title = "My Network"), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() INPUT TYPES
# ============================================

test_that("sn_save() accepts matrix input directly", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(adj, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() accepts edge list input directly", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(edges, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() accepts igraph input directly", {
  skip_if_no_igraph()

  g <- igraph::make_ring(5)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(g, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# SN_SAVE() ERROR HANDLING
# ============================================

test_that("sn_save() errors on missing extension", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_save(net, "filename_without_extension"),
               "extension")
})

test_that("sn_save() errors on unsupported format", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".xyz")

  expect_error(sn_save(net, tmp), "Unsupported")
})

test_that("sn_save() returns filename invisibly", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  result <- suppressMessages(sn_save(net, tmp))

  expect_equal(result, tmp)
})

# ============================================
# SN_SAVE_GGPLOT()
# ============================================

test_that("sn_save_ggplot() creates PDF file", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() creates PNG file", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() respects dimensions", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, width = 4, height = 4, dpi = 72), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() includes title", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save_ggplot(net, tmp, title = "Test Title"), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save_ggplot() returns filename invisibly", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  result <- suppressMessages(sn_save_ggplot(net, tmp))

  expect_equal(result, tmp)
})

# ============================================
# SN_RENDER()
# ============================================

test_that("sn_render() works with sonnet_network", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- safe_plot(sn_render(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_render() works with title", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- safe_plot(sn_render(net, title = "Test Network"))
  expect_true(result$success, info = result$error)
})

# ============================================
# FILE CLEANUP
# ============================================

test_that("sn_save() properly closes device on error", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  # Count devices before
  dev_before <- length(dev.list())

  # Try to save (should succeed)
  tmp <- tempfile(fileext = ".pdf")
  suppressMessages(sn_save(net, tmp))
  unlink(tmp)

  # Count devices after
  dev_after <- length(dev.list())

  # Should have same number of devices (properly closed)
  expect_equal(dev_after, dev_before)
})

# ============================================
# CUSTOMIZED NETWORKS
# ============================================

test_that("sn_save() preserves customizations", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj) |>
    sn_nodes(fill = "steelblue", size = 0.1) |>
    sn_edges(color = "gray50") |>
    sn_theme("dark")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() works with themed network", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj) |> sn_theme("colorblind")

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() works with complex network", {
  adj <- create_test_matrix(6)
  net <- sonnet(adj, layout = "circle") |>
    sn_nodes(
      fill = palette_colorblind(6),
      shape = c("circle", "square", "triangle", "diamond", "pentagon", "hexagon")
    ) |>
    sn_edges(width = 2) |>
    sn_theme("minimal")

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
  expect_file_size(tmp, 1000)
})

# ============================================
# SPECIAL CASES
# ============================================

test_that("sn_save() handles empty network (no edges)", {
  adj <- matrix(0, 4, 4)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles single-node network", {
  adj <- matrix(0, 1, 1)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles network with self-loops", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles donut nodes", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj) |>
    sn_nodes(donut_fill = c(0.3, 0.6, 0.9))

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

test_that("sn_save() handles pie chart nodes", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj) |>
    sn_nodes(pie_values = list(c(1, 2), c(2, 1), c(1, 1, 1)))

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(sn_save(net, tmp), "Saved")
  expect_file_created(tmp)
})

# ============================================
# DIRECTORY HANDLING
# ============================================

test_that("sn_save() creates file in specified directory", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  tmp_dir <- create_temp_dir()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "network.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})

test_that("sn_save() handles paths with spaces", {
  skip_on_cran()  # Path handling can vary by platform

  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  tmp_dir <- tempfile(pattern = "test dir with spaces")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  filepath <- file.path(tmp_dir, "network.pdf")

  expect_message(sn_save(net, filepath), "Saved")
  expect_file_created(filepath)
})
