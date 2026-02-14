# Tests for splot-labels.R - Edge Label Template Formatting
# Comprehensive coverage for label rendering, formatting, positioning, and styling
# ============================================

# ============================================
# Test Setup
# ============================================

# Create simple test network helper
create_test_network <- function(n = 4, seed = 42) {
  set.seed(seed)
  mat <- matrix(runif(n * n, 0.1, 0.9), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  mat
}

# ============================================
# get_significance_stars() Tests
# ============================================

test_that("get_significance_stars returns *** for p < 0.001", {
  result <- get_significance_stars(0.0005)
  expect_equal(result, "***")
})

test_that("get_significance_stars returns ** for p in [0.001, 0.01)", {
  result <- get_significance_stars(0.005)
  expect_equal(result, "**")
})

test_that("get_significance_stars returns * for p in [0.01, 0.05)", {
  result <- get_significance_stars(0.03)
  expect_equal(result, "*")
})

test_that("get_significance_stars returns empty for p >= 0.05", {
  result <- get_significance_stars(0.1)
  expect_equal(result, "")
})

test_that("get_significance_stars handles NA values", {
  result <- get_significance_stars(NA)
  expect_equal(result, "")
})

test_that("get_significance_stars handles NULL input", {
  result <- get_significance_stars(NULL)
  expect_null(result)
})

test_that("get_significance_stars handles vector of p-values", {
  p_values <- c(0.0001, 0.005, 0.03, 0.1, NA)
  result <- get_significance_stars(p_values)
  expect_equal(result, c("***", "**", "*", "", ""))
})

test_that("get_significance_stars handles boundary values", {
  expect_equal(get_significance_stars(0.001), "**")
  expect_equal(get_significance_stars(0.01), "*")
  expect_equal(get_significance_stars(0.05), "")
  expect_equal(get_significance_stars(0.0009999), "***")
})

# ============================================
# format_pvalue() Tests
# ============================================

test_that("format_pvalue formats regular p-values correctly", {
  result <- format_pvalue(0.035, digits = 3, prefix = "p=")
  expect_equal(result, "p=0.035")
})

test_that("format_pvalue handles very small p-values", {
  result <- format_pvalue(0.00005, digits = 3, prefix = "p=")
  expect_equal(result, "p=<0.001")
})

test_that("format_pvalue respects digit parameter", {
  result <- format_pvalue(0.0354, digits = 2, prefix = "p=")
  expect_equal(result, "p=0.04")
})

test_that("format_pvalue respects custom prefix", {
  result <- format_pvalue(0.05, digits = 2, prefix = "P: ")
  expect_equal(result, "P: 0.05")
})

test_that("format_pvalue handles NULL input", {
  result <- format_pvalue(NULL)
  expect_equal(result, "")
})

test_that("format_pvalue handles NA input", {
  result <- format_pvalue(NA)
  expect_equal(result, "")
})

test_that("format_pvalue handles exact threshold values", {
  result <- format_pvalue(0.001, digits = 3, prefix = "p=")
  expect_equal(result, "p=0.001")
})

# ============================================
# format_ci_range() Tests
# ============================================

test_that("format_ci_range uses bracket format by default", {
  result <- format_ci_range(0.2, 0.8, digits = 2, format = "bracket")
  expect_equal(result, "[0.20, 0.80]")
})

test_that("format_ci_range uses dash format when specified", {
  result <- format_ci_range(0.2, 0.8, digits = 2, format = "dash")
  expect_equal(result, "0.20-0.80")
})

test_that("format_ci_range handles negative values", {
  result <- format_ci_range(-0.5, 0.3, digits = 2, format = "bracket")
  expect_equal(result, "[-0.50, 0.30]")
})

test_that("format_ci_range respects digits parameter", {
  result <- format_ci_range(0.123, 0.789, digits = 3, format = "bracket")
  expect_equal(result, "[0.123, 0.789]")
})

test_that("format_ci_range handles NULL lower bound", {
  result <- format_ci_range(NULL, 0.8, digits = 2)
  expect_equal(result, "")
})

test_that("format_ci_range handles NULL upper bound", {
  result <- format_ci_range(0.2, NULL, digits = 2)
  expect_equal(result, "")
})

test_that("format_ci_range handles NA lower bound", {
  result <- format_ci_range(NA, 0.8, digits = 2)
  expect_equal(result, "")
})

test_that("format_ci_range handles NA upper bound", {
  result <- format_ci_range(0.2, NA, digits = 2)
  expect_equal(result, "")
})

test_that("format_ci_range removes leading zero when requested", {
  result <- format_ci_range(0.2, 0.8, digits = 2, format = "bracket", leading_zero = FALSE)
  expect_true(grepl("^\\[\\.", result))  # Starts with [.
})

test_that("format_ci_range keeps leading zero by default", {
  result <- format_ci_range(0.2, 0.8, digits = 2, format = "bracket", leading_zero = TRUE)
  expect_true(grepl("^\\[0\\.", result))  # Starts with [0.
})

# ============================================
# resolve_stars() Tests
# ============================================

test_that("resolve_stars returns empty for NULL input", {
  result <- resolve_stars(NULL, p_values = NULL, n = 5)
  expect_equal(result, rep("", 5))
})

test_that("resolve_stars computes from p-values when TRUE", {
  p_values <- c(0.0001, 0.005, 0.03, 0.1)
  result <- resolve_stars(TRUE, p_values = p_values, n = 4)
  expect_equal(result, c("***", "**", "*", ""))
})

test_that("resolve_stars returns empty when TRUE but no p-values", {
  result <- resolve_stars(TRUE, p_values = NULL, n = 3)
  expect_equal(result, rep("", 3))
})

test_that("resolve_stars returns empty for FALSE input", {
  result <- resolve_stars(FALSE, p_values = c(0.001, 0.01), n = 2)
  expect_equal(result, rep("", 2))
})

test_that("resolve_stars computes from numeric input as p-values", {
  result <- resolve_stars(c(0.0001, 0.03), p_values = NULL, n = 2)
  expect_equal(result, c("***", "*"))
})

test_that("resolve_stars uses character input directly", {
  result <- resolve_stars(c("*", "**"), p_values = NULL, n = 2)
  expect_equal(result, c("*", "**"))
})

test_that("resolve_stars recycles character input", {
  result <- resolve_stars("*", p_values = NULL, n = 3)
  expect_equal(result, rep("*", 3))
})

# ============================================
# format_edge_label_template() Tests
# ============================================

test_that("format_edge_label_template formats estimate placeholder", {
  result <- format_edge_label_template(
    template = "{est}",
    weight = 0.567,
    digits = 2
  )
  expect_equal(result, "0.57")
})

test_that("format_edge_label_template formats range placeholder", {
  result <- format_edge_label_template(
    template = "{range}",
    ci_lower = 0.2,
    ci_upper = 0.8,
    digits = 2,
    ci_format = "bracket"
  )
  expect_equal(result, "[0.20, 0.80]")
})

test_that("format_edge_label_template formats combined est and range", {
  result <- format_edge_label_template(
    template = "{est} {range}",
    weight = 0.5,
    ci_lower = 0.3,
    ci_upper = 0.7,
    digits = 2
  )
  expect_equal(result, "0.50 [0.30, 0.70]")
})

test_that("format_edge_label_template formats individual CI bounds", {
  result <- format_edge_label_template(
    template = "{low} to {up}",
    ci_lower = 0.2,
    ci_upper = 0.8,
    digits = 2
  )
  expect_equal(result, "0.20 to 0.80")
})

test_that("format_edge_label_template formats p-value placeholder", {
  result <- format_edge_label_template(
    template = "{p}",
    p_value = 0.035,
    p_digits = 3,
    p_prefix = "p="
  )
  expect_equal(result, "p=0.035")
})

test_that("format_edge_label_template formats stars placeholder", {
  result <- format_edge_label_template(
    template = "{est}{stars}",
    weight = 0.5,
    stars = "**",
    digits = 2
  )
  expect_equal(result, "0.50**")
})

test_that("format_edge_label_template handles empty template", {
  result <- format_edge_label_template(template = "")
  expect_equal(result, "")
})

test_that("format_edge_label_template handles NULL template", {
  result <- format_edge_label_template(template = NULL)
  expect_equal(result, "")
})

test_that("format_edge_label_template removes leading zeros when requested", {
  result <- format_edge_label_template(
    template = "{est}",
    weight = 0.567,
    digits = 2,
    leading_zero = FALSE
  )
  expect_equal(result, ".57")
})

test_that("format_edge_label_template handles negative values without leading zero", {
  result <- format_edge_label_template(
    template = "{est}",
    weight = -0.567,
    digits = 2,
    leading_zero = FALSE
  )
  expect_equal(result, "-.57")
})

test_that("format_edge_label_template cleans up extra whitespace", {
  result <- format_edge_label_template(
    template = "{est}   {stars}",
    weight = 0.5,
    stars = "",
    digits = 2
  )
  expect_equal(trimws(result), "0.50")
})

test_that("format_edge_label_template handles NA weight", {
  result <- format_edge_label_template(
    template = "{est}",
    weight = NA,
    digits = 2
  )
  expect_equal(result, "")
})

# ============================================
# get_template_from_style() Tests
# ============================================

test_that("get_template_from_style returns NULL for none", {
  result <- get_template_from_style("none")
  expect_null(result)
})

test_that("get_template_from_style returns estimate template", {
  result <- get_template_from_style("estimate")
  expect_equal(result, "{est}")
})

test_that("get_template_from_style returns full template", {
  result <- get_template_from_style("full")
  expect_equal(result, "{est} {range}")
})

test_that("get_template_from_style returns range template", {
  result <- get_template_from_style("range")
  expect_equal(result, "{range}")
})

test_that("get_template_from_style returns stars template", {
  result <- get_template_from_style("stars")
  expect_equal(result, "{stars}")
})

test_that("get_template_from_style returns NULL for unknown style", {
  result <- get_template_from_style("unknown_style")
  expect_null(result)
})

# ============================================
# build_edge_labels_from_template() Tests
# ============================================

test_that("build_edge_labels_from_template builds estimate labels", {
  result <- build_edge_labels_from_template(
    style = "estimate",
    weights = c(0.1, 0.5, 0.9),
    n = 3,
    digits = 2
  )
  expect_equal(result, c("0.10", "0.50", "0.90"))
})

test_that("build_edge_labels_from_template builds range labels", {
  result <- build_edge_labels_from_template(
    style = "range",
    ci_lower = c(0.1, 0.2),
    ci_upper = c(0.3, 0.5),
    n = 2,
    digits = 2
  )
  expect_equal(result, c("[0.10, 0.30]", "[0.20, 0.50]"))
})

test_that("build_edge_labels_from_template builds full labels", {
  result <- build_edge_labels_from_template(
    style = "full",
    weights = c(0.5),
    ci_lower = c(0.3),
    ci_upper = c(0.7),
    n = 1,
    digits = 2
  )
  expect_equal(result, "0.50 [0.30, 0.70]")
})

test_that("build_edge_labels_from_template builds stars labels", {
  result <- build_edge_labels_from_template(
    style = "stars",
    stars = TRUE,
    p_values = c(0.001, 0.01, 0.05, 0.5),
    n = 4
  )
  expect_equal(result, c("**", "*", "", ""))
})

test_that("build_edge_labels_from_template returns NULL for none style", {
  result <- build_edge_labels_from_template(
    style = "none",
    weights = c(0.5),
    n = 1
  )
  expect_null(result)
})

test_that("build_edge_labels_from_template uses custom template over style", {
  result <- build_edge_labels_from_template(
    template = "{est}%",
    style = "full",  # Should be ignored
    weights = c(0.5),
    n = 1,
    digits = 0
  )
  expect_equal(result, "0%")
})

test_that("build_edge_labels_from_template recycles weights", {
  result <- build_edge_labels_from_template(
    style = "estimate",
    weights = 0.5,  # Single value, recycled
    n = 3,
    digits = 1
  )
  expect_equal(result, rep("0.5", 3))
})

test_that("build_edge_labels_from_template handles missing CI bounds", {
  result <- build_edge_labels_from_template(
    style = "full",
    weights = c(0.5, 0.6),
    ci_lower = NULL,
    ci_upper = NULL,
    n = 2,
    digits = 2
  )
  # Should have estimate but no range
  expect_true(all(grepl("0\\.", result)))
})

test_that("build_edge_labels_from_template respects p_prefix", {
  result <- build_edge_labels_from_template(
    template = "{p}",
    p_values = c(0.05),
    n = 1,
    p_digits = 2,
    p_prefix = "P: "
  )
  expect_equal(result, "P: 0.05")
})

test_that("build_edge_labels_from_template respects ci_format", {
  result <- build_edge_labels_from_template(
    style = "range",
    ci_lower = c(0.2),
    ci_upper = c(0.8),
    n = 1,
    digits = 2,
    ci_format = "dash"
  )
  expect_equal(result, "0.20-0.80")
})

# ============================================
# Integration Tests - Labels with splot
# ============================================

test_that("splot renders edge labels without error", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE)
  ))
})

test_that("splot renders edge labels with custom format", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_digits = 3)
  ))
})

test_that("splot renders edge labels with halo effect", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_halo = TRUE)
  ))
})

test_that("splot renders edge labels without halo", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_halo = FALSE)
  ))
})

test_that("splot renders edge labels with background", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_bg = "white")
  ))
})

test_that("splot renders edge labels with custom color", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_color = "red")
  ))
})

test_that("splot renders edge labels with custom font size", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_size = 1.5)
  ))
})

test_that("splot renders edge labels with small font", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_size = 0.5)
  ))
})

# ============================================
# Edge Cases and Boundary Tests
# ============================================

test_that("labels work with single edge network", {
  mat <- matrix(0, 2, 2)
  mat[1, 2] <- 0.5
  colnames(mat) <- rownames(mat) <- c("A", "B")

  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE)
  ))
})

test_that("labels work with dense network", {
  mat <- create_test_network(n = 6)
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_size = 0.5)
  ))
})

test_that("labels work with negative edge weights", {
  mat <- create_test_network()
  mat[mat > 0] <- mat[mat > 0] - 0.5  # Make some negative

  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE)
  ))
})

test_that("labels work with very small edge weights", {
  mat <- create_test_network()
  mat[mat > 0] <- mat[mat > 0] * 0.001

  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_digits = 4)
  ))
})

test_that("labels work with very large edge weights", {
  mat <- create_test_network()
  mat[mat > 0] <- mat[mat > 0] * 100

  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, edge_label_digits = 0)
  ))
})

test_that("splot with node labels does not conflict with edge labels", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = TRUE, labels = TRUE)
  ))
})

test_that("splot works with both edge and node labels off", {
  mat <- create_test_network()
  expect_no_error(with_temp_png(
    splot(mat, edge_labels = FALSE, labels = FALSE)
  ))
})

# ============================================
# PDF Device Tests
# ============================================

test_that("labels render correctly on PDF device", {
  mat <- create_test_network()
  expect_no_error(with_temp_pdf(
    splot(mat, edge_labels = TRUE)
  ))
})

test_that("labels with halo render correctly on PDF device", {
  mat <- create_test_network()
  expect_no_error(with_temp_pdf(
    splot(mat, edge_labels = TRUE, edge_label_halo = TRUE)
  ))
})

# ============================================
# Combined Formatting Tests
# ============================================

test_that("build_edge_labels handles complex template", {
  result <- build_edge_labels_from_template(
    template = "{est}{stars} ({low}-{up})",
    weights = c(0.5),
    ci_lower = c(0.3),
    ci_upper = c(0.7),
    stars = c("**"),
    n = 1,
    digits = 2
  )
  expect_true(grepl("0\\.50\\*\\*", result))
  expect_true(grepl("0\\.30-0\\.70", result))
})

test_that("build_edge_labels handles all placeholders", {
  result <- build_edge_labels_from_template(
    template = "{est} {range} {p} {stars}",
    weights = c(0.5),
    ci_lower = c(0.3),
    ci_upper = c(0.7),
    p_values = c(0.01),
    stars = TRUE,
    n = 1,
    digits = 2,
    p_digits = 2
  )
  # Should contain estimate, range, p-value, and stars
  expect_true(grepl("0\\.50", result))
  expect_true(grepl("\\[", result))
  expect_true(grepl("p=", result))
  expect_true(grepl("\\*", result))
})

# ============================================
# Total Test Count Verification
# ============================================
# Tests in this file:
# - get_significance_stars: 8 tests
# - format_pvalue: 7 tests
# - format_ci_range: 10 tests
# - resolve_stars: 7 tests
# - format_edge_label_template: 12 tests
# - get_template_from_style: 6 tests
# - build_edge_labels_from_template: 10 tests
# - Integration tests with splot: 8 tests
# - Edge cases and boundary tests: 7 tests
# - PDF device tests: 2 tests
# - Combined formatting tests: 2 tests
# Total: 79 tests
