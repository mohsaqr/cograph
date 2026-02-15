# Test plot_mcml with user's parameters
devtools::load_all(".")

# Create test data
set.seed(123)
mat <- matrix(runif(100), 10, 10)
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- LETTERS[1:10]

# Create cograph network with clusters
Net <- as_cograph(mat)
Net$nodes$clusters <- c("G1", "G1", "G1", "G2", "G2", "G2", "G3", "G3", "G3", "G3")

# Save plot to HTML
html_file <- "tmp/mcml_plot.html"
png_file <- tempfile(fileext = ".png")

png(png_file, width = 1200, height = 1000, res = 100)
plot_mcml(Net, "clusters",
          spacing = 18,
          shape_size = 6,
          scale = 4,
          layout_margin = 0.3,
          labels = TRUE,
          summary_edges = TRUE,
          show_labels = TRUE,
          node_size = 2,
          summary_size = 4,
          edge_width_range = c(0.1, 3))
dev.off()

# Create HTML with embedded image
img_data <- base64enc::base64encode(png_file)
html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>plot_mcml Demo</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
    h1 { color: #333; }
    .plot-container { background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    img { max-width: 100%%; height: auto; }
    pre { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; }
  </style>
</head>
<body>
  <h1>plot_mcml() Demo</h1>

  <h2>Code</h2>
  <pre>
plot_mcml(Net, "clusters",
          spacing = 18,
          shape_size = 6,
          scale = 4,
          layout_margin = 0.3,
          labels = TRUE,
          summary_edges = TRUE,
          show_labels = TRUE,
          node_size = 2,
          summary_size = 4,
          edge_width_range = c(0.1, 3))
  </pre>

  <h2>Output</h2>
  <div class="plot-container">
    <img src="data:image/png;base64,%s" alt="plot_mcml output">
  </div>
</body>
</html>
', img_data)

writeLines(html_content, html_file)
cat("HTML saved to:", html_file, "\n")

# Clean up
unlink(png_file)
