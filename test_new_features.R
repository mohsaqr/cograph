# Test script for new Sonnet features
# Run this to generate HTML visualization of all new features

library(htmltools)

# Load package using devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::load_all(".")

# Create output directory
output_dir <- "test_output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Test adjacency matrix
adj <- matrix(c(
  0, 1, 1, 0.5,
  1, 0, 1, 0,
  0.5, 1, 0, 1,
  0, 0.5, 1, 0
), 4, 4, byrow = TRUE)

colnames(adj) <- rownames(adj) <- c("A", "B", "C", "D")

# Function to save plot as PNG and return code
save_plot <- function(expr, filename, width = 500, height = 500) {
  filepath <- file.path(output_dir, filename)
  png(filepath, width = width, height = height, res = 100)
  tryCatch({
    eval(expr)
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 0.8)
  })
  dev.off()
  # Return both filepath and code as a list
  list(
    path = filepath,
    code = deparse(expr, width.cutoff = 60)
  )
}

# Helper to format code for HTML display
format_code <- function(code_lines) {
  # Remove quote() wrapper and clean up
  code_text <- paste(code_lines, collapse = "\n")
  code_text <- gsub("^\\{\\n", "", code_text)
  code_text <- gsub("\\n\\}$", "", code_text)
  code_text <- gsub("^    ", "", code_text)  # Remove leading indent
  code_text
}

plots <- list()

# Test 1: Enhanced In-Node Text Control
message("Testing Feature 5: Enhanced In-Node Text Control...")
plots$text_control <- save_plot(quote({
  splot(adj,
    layout = "circle",
    labels = TRUE,
    label_size = 1.2,
    label_fontface = "bold",
    label_fontfamily = "serif",
    label_angle = 0,
    node_size = 5,
    node_fill = "lightblue"
  )
  title("Feature 5: Bold Serif Labels")
}), "01_text_control.png")

# Test 2: Basic donut - just node_shape = "donut" (BUG FIX TEST)
message("Testing basic donut (node_shape = 'donut')...")
plots$basic_donut <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",  # Should auto-fill with lightgray, not show uniform circles!
    node_size = 6
  )
  title("BUG FIX: node_shape = 'donut' (should show full gray rings)")
}), "02_basic_donut.png")

# Test 2b: Donut with single fill color (all nodes steelblue)
message("Testing donut with single color...")
plots$donut_single_color <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",
    donut_color = "steelblue",  # NEW simplified API: single color
    node_size = 6
  )
  title("donut_color = 'steelblue' (all nodes)")
}), "02b_donut_single_color.png")

# Test 2c: Donut with fill + background colors
message("Testing donut with fill + background...")
plots$donut_fill_bg <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",
    donut_color = c("steelblue", "lightyellow"),  # fill + background
    node_size = 6
  )
  title("donut_color = c('steelblue', 'lightyellow')")
}), "02c_donut_fill_bg.png")

# Test 2d: Partial fill with color
message("Testing partial fill with color...")
plots$donut_partial <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",
    donut_fill = 0.5,
    donut_color = "coral",
    node_size = 6
  )
  title("donut_fill = 0.5, donut_color = 'coral'")
}), "02d_donut_partial.png")

# Test 2e: Per-node fill values (same color)
message("Testing per-node fill values...")
plots$donut_per_node <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",
    donut_fill = c(0.25, 0.5, 0.75, 1.0),
    node_size = 6
  )
  title("donut_fill = c(0.25, 0.5, 0.75, 1.0)")
}), "02e_donut_per_node.png")

# Test 2f: Per-node fill + per-node colors (>2 colors = per-node)
message("Testing per-node fill + per-node colors...")
plots$donut_per_node_colors <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "donut",
    donut_fill = c(0.25, 0.5, 0.75, 1.0),
    donut_color = c("red", "orange", "green", "blue"),  # 4 colors for 4 nodes
    node_size = 6
  )
  title("Per-node fill + per-node colors")
}), "02f_donut_per_node_colors.png")

# Test 2g: Donut Value Formatting (with show_value = TRUE)
message("Testing Donut Value Formatting...")
plots$donut_format <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_fill = c(0.75, 0.50, 0.25, 0.90),
    donut_color = c("steelblue", "coral", "forestgreen", "purple"),
    donut_value_size = 1.0,
    donut_value_fontface = "bold",
    donut_value_digits = 0,
    donut_value_suffix = "%",
    donut_show_value = TRUE,  # Explicit TRUE to show values
    node_size = 6
  )
  title("Donut Values with % Suffix")
}), "02g_donut_format.png")

# Test 2h: donut_fill with polygon shape
plots$donut_fill_polygon <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_fill = c(0.25, 0.5, 0.75, 1.0),
    donut_color = c("red", "orange", "green", "blue"),
    donut_shape = "hexagon",
    node_size = 6
  )
  title("donut_fill with hexagon shape")
}), "02h_donut_fill_polygon.png")

# Test 3: Edge Label Shadow
message("Testing Feature 4: Edge Label Shadow...")
plots$shadow <- save_plot(quote({
  splot(adj,
    layout = "circle",
    edge_labels = TRUE,
    edge_label_shadow = TRUE,
    edge_label_shadow_color = "gray30",
    edge_label_shadow_offset = 1,
    edge_label_shadow_alpha = 0.5,
    edge_label_size = 1.0,
    node_size = 4,
    node_fill = "lightgreen"
  )
  title("Feature 4: Edge Labels with Drop Shadow")
}), "03_shadow_labels.png")

# Test 4: AI Shapes - Neural
message("Testing Feature 2: AI Shapes...")
plots$neural <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "neural",
    node_size = 6,
    node_fill = c("steelblue", "coral", "forestgreen", "purple"),
    labels = TRUE,
    label_size = 0.8
  )
  title("AI Shape: Neural")
}), "04_neural.png")

# Test 5: AI Shapes - Chip
plots$chip <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "chip",
    node_size = 6,
    node_fill = c("#4A90D9", "#E74C3C", "#2ECC71", "#9B59B6"),
    labels = TRUE
  )
  title("AI Shape: Chip")
}), "05_chip.png")

# Test 6: AI Shapes - Robot
plots$robot <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "robot",
    node_size = 6,
    node_fill = c("silver", "gold", "#CD7F32", "gray"),
    labels = TRUE
  )
  title("AI Shape: Robot")
}), "06_robot.png")

# Test 7: AI Shapes - Database
plots$database <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "database",
    node_size = 5,
    node_fill = c("#3498DB", "#E67E22", "#1ABC9C", "#8E44AD"),
    labels = TRUE
  )
  title("AI Shape: Database")
}), "07_database.png")

# Test 8: AI Shapes - Cloud
plots$cloud <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "cloud",
    node_size = 6,
    node_fill = c("skyblue", "lightgray", "white", "aliceblue"),
    labels = TRUE
  )
  title("AI Shape: Cloud")
}), "08_cloud.png")

# Test 9: AI Shapes - Gear
plots$gear <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "gear",
    node_size = 6,
    node_fill = c("#95A5A6", "#7F8C8D", "#BDC3C7", "#34495E"),
    labels = TRUE
  )
  title("AI Shape: Gear")
}), "09_gear.png")

# Test 10: AI Shapes - Brain
plots$brain <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "brain",
    node_size = 6,
    node_fill = c("#FFB6C1", "#FFA07A", "#DDA0DD", "#F0E68C"),
    labels = TRUE
  )
  title("AI Shape: Brain")
}), "10_brain.png")

# Test 11: AI Shapes - Network
plots$network <- save_plot(quote({
  splot(adj,
    layout = "circle",
    node_shape = "network",
    node_size = 6,
    node_fill = c("#3498DB", "#E74C3C", "#2ECC71", "#F39C12"),
    labels = TRUE
  )
  title("AI Shape: Network")
}), "11_network.png")

# Test 12: Polygon Donut - Square
message("Testing Feature 1: Polygon Donuts...")
plots$square_donut <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_values = list(0.7, 0.5, 0.3, 0.9),
    donut_color = c("steelblue", "coral", "forestgreen", "purple"),
    donut_shape = "square",
    node_size = 6
  )
  title("Feature 1: Square Donut")
}), "12_square_donut.png")

# Test 13: Polygon Donut - Hexagon
plots$hex_donut <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_values = list(0.8, 0.6, 0.4, 0.7),
    donut_color = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6"),
    donut_shape = "hexagon",
    node_size = 6
  )
  title("Feature 1: Hexagon Donut")
}), "13_hex_donut.png")

# Test 14: Polygon Donut - Triangle
plots$tri_donut <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_values = list(0.6, 0.75, 0.5, 0.85),
    donut_color = c("orange", "dodgerblue", "limegreen", "hotpink"),
    donut_shape = "triangle",
    node_size = 7
  )
  title("Feature 1: Triangle Donut")
}), "14_tri_donut.png")

# Test 15: Polygon Donut - Diamond
plots$diamond_donut <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_values = list(0.9, 0.4, 0.65, 0.55),
    donut_color = c("gold", "silver", "#CD7F32", "gray"),
    donut_shape = "diamond",
    node_size = 6
  )
  title("Feature 1: Diamond Donut")
}), "15_diamond_donut.png")

# Test 16: All AI shapes in one plot
plots$all_ai <- save_plot(quote({
  adj8 <- matrix(0, 8, 8)
  adj8[1, 2:8] <- 1
  adj8[2:8, 1] <- 1

  splot(adj8,
    node_shape = c("neural", "chip", "robot", "brain", "network", "database", "cloud", "gear"),
    node_size = 5,
    node_fill = c("#3498DB", "#E74C3C", "#2ECC71", "#9B59B6", "#F39C12", "#1ABC9C", "#95A5A6", "#34495E"),
    labels = c("Neural", "Chip", "Robot", "Brain", "Network", "DB", "Cloud", "Gear"),
    label_size = 0.7,
    layout = "circle"
  )
  title("All AI Shapes")
}), "16_all_ai_shapes.png", width = 600, height = 600)

# Test 17: Combined features
plots$combined <- save_plot(quote({
  splot(adj,
    layout = "circle",
    donut_values = list(0.7, 0.5, 0.8, 0.6),
    donut_color = c("steelblue", "coral", "forestgreen", "purple"),
    donut_shape = "hexagon",
    donut_value_digits = 0,
    donut_value_suffix = "%",
    edge_labels = TRUE,
    edge_label_shadow = TRUE,
    edge_label_shadow_alpha = 0.4,
    node_size = 6,
    label_fontface = "bold"
  )
  title("Combined: Hex Donuts + Shadow Labels")
}), "17_combined.png")

# Helper function to create a plot card with code
make_plot_card <- function(img_src, title, code_text, width = 400) {
  tags$div(class = "plot-with-code",
    tags$img(src = img_src, width = width),
    tags$p(tags$strong(title)),
    tags$pre(class = "code-block", code_text)
  )
}

# Generate HTML report
message("Generating HTML report...")

html_content <- tags$html(
  tags$head(
    tags$title("Sonnet New Features Test"),
    tags$style(HTML("
      body { font-family: Arial, sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px; }
      h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
      h2 { color: #34495e; margin-top: 30px; }
      .feature { background: #f8f9fa; padding: 15px; margin: 20px 0; border-radius: 8px; }
      .feature h3 { color: #2980b9; margin-top: 0; }
      .plots { display: flex; flex-wrap: wrap; gap: 20px; justify-content: center; }
      .summary { background: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; margin: 20px 0; }
      code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; font-family: 'Consolas', 'Monaco', monospace; }
      pre.code-block { background: #1e1e1e; color: #d4d4d4; padding: 12px; border-radius: 6px;
                       overflow-x: auto; font-size: 0.8em; margin: 10px 0; text-align: left; white-space: pre-wrap;
                       font-family: 'Consolas', 'Monaco', 'Courier New', monospace; line-height: 1.4; }
      .plot-with-code { display: flex; flex-direction: column; align-items: center;
                        background: #fff; padding: 15px; border-radius: 8px;
                        box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin: 10px; width: 420px; }
      .plot-with-code img { max-width: 100%; border: 1px solid #ddd; border-radius: 4px; }
      .plot-with-code pre { width: 100%; box-sizing: border-box; }
      .plot-with-code .title { font-weight: bold; margin: 8px 0; color: #333; }
    "))
  ),
  tags$body(
    tags$h1("Sonnet New Features Test Results"),

    tags$div(class = "summary",
      tags$h3("Features Implemented"),
      tags$ul(
        tags$li(tags$strong("BUG FIX:"), " node_shape = 'donut' now works (auto-triggers donut rendering)"),
        tags$li(tags$strong("NEW API:"), " Simplified donut_color (1 color = all, 2 = fill+bg, >2 = per-node)"),
        tags$li(tags$strong("Feature 1:"), " Polygon Donut Rings (square, hexagon, triangle, diamond, pentagon)"),
        tags$li(tags$strong("Feature 2:"), " AI-Themed Shapes (neural, chip, robot, brain, network, database, cloud, gear)"),
        tags$li(tags$strong("Feature 3:"), " Custom SVG Node Shapes (register_svg_shape())"),
        tags$li(tags$strong("Feature 4:"), " Edge Label Drop Shadow"),
        tags$li(tags$strong("Feature 5:"), " Enhanced In-Node Text Control")
      )
    ),

    # ===== BUG FIX: node_shape = "donut" =====
    tags$div(class = "feature",
      tags$h3("BUG FIX: node_shape = 'donut' Now Works!"),
      tags$p("Previously, ", tags$code("node_shape = 'donut'"), " showed uniform circles instead of donut rings."),
      tags$p("Now it automatically sets ", tags$code("donut_fill = 1.0"), " and uses 'lightgray' as default color."),
      tags$div(class = "plots",
        make_plot_card("02_basic_donut.png", "Basic donut (just node_shape)",
'splot(adj,
  layout = "circle",
  node_shape = "donut",  # Now works!
  node_size = 6
)  # Auto: donut_fill=1.0, color="lightgray"'),
        make_plot_card("02b_donut_single_color.png", "Single fill color",
'splot(adj,
  layout = "circle",
  node_shape = "donut",
  donut_color = "steelblue",
  node_size = 6
)'),
        make_plot_card("02c_donut_fill_bg.png", "Fill + background color",
'splot(adj,
  layout = "circle",
  node_shape = "donut",
  donut_color = c("steelblue", "lightyellow"),
  node_size = 6
)  # 2 colors = fill + background')
      )
    ),

    # ===== Simplified Donut API (donut_color) =====
    tags$div(class = "feature",
      tags$h3("NEW: Simplified donut_color API"),
      tags$p("The new ", tags$code("donut_color"), " parameter replaces ", tags$code("donut_colors"), " (deprecated)."),
      tags$ul(
        tags$li(tags$code("1 color"), " = fill color for ALL nodes"),
        tags$li(tags$code("2 colors"), " = fill + background for ALL nodes"),
        tags$li(tags$code(">2 colors"), " = per-node fill colors")
      ),
      tags$div(class = "plots",
        make_plot_card("02d_donut_partial.png", "Partial fill with color",
'splot(adj,
  layout = "circle",
  node_shape = "donut",
  donut_fill = 0.5,
  donut_color = "coral",
  node_size = 6
)'),
        make_plot_card("02e_donut_per_node.png", "Per-node fill values",
'splot(adj,
  layout = "circle",
  node_shape = "donut",
  donut_fill = c(0.25, 0.5, 0.75, 1.0),
  node_size = 6
)  # Uses default lightgray'),
        make_plot_card("02f_donut_per_node_colors.png", "Per-node fill + colors",
'splot(adj,
  layout = "circle",
  node_shape = "donut",
  donut_fill = c(0.25, 0.5, 0.75, 1.0),
  donut_color = c("red", "orange",
                  "green", "blue"),
  node_size = 6
)  # 4 colors = per-node')
      )
    ),

    # ===== Feature 5: Enhanced In-Node Text Control =====
    tags$div(class = "feature",
      tags$h3("Feature 5: Enhanced In-Node Text Control"),
      tags$p("New parameters: ", tags$code("label_fontface"), ", ", tags$code("label_fontfamily"), ", ",
             tags$code("label_hjust"), ", ", tags$code("label_vjust"), ", ", tags$code("label_angle")),
      tags$div(class = "plots",
        make_plot_card("01_text_control.png", "Bold serif labels",
'splot(adj,
  layout = "circle",
  labels = TRUE,
  label_size = 1.2,
  label_fontface = "bold",
  label_fontfamily = "serif",
  node_size = 5,
  node_fill = "lightblue"
)'),
        make_plot_card("02g_donut_format.png", "Donut values with % suffix",
'splot(adj,
  layout = "circle",
  donut_fill = c(0.75, 0.50, 0.25, 0.90),
  donut_color = c("steelblue", "coral",
                  "forestgreen", "purple"),
  donut_show_value = TRUE,  # Must set TRUE
  donut_value_digits = 0,
  donut_value_suffix = "%",
  node_size = 6
)')
      )
    ),

    # ===== Feature 4: Edge Label Drop Shadow =====
    tags$div(class = "feature",
      tags$h3("Feature 4: Edge Label Drop Shadow"),
      tags$p("New parameters: ", tags$code("edge_label_shadow"), ", ", tags$code("edge_label_shadow_color"), ", ",
             tags$code("edge_label_shadow_offset"), ", ", tags$code("edge_label_shadow_alpha")),
      tags$div(class = "plots",
        make_plot_card("03_shadow_labels.png", "Edge labels with drop shadow",
'splot(adj,
  layout = "circle",
  edge_labels = TRUE,
  edge_label_shadow = TRUE,
  edge_label_shadow_color = "gray30",
  edge_label_shadow_offset = 1,
  edge_label_shadow_alpha = 0.5,
  edge_label_size = 1.0,
  node_size = 4,
  node_fill = "lightgreen"
)')
      )
    ),

    # ===== Feature 2: AI-Themed Node Shapes =====
    tags$div(class = "feature",
      tags$h3("Feature 2: AI-Themed Node Shapes"),
      tags$p("8 new shapes: ", tags$code("neural"), ", ", tags$code("chip"), ", ", tags$code("robot"), ", ",
             tags$code("brain"), ", ", tags$code("network"), ", ", tags$code("database"), ", ",
             tags$code("cloud"), ", ", tags$code("gear")),
      tags$div(class = "plots",
        make_plot_card("04_neural.png", "Neural shape",
'splot(adj,
  layout = "circle",
  node_shape = "neural",
  node_size = 6,
  node_fill = c("steelblue", "coral",
                "forestgreen", "purple")
)', 300),
        make_plot_card("05_chip.png", "Chip shape",
'splot(adj,
  layout = "circle",
  node_shape = "chip",
  node_size = 6,
  node_fill = c("#4A90D9", "#E74C3C",
                "#2ECC71", "#9B59B6")
)', 300),
        make_plot_card("06_robot.png", "Robot shape",
'splot(adj,
  layout = "circle",
  node_shape = "robot",
  node_size = 6,
  node_fill = c("silver", "gold",
                "#CD7F32", "gray")
)', 300),
        make_plot_card("07_database.png", "Database shape",
'splot(adj,
  layout = "circle",
  node_shape = "database",
  node_size = 5,
  node_fill = c("#3498DB", "#E67E22",
                "#1ABC9C", "#8E44AD")
)', 300)
      ),
      tags$div(class = "plots",
        make_plot_card("08_cloud.png", "Cloud shape",
'splot(adj,
  layout = "circle",
  node_shape = "cloud",
  node_size = 6,
  node_fill = c("skyblue", "lightgray",
                "white", "aliceblue")
)', 300),
        make_plot_card("09_gear.png", "Gear shape",
'splot(adj,
  layout = "circle",
  node_shape = "gear",
  node_size = 6,
  node_fill = c("#95A5A6", "#7F8C8D",
                "#BDC3C7", "#34495E")
)', 300),
        make_plot_card("10_brain.png", "Brain shape",
'splot(adj,
  layout = "circle",
  node_shape = "brain",
  node_size = 6,
  node_fill = c("#FFB6C1", "#FFA07A",
                "#DDA0DD", "#F0E68C")
)', 300),
        make_plot_card("11_network.png", "Network shape",
'splot(adj,
  layout = "circle",
  node_shape = "network",
  node_size = 6,
  node_fill = c("#3498DB", "#E74C3C",
                "#2ECC71", "#F39C12")
)', 300)
      ),
      tags$div(class = "plots",
        make_plot_card("16_all_ai_shapes.png", "All AI shapes together",
'adj8 <- matrix(0, 8, 8)
adj8[1, 2:8] <- 1; adj8[2:8, 1] <- 1

splot(adj8,
  node_shape = c("neural", "chip", "robot",
    "brain", "network", "database",
    "cloud", "gear"),
  node_size = 5,
  labels = c("Neural", "Chip", "Robot",
    "Brain", "Network", "DB", "Cloud", "Gear"),
  layout = "circle"
)', 500)
      )
    ),

    # ===== Feature 1: Polygon Donut Rings =====
    tags$div(class = "feature",
      tags$h3("Feature 1: Polygon Donut Rings"),
      tags$p("New parameter: ", tags$code("donut_shape"), " - supports ",
             tags$code("circle"), ", ", tags$code("square"), ", ", tags$code("hexagon"), ", ",
             tags$code("triangle"), ", ", tags$code("diamond"), ", ", tags$code("pentagon")),
      tags$div(class = "plots",
        make_plot_card("12_square_donut.png", "Square Donut",
'splot(adj,
  layout = "circle",
  donut_values = list(0.7, 0.5, 0.3, 0.9),
  donut_color = c("steelblue", "coral",
                      "forestgreen", "purple"),
  donut_shape = "square",
  node_size = 6
)', 300),
        make_plot_card("13_hex_donut.png", "Hexagon Donut",
'splot(adj,
  layout = "circle",
  donut_values = list(0.8, 0.6, 0.4, 0.7),
  donut_color = c("#E74C3C", "#3498DB",
                      "#2ECC71", "#9B59B6"),
  donut_shape = "hexagon",
  node_size = 6
)', 300),
        make_plot_card("14_tri_donut.png", "Triangle Donut",
'splot(adj,
  layout = "circle",
  donut_values = list(0.6, 0.75, 0.5, 0.85),
  donut_color = c("orange", "dodgerblue",
                      "limegreen", "hotpink"),
  donut_shape = "triangle",
  node_size = 7
)', 300),
        make_plot_card("15_diamond_donut.png", "Diamond Donut",
'splot(adj,
  layout = "circle",
  donut_values = list(0.9, 0.4, 0.65, 0.55),
  donut_color = c("gold", "silver",
                      "#CD7F32", "gray"),
  donut_shape = "diamond",
  node_size = 6
)', 300)
      )
    ),

    # ===== Combined Features =====
    tags$div(class = "feature",
      tags$h3("Combined Features"),
      tags$div(class = "plots",
        make_plot_card("17_combined.png", "Hex donuts + shadow labels",
'splot(adj,
  layout = "circle",
  donut_values = list(0.7, 0.5, 0.8, 0.6),
  donut_color = c("steelblue", "coral",
                      "forestgreen", "purple"),
  donut_shape = "hexagon",
  donut_value_digits = 0,
  donut_value_suffix = "%",
  edge_labels = TRUE,
  edge_label_shadow = TRUE,
  edge_label_shadow_alpha = 0.4,
  node_size = 6,
  label_fontface = "bold"
)', 450)
      )
    ),

    # ===== Feature 3: Custom SVG Node Shapes =====
    tags$div(class = "feature",
      tags$h3("Feature 3: Custom SVG Node Shapes"),
      tags$p("New functions: ", tags$code("register_svg_shape(name, svg_source)"), ", ",
             tags$code("list_svg_shapes()"), ", ", tags$code("unregister_svg_shape(name)")),
      tags$p("New parameters: ", tags$code("node_svg"), ", ", tags$code("svg_preserve_aspect")),
      tags$pre(class = "code-block",
'# Register a custom SVG shape
register_svg_shape("my_icon", "path/to/icon.svg")

# Use it in a plot
splot(adj, node_shape = "my_icon")

# Or use inline SVG directly
splot(adj, node_svg = \'<svg>...</svg>\')'),
      tags$p(tags$em("Note: Requires optional packages 'grImport2' (Grid) or 'rsvg' (Base R) for SVG rendering."))
    )
  )
)

# Save HTML
html_file <- file.path(output_dir, "test_results.html")
save_html(html_content, html_file)

message("\n=== Test Complete ===")
message("HTML report saved to: ", normalizePath(html_file))
message("Open this file in a browser to view results.")
