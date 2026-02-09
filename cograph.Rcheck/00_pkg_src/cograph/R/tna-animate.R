#' Generate Windowed TNA Objects from Sequence Data
#'
#' Creates a series of TNA (Transition Network Analysis) models from rolling
#' windows over wide-format sequence data.
#'
#' @param x A wide data.frame where rows are sequences (individuals/cases) and
#'   columns are time points. Cell values should be states (character or factor).
#' @param window_size Number of time points per window. Default is 5.
#' @param step Step size between windows. Use 1 for sliding windows (default),
#'   or set equal to `window_size` for tumbling (non-overlapping) windows.
#' @param na_threshold Maximum proportion of NA values allowed in a window.
#'   If exceeded, window generation stops early. Default is 0.5.
#'
#' @return A list with components:
#'   \item{windows}{List of tna objects, one per valid window}
#'   \item{start_times}{Integer vector of window start column indices}
#'   \item{end_times}{Integer vector of window end column indices}
#'   \item{na_proportions}{Numeric vector of NA proportions per window}
#'   \item{stopped_early}{Logical indicating if generation stopped due to NA threshold}
#'   \item{stopped_at}{If stopped early, the window index where it stopped}
#'
#' @details
#' The function iterates through the columns of the input data frame using

#' sliding or tumbling windows. For each window, it:
#' 1. Extracts the subset of columns
#' 2. Checks the NA proportion against the threshold
#' 3. If valid, estimates a TNA model using `tna::tna()`
#'
#' Window generation stops when either all windows are processed or the NA
#' proportion exceeds the threshold.
#'
#' @seealso [tna_animate()] for creating animated GIFs from sequence data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample sequence data
#' set.seed(123)
#' states <- c("A", "B", "C")
#' data <- data.frame(matrix(
#'   sample(states, 100 * 10, replace = TRUE),
#'   nrow = 100, ncol = 10
#' ))
#'
#' # Generate windowed TNA models
#' result <- tna_windows(data, window_size = 5, step = 1)
#'
#' # Check how many windows were created
#' length(result$windows)
#' }
tna_windows <- function(x, window_size = 5, step = 1, na_threshold = 0.5) {
  # Check for tna package

if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' required for TNA analysis. Install with: install.packages('tna')")
  }

  # Validate input
  if (!is.data.frame(x)) x <- as.data.frame(x)
  n_times <- ncol(x)

  if (window_size > n_times) {
    stop(sprintf(
      "window_size (%d) cannot exceed number of time points (%d)",
      window_size, n_times
    ))
  }

  if (window_size < 2) {
    stop("window_size must be at least 2")
  }

  if (step < 1) {
    stop("step must be at least 1")
  }

  # Generate window start indices
  starts <- seq(1, n_times - window_size + 1, by = step)

  if (length(starts) == 0) {
    stop("No valid windows can be generated with the given parameters")
  }

  windows <- list()
  na_props <- numeric()

  for (i in seq_along(starts)) {
    start <- starts[i]
    end <- start + window_size - 1

    # Subset columns for this window
    window_data <- x[, start:end, drop = FALSE]

    # Check NA proportion
    na_prop <- mean(is.na(window_data))
    if (na_prop > na_threshold) {
      # Stop early - return what we have so far
      return(list(
        windows = windows,
        start_times = if (i > 1) starts[seq_len(i - 1)] else integer(0),
        end_times = if (i > 1) starts[seq_len(i - 1)] + window_size - 1 else integer(0),
        na_proportions = na_props,
        stopped_early = TRUE,
        stopped_at = i
      ))
    }

    # Estimate TNA model
    tna_model <- tna::tna(window_data)
    windows[[i]] <- tna_model
    na_props[i] <- na_prop
  }

  list(
    windows = windows,
    start_times = starts,
    end_times = starts + window_size - 1,
    na_proportions = na_props,
    stopped_early = FALSE
  )
}


#' Create Animated GIF of TNA Network Evolution
#'
#' Generates an animated GIF showing how a TNA (Transition Network Analysis)
#' network evolves over time, using rolling windows over sequence data.
#'
#' @param x A wide data.frame where rows are sequences (individuals/cases) and
#'   columns are time points. Cell values should be states (character or factor).
#' @param window_size Number of time points per window. Default is 5.
#' @param step Step size between windows. Use 1 for sliding windows (default),
#'   or set equal to `window_size` for tumbling (non-overlapping) windows.
#' @param fps Frames per second in the output GIF. Default is 2.
#' @param loop Number of times the animation loops. 0 means infinite loop (default).
#' @param na_threshold Maximum proportion of NA values allowed in a window.
#'   If exceeded, animation stops at that point. Default is 0.5.
#' @param output Output file path for the GIF. Default is "tna_animation.gif".
#' @param width Width of the output GIF in pixels. Default is 600.
#' @param height Height of the output GIF in pixels. Default is 600.
#' @param title_template Template for frame titles using sprintf format.
#'   Should contain two `%d` placeholders for start and end time indices.
#'   Default is "Time %d-%d".
#' @param ... Additional arguments passed to [tplot()] for customizing
#'   the network appearance (e.g., layout, node_fill, edge_color).
#'
#' @return Invisibly returns the path to the output GIF file.
#'
#' @details
#' The animation workflow:
#' 1. Generate rolling windows over the input sequence data
#' 2. For each window, estimate a TNA model using `tna::tna()`
#' 3. Render each TNA model as a plot frame using [tplot()]
#' 4. Combine frames into an animated GIF using `gifski::gifski()`
#'
#' The function requires the `gifski` and `tna` packages to be installed.
#'
#' @seealso [tna_windows()] for generating windowed TNA objects without animation,
#'   [tplot()] for the underlying plotting function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample sequence data
#' set.seed(123)
#' states <- c("A", "B", "C")
#' data <- data.frame(matrix(
#'   sample(states, 100 * 15, replace = TRUE),
#'   nrow = 100, ncol = 15
#' ))
#'
#' # Basic animation
#' tna_animate(data, window_size = 5, fps = 2)
#'
#' # Slower animation with larger windows
#' tna_animate(data,
#'   window_size = 10,
#'   step = 2,
#'   fps = 1,
#'   output = "slow_animation.gif"
#' )
#'
#' # Customize appearance
#' tna_animate(data,
#'   window_size = 5,
#'   layout = "circle",
#'   node_fill = "steelblue",
#'   edge_color = "gray40"
#' )
#' }
tna_animate <- function(
    x,
    window_size = 5,
    step = 1,
    fps = 2,
    loop = 0,
    na_threshold = 0.5,
    output = "tna_animation.gif",
    width = 600,
    height = 600,
    title_template = "Time %d-%d",
    ...
) {
  # Check gifski availability
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("Package 'gifski' required for animation. Install with: install.packages('gifski')")
  }

  # Generate windows (this also checks for tna package)
  result <- tna_windows(x, window_size, step, na_threshold)
  n_frames <- length(result$windows)

  if (n_frames == 0) {
    stop("No valid windows generated (NA threshold exceeded at start)")
  }

  if (result$stopped_early) {
    message(sprintf(
      "Note: Animation stopped early at window %d due to NA threshold (%.1f%% > %.1f%%)",
      result$stopped_at,
      result$na_proportions[length(result$na_proportions)] * 100,
      na_threshold * 100
    ))
  }

  # Create temp directory for frames
  tmp_dir <- tempfile("tna_frames_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  frame_files <- character(n_frames)

  # Capture additional arguments for tplot
  extra_args <- list(...)

  # Render each frame
  for (i in seq_len(n_frames)) {
    frame_file <- file.path(tmp_dir, sprintf("frame_%04d.png", i))
    frame_files[i] <- frame_file

    # Generate title for this frame
    title <- sprintf(title_template, result$start_times[i], result$end_times[i])

    # Render frame to PNG
    grDevices::png(frame_file, width = width, height = height, res = 150)
    tryCatch({
      # Call tplot with the TNA model and extra arguments
      tplot_args <- c(
        list(x = result$windows[[i]], title = title),
        extra_args
      )
      do.call(tplot, tplot_args)
    }, finally = {
      grDevices::dev.off()
    })
  }

  # Combine frames into GIF
  gifski::gifski(
    png_files = frame_files,
    gif_file = output,
    width = width,
    height = height,
    delay = 1 / fps,
    loop = loop
  )

  message(sprintf(
    "Animation saved to: %s (%d frames, %.1f seconds)",
    output, n_frames, n_frames / fps
  ))

  invisible(output)
}
