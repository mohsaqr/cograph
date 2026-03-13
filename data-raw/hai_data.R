# Create HAI (Human-AI Interaction) datasets for cograph
#
# Source: tutorials/data.csv — coded human-AI programming interactions
# from 34 projects, 429 sessions.
#
# Datasets (no tna dependency — manual wide-format pivot):
#   coding            — Human actions, broader categories (9 states)
#   coding_detailed   — Human actions, fine-grained codes (15 states)
#   ai_coding         — AI actions, broader categories (8 states)
#   ai_detailed       — AI actions, fine-grained codes (18 states)
#   human_ai          — Both actors, broader categories
#   human_ai_detailed — Both actors, fine-grained codes

d <- read.csv("tutorials/data.csv", stringsAsFactors = FALSE)

# Split by actor
human <- d[d$actor == "Human", ]
ai <- d[d$actor == "AI", ]

# --- Helper: long-to-wide without tna ---
# Groups by project+session_id, orders by id, pivots action column to wide
to_wide <- function(df, action_col) {
  # Create compound session key
  df$session_key <- paste(df$project, df$session_id, sep = "_")

  # Order by session_key then id
  df <- df[order(df$session_key, df$id), ]

  # Assign within-session sequence number
  df$seq <- ave(seq_len(nrow(df)), df$session_key, FUN = seq_along)

  # Pivot to wide
  sessions <- unique(df$session_key)
  max_len <- max(df$seq)

  mat <- matrix(NA_character_, nrow = length(sessions), ncol = max_len)
  rownames(mat) <- sessions
  colnames(mat) <- paste0("T", seq_len(max_len))

  for (i in seq_along(sessions)) {
    rows <- df[df$session_key == sessions[i], ]
    mat[i, rows$seq] <- rows[[action_col]]
  }

  as.data.frame(mat, stringsAsFactors = FALSE)
}

# Create 6 wide datasets
coding <- to_wide(human, "category")
coding_detailed <- to_wide(human, "code")
ai_coding <- to_wide(ai, "category")
ai_detailed <- to_wide(ai, "code")
human_ai <- to_wide(d, "category")
human_ai_detailed <- to_wide(d, "code")

usethis::use_data(coding, coding_detailed, ai_coding, ai_detailed,
                  human_ai, human_ai_detailed, overwrite = TRUE)
