#' Learning Regulation Transition Network
#'
#' A synthetic weighted transition network among ten learning regulation
#' states, used in the package examples and the introduction vignette. Each
#' cell holds the weight of the transition from the row state to the column
#' state.
#'
#' @format A 10 x 10 numeric matrix with row and column names \code{Explore},
#'   \code{Plan}, \code{Monitor}, \code{Adapt}, \code{Reflect}, \code{Discuss},
#'   \code{Synthesize}, \code{Evaluate}, \code{Create} and \code{Share}. Thirty
#'   of the 90 off-diagonal cells carry weights between 0.05 and 0.49; the
#'   remaining cells, including the diagonal, are zero.
#'
#' @details The network is synthetic and represents no observed data. It was
#'   generated with \code{set.seed(42)}: 30 off-diagonal cells were drawn at
#'   random and given weights drawn uniformly between 0.05 and 0.5, rounded to
#'   two decimals. Rows are not normalized.
#'
#' @return A 10 x 10 numeric matrix of transition weights with state names as
#'   row and column names.
#'
#' @source Synthetic, generated for the package examples.
#'
#' @examples
#' regulation_net
#' splot(regulation_net, tna_styling = TRUE)
#'
#' @name regulation_net
"regulation_net"
