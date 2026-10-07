#' Student Interaction Edge List
#'
#' An edge list of observed interactions between 34 students during
#' collaborative learning sessions. Each row represents one observed
#' interaction between two students. The same pair may appear multiple
#' times, reflecting repeated interactions.
#'
#' @format A data frame with 389 rows and 2 columns:
#' \describe{
#'   \item{from}{Character. Anonymized two-letter student code (e.g., "Ac", "Bd")}
#'   \item{to}{Character. Anonymized two-letter student code (e.g., "Ce", "Df")}
#' }
#'
#' @details
#' The dataset includes self-loops (34 rows where \code{from == to}).
#' These can be removed with \code{subset(student_interactions, from != to)}.
#'
#' The 389 rows contain 226 distinct ordered pairs. Because interactions
#' repeat, the edge list forms a multigraph when loaded into igraph with
#' \code{igraph::graph_from_data_frame()}.
#'
#' @examples
#' as_cograph(student_interactions)
#'
#' @return A data frame with 389 rows and 2 columns:
#' \describe{
#'   \item{from}{Character. Anonymized two-letter student code.}
#'   \item{to}{Character. Anonymized two-letter student code.}
#' }
#' @source Anonymized collaborative learning interaction data.
#' @name student_interactions
"student_interactions"
