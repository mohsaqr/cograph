#' Human-AI Interaction Coding Sequences
#'
#' Coded sequences of human-AI programming interactions from 34 projects
#' across 429 sessions. Actions are coded at two granularity levels
#' (broad categories and fine-grained codes) and split by actor
#' (human, AI, or both combined). The AI datasets contain 428 sessions. Each row is one session and every column is
#' a time step: the columns are named T1, T2, ... Tn and hold the sequential
#' actions. \code{NA} indicates the session ended before that time step.
#'
#' @format
#' \describe{
#'   \item{coding}{429 x 164 data.frame. Human actions by category
#'     (9 states: Command, Correct, Frustrate, Inquire, Interrupt,
#'     Refine, Request, Specify, Verify).}
#'   \item{coding_detailed}{429 x 164 data.frame. Human actions by
#'     fine-grained code (15 states: Accept, Arguing, Ask, Command,
#'     Context, Correction, Direct, Frustration, Interrupt, Refinement,
#'     Reject, Request, Specification, Thinking, Verification).}
#'   \item{ai_coding}{428 x 138 data.frame. AI actions by category
#'     (8 states: Ask, Delegate, Execute, Explain, Investigate, Plan,
#'     Repair, Report).}
#'   \item{ai_detailed}{428 x 138 data.frame. AI actions by fine-grained
#'     code (18 states: Acknowledge, Apologize, Ask, Comply, Delegate,
#'     Diagnose, Escape, Execute, Explain, Hedge, Investigate, Plan,
#'     Refuse, Report, Retry, Scaffold, Suggest, Warn).}
#'   \item{human_ai}{429 x 287 data.frame. Both actors combined, by
#'     category (17 states).}
#'   \item{human_ai_detailed}{429 x 287 data.frame. Both actors combined,
#'     by fine-grained code (32 states).}
#' }
#'
#' @return Each dataset is a \code{data.frame} with one row per session and
#'   one character column per time step (T1, T2, ... Tn). It has no
#'   identifier columns.
#'
#' @source Human-AI programming interaction study, 34 projects, 429 sessions.
#' @name hai_datasets
#' @aliases coding coding_detailed ai_coding ai_detailed human_ai human_ai_detailed
#'
#' @examples
#' data(coding)
#' str(coding, list.len = 6)
#' dim(coding)
NULL

#' @rdname hai_datasets
"coding"

#' @rdname hai_datasets
"coding_detailed"

#' @rdname hai_datasets
"ai_coding"

#' @rdname hai_datasets
"ai_detailed"

#' @rdname hai_datasets
"human_ai"

#' @rdname hai_datasets
"human_ai_detailed"
