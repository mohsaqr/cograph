# Human-AI Interaction Coding Sequences

Coded sequences of human-AI programming interactions from 34 projects
across 429 sessions. Actions are coded at two granularity levels (broad
categories vs fine-grained codes) and split by actor (Human, AI, or both
combined). Each row is one session and every column is a time step: the
columns are named T1, T2, ... Tn and hold the sequential actions. `NA`
indicates the session ended before that time step.

## Usage

``` r
coding

coding_detailed

ai_coding

ai_detailed

human_ai

human_ai_detailed
```

## Format

- coding:

  429 x 164 data.frame. Human actions by category (9 states: Command,
  Correct, Frustrate, Inquire, Interrupt, Refine, Request, Specify,
  Verify).

- coding_detailed:

  429 x 164 data.frame. Human actions by fine-grained code (15 states:
  Accept, Arguing, Ask, Command, Context, Correction, Direct,
  Frustration, Interrupt, Refinement, Reject, Request, Specification,
  Thinking, Verification).

- ai_coding:

  428 x 138 data.frame. AI actions by category (8 states: Ask, Delegate,
  Execute, Explain, Investigate, Plan, Repair, Report).

- ai_detailed:

  428 x 138 data.frame. AI actions by fine-grained code (18 states:
  Acknowledge, Apologize, Ask, Comply, Delegate, Diagnose, Escape,
  Execute, Explain, Hedge, Investigate, Plan, Refuse, Report, Retry,
  Scaffold, Suggest, Warn).

- human_ai:

  429 x 287 data.frame. Both actors combined, by category (17 states).

- human_ai_detailed:

  429 x 287 data.frame. Both actors combined, by fine-grained code (32
  states).

An object of class `data.frame` with 429 rows and 164 columns.

An object of class `data.frame` with 429 rows and 164 columns.

An object of class `data.frame` with 428 rows and 138 columns.

An object of class `data.frame` with 428 rows and 138 columns.

An object of class `data.frame` with 429 rows and 287 columns.

An object of class `data.frame` with 429 rows and 287 columns.

## Source

Human-AI programming interaction study, 34 projects, 429 sessions.

## Value

A `data.frame` where each row is one session and each column is one time
step. Every column is named T1, T2, ... Tn and holds the action code at
that step, with `NA` indicating the session ended before that time step;
there are no identifier columns. Six variants are provided: `coding`
(human actions by category, 9 states), `coding_detailed` (human actions
by fine-grained code, 15 states), `ai_coding` (AI actions by category, 8
states), `ai_detailed` (AI actions by fine-grained code, 18 states),
`human_ai` (both actors by category, 17 states), and `human_ai_detailed`
(both actors by fine-grained code, 32 states).

## Examples

``` r
data(coding)
str(coding, list.len = 6)
#> 'data.frame':    429 obs. of  164 variables:
#>  $ T1  : chr  "Request" "Command" "Specify" "Specify" ...
#>  $ T2  : chr  "Specify" "Request" "Command" "Command" ...
#>  $ T3  : chr  "Command" "Command" "Specify" "Specify" ...
#>  $ T4  : chr  "Correct" "Interrupt" "Interrupt" "Interrupt" ...
#>  $ T5  : chr  "Specify" "Correct" "Request" "Command" ...
#>  $ T6  : chr  "Frustrate" "Request" "Specify" "Request" ...
#>   [list output truncated]
dim(coding)
#> [1] 429 164
```
