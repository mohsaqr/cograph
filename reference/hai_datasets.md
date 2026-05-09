# Human-AI Interaction Coding Sequences

Coded sequences of human-AI programming interactions from 34 projects
across 429 sessions. Actions are coded at two granularity levels (broad
categories vs fine-grained codes) and split by actor (Human, AI, or both
combined). Each row is one session (project + session_id); columns T1,
T2, ... hold the sequential actions. `NA` indicates the session ended
before that time step.

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

A `data.frame` where each row is one session. The first columns identify
the session; the remaining columns (T1, T2, ...) hold the sequential
action codes, with `NA` indicating the session ended before that time
step. Six variants are provided: `coding` (human actions by category, 9
states), `coding_detailed` (human actions by fine-grained code, 15
states), `ai_coding` (AI actions by category, 8 states), `ai_detailed`
(AI actions by fine-grained code, 18 states), `human_ai` (both actors by
category, 17 states), and `human_ai_detailed` (both actors by
fine-grained code, 32 states).

## Examples

``` r
data(coding)
head(coding[, 1:6])
#>                      T1      T2      T3        T4      T5        T6
#> Project_1_S1    Request Specify Command   Correct Specify Frustrate
#> Project_10_S186 Command Request Command Interrupt Correct   Request
#> Project_10_S187 Specify Command Specify Interrupt Request   Specify
#> Project_10_S188 Specify Command Specify Interrupt Command   Request
#> Project_10_S189 Specify Command Specify Interrupt Request   Specify
#> Project_11_S190 Command Inquire Correct   Inquire  Verify   Request
dim(coding)
#> [1] 429 164
```
