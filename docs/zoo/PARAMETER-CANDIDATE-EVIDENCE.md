# Parameter candidate implementation evidence

For the complete working approach, batch index, testing workflow, correlation
comparisons and current pause, read the [next-agent guide](NEXT-AGENT-GUIDE.md).
This document retains detailed implementation evidence and qualifications.

The scope is the original 160 rows of `parameter_candidates.csv`. That input
is preserved. `parameter_candidate_status.csv` records progress for every row,
including rows already implemented before this work. Regenerate the ledger
with `python3 docs/zoo/parameter_status.py` after updating the coverage map.
The remaining rows are research tasks, not interchangeable measures inferred
from their Kendall correlation.

## Acceptance standard

Each numerical measure needs a pinned published definition, a numerical
reference independent of the production algorithm, and public API checks.
Record whether a reference is maintained external software, a published
numerical example, or a locally written definition-based oracle. The latter
does not establish parity with the author's software. Likewise, equivalence
to a numerical definition does not establish predictive spreading performance.

For each topology-only method, pin whether the source requires an undirected
graph or retains direction. Document the exact skeleton projection and
whether duplicate edges count once. Test this projection, node labels,
permutation of input order, empty graphs, isolates and normalization
separately from numerical parity.

## Batch51: trust-PageRank

Added `trust_pagerank` / `centrality_trust_pagerank()`.
**63/160 original candidates implemented,97 pending**;191 runtime measures
and166 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Source audit:
[batch51/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch51/SOURCE_AUDIT.md),
which supersedes
[candidate_review40/trust-pagerank.md](../../local_testing_and_equivalence/candidate_review40/trust-pagerank.md)
(verdict BLOCKED) and confirms, extends and in two places corrects
[candidate_review48/trust-pagerank.md](../../local_testing_and_equivalence/candidate_review48/trust-pagerank.md).

### The Zoo cites the wrong paper

Zoo entry2.381 attributes Trust-PageRank to reference [350], Sheng *et al.*
(2020), *Physica A*541:123262,
[DOI10.1016/j.physa.2019.123262](https://doi.org/10.1016/j.physa.2019.123262).
**That paper defines the global-and-local-structure index, which the Zoo
itself lists separately at2.149.** A reader following the reference lands on
a different measure.

The actual source is Sheng, Zhu, Wang, Wang and Hou (2020), "Identifying
Influential Nodes of Complex Networks Based on Trust-Value", *Algorithms*
13(11):280,
[DOI10.3390/a13110280](https://doi.org/10.3390/a13110280), MDPI CC BY, fully
open access, 15pp. The Zoo's §2.381 formulas are equations (2), (5), (6) and
(7) of that paper verbatim — but the Zoo **omits the `s(a,a) = 1` base case
of equation (4) and says nothing about the recursion's domain**, and both are
load-bearing. The mis-citation is recorded on the help page, in the coverage
map's comment, in `NEWS.md` and in the ledger note.

**The original was read.** PDF SHA256
`0b1fdd201fee74c33191fe548f8aa0d9ae05a86f73736a90e8f9c39db6354541`,
**recomputed and matched this session**. Pages5, 6, 7 and10 were rendered to
200dpi (`batch51/png/sheng-05.png`, `-06`, `-07`, `-10`) and **all four
visually inspected**, plus a 500dpi crop of Figure5(a) for the kite's node
labelling. *Access note, reusable for any MDPI paper:*
`mdpi.com/<journal>/<vol>/<iss>/<art>/pdf` returns HTTP403 to a non-browser
client; `res.mdpi.com/d_attachment/...` returns200.

### The definition

```
Rs(i,j) = s(i,j) / sum_{k in N_j} s(j,k)                                  (2)
s(a,b)  = 1                                            a = b
        = (C/(|N_a||N_b|)) sum_{l in N_a} sum_{m in N_b} s(l,m)  a != b   (4)
Rd(i,j) = d_i / sum_{k in N_j} d_k                                        (5)
T(i,j)  = (1 - k) Rs(i,j) + k Rd(i,j)                                     (6)
TPR_i^t = (1-alpha)/n + alpha sum_{j in N_i} T(i,j) TPR_j^{t-1}           (7)
```

Source defaults: `C = 1`, `alpha = 0.85`, `k = 0.85` with domain `[0,1]`.

**Figure3 on page6, read from the page image, decodes every symbol.** It
annotates the two arcs between nodes1 and2 with
`(1-k) x 0.6/1.11 + k x 3/7` and `(1-k) x 0.6/1.59 + k x 2/9`, and each
printed number resolves against a quantity computed here: `0.6 = s(1,2)`,
`1.11 = S_2`, `3 = d_1`, `7 = D_2`, `1.59 = S_1`, `2 = d_2`, `9 = D_1`. That
instantiates equations (2) and (5) and **fixes the `(1-k)`-on-similarity
orientation of equation (6)** — which matters, because the independent
restatement in Hajarathaiah *et al.* (2022), *Entropy*24(5):704,
[DOI10.3390/e24050704](https://doi.org/10.3390/e24050704), also read, prints a
`TV` matrix that is `0.85 SR + 0.15 DR` while its own equation says
`(1-k)SR + k DR`. Sheng's own figure settles it in favour of the equation as
printed.

### The blocker from `candidate_review40` was never an iteration count

It was the **domain** of equation (4). Algorithm1 line4, read from the page
image, is `for connected node v_u, v_v in V do`, and Table3's caption says
"the short dash indicates that there is no edge between the two nodes". The
similarity map therefore holds only the lines plus the diagonal, and **a
non-adjacent pair contributes 0**, not the `0.1` that initialises the lines.

Under that domain the diagonal is the only inhomogeneous term; it reaches a
line exactly through the triangles that line carries; every row sum of the
linear part is at most `1 - p/(d_a d_b)`; and the recursion **contracts on any
block carrying a triangle even at the source's `C = 1`**. Measured spectral
radii this session: `K3` 0.750, `K4` 0.778, `K5` 0.813, bull0.742, kite0.681,
karate0.575, house0.784, Sheng Fig.3 0.719 — and exactly `1.000000` on `K2`,
`P3`, the star, `C4` and `K3,3`, all triangle-free.

**Two sets of numbers in the `candidate_review48` audit are the *rejected*
reading's, and the cause is identified rather than merely reported.** Its
Table3 sweep deviations (0.350, 0.240, 0.162, 0.067, 0.042 at 1/2/3/5/10
sweeps) do not reproduce under the accepted reading, which gives 0.350, 0.252,
0.182, 0.097, 0.022 — but they reproduce **exactly, all five**, under the
non-adjacent-`0.1` reading that the same audit rejects, and `published_audit.R`
computes both lists and asserts the match. Its kite scores
(`7:0.18800388, 4,5:0.14934736, ...`) are the pinned reading's too, agreeing to
the printed digits with `kite_worked.csv`'s `rejected_nonadjacent_pin` column;
the accepted reading gives `0.1883952454` at node7, which is what that audit's
**own retained worked CSV holds** — its prose and its fixture disagreed. Its
`|delta| = 1.4e-17` for the automorphism-forced ties is not what is measured
here either: two of the three pairs are **exactly** equal in double precision.
None of this changes a conclusion, and all of it was caught only by
recomputing.

### The degenerate class, and the decision

**On a component that has lines but no triangle the measure has no value.**
The recursion is homogeneous there, its least nonnegative fixed point is
`s = 0` on every line, and equation (2) is `0/0`. **That class is most of
cograph's standard analytic test set** — every path, tree, star, even cycle
and complete bipartite graph is in it, and so is the Petersen graph — and
**1,394 of the 5,532 retained fixtures fall in it**, against 4,136 inside the
domain and2 empty graphs.

cograph returns **`NA` at every node of such a component**, with a
`cograph_undefined_measure` warning. Two readings were rejected:

- **`Rs(i,j) := 1/d_j`**, the fallback `candidate_review48` recommends.
  Rejected because it is **not** forced by the vanishing numerators the way
  the zeros of **batch48** and **batch50** are. Those quotients are
  determined: every admissible value gives the same answer. Here the ratios
  need only sum to one over `N_j`, nothing in the source chooses between the
  ways of doing that, and a different admissible choice gives a different
  score. Adopting `1/d_j` would silently turn the measure into a degree-ratio
  PageRank over the whole triangle-free class while still calling it
  Trust-PageRank. **This follows batch49**, which returns `NA` rather than the
  finite number its closed form would otherwise print.
- **Starting the recursion at the source's `0.1` rather than at zero.** On the
  sub-class where that start is itself a fixed point (`K2`, `P3`, stars,
  `C4`, complete bipartite graphs — spectral radius exactly1 with the uniform
  vector fixed) it gives `Rs = 1/d_j`, and the constant's *size* cancels in
  the ratio. Rejected because the value is then an artifact of the
  initialisation being *uniform* rather than of the graph; because it leaves
  `P4` and longer, `C5` and longer, trees and Petersen undefined anyway; and
  because separating a frozen start from an exponentially decaying zero needs
  a numerical threshold where cograph can settle the question **structurally**,
  by a boolean closure asking which lines can reach a triangle at all. The
  paper's own Table3 is reproduced only at the fixed point, so the
  initialisation is scaffolding, not a modelling choice.

The `NA` covers the **whole component**, because an undefined trust column
makes equation (7) undefined for everything that solves against it. The
undefined set was checked over 5,417 fixtures to be **exactly** the components
that have lines but no triangle.

**An isolate is not in the class.** It is never a `j` in equation (2), and
equation (7) gives it the bare `(1-alpha)/n` through an empty sum, so a graph
with isolates has scores summing to less than one. **The domain does not move
with `k`**, `k = 1` included: the similarity ratio is part of the trust-value
at every mixing weight, and switching a measure's domain on the knife-edge
value `k = 1` would also mean an `==` on a double.

### Other conventions

- **`T` is column-stochastic on the lines** (measured largest deviation
  `1.110223e-16`), because both ratios are normalised over `N_j` and `s` is
  symmetric. Equation (7) is therefore an ordinary damped PageRank with a
  unique fixed point, the scores sum to one on a graph without isolates, and
  **the iteration count is a convergence tolerance rather than a modelling
  choice**. `tpr_tol` and `tpr_max_iter` are exposed; a recursion still moving
  at the bound raises `cograph_no_converge`.
- **Both convergence tests are relative, and that is a fix to the stopping
  rule rather than a widened tolerance.** The similarities on one graph span
  twenty orders of magnitude — fixture1224 at `C = 0.2` has a largest
  similarity of `2.36e-2` and a smallest positive one of `2.75e-20`, because
  the mass reaching a line decays geometrically with its distance from the
  nearest triangle. An absolute test at `1e-14` stopped after16 sweeps where22
  were needed and produced a scaled error of `1.85e-5` against the direct
  linear solve. With the relative test the same fixture agrees to `2.4e-16`.
  The reference's own iteration needed the same correction and got it.
- `tpr_tol` defaults to `1e-14` and `tpr_max_iter` to1000: the largest sweep
  count over the collection at that setting is203 and no fixture fails to
  converge, while at `1e-16` the relative change reaches the double-precision
  noise floor and some fixtures never settle.
- **The source's claim that `C` does not matter is false for the converged
  recursion.** True for a homogeneous recursion, where `C` is an overall scale
  that cancels in the ratio; the diagonal makes this one affine, so `C` enters
  the resolvent. Measured on the karate club, `C` from1 to0.5 moves `Rs` by up
  to **0.141** and the scores by up to **9.074920e-4**, leaving the top five
  unmoved — defensible about rankings, wrong about results.
- Direction, weights, loops and parallel edges are projected onto the simple
  undirected skeleton the source defines on (page3: an undirected network with
  `a(i,j) = 1`, every quantity a count), silently, as every other
  undirected-domain measure in `centrality()` does. `mode`, `cutoff` and
  `invert_weights` are ignored; `normalized = TRUE` max-scales.
- Empty graph returns no scores; a singleton scores `0.15`; an edgeless graph
  scores `0.15/n` at every node.
- **Costly**: two fixed-point recursions over dense `n x n` matrices, one a
  triple product per sweep, plus a boolean closure. Held back from
  `type = "all"`.

### Published fixtures — both, 46 of 47 printed values

`batch51/published_audit.R`. **The two figure readings are the weak step and
are corroborated before any score is compared.** The seven lines read off
Figure3 imply `D_i = 9, 7, 9, 7, 10`, matching the paper's own printed prose
at all five nodes, and Table3's dash pattern independently implies exactly the
same seven lines. The kite reading is checked against **all five** statistics
Table4 prints for it — `|V| = 10`, `|E| = 18`, `<k> = 3.6`, `k_max = 6`,
`CC = 0.578`, all five reproduced — and is confirmed **isomorphic to
`networkx.krackhardt_kite_graph`**. The `CC` match needs a convention named:
it is igraph's `localaverage`, which excludes degree-one nodes; NetworkX's
`average_clustering` scores those zero and gives 0.520, which is not what
Table4 prints. The karate club is not read off a drawing — it is
`igraph::make_graph("Zachary")` — and four of its five printed statistics
reproduce.

1. **Table3, page6.** Computed `s = 0.5951417, 0.5195682, 0.4730094,
   0.5101215, 0.5951417, 0.4730094, 0.5101215` against printed `0.60, 0.52,
   0.47, 0.51, 0.60, 0.47, 0.51`. **All seven round to the printed two
   decimals** under round-half-away-from-zero, none needs truncation, largest
   `|computed - printed|` `0.0048583`. **The `S_v` column needs a second,
   separate rule, and it is stated rather than fudged**: the printed `S_v` is
   the **sum of the paper's own rounded cells**, not the rounded sum. Node5's
   similarities sum to `1.966262`, which rounds to `1.97`, but the paper
   prints `1.96 = 0.47 + 0.51 + 0.47 + 0.51`. Both readings are computed for
   all five nodes: the rounded sum explains4 of5, the sum of rounded cells
   explains5 of5, and node5 is the one that separates them.
2. **Table5, page10.** The **karate top ten reproduces in order at all ten
   positions**, `1, 34, 33, 3, 2, 32, 4, 14, 9, 24`, with
   `sum_i TPR_i = 1.000000000000`. **A ranking fixture is only as strong as
   its tie handling**, so ties are reported rather than sorted away: the
   smallest gap that actually decides a karate position is `2.95556e-05`,
   between positions8 and9 (nodes14 and9), which are **not** in the same
   automorphism orbit, so that ordering is a real claim. The **kite top ten
   reproduces up to three exact ties** forced by its own automorphism group,
   computed by exhaustive isomorphism search as
   `{1}{2}{3}{4,5}{6,8}{7}{9,10}`: pairs `(4,5)` and `(6,8)` are equal to0 in
   double precision and `(9,10)` to `1.39e-17`, so **6 of the10 positions are
   inside a tie** and4 are separated by a real gap, the smallest `0.00859196`.
   Ties are broken by ascending node label, a deterministic secondary key
   fixed before any comparison, and the paper's printed order within each tied
   pair is that same ascending order.

**One printed value does not reproduce and is named rather than rounded
away.** Table4's karate `<k>` is printed `4.5888` where `2|E|/|V| = 156/34 =
4.588235`. Its own `|V| = 34` and `|E| = 78` both reproduce and no `|E|` gives
`4.5888`, so it is a last-digit transcription slip. It is the only allowed
failure and the audit asserts that it is.

**The two rejected readings are shown to fail the published fixtures rather
than argued away.** Pinning non-adjacent pairs at `0.1` explains **0 of7**
Table3 entries, largest `|pinned - printed|` `0.0557085`, and gets **8 of10**
karate positions, swapping14 and9. Reading Algorithm1 literally as a fixed
sweep count from the `0.1` start explains **0 of7** entries at 1, 2, 3, 5 and
10 sweeps (max abs error 0.350, 0.252, 0.182, 0.097, 0.022) and **7 of7 only
at the fixed point** (0.00486).

### Equivalence

`batch51/run_equivalence.R`: **244,644 comparisons carrying 1,312,874 scalar
values, zero failures**. The 207,630 numerical rows carry 1,275,860 values, of
which **1,108,061 are defined on both sides**, at a maximum scaled error of
`3.463896e-14` against tolerance `1e-11`; the 37,014 invariant rows record a
largest **structural** residual of `6.661338e-16`. The two larger invariant
residuals, `9.074920e-04` and `1.241044e-03`, are
`decay_does_change_the_scores` and `nonadjacent_pin_differs`, which record the
*distance from a rejected reading* on purpose and are reported apart.

**A comparison has two parts that are never mixed.** The `NA` pattern must
agree node for node, and the numbers must agree where both sides have one.
4,136 fixtures are inside the source's domain, 1,394 outside it, 2 empty; the
**20,928 `cograph_undefined_measure` warnings were observed and counted, never
suppressed**.

**The three modelling parameters are crossed over the whole collection at
eight settings** — the source's `alpha = k = 0.85, C = 1`, `alpha = 0.5`,
`alpha = 0.95`, `k = 0` (similarity only), `k = 0.5`, `k = 1` (degree only),
`C = 0.5` and `C = 0.2` — and the two numerical ones are checked by
*tightening* them, `tpr_tol = 1e-15` with `tpr_max_iter = 20000` against the
default, at 5,532 comparisons.

**Three references that fail differently from production and from each
other:**

- `pure_sets` — **pure Python**: the graph as a dict of sets, the similarity
  as a dict keyed by `frozenset`, equation (4) evaluated by an explicit double
  loop over `N_a x N_b` and equation (7) as a power iteration over neighbour
  lists. No NumPy, no NetworkX, **no matrix of any kind**; the domain decided
  by an explicit breadth-first search over the line relation (43,776
  comparisons, `n <= 20`).
- `linsolve` — **NumPy, and not an iteration**: both fixed points built as
  linear systems and handed to `numpy.linalg.solve`, `(I - CM)s = Ct` on the
  **`|E|`-dimensional line space**, a matrix production never forms — it works
  on an `n x n` sandwich instead — and `(I - alpha T)x = (1-alpha)/n 1` on the
  node space, with the domain decided by boolean powers of `M`'s pattern
  (44,256 comparisons, all `n`).
- `exact_fraction` — **exact `fractions.Fraction` arithmetic end to end** with
  a hand-written rational Gauss-Jordan for both systems and no floating point
  anywhere. Being exact, its zero test on the similarity denominator is a
  genuine equality on rationals, so it **decides the domain question without
  any tolerance** (42,504 comparisons, `n <= 7`).

All three take the **raw** fixture matrix and redo the projection themselves,
so a projection bug in production cannot hide. Shared primitive, stated
plainly: all three evaluate the same five published equations, so agreement
confirms the arithmetic and the conventions, not the equations.

**The domain decision, which is cograph's rather than the source's, is checked
three further ways:** production's boolean matrix closure against the
reference set-BFS (5,532) and against a **NetworkX line-relation digraph**
walked by `networkx.descendants` (5,379, `n <= 10`), and the resulting `NA`
mask against an independent component-plus-**exhaustive-triple-enumeration**
computation (5,417, `n <= 12`).

**The parts are checked separately, not only the score:** the degree column
and the neighbour-degree-sum column (5,532 each), the similarity-sum column
(5,472) and the **full `n x n` similarity matrix entry by entry** (5,472).

**Invariants:** `T` column-stochastic, checked inside Python from an
independently computed similarity, largest deviation `1.110223e-16` (5,472);
the scores sum to one on the 3,876 fixtures with no isolate and no undefined
component, largest residual `6.661338e-16`; the similarity symmetric and
nonnegative (5,532 each); every defined score strictly positive (5,532);
`NA`-ness constant on each component (5,532); and both recursions converged at
the default tolerance on all 5,532. Wrapper, `normalized = TRUE`,
node-permutation, input-projection (`weights * 13`, loops, `mode = "in"`,
`cutoff = 1`, `invert_weights`) and an explicit symmetrisation check each ran
at 5,532 comparisons.

**Closed forms and degenerate cases**, independent of every reference route:
`K_n` scores `1/n` at every node, symmetry making `T` the uniform
column-stochastic matrix (10 comparisons, `n = 3..12`); every ring `C4..C9`,
every star `S1..S9` and the **Petersen graph** are undefined, asserted as
full-`NA` vectors rather than as an absence of a value; a `K3` beside a `P4`
splits, the triangle keeping numbers and the path going `NA`; a triangle
beside an **isolate** keeps the isolate at exactly `(1-alpha)/n = 0.0375` and
leaks mass, the total falling below one; the empty graph returns no scores, a
singleton scores `0.15` and an edgeless six-node graph `0.15/6` at every node;
and ten bad parameter settings are each refused.

**122 public-test expectations** passed with zero warnings. The Krackhardt
kite is retained as a named acceptance fixture in `kite_worked.csv`, every
value computed here.

## Batch50: degree and importance of lines

Added `dil` / `centrality_dil()`.
**62/160 original candidates implemented,98 pending**;190 runtime measures
and165 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Source audit:
[batch50/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch50/SOURCE_AUDIT.md),
which supersedes the research agents'
[candidate_review47/dil-degree-importance-of-lines.md](../../local_testing_and_equivalence/candidate_review47/dil-degree-importance-of-lines.md)
and builds on
[candidate_review49/dil-degree-importance-of-lines.md](../../local_testing_and_equivalence/candidate_review49/dil-degree-importance-of-lines.md).

Primary source: Liu, Xiong, Shi, Shi and Wang2016,
[DOI10.1016/j.physa.2016.02.049](https://doi.org/10.1016/j.physa.2016.02.049),
*Physica A*452:209-219, "Evaluating the importance of nodes in complex
networks", PII S0378437116002156, CC-BY-NC-ND full open access: equation (1)
with the definitions of `U`, `p` and `lambda` on journal page210, equations
(2) and (3) on page211, the ARPA fixture in Table3 and Fig.6 on page217, and
the `O(n<k>^2)` "Local information" complexity claim in Table4 on page218.

**The original was read.** `candidate_review49` retrieved it with a real
browser after every scripted route in `candidate_review47` had failed; the
block was an Elsevier/Radware bot wall, not a licence restriction. The PDF is
retained at `candidate_review49/dil_liu2016_physa.pdf`, SHA256
`6964112b658be6c74d409804a125b87e02b0921d895e710863e9fb833cf1422b`,
**recomputed and matched this session**. Pages2, 3 and9 were rendered to
300dpi images this session (`batch50/png/dil-02.png`, `dil-03.png`,
`dil-09.png`) and every equation, every printed value and both figure
topologies were read from those images. The `candidate_review47` requirement
to say the original had not been read is **retired**.

### The definition

```
p          = number of triangles one of whose edges is e_mn
           = |N(m) intersect N(n)| in a simple graph
U          = (k_m - p - 1)(k_n - p - 1)
lambda     = p/2 + 1
I_emn      = U / lambda                                              (1)
W_vivj     = I_eij (k_i - 1) / (k_i + k_j - 2)                       (2)
L_vi       = k_i + sum_{vj in Gamma_i} W_vivj                        (3)
```

**The `lambda` trap is real in the original.** `pdftotext` renders that
stacked fraction as `lambda = 2p + 1`. The page image shows `p` over `2`, and
the paper's own worked example prints `lambda = 1/2 + 1 = 1.5` at `p = 1`,
giving `I_e45 = 8/3`. Recomputed here, the wrong reading returns `4/3` on that
fixture, not the `3` the `candidate_review49` note asserted: `U = 4` does not
depend on `lambda`. The fixture catches the error under either arithmetic.

**No divergence anywhere.** Zoo entry2.62 transcribes the three equations
correctly, and the original agrees symbol for symbol with the Almasi and Hu
(2019) reproduction, *PLoS ONE*14(3):e0205936 equations (7)-(9) page10, that
`candidate_review47` had had to lean on. Only symbol names differ
(`t`->`p`, `C`->`W`, `DIL`->`L`).

### Conventions

- **Domain.** The authors state it themselves on page210: "a network
  `G = (V, E)` is an undirected and unweighted network". Directed, weighted
  and multigraph input is **projected** onto the simple undirected skeleton
  rather than refused, and the projection is silent. That is the convention
  every other undirected-domain measure in `centrality()` already follows;
  warning on each would be noisy and inconsistent with its twenty-odd
  siblings. The projection is documented on the help page and asserted in
  the tests and in the equivalence run.
- **The isolated `K2` `0/0`, and why it is not `NA`.** `k_i + k_j - 2`
  vanishes only when both endpoints have degree one, since every endpoint of
  a line has degree at least one. There `p = 0` and
  `U = (1-0-1)(1-0-1) = 0`, so the importance being split is **exactly
  zero** while the split of it is `0/0`. Because `W` is a *share* of `I` and
  the two shares sum to one wherever they are defined, every admissible
  split of an exactly zero importance gives an exactly zero contribution:
  the answer does not depend on resolving the indeterminacy. The share is
  written as zero **with the test taken before the division**, and both
  nodes of a `K2` score1. The source says nothing about the case. This
  follows the **batch48** precedent, where `TNTS = 0` makes `TP` a `0/0` on
  every triangle-free graph and is likewise written as zero because the
  denominator vanishes exactly where every numerator does. It deliberately
  does **not** follow **batch49**, which returns `NA` on reducible input:
  there the closed form returns a finite wrong number where the truth is
  infinite, so a value would be wrong; here every candidate value is the
  same value.
- **`U` is never negative**, since `j` lies in `N(i)` but in neither `N(j)`
  nor the intersection, so `p <= k_i - 1`. Hence `I >= 0`, `W >= 0` and no
  score falls below its node's degree.
- **Conservation.** The two endpoint shares sum to one, so
  `sum_i (L_i - k_i) = sum_e I_e`.
- Isolates score0, a singleton and an edgeless graph score0 everywhere, a
  disconnected graph needs no rule, and an empty graph returns no scores.
  Raw scores are **component-local**, the measure never reaching past a
  node's second neighbours. `normalized = TRUE` max-scales, the source
  stating no normalization.
- **Not costly**, on the source's own `O(n<k>^2)` claim; it stays in
  `type = "all"`. No new parameter is added to `centrality()`.

### Published fixtures — all three, 29 printed values

`batch50/published_audit.R`. `source_graph_expected.csv` holds only numbers
printed in the paper; `source_graph_edges.csv` holds the four topologies read
off the figure images. Reading an edge list off a scan is the weak step, so
the ARPA reading is **corroborated independently before any score is
compared**: the 26 lines read off Fig.6 imply degrees matching Table3's own
DC column at **all 21 nodes**, which is a fatal precondition of the audit.

1. **Fig.1, page210.** `I_e45 = 9/1` at `p = 0` and `8/3` at `p = 1`, both
   **exact fraction matches** against the printed fractions.
2. **Fig.2, page211.** `I_e12 = 8`, `I_e227 = 0`, `I_e45 = 2`, `I_e56 = 4`
   all match, and `L_v2 = 26/9` and `L_v5 = 52/15` are **exact fraction
   matches**, with `L_v5 > L_v2` as the paper states.
3. **Table3, page217.** All 21 printed values reproduce. **The paper's
   rounding is named, not absorbed into a tolerance**: both sides are exact
   multiples of `1e-4` and each value must match under round-half-away-from-
   zero or truncation at four decimals. **All 21 match under rounding, 0
   need truncation, 0 match neither**; largest `|computed - printed|` is
   `3.3333333e-05`, entirely the paper's own printing of `23/3` as `7.6667`,
   `41/12` as `3.4167`, `89/15` as `5.9333` and `19/6` as `3.1667`.
   **Table3's printed row order is also reproduced** as the descending order
   of the computed values. The graph contains the triangle `(1, 2, 15)`,
   found by exhaustive triple enumeration, so the fixture exercises `p >= 1`.

**One inconsistency inside the paper is preserved, not fixed.** The prose on
page216 calls the ARPA network "twenty-one nodes and twenty-three lines",
where Table3's DC column sums to52 (26 lines) and Fig.6 draws26. The DC
column and the figure are followed; trimming the edge list to23 would break
the degree check.

### Equivalence

`batch50/run_equivalence.R`: **126,967 comparisons carrying 760,577 scalar
values, zero failures**. The 99,304 numerical rows carry 732,914 values at a
maximum scaled error of `5.703394e-16` against tolerance `1e-11`; the 27,663
invariant rows record a largest **structural** residual of `2.728484e-12`.
The one larger invariant residual, `0.1666667`, is `lambda_is_not_2p_plus_1`,
which records the *distance from the wrong reading* on purpose and is
reported apart rather than folded into a headline. All 5,532 retained
fixtures are inside the domain — the measure has no undefined input — and no
score is ever `NA` or non-finite.

**Four references, three of which share no data structure with production:**

- `exact_sets` — exact `fractions.Fraction` arithmetic end to end with no
  floating point anywhere, the graph held as a dict of **Python sets**, `p`
  from `len(nbr[i] & nbr[j])`, equation (3) accumulated by explicit loops. No
  NumPy, no NetworkX, no matrix of any kind (5,532 comparisons).
- `definitional` — NetworkX float64: degrees from `G.degree`, `p` from
  `networkx.common_neighbors`, equations (2)-(3) accumulated one incident
  line at a time in a Python loop over `G[i]`, again with no matrix product
  (5,532).
- `matrix_np` — dense float64 NumPy reading `p` off `A@A` masked by `A`.
  **This route does share production's matrix-product triangle census and
  its whole-matrix share expression, is named as such, and is not counted as
  independent** (5,532).
- `exact_triples` — exact `Fraction` again, but `p` by **exhaustive
  enumeration of every node triple** rather than by set algebra or a matrix
  power, on the `n <= 20` part (5,472).

All four take the **raw** fixture matrix and do their own projection, so a
projection bug in production cannot hide.

**The parts are checked separately, not only the score:** the degree column,
the summed line importance and the summed contribution against set-based
references (5,532 each), plus the full per-edge `p`, `I` and `W` matrices
entry by entry on `n <= 20` (5,472 each).

**Invariants at 5,532 each:** `U >= 0` on every line, verified independently
by checking `p <= k - 1` at both endpoints; every score at least the node's
degree; `I` symmetric while `W` is not; and the conservation identity
`sum_i (L_i - k_i) = sum_e I_e`, which fails if either the share or the `K2`
branch is wrong. **Component locality is asserted in both directions** on the
5,489 fixtures with `n <= 30`: appending a disjoint triangle-with-pendant
leaves every existing score unmoved and the appended block scores exactly
what it scores alone — unlike batch48's Lhc, which is not component-local.
Wrapper, `normalized = TRUE`, node-permutation, input-projection
(`weights * 13`, loops, `mode = "in"`, `cutoff = 1`, `invert_weights`) and an
explicit symmetrisation check each ran at 5,532 comparisons.

**Seven hand-derived closed forms**, independent of every reference route:
`K_n` scores `n - 1` at every node because `p = n - 2` makes every `U`
vanish; a star scores its degree at every node for the same reason, every
line touching a leaf; a triangle-free `k`-regular graph scores
`k + k(k-1)^2/2`, so a ring scores3 and the **Petersen graph** scores9; the
path `P_n` scores `1, 2.5, 3, ..., 3, 2.5, 1`; and `K_{a,b}` scores
`b + b(a-1)(b-1)^2/(a+b-2)` on its `a`-side. **The `C_3` boundary is asserted
rather than assumed**, as in batch48: `C_3` is `K_3`, is not triangle-free,
and follows the complete-graph form2 and not the ring form3.

**The `K2` `0/0` is tested directly** — an isolated `K2`, a `K2` beside an
isolate, and two disjoint `K2`s: every score is finite, never `NA`, and the
kernel's share matrix is a clean zero, so the `0/0` is never evaluated.

**178 public-test expectations** passed with zero warnings. The Krackhardt
kite is retained as a named acceptance fixture in `kite_worked.csv`, every
value computed here.

## Batch49: immediate effects centrality

Added `iec` / `centrality_iec()`.
**61/160 original candidates implemented,99 pending**;189 runtime measures
and164 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch49/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch49/SOURCE_AUDIT.md),
building on the prior research agent's
[candidate_review47/immediate-effects-centrality.md](../../local_testing_and_equivalence/candidate_review47/immediate-effects-centrality.md).

Primary source: Friedkin1991,
[DOI10.1086/229694](https://doi.org/10.1086/229694), *American Journal of
Sociology*96(6):1478-1504, "Theoretical Foundations for Centrality Measures":
equation (9) on journal page1485, equation (11) on page1486, equation (20) on
page1489, the construction of **W** stated twice on page1494, footnote9 on
page1483 and footnote10 on page1484, and Table1 on pages1492-1494.

**The original full text was read, clearing a block that had stood since
`candidate_review32`.** The paper is open access at the author's UC
eScholarship deposit, retained as
`batch49/iec_friedkin1991_ajs_escholarship.pdf`, SHA256
`25913685fa28569842dc5ccf564665b45f796c4ad65a7bf2346973f0e5472cf9`,
**recomputed and matched this session**. The deposit is a **600 dpi CCITT G4
scan with no text layer** — `pdftotext` yields only the eScholarship cover
sheet — so there is no text dump to fall back on and **every equation below
was read visually from a rendered page image**. Pages1484, 1485, 1486, 1489,
1492, 1493 and1494 were each inspected in full by the implementing session;
the renders are retained under `batch49/png/`, and the page-number map is
journal page = image index +1476.

Definition:

```
a_ii     = 1  for every i;      W = A / rowSums(A)              p. 1494
c        = left eigenvector of W at eigenvalue 1, sum 1         eq. (9)
Z        = (I - W + 1 c')^-1
M        = (I - Z + E Z_dg) diag(1/c)                           eq. (11)
c_IEC(j) = (n - 1) / sum_{i != j} m_ij                          eq. (20)
```

**M** is the Kemeny-Snell mean first passage matrix in the convention
`m_ij = E[steps from i until j is first reached]`, so equation (20) sums
*down* column `j` and the score is a reciprocal mean first passage time
**into** `j`. Zoo2.180 transcribes the source faithfully, renaming `c` to
`v`; no discrepancy with the Zoo was found for this row. The sibling labels
"Total effects centrality (TEC)" (equation18) and "Mediative Effects
Centrality (MEC)" (equations21-23) come from the same paper and are **not**
covered by this batch; the MEC pages were not read and the ledger keeps that
row pending with the reason stated.

### The self-loop is load-bearing, and this is not `markov`

Page1494 states the construction twice, in the body and in the note to
Table1: "the diagonal entries of its adjacency matrix `A = [a_ij]` were set
to one and its influence network was computed as
`W = [w_ij] = a_ij / sum_j^n a_ij`", following French (1956). Footnote10 on
page1484 gives the reason — "Given `w_ii > 0` for any `i`, all strong
networks must be regular", regular meaning aperiodic — and footnote9 on
page1483 the periodic counterexample a zero diagonal admits.

**The candidate ledger's long-standing assumption that cograph's existing
`markov` is IEC up to an `N` versus `N - 1` numerator is wrong.** `markov`
*also* omits the self-loop. The numerator difference is a constant factor
`n/(n-1)` and cannot reorder anything; the missing self-loop can and does.
Measured this session:

* largest within-graph spread of `iec / markov` over the21 connected
  five-node graphs: **0.096** (a rescaling would give0);
* the two **rank the nodes differently on2 of those21** graphs;
* Krackhardt kite, `iec`: `0.069051322 0.069051322 0.052899126 0.103448276
  0.052899126 0.099984987 0.099984987 0.055384615 0.018730489 0.010291595`;
  the same graph, `markov`: `0.102457598 0.102457598 0.077345179 0.158102767
  0.077345179 0.149691514 0.149691514 0.077669903 0.025856496 0.014270425`.

`markov` was **not** changed and **not** remapped — other tests and documented
results depend on its behaviour — and `iec` ships as a separate measure with
each help page pointing at the other. A recommendation that `markov`'s own
one-line description be expanded, since it is the sentence that let the
ledger make the mistake, is recorded in the source audit and deliberately
**not** acted on here.

### Reducible input is refused, departing from batch47 on purpose

Batch47 evaluated `rsp_betweenness` masked by reachability because *its*
source states a zero rule for an unreachable pair. **This source states
none.** Three things go wrong at once on a reducible chain: the
eigenvalue-one eigenspace of **W** has one dimension per closed class so `c`
is undetermined; `diag(1/c)` is undefined wherever `c` vanishes; and, the
decisive point, **the closed form does not announce either failure** — with
**Z** block diagonal a cross-class pair has `z_ij = 0` and equation (11)
returns the entirely finite `m_ij = z_jj / c_j` where the true mean first
passage time is infinite.

A finite wrong number is the one outcome that must not be produced, so
cograph tests irreducibility by **boolean closure before any solve** and
returns `NA` at every node with a `cograph_undefined_measure` warning — never
a silent zero. In practice the requirement is a connected undirected graph or
a **strongly** connected digraph, the mandated self-loops settling
aperiodicity for free. A per-component reading was considered and rejected:
it would have to invent whether the `n - 1` of equation (20) counts the
component or the network. `n = 1` is `NA` for the separate reason that
equation (20) divides by `n - 1 = 0`; an empty graph returns no scores
silently; an isolate has no case of its own, because a graph containing one is
reducible. The in-package precedent agrees: `calculate_markov()` already
returns `NA` with a warning on disconnected input.

Direction is kept, because **W** is directed influence by construction and row
`i` is what actor `i` attends to; there is no in/out/all variant, so the
measure sits in `.cg_no_mode_measures()` and `mode`, `cutoff` and
`invert_weights` are ignored. Weights are dropped **deliberately**: `a_ii = 1`
is calibrated against `a_ij = 1`, so rescaling the weights would silently
re-weight each actor's self-reliance against the network, and the source
demonstrates only the binary case. Loops in the input are absorbed by the
mandated diagonal, parallel edges collapse, `normalized = TRUE` max-scales,
and the measure is marked **costly** (one boolean closure plus two dense
`n x n` solves) and is therefore held back from `type = "all"`.

### The published table: 105 of 105, with the rounding handled explicitly

Table1, pages1492-1494, prints TEC, IEC and MEC to three decimals for every
node of all21 connected non-isomorphic five-node graphs, described on
page1494 as "all the connected networks from the population of nonisomorphic
symmetric networks with five points". **This is the only candidate in the
cohort so far with a complete published table.**
`batch49/published_audit.R` reproduces it. Real output of this session's run:

* The paper labels each network's points "counterclockwise with first points
  at 12:00" over a small line drawing, which is not readable at 130dpi, so
  **the labelling is recovered rather than read**. All 728 labelled connected
  five-node graphs are enumerated from every subset of the ten possible
  edges and reduced to exactly **21 isomorphism classes**; a printed row
  counts as reproduced only when some relabelling of some class reproduces
  **both** its TEC column **and** its IEC column, node by node. All21 rows
  matched **exactly one** class each, none was ambiguous and none unmatched,
  and the21 matches are a **bijection** onto the21 classes. The recovered
  edge lists are written to `source_graph_edges.csv` and are a *result* of
  the audit, not an input to it.
* **The paper's rounding is handled explicitly rather than by loosening a
  tolerance.** Both sides are exact multiples of0.001, so the primary claim
  carries **no numerical tolerance at all**: each value must match under one
  of exactly two stated printing rules, round-half-away-from-zero at three
  decimals or truncation at three decimals.
* **IEC: 103 of105 values match under rounding,2 under truncation,0 under
  neither.** The two are network16 nodes2 and5, where the exact value is
  `0.1875` — an exact halfway case — printed as `.187`. Largest
  `|computed - printed|` is `5e-04` overall and `0.00049019608` among the
  rounding-rule values.
* **TEC is audited as a second reading of the same table and also reproduces
  105 of105**: 102 under rounding and3 under truncation, the three being
  `0.2105263` printed as `.210` at network17 nodes1-3. Largest
  `|computed - printed|` `0.00052631579`. (The prior research audit had
  reported102/105 for TEC "within5e-4"; the difference is that the two
  printing rules account for the remaining three exactly.)
* On the same21 graphs the public wrapper agrees with the kernel to **0** and
  the exact `Fraction` reference to **1.9428903e-16**.
* Three hand-derived closed forms land on printed cells independently of
  every reference route: `K_5` against `1/n` to **0** (printed `.200`,
  network21), `C_5` against `4/(q(q+1))` to **2.7755576e-17** (printed
  `.133`, network8), and the star against `1/2` and `m/((3m-2)(m+1))` to
  **4.1633363e-17** (printed `.500` and `.080`, network1).

### Equivalence run

`batch49/run_equivalence.R`: **65,499 comparisons carrying 245,582 values,
zero failures.** The 35,829 numerical rows carry 215,912 values at a maximum
scaled error of **8.136369e-15** against tolerance **1e-11**; the 29,670
invariant rows record a largest residual of **1.705303e-13**.

Of the5,532 retained fixtures, **2,511 lie inside the source's domain**
(irreducible, `n >= 2`) and 3,021 outside it. **All5,532 have their
classification checked** against `networkx.is_strongly_connected`, and every
one of the3,021 is separately checked to be `NA` rather than a number, with
the **6,043** `cograph_undefined_measure` warnings observed and counted rather
than suppressed.

**Four references** that fail differently from production and from each other:

* `closed_form` — equation (11) in float64 NumPy with the stationary vector
  from an **eigendecomposition** (`scipy.linalg.eig` on `W'`) and `Z` from an
  explicit `numpy.linalg.inv`, where production solves one nonsingular linear
  system for `c` and uses `solve` against an identity for `Z` (2,511
  comparisons).
* `first_step` — float64 NumPy that **never forms `c`, `Z`, `W^inf` or
  equation (11) at all**, taking the mean first passage times from Kemeny and
  Snell's first-step characterisation `m_ij = 1 + sum_{k != j} w_ik m_kj` as
  one `(n-1) x (n-1)` system per target. A structurally different definition
  of the same matrix; the only primitive it shares with production is "solve a
  dense linear system" (2,511).
* `exact` — **exact `fractions.Fraction` arithmetic end to end with no
  floating point anywhere**, the first-step systems solved by a hand-written
  Gauss-Jordan elimination over the rationals with exact pivoting, on the
  `n <= 8` part of the collection (2,420).
* `highprec` — **mpmath at 60 decimal digits** with mpmath's own LU solve and
  matrix inverse, on `n <= 12` (2,459), and again on the **12 largest strongly
  connected fixtures**, `n = 40` to50 (12 comparisons, largest scaled error
  **4.163336e-17**). This is the conditioning audit the measure needs, since
  it inverts a matrix built from a Perron vector; the float64 closed form
  loses nothing measurable on these graphs.

**The parts are checked separately, not only the score.** The stationary
vector against the SciPy eigendecomposition (2,511), against the **Markov
chain tree theorem** as the exact rational cofactor `det(L_ii)` of `I - W`
(2,420 on `n <= 8`), and against the closed form
`(d_i + 1)/sum_k (d_k + 1)` on the 890 symmetric fixtures; the full mean first
passage matrix **M** against the first-step route entry by entry on `n <= 20`
(2,477); and the recurrence and column-sum columns against their definitions
(2,511 each).

**Invariants at2,511 each:** the first-step identity evaluated inside Python
from production's own **M**; **Kemeny's constant** (`sum_j c_j m_ij`
independent of `i`); `m_ii = 1/c_i`; the stationary vector summing to one; and
every score strictly positive. Wrapper, `normalized = TRUE`, node-permutation
and input-projection (`weights * 13`, loops, `mode = "in"`, `cutoff = 1`,
`invert_weights`) checks ran at2,511 comparisons apiece.

**Four hand-derived closed forms**, three of them landing on printed Table1
cells: `K_q` scores `1/q` at every node (10 comparisons, `q = 3..12`); the
ring `C_q` scores `4/(q(q+1))`, derived from the lazy-cycle mean first passage
`(3/2) d(q-d)` (10, with `C_3 = K_3` checked to agree, so unlike batch48 the
small ring is not a separate case); the star `K_{1,m}` scores `1/2` at the
centre and `m/((3m-2)(m+1))` at a leaf, derived by solving the two-state
lumped first-step system by hand (8, `m = 2..9`); and **the directed
`q`-cycle** scores `1/q` (10, `q = 3..12`), the one closed form exercising
the directed path through the kernel.

**Degenerate and out-of-domain inputs:** an empty graph returns no scores
silently; a singleton is `NA` and warns; an edgeless six-node graph, a
two-component graph, the Krackhardt kite plus one isolate, and a **weakly but
not strongly connected digraph** are each all-`NA`; the two-component and
singleton cases are checked to raise the `cograph_undefined_measure` class.
The kite is retained as a named acceptance fixture in `kite_worked.csv`, every
value computed here.

**88 public-test expectations** passed with **zero warnings**. The broad
`^centrality|^kernels|coverage-centrality` run is reported in the batch's
handoff entry. Catalogue count **189**.

## Batch48: the Lhc index

Added `lhc` / `centrality_lhc()`.
**60/160 original candidates implemented,100 pending**;188 runtime measures
and163 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch48/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch48/SOURCE_AUDIT.md),
alongside the prior research agent's
[batch48/lhc-index.md](../../local_testing_and_equivalence/batch48/lhc-index.md).

Primary source: Wang, Yang, Liu and Ma2021,
[DOI10.1371/journal.pone.0251208](https://doi.org/10.1371/journal.pone.0251208),
PLoS ONE16(5):e0251208, equation (1) and its full symbol list on journal
page3, equation (2), the `d = 2` statement and Algorithm1 on page4, and the
`d` sweep on page7. **The article is fully open access (CC-BY) and the
publisher PDF was read**, SHA256
`e16c1c4ef492748a1bfcbf414e66dd1693b3ac46e99b7251b7f62b5545332bd2`,
**recomputed this session**. Pages3 and4 were rendered and **visually
inspected**; page3 was additionally re-rendered at 400dpi and cropped around
equation (1) and around the `TNTS` sentence, because `pdftotext` mangles the
stacked fraction `1/3 * TNTS` into `13 * TNTS` and a transcription error there
moves every score by up to 25 %.

Definition as pinned:

```
   C(v)   = sum over u with 1 <= dist(u,v) <= d  of  k_u (1 + TP(u)) / dist(u,v)^2
   Lhc(v) = sum over w in the OPEN neighbourhood tau(v)  of  C(w)
   TP(u)  = NTS(u) / TNTS,   TNTS = sum_u NTS(u) = 3 * (number of triangles)
   default d = 2
```

**The denominator is `TNTS`, and the paper settles it rather than the Zoo.**
Immediately after defining `TNTS` the source writes that "the total number of
triangle structure exists in the network are `1/3 * TNTS`", so `TNTS = 3 Delta`
and the share sums to exactly one over the nodes. Zoo 2.221 (comparison-matrix
label "Lhc method", encyclopedia heading "Lhc index") transcribes the structure
of both equations correctly but names the denominator "`Delta`, the total number
of triangular structures in the network", which read literally is three times too
small. On the Krackhardt kite the paper's reading scores node1 at
`3305/33 = 100.15152` where the Zoo's literal wording gives
`1380/11 = 125.45455`. Kendall tau between the two is 1.0 on that particular
graph, but the two are not related by a monotone transform in general. cograph
follows the paper; both readings are recorded in `batch48/kite_worked.csv` only
to show the size of the divergence.

**`lhc_radius` is the source's own parameter, exposed with the source's
default.** The paper writes it `d`, sets it to 2 on page4, and sweeps it in
section3 over eleven real networks, reporting "the optimal value of `d` is about
2-3" and stability beyond 3. At `lhc_radius = 1` the ball collapses to the
neighbours; at or above the graph's radius the ball has saturated. The domain is
a whole number of at least one.

**Cograph decisions the source does not make**, each documented on the help page,
in the vignette, in `NEWS.md` and in the ledger note:

- **Triangle-free graphs.** `TNTS = 0` on every tree, star, path, even cycle and
  bipartite graph, making `TP(u)` a `0/0` at every node. Because `TNTS` is a sum
  of nonnegative counts it vanishes exactly when *every* numerator does, so there
  is no share to distribute: `TP` is written as **zero** and the index reduces to
  the pure degree-over-squared-distance sum. The test is made before any
  division, so no `0/0` is evaluated. This is an explicit, tested and documented
  branch, not a silent zero; `NA` or an error would refuse every tree.
- **Non-locality.** `TNTS` is a global sum, so attaching a disconnected component
  that carries a triangle rescales every score, while attaching a triangle-free
  one -- an isolate included -- changes nothing. Both halves are asserted.
- **Isolates, singleton, edgeless, empty.** An isolate scores a derived zero
  because `tau(v)` is empty and equation (2) is an empty sum; singleton and
  edgeless graphs score zero everywhere; empty graphs return no scores.
- Simple undirected unweighted skeleton; `mode`, `cutoff` and `invert_weights`
  ignored; `normalized = TRUE` max-scales. Not marked costly: one all-pairs
  shortest-path solve and one `A.A` product, the same order as `closeness`.

Numerical acceptance, all produced this session:

- **436,150 comparisons /1,680,683 scalar values, zero failures, maximum scaled
  error `9.632896e-16`** at tolerance `1e-11`, over all5,532 retained fixtures
  crossed with five `lhc_radius` settings (1, 2, 3, 5 and 60).
- **Three references, sharing no more than the notion of a hop distance with
  production.** `vectorised` reads triangles off the diagonal of the integer cube
  `A@A@A`, takes distances from `scipy.sparse.csgraph.shortest_path` and
  assembles by matrix-vector products (27,660 comparisons); `definitional` uses
  **no matrix of any kind**, taking triangles from `networkx.triangles`,
  distances from a NetworkX BFS per source and accumulating equations (1) and (2)
  by explicit loops over `Phi(v)` and `tau(v)` (27,660); `exact` works in
  **exact `Fraction` arithmetic with no floating point anywhere** and counts
  triangles by **exhaustive enumeration of every node triple**, on the `n <= 8`
  part of the collection (26,700). Production instead masks `A@A` with `A` and
  uses cograph's own Dijkstra.
- The kernel's intermediate columns were checked separately against the
  definitional route: degree, `NTS`, `TP` and `C` at 27,660 comparisons each,
  plus `TNTS` and the distinct-triangle count at 27,660 each. Wrapper,
  `normalized = TRUE`, node-permutation and input-projection checks ran at every
  setting, 27,660 apiece.
- **Invariants at5,532 each**: the share sums to one; `TNTS = 3 Delta` against an
  independent unordered-triple enumeration; the three triangle-count routes
  agreeing node by node; isolates scoring zero; isolate padding leaving every
  score untouched; radius monotonicity; the triangle-free collapse; and
  saturation beyond the eccentricity. The radius-1 collapse to
  `sum_{u in N(v)} k_u (1 + TP(u))` was checked separately on all5,532.
- **Four hand-derived closed forms replace the missing published table.** A star
  with `m` leaves scores `m(m + (m-1)/4)` at the centre and `m` at each leaf for
  radius >=2, and `m^2` and `m` at radius1 (24 comparisons); `K_n` scores
  `(n-1)^3 (n+1) / n` at every node for every radius, 76.8 on `K_5` (21); a
  triangle-free ring `C_n` with `n > 2 * radius` scores
  `2 * sum_{d=1..radius} 4 / d^2` (18); and the path1-2-3 scores 2, 4.5, 2 at
  radius2 (3).
- **One real failure was found and fixed structurally rather than by widening a
  tolerance.** The first full sweep applied the ring closed form at `n = 3` and
  failed at scaled error `0.3333333`. `C_3` is `K_3`: it is **not** triangle-free,
  its `TP` term does not vanish, and it follows the complete-graph form `32/3`
  rather than the ring form `8`. Production was right; the harness precondition
  was wrong, and it was corrected in both `reference.py` and the public test,
  where the boundary is now asserted explicitly.
- **There is no `published_audit.R` for this batch, and that is stated rather
  than worked around**: the paper prints no toy graph and no per-node score
  table. Table1 lists network statistics for its eleven benchmarks and figures1-7
  are aggregate SIR, CCDF and Kendall plots.
- Eight out-of-domain `lhc_radius` values (0, -1, 0.5, 2.5, `NA`, `Inf`, a
  length-two vector and a string) all raise `cograph_bad_parameter`, and integer
  storage `2L` gives the double `2` answer.
- **166 public-test expectations** passed with zero warnings.

Limitations: no author software exists to compare against -- the paper offers
pseudocode but names no repository, and the Zoo lists implementations for this
entry as "TBA" -- so **no author-parity claim is made**. The paper's SIR
benchmark and its eleven real networks were not reproduced; nothing above depends
on them. Equivalence to the printed definition is not evidence of the spreading
performance the paper reports.

## Batch47: randomized shortest paths (RSP) betweenness

Added `rsp_betweenness` / `centrality_rsp_betweenness()`.
**59/160 original candidates implemented,101 pending**;187 runtime measures
and162 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch47/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch47/SOURCE_AUDIT.md),
alongside the prior research agent's
[batch47/rsp-betweenness.md](../../local_testing_and_equivalence/batch47/rsp-betweenness.md).

Primary source: Kivimaki, Lebichot, Saramaki and Saerens2016,
[DOI10.1038/srep19668](https://doi.org/10.1038/srep19668), Scientific
Reports6:19668, equations (6) and (8) on journal pages5-6, equations (14)
and (15) and Algorithm1 on pages6-7, and the `beta -> 0+` limit on page9.
**The article is fully open access and the publisher PDF was read**, SHA256
`c8a93d9ad683852794132efb2ebbdbae932c8eef377955bdab86bb26816801f7`,
**recomputed this session**; pages5, 6, 7 and9 were rendered and **visually
read by the implementing session**, not taken from the prior audit's
transcription, for the equations, Algorithm1's Input block and the limit
claim. The Zoo's transcription of2.324 reproduces the source without
alteration. The sibling Zoo label2.325 "RSP net betweenness" is equation
(16) of the same paper and is **not** covered by this row.

**The published closed form is defined only on a strongly connected graph,
and the source says what to do otherwise.** Equation (15) divides by every
entry of `Z` and Algorithm1's Input demands strong connectivity, but the
text below equation (9) settles the general case: the derivation "holds only
if there exists a path from `s` to `t`. Otherwise, naturally,
`eta_ij(s, t) = 0`." cograph applies that rule to the **whole term** of an
unreachable pair and evaluates equation (15) masked by reachability. On a
strongly connected graph the mask is vacuous and the expression **is**
equation (15), which the public tests assert against equation (15) written
out verbatim with no mask anywhere. The mask must reach **both** halves of
the term, since both come from the same equation (14) expression;
`NetworkToolbox::rspbc()` masks only the reciprocal and leaves
`n Diag(Zdiv)` counting every source, which is the sole reason the two part
company on a disconnected graph while agreeing exactly on a strongly
connected one.

The consequence is that scores are **component-local**: two disjoint
triangles score exactly what one triangle scores, and adding a disconnected
component — an isolate included — leaves every existing score untouched.

**A zero out-degree gives a derived zero, not an imputed one.** `D^-1` is
undefined there, so that row of `P^ref` is written as zero, which is the
paper's own killed random walk read at a node where the walker dies at once;
`Z` then has `z_ii = 1` and the arithmetic gives exactly `1 - 1 = 0`.
`NetworkToolbox::rspbc()` raises an error on such input and cograph's own
`current_flow_betweenness` returns `NA` on disconnected input; neither was
copied, because for current flow the Laplacian pseudo-inverse genuinely has
no answer whereas here the source states one.

**A numerical finding, recorded rather than tuned away.** Reachability is
taken structurally, by repeated boolean squaring, not from `z > 0`; and `Z`
is then set to exactly zero at unreachable pairs. A LAPACK solve on a
reducible system leaves roundoff of either sign of order `1e-16` there, the
reciprocals — up to about 2,450 on the affected fixture — amplify it to
around `1e-11`, and its pattern depends on the pivoting and therefore on the
**node order**. The first full sweep failed permutation invariance on
exactly one fixture for this reason, a 35-node sparse directed weighted
graph, at scaled error `4.768122e-11`. Imposing the known structure exactly
drops that to `9.1e-14` and makes `Z` nonnegative as theory requires. The
same masking was applied to the two reference routes that use an explicit
inverse; the Neumann route never had the problem, since it sums nonnegative
terms and leaves exact zeros.

**`rsp_beta` defaults to 0.01, which is not the source's number.** The paper
fixes no default and treats `beta` as a modelling choice; 0.01 is the value
`NetworkToolbox::rspbc()` calls recommended, adopted so the two are
comparable out of the box. It sits near the high-temperature end, so the
default reading is close to the random-walk limit and far from shortest-path
betweenness; the help page, vignette and `NEWS.md` all say so. The domain is
`beta > 0` and anything outside it raises `cograph_bad_parameter`.

**`rsp_cost` exposes a choice the source leaves free.** Algorithm1 takes `C`
as an input and never derives it from the weights. `"inverse"` (default)
sets `C = 1/w`, reading a weight as an affinity, as the CRAN reference
hard-codes; `"weight"` sets `C = w`, reading it as a distance. The two
coincide on a binary graph, both giving unit cost per arc, which is checked
on every fixture. Negative and non-finite weights raise
`cograph_bad_input`, Algorithm1 requiring a non-negative cost matrix;
`NetworkToolbox::rspbc()` silently takes `abs(A)` instead.

Direction is read from the graph rather than from `mode`, so the measure
sits in `.cg_no_mode_measures()` beside `pagerank` and `leaderrank`. Marked
**costly**: one dense `n x n` inverse, which page7 itself calls the
computational bottleneck at `O(n^3)` time and `O(n^2)` memory.

**There is no `published_audit.R` for this row, because the paper prints no
per-node score table on a small graph.** Saying so is more honest than
manufacturing one. What is checked against the paper instead is its printed
page9 limit claim, that an undirected network's score becomes proportional
to degree as `beta -> 0+`; it holds, and does so at first order in `beta`,
the spread of `score / degree` dividing by ten for each decade of `beta`
below `1e-3`. Recorded in `batch47/beta_limit.csv`.

Verification used four references sharing none of production's masked triple
product, its solve-against-identity or its repeated-squaring closure:
equation (15) in float64 NumPy through an explicit `numpy.linalg.inv` with
NetworkX reachability, the definitional double sum of equations (8) and (14)
accumulated pair by pair in an explicit `O(n^3)` Python loop with no matrix
product anywhere, the same double sum over a fundamental matrix built as a
truncated Neumann series with **no inverse and no solve at all**, and
equation (15) again in mpmath at60 decimal digits with mpmath's own
Gauss-Jordan inverse. Three hand-derived closed forms are checked as well: a
single undirected edge scores exactly1 at every `beta`, a directed `n`-cycle
scores `n(n-1)/2` at every `beta`, and a complete graph matches a
Sherman-Morrison derivation. `NetworkToolbox::rspbc`1.4.4 was run this
session on every strongly connected fixture at `beta` 0.01 and1; all values
agree after undoing that function's round-to-integer and
shift-so-the-minimum-is-one, post-processing that is nowhere in the paper
and is not copied. The authors' MATLAB at
`github.com/ikivimak/RSP-betweenness`, named in the paper's Materials
section, was **not fetched**, so no author-code parity is claimed, and the
paper's OpenStreetMap and Wikipedia experiments were not reproduced.

## Batch46: hybrid characteristic centrality (HCC) and its extension (EHCC)

Added `hcc` / `centrality_hcc()` and `ehcc` / `centrality_ehcc()`.
**58/160 original candidates implemented,102 pending**;186 runtime measures
and161 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch46/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch46/SOURCE_AUDIT.md).

Primary source: Liu and Zheng2023,
[DOI10.1038/s41598-023-30308-5](https://doi.org/10.1038/s41598-023-30308-5),
Scientific Reports13:3197, equations (3), (4) and (5) and the eight-step
E-shell hierarchy decomposition on journal page3, with the worked example,
figure1 and tables1-3 on page4. **The article is fully open access
(CC-BY) and the publisher PDF — the version of record — was read**, SHA256
`f0c81bd945cd021ce3636bee9bd5fe73f37cdd9aa67a5715d4823490049b9b95`,
**recomputed this session**; pages2 and3 rendered at 165dpi, page4 at
200dpi, all three **visually read**, plus a 500dpi crop of the figure1
graph, which is the render the 14-edge list was transcribed from. Zoo
pages99-100 (2.167) and81 (2.122) were read for the divergence. The paper
names no repository; the Zoo lists code for both entries as "TBA".

**One implementation, two ledger rows.** HCC is equations (3)-(4) and EHCC
is equation (5), the closed-neighbourhood sum of HCC.

```
k^ex(u) = delta k(u) + (1 - delta) sum_{v in phi(u)} k(v)       delta = 0.5
E-shell: each round removes EVERY node attaining the current minimum
         k^ex and recomputes k^ex on the residual graph; pos(u) is the
         round number
HCC(u)  = k^ex(u) / k^ex_max + pos(u) / pos_max     (both maxima ORIGINAL)
EHCC(u) = HCC(u) + sum_{v in phi(u)} HCC(v)
```

**The source's printed step 3 is a typo and cograph implements the
correction its own tables require.** Step 3 reads
`S_p = arg max_{u in G_p} {k^ex(u)}` while the same sentence calls `S_p`
"the set of minimum nodes", the paragraph above it says the
minimum-extended-degree nodes are deleted, and table2's column is headed
"Minimum extended degree" with the increasing values 2, 2.5, 3, 4.5, 5, 6.
The `arg min` reading reproduces every printed row. The literal `arg max`
peel deletes `{e, g}` first rather than `{j}` and finishes in four rounds
instead of six.

**The peel recomputes; equation (4) does not.** Step6 updates `k^ex` on the
residual graph, and a peel that never recomputed would take eight rounds on
figure1 with minima 2, 2.5, 3, 4.5, 6.5, 9.5, 10.5, 11. But equation (4)'s
`k^ex` and `k^ex_max` are original-graph values, as the printed
`HCC(a) = 4.5/11 + 4/6` and the printed maxima "11 and 6" show. Node `d`
settles it: original 9.5 gives the printed 1.86, residual 6 gives 1.55
against the original maximum and 2.00 against the residual maximum, missing
five and eight respectively of the ten printed HCC values.

**`hcc_delta` is confined to the source's stated `[0, 1]`.** This is a
deliberate narrowing. At `delta > 1` the extended degree goes negative and
equation (4) then divides by a nonpositive maximum: at `delta = 2` the
five-vertex fixture `K4 + K1` has extended degrees `-3,-3,-3,-3,0`, giving
`k^ex_max = 0` with four strictly negative numerators, a real division by
zero rather than the `0/0` an edgeless graph gives. Out-of-domain values
raise `cograph_bad_parameter`.

**One printed value is NOT reproduced, and is recorded rather than tuned
away.** Table3 prints `EHCC(g) = 10.01`; the exact value is
`661/66 = 10.015151...`, which rounds to `10.02`, a gap of `5.15e-3`
outside the half-unit. `10.01` is its truncation, but six other printed
cells (`HCC(a)`, `HCC(b)`, `HCC(j)`, `EHCC(d)`, `EHCC(e)`, `EHCC(i)`)
require rounding and fail under truncation, so **no single convention
reproduces all twenty table3 entries**. Everything upstream of that cell
reproduces exactly and the paper's ranking is unaffected.

**Two prose claims of the research note
`candidate_review40/hybrid-characteristic-centrality.md` are corrected in
`batch46/SOURCE_AUDIT.md`.** First, its summary that all twenty printed
table3 values reproduce at the printed precision: nineteen do, and its own
exact column already carries the `661/66` that the twentieth does not
match, so only the summarising sentence is wrong. Second, its claim that
the Zoo's "variant of k-shell decomposition" description "gives different
answers" on figure1: that holds only under an integer level grid (where the
k-shell reading merges `{c, h}` and `{a, b, i}`); under a distinct-value
level grid it coincides on figure1, because nothing cascades there. The
**four-node path** separates the two procedures under either reading —
E-shell positions `1, 2, 2, 1` against one single shell. As in batches44
and45: treat a research note's *numbers* as leads and recompute its
*claims*.

**A reference bug was found by the sweep and recorded rather than hidden.**
The first `reference.py` asserted the two-round q-star closed form for
every `delta`. At `delta = 0` a star's centre and leaves both have extended
degree `q`, so one round removes the whole star and the uniform closed form
applies instead. It failed on 148 fixture-settings, all at `delta = 0`;
`reference.py` and the public star test were corrected and production was
never involved.

Conventions: simple undirected unweighted skeleton (either arc makes one
edge, parallels once, loops removed); weights, mode, cutoff and inversion
ignored; directed input symmetrised, the source defining no directed case;
a round removes the whole current-minimum set simultaneously, so ties need
no ordering rule, and production's relative tolerance for that set never
fires at a dyadic `delta`; extended degree zero exactly at an isolate for
every admissible `delta`, so isolates always leave in round one; an edgeless
graph's `0/0` first term written as zero, so every node of an edgeless
graph including a singleton scores exactly1; empty graphs return no
scores; `k^ex_max` and `pos_max` global, so the measure is **not
component-local**; raw HCC in `[0, 2]` and `normalized = TRUE` max-scaling
on top of equation (4)'s own two divisions.

Verification actually run this session
([run_equivalence.R](../../local_testing_and_equivalence/batch46/run_equivalence.R),
[reference.py](../../local_testing_and_equivalence/batch46/reference.py),
[published_audit.R](../../local_testing_and_equivalence/batch46/published_audit.R)):

- **773,147 comparisons / 2,615,082 scalar values, 0 failures, maximum
  scaled error `4.440892e-16`** at tolerance `1e-11`. Five `delta`
  settings 0, 1/4, 1/2, 3/4 and 1 on all **5,532 fixtures**, against four
  references that share none of production's incrementally updated
  residual-degree vector, its dense float64 round evaluation or its
  tolerance-based minimum set: a NetworkX induced-subgraph rebuild every
  round with exact `Fraction` extended degrees and exact rational minima,
  a repeated-minimum-extraction peel over rational-keyed `defaultdict`
  buckets with in-place residual decrements and two-hop local
  rebucketing, the same peel decided by 60-digit `mpmath` comparisons,
  and EHCC as an exact object-dtype `(A + I)` product. Two of the 5,532
  fixtures are the empty graph, which has only the one route, so the
  three-route columns show 27,650 rather than 27,660.
- **27,660 agreements of the three peel routes on every removal set and
  every round count**, which is the direct test of production's
  floating-point tie tolerance against exact rational minima; **27,660
  extended-degree identity checks**
  (`sum_u k^ex(u) = delta 2m + (1 - delta) sum_j k_j^2`); **27,660
  position-index completeness checks** (`1..pos_max` all attained);
  **27,660 zero-extended-degree-iff-isolate classifications**; **27,660
  isolates-leave-in-round-one checks**; **27,660 `HCC` in `[0, 2]`
  checks**; **27,660 uniform-extended-degree closed-form flags** and
  **27,660 q-star closed-form flags**; **27,660 degree-agreement
  checks**; and **26,370 brute-force ordered-walk extended degrees** on
  the 5,274 fixtures through five vertices. All pass. Seven out-of-domain
  `hcc_delta` values raise.
- `published_audit.R` checks **111 values, 0 failures, 86 of them printed
  in the paper, of which 85 reproduce and 1 does not** — table 3's
  `EHCC(g)`, described above.
- **267 public-test expectations** passed
  (`test-centrality-batch46.R`). The broad
  `^centrality|^kernels|coverage-centrality` regression run had **4,043
  passes, 0 failures and 21 warnings** — the same warning count batches
  41-45 recorded, so batch 46 adds none. Catalogue count 186.
- `tools::checkRd()` clean on `man/centrality_hcc.Rd` and
  `man/centrality_ehcc.Rd`; `lintr` clean on both new `R/` files and the
  test file apart from the usual cross-file `object_usage_linter` notes
  every batch file produces; `docs/zoo/parameter_candidates.csv` SHA256
  unchanged. R 4.5.2, igraph 2.3.3, NumPy 2.4.2, NetworkX 3.6.1, mpmath
  1.3.0.
- **The correlation audit was not rerun**, so `hcc` and `ehcc` are absent
  from it. **Package build and `R CMD check` were not rerun for this
  batch.**

## Batch45: the KED method

Added `ked` / `centrality_ked()`.
**56/160 original candidates implemented,104 pending**;184 runtime measures
and159 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch45/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch45/SOURCE_AUDIT.md).

Primary source: Chen, Xiao, Zeng and Zhang2014,
[DOI10.1209/0295-5075/104/68006](https://doi.org/10.1209/0295-5075/104/68006),
EPL104(6):68006, equations (1) and (2) page2 and equation (6) with its
`D_i` definition page4. **The IOP version of record was NOT read.** What
was read is the author preprint arXiv:1305.7480, PDF SHA256
`d5fa04bdfd6669f6bc708e1af1558f0cce5138308ce4d98df5722255d547599c`,
**recomputed this session**; PDF pages2, 3 and4 rendered at 200dpi and
**visually read**, plus 500dpi crops of the figure1 graphs, which are the
renders the edge lists were transcribed from. Zoo pages117 and118 were
visually read for the divergence below. The paper names no repository or
supplementary material and none was located; the Zoo lists code for this
entry as "TBA".

**The measure is parameter-free.** Equation (6) is a bare product, and the
`alpha`/`beta` exponents the Zoo attributes to Chen et al. appear nowhere
in the paper. `centrality_ked()`'s formals are `(x, ...)` and nothing was
added to the `centrality()` signature.

```
KED(i) = k_i (1 + H_i) exp(K_i / N)
K_i    = sum_{j in N(i)} k_j          p_j = k_j / K_i
H_i    = [sum_{j in N(i)} -p_j log p_j] / log(k_i)      N = |V|
```

with `H_i := 0` at `k_i <= 1` and the score `0` at `k_i = 0`, both cograph
decisions.

**Correction to the research note that preceded this batch.**
`candidate_review40/ked-method.md` reports that the natural logarithm is
pinned by the printed values and that base ten fails to reproduce them.
It is not, and it does not: equation (2) divides the entropy by the
entropy of the uniform distribution on `k_i` outcomes, so the base cancels
top and bottom. Recomputed three ways this session - a base-ten route in
`reference.py` compared value by value on all5,532 fixtures, an
`entropy_base_invariance` assertion to1e-50 on every fixture, and a direct
base-ten recomputation of both red nodes in `published_audit.R` agreeing
to3.3e-16. The note's substantive findings, that the printed values pin the
`1 +` in `E_i` and the denominator `N` in `D_i`, are confirmed.

**The paper's stated range `1 <= D_i <= e` is false in general.** It needs
`K_i <= N`. Measured across the verification collection this session, that
holds on **1,598 of the5,532 fixtures**; every node of `K_5` has `K_i = 16`
against `N = 5`, giving `D_i = e^3.2 = 24.53`. cograph implements the
formula, not the range claim. The other stated range, `1 <= E_i <= 2`, is a
theorem about equation (2) and is asserted as an invariant on every
fixture, together with the sharper `H_i = 1` exactly when the neighbour
degrees are all equal.

**`N` is global and the dependence is not a rescaling.** Unlike batch44's
`1/(n-1)`, `exp(K_i/N)` shrinks a large neighbour-degree sum more than a
small one, so adding a disconnected component can **reorder** nodes. Shown
in the public tests on the seven-node graph
`1-4,3-4,2-5,3-5,4-5,5-6,1-7,2-7,3-7,6-7`, where one extra isolate turns
`28.7687 > 28.7567` into `23.6381 < 24.4873`.

**The Centrality Zoo entry is wrong twice and was not implemented.** Zoo
2.215 drops the `1 +` from `E_i` and divides `K_i` by `max_l K_l` instead
of by `N`. Recomputed here, that reading gives13.5914 and6.5672 on the
paper's own figure1, where the paper prints25.9187 and19.2212; dropping the
`1 +` halves panel (a) exactly, because `H = 1` there. The intermediate
reading that keeps the `1 +` and changes only the denominator
gives27.1828 and20.1586 and reproduces neither. The `max_l K_l` denominator
is a plausible misreading precisely because it makes the paper's own range
sentence true.

Verification. `run_equivalence.R` over the retained 5,532-graph collection:
**105,895 comparisons,353,326 scalar values,0 failures, maximum scaled
error `1.755829e-15`** at tolerance `1e-11`. Five references, none using
production's identity `h_i = log(K_i) - (1/K_i) sum_j k_j log(k_j)` or its
dense float64 matrix-vector products: exact `Fraction` probabilities with
equation (1) accumulated one neighbour at a time at 60 decimal digits, the
same terms in float64 summed by `math.fsum`, a `Counter`-bucketed traversal
summing once per distinct neighbour degree, the whole ratio taken in base
ten, and `exp(K/N)` split as `exp(q)exp(r/N)` at the exact integer quotient
and remainder; plus a brute-force ordered-walk cluster degree through five
vertices. Invariants on every fixture: the cluster-degree identity, the
unit interval for `H` with equality exactly at equal neighbour degrees,
base invariance, the zero-iff-isolate classification, degree agreement, and
the `r`-regular and `q`-star closed forms. `published_audit.R` checks
**36 values,0 failures,5 of them printed in the paper** - both printed
scores and the figure1 caption's three structural claims. **107 public-test
expectations** passed. Catalogue count184.

One reference bug is recorded rather than hidden: the first regular-graph
closed form asserted `2r exp(r^2/n)` for every `r`, which is wrong at
`r = 1`, where the `H := 0` convention gives `E = 1`. It failed on the34
one-regular fixtures; the reference was corrected and production was never
touched.

Not implemented, and recorded as a limitation: the source's directed
out-degree variant (its equation3). A directed input is symmetrised rather
than read as that case.

## Batch44: local neighbor contribution (LNC)

Added `lnc` / `centrality_lnc()`.
**55/160 original candidates implemented,105 pending**;183 runtime measures
and158 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch44/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch44/SOURCE_AUDIT.md).

Primary source: Dai, Wang, Sheng, Sun, Khawaja, Ullah, Dejene and Duan2019,
[DOI10.1109/ACCESS.2019.2939804](https://doi.org/10.1109/ACCESS.2019.2939804),
IEEE Access7:131719-131731, Definitions1-5, equations (1)-(6) and
Algorithm1, journal pages131721-131723, with the Figure1 graph and Table1
on page131720. **The original open-access PDF was obtained and read.** IEEE
Xplore refuses non-browser clients, so what was fetched is the Internet
Archive capture of 2023-11-17 of the publisher `ielx7` file — the same
Wayback route that rescued the IRA source in batch43. PDF SHA256
`7db704bad41e7fce221fbe7ad06733a1d11fa259811f3702f3d93f1429ecbe0b`,
**recomputed this session**; PDF pages2, 3, 4 and5 rendered at 200dpi and
**visually read**, plus a 400dpi crop of the Figure1 graph, which is the
render the edge list was transcribed from. The paper names no repository or
supplementary material and none was located; the Zoo lists code for this
entry as "TBA".

**The measure is parameter-free**, which the paper advertises as one of its
three contributions (journal p.131721, visually read: "**Parameter-Free:**
LNC does not rely on prior knowledge and parameter adjustments").
`centrality_lnc()`'s formals are `(x, ...)` and nothing was added to the
`centrality()` signature.

```
LNC(i) = d_i^3 (1 - 1/d_i)^(d_i - 1) (sum_{j in N(i)} d_j) / (n - 1),   0^0 := 1
```

**The printed equations do not literally give the paper's own printed
numbers, and cograph follows the numbers.** Equations (4) and (5) both sum a
term over `j = 1..k`, and `k` is described three incompatible ways: the
prose calls it the number of nearest plus next nearest neighbours,
Algorithm1 line12 sets it to `G.degree(v)`, and (5) taken literally carries
one factor `d_i` too many — recomputed this session, the literal (5) gives
`ownCon(v5) = 6.75` where the paper prints `1.6875`. Two independent
arguments pin the measure. First, `P(v_j) DC(v_j) = (1/d_j)(d_j/(n-1))
= 1/(n-1)`, so the inner sum of (4) collapses to a bare count and
`neiCon(i) = D(i) k/(n-1)`; inverting each of the **eleven** printed Table1
influences, `k_i = Influ_i (n-1)/(D_i ownCon_i)`, returns `d_i^2` at every
node. Second, `ownCon` must be `d_i (1-1/d_i)^{d_i-1}` for the printed
1.6875, and the three printed influences of 0.4 at degree one force
`0^0 = 1`. The equally literal alternative split — moving one `d_i` from
`neiCon` to `ownCon` — gives the **same product**, asserted exactly on all
5,532 fixtures, so the measure itself is unambiguous. The
neighbourhood-size readings are ruled out numerically: the two-hop sizes on
the Figure1 graph, computed here two ways, are `7,4,4,4,8,5,5,7,6,3,3` for
nodes1..11, and they equal neither `d_i` nor `d_i^2` anywhere.

**Raw scores depend on the whole graph's order.** `n` in equation (1) is
"the total number of nodes", so adding a disconnected component multiplies
every score by `(n-1)/(n'-1)`. The ranking is untouched, the raw values are
not; this is the published definition, not a cograph choice, and it is
documented on the help page, in the catalogue and in a public test.

**Documented divergence from the Zoo.** Zoo2.238 (p.126) prints
`O_c(i) = d_i |M| sum_{j in M} (1/d_j)(1-1/d_j)^(|M|-1)` with
`M = N^(<=2)(i)`, replacing the focal node's own contribution probability
`P(v_i)` by each neighbour's `P(v_j)` and the binomial count `d_i` by the
two-hop size; its `N_c` is right in form but uses that same `|M|` where the
printed numbers need `d_i^2`. Recomputed this session, the Zoo reading on
Figure1 gives `v8 = 32.23 > v5 = 28.90` where the paper prints
`v5 = 32.4 > v8 = 29.7`, lifts `v9` above `v1` where the paper prints
`18.9 > 9.6`, and reproduces **none** of the eleven printed values.
**cograph implements the paper; no Zoo variant is offered.**

**Two corrections to the research note that preceded this batch**
(`candidate_review40/local-neighbor-contribution.md`). Its formula, its
conventions and its Zoo value column are all reproduced exactly here, but
two of its prose claims are wrong, both checked numerically this session.
(i) It says the Zoo reading lifts the degree-two `v6`, `v7` above the
degree-three `v9`; on the Zoo's own numbers `v9 = 15.65 > v6 = v7 = 13.38`.
The real inversion, besides `v5`/`v8`, is `v9` above `v1`. (ii) It gives the
two-hop sizes as `4,6,6,8` by degree, as if the size were a function of the
degree; the degree-two nodes give `5,5,3,3` and the degree-four nodes
`7,8,7`. Neither affects the implemented formula, and the note's conclusion
that `|N^(<=2)|` is ruled out stands.

Independent verification: `batch44/reference.py` supplies five routes plus a
sixth structural check, none of them production's `A %*% rowSums(A)` with a
float64 `^` — NetworkX adjacency lists in exact `Fraction` arithmetic with no
matrix at all; the cluster degree as the row sums of an exact object-dtype
integer `A^2`, where the square is formed first and summed afterwards, and
is additionally rebuilt by a pure-Python triple loop and compared through6
vertices; the binomial factor expanded as the alternating exact-rational sum
`sum_m C(d-1,m)(-1)^m d^-m` with the cluster degree from explicit
enumeration of every ordered length-two walk, which is the route that would
break if `^` or `0^0` misbehaved; `mpmath` at 60 digits; a separately formed
exact `ownCon`/`neiCon` split that also asserts the alternative literal
split agrees; and a fourth brute-force cluster-degree route with its own
exhaustive pair-scan degree assertion on graphs through5 vertices.
Invariants asserted in exact arithmetic on every fixture: the cluster
degrees sum to the sum of squared degrees; on an `r`-regular graph every
score is exactly `r^5 (1-1/r)^(r-1)/(n-1)`; a score is zero exactly at the
isolates; NetworkX degrees equal the skeleton's row sums.

`batch44/run_equivalence.R` runs on all 5,532 retained fixtures. There is no
parameter grid to cross, so the crossing is over the input projections
instead: the public wrapper, `normalized = TRUE`, a random node permutation,
and a weighted, self-looped, `mode = "in"`, `cutoff = 1`,
`invert_weights = TRUE` projection that must return the skeleton's scores.
**83,767 comparisons /289,832 scalar values, zero failures, maximum scaled
error `1.6468e-15`** at a tolerance of `1e-11`. Beyond value equality the
run asserts **5,532 cluster-degree identity checks**, **5,532 zero-iff-isolate
classifications**, **5,532 degree-agreement checks**, **5,532 own/neiCon
split-agreement checks** and **5,532 regular-graph closed-form flags**, of
which **1,053 fixtures are regular** and are additionally compared against
the closed form value by value; a further **5,274 brute-force cluster-degree
comparisons** cover the non-empty fixtures through five vertices. All pass.

`batch44/published_audit.R` checks **91 values, 0 failures, 32 of them
printed in the paper**. Reproduced from the paper: the Figure1 caption's 11
nodes and 13 edges (which also validates the transcribed edge list, as does
the printed `D(v5) = 12`), the prose naming `v5`'s four nearest neighbours,
`D(v5) = 12`, `ownCon(v5) = 1.6875`, `neiCon(v5) = 19.2`,
`Influ(v5) = 32.4`, **all eleven Table1 influences** at the printed one
decimal and the printed node order. Computed here rather than printed: the
exact rationals behind Table1, the literal readings of (4) and (5) that the
printed intermediates rule out, the two-hop sizes, and the Zoo's values. One
inconsistency inside the paper is recorded rather than smoothed over: the
prose lists six next nearest neighbours of `v5` while the
neighbours-of-neighbours are seven, the list dropping `v8` but keeping `v6`
and `v7`, which are equally both nearest and next nearest. Nothing in the
implemented formula depends on it. Not attempted: the SIR benchmarks of
Section IV, which need the seven real networks and stochastic runs.

Conventions. Simple undirected unweighted skeleton, the source domain:
either arc creates one edge, parallel edges count once, loops are removed,
and weights, mode, cutoff and path-weight inversion are ignored. Open
neighbourhoods. `n` is the whole vertex set. Isolates are outside the
definition, since `P(v_i) = 1/0`, and score **zero** by explicit cograph
extension, chosen because `d_i^3` and the empty cluster degree are both
zero; the zero is written before the division, so the singleton graph, where
`n - 1` also vanishes, returns 0 rather than `NaN`. Empty graphs return no
scores. The source states no normalization; `normalized = TRUE` max-scales,
a cograph convention. Nothing overflows: `d^3 <= n^3`, the cluster degree is
at most `2m`, and `(1 - 1/d)^(d-1)` lies in `[1/4, 1]`. Cost is one sparse
matrix-vector product. **No author-software parity claim is made** — no
author software exists — and no claim is made about spreading performance.

## Batch43: iterative resource allocation (IRA and IIRA)

Added `ira` / `centrality_ira()` and `iira` / `centrality_iira()`.
**54/160 original candidates implemented,106 pending**;182 runtime measures
and157 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
Full source audit:
[batch43/SOURCE_AUDIT.md](../../local_testing_and_equivalence/batch43/SOURCE_AUDIT.md).

Primary sources. IRA: Ren, Zeng, Chen, Liao and Liu2014 — read as
[DOI10.1209/0295-5075/106/48005](https://doi.org/10.1209/0295-5075/106/48005),
EPL106(4):48005, equations1-3 page2 and algorithm i)-iii) page3. **The
original was obtained and read**, as the University of Fribourg RERO DOC
author postprint recovered through the Internet Archive; the live
`doc.rero.ch` URL that Unpaywall and OpenAlex record now serves a
decommissioning notice and is dead. PDF SHA256
`6afce148d2be9945a49a75f69ae39dd927c6d1aa79fb459171482c79de02dec0`,
recomputed this session; pages2 and3 rendered at 200dpi and **visually
read**. IIRA: Zhong, Liu and Shang2015,
[DOI10.1016/j.physleta.2015.05.021](https://doi.org/10.1016/j.physleta.2015.05.021),
Physics LettersA379(38):2272-2276, equations1, 2 and4 page2. **The Elsevier
version of record was NOT read**; what was read is the sole author preprint
arXiv:1505.03214v1, PDF SHA256
`f7b23645d11ad9a19e6411b10baac5066420231c534cf388eb8b7810d2e660b1`,
recomputed this session, page2 rendered and **visually read**. arXiv lists
only v1. Neither paper names a repository, supplementary material or
software, and none was located; the Zoo lists code for both entries as
"TBA".

Both measures are one recursion, `I(t+1) = A I(t)` from `I(0) = 1`, with two
allocation matrices. IRA:
`a_ij = theta_i^alpha / (sum_{u in Gamma(j)} theta_u^alpha) delta_ij`,
iterated until `max_i |I_i(t) - I_i(t-1)| < eps`. IIRA:
`a_ij = [1 - (1-beta)^{k_i}] theta_i (sum_{u in Gamma(j)} theta_u)^{-1}
delta_ij`, run for a fixed number of steps, with **no** alpha exponent.
Defaults `ira_mass = "coreness"`, `ira_alpha = 1`, `ira_tol = 1e-6`,
`iira_beta = 0.2` and `iira_steps = 50` are the sources' own values;
`ira_max_iter = 1000` covers the paper's stated `t_c <= 2N - 1` for
`n <= 500`.

**IRA provably does not always converge, and cograph reports it.** `A` is
the transition matrix of a reversible walk, so its unit-modulus eigenvalues
are exactly `+1` per component and exactly `-1` per bipartite component. The
`+1` eigenvector is `x_i = theta_i^alpha s_i` with
`s_i = sum_{u in Gamma(i)} theta_u^alpha`, verified directly, and the `-1`
eigenvector on a bipartite component is that vector with the sign flipped on
one class; taking inner products with `I(0) = 1` gives coefficients
`n_c / S` and `(|X| - |Y|) / S` with `S = sum_i theta_i^alpha s_i`. So the
iteration has a limit **exactly when every bipartite component has equal
class sizes**, and otherwise settles into a period-two cycle. Recomputed
this session: the 3-star alternates for ever between `(3, 1/3, 1/3, 1/3)`
and `(1, 1, 1, 1)`, while `P4`, whose classes are 2 and 2, converges to
`(2/3, 4/3, 4/3, 2/3)`. cograph keeps the source's rule, stops at
`ira_max_iter`, raises the classed warning `cograph_no_converge` naming the
largest remaining change, and returns `I(ira_max_iter)`, which is
parity-dependent and documented as such. It ships **no** Cesaro-average
variant and does not substitute the Zoo's eigenvector: both converge, and
neither is the source's algorithm. **585 of the 5,532 retained fixtures do
not settle** at the default configuration, measured in this session.

**Documented divergences from the Zoo.** Zoo2.204 (IRA) states the transpose
`p_ij = a_ij c_j^alpha / sum_k a_ik c_k^alpha` and asks for the principal
left eigenvector: the same object up to scale wherever a limit exists, but
it carries no `sum_i I_i = n` scale and it silently sidesteps the bipartite
case rather than reporting it. Zoo2.185 (IIRA) prints
`p_ij = (1 - (1-beta)^{d_i}) a_ij c_i / sum_k a_ik c_k`, pairing the
numerator index with the denominator's own neighbourhood while the paper
pairs them with opposite sets. As printed, its row sums are
`psi_i c_i d_i / sum_{k in N(i)} c_k`, so the matrix is stochastic in
neither direction although the entry calls it stochastic, and it reproduces
neither the printed equation5 matrix nor the printed `I(50)`. **cograph
implements the papers.**

Independent verification: `batch43/reference.py` supplies six routes, none
of them production's dense float64 push with a literal difference test —
pure-Python adjacency-list propagation in exact `Fraction` arithmetic whose
stop rule is an exact rational comparison (so the stopping index is decided
without floating-point rounding, and is asserted equal to the spectral
route's); binary exponentiation of `A` in exact `Fraction` arithmetic
applied once to `I(0)`; a per-component `numpy.linalg.eigh` of the
symmetrised matrix
`C_ij = delta_ij sqrt(w_i w_j)/sqrt(s_i s_j)` with `I(t)` from eigenvalue
powers and the stopping index taken from the closed form of
`I(t+1) - I(t)`, so no two nearly equal numbers are ever subtracted, and
unit-modulus eigenvalues snapped to the structurally exact `+1` / `-1`;
the exact closed-form `+1` and `-1` spectral projections evaluated from a
bipartition and integer products, with no iteration, matrix or
eigen-solver; `mpmath` at 60 digits; and NumPy 80-bit extended precision.
Core numbers come from `networkx.core_number`, additionally asserted
through five vertices against the maximum over induced subsets of their
minimum degree.

`batch43/run_equivalence.R` crossed `ira_mass` in {coreness, degree} x
`ira_alpha` in {1, 1/2, 2} for IRA (6 settings) and `ira_mass` x
`iira_beta` in {1/5, 1/2} x `iira_steps` in {50, 10} for IIRA (8 settings)
on all 5,532 retained fixtures, plus public wrapper, `normalized = TRUE`,
node permutation and input-projection checks at each default. Exponents and
rates were passed as exact rational strings beside the doubles R uses.
**548,075 comparisons /1,989,981 scalar values, zero failures, maximum
scaled error `1.290616e-13` and maximum relative error `1.290616e-13` at a
tolerance of `1e-11`.** Both metrics are required because IIRA's raw values
are of order `1e-20` to `1e-40`, where the usual
`abs(got-ref)/max(1,abs(ref))` is vacuous; every iterate is a nonnegative
sum of nonnegative terms, so there is no cancellation and relative accuracy
is meaningful. Beyond value equality the run asserts **33,192 exact
column-stochasticity checks** for IRA, **44,256 sub-stochastic column
checks** for IIRA, **33,192 convergence-classification checks** comparing
production's captured `cograph_no_converge` warning against the reference's
own stop, and **33,192 structural bipartite-prediction checks**; all pass.

`batch43/published_audit.R` checks **185 values, 0 failures, 134 of them
printed in a paper**. Reproduced from the papers: Ren's figure1 caption
k-shell values for all four panels (which also validates the transcribed
edge lists), all 25 printed entries of equation4, the stated column
stochasticity and `sum_i I_i = n` conservation, all 20 printed table1
entries from the exact steady states, the printed
`I(50) = [15/8, 5/4, 5/4, 5/16, 5/16]` matched by cograph's settled iterate
to `3.5e-7`, Zhong's figure2 caption k-shells, all 25 printed entries of
equation5 at the paper's own truncation (it truncates rather than rounds:
0.2952 prints as 0.29, 0.0333 as 0.03), and all five printed `I(50)` values
at three significant figures. Two qualifications are recorded rather than
smoothed over. First, the paper writes `I(50) = A^50 I(0) = [15/8, ...]`,
which holds only in the limit: the literal `A^50 I(0)` computed here differs
by up to **1.86e-7**. Second, **one printed table1 entry is not reproduced
by rounding cograph's iterate**: panel (a) node0 has exact limit
`15/8 = 1.875`, which the paper rounds up to `1.88`, while the iteration
approaches from below and at the source's own `epsilon = 1e-6` returns
`1.8749998`, which rounds to `1.87`. Nineteen of the twenty round correctly
from the iterate; the twentieth needs the exact limit. Not attempted: either
paper's SIR benchmarks, which need the real networks and 10^4-2*10^4
stochastic runs, and Zhong's figure1 eight-node illustration, whose quoted
`1.079`/`1.083` are SIR outputs rather than IIRA scores.

Conventions. Simple undirected unweighted skeleton, the source domain:
either arc creates one edge, parallel edges count once, loops are removed,
and weights, mode, cutoff and path-weight inversion are ignored. Open
neighbourhoods; `I(0) = 1` at every node. Isolates score zero from the first
step under both measures — the value of the source's own empty sum, since an
isolate is in nobody's neighbourhood and appears in no column, so its
undefined column denominator is never evaluated and there is no `0/0`; the
consequence, documented, is that `sum_i I_i = n` holds only on graphs
without isolates. `theta = 0` never reaches a denominator, because
`theta = 0` exactly for isolates under both masses and production builds the
matrix over the adjacency support alone. IRA raw scores are comparable
across components; **IIRA raw scores are not**, because each component
decays at its own rate. `iira_beta = 0` is refused rather than silently
returning zeros; `iira_steps = 0` returns `I(0)`; a large `iira_steps`
underflows to zero. Empty graphs return no scores. Neither source
normalizes; `normalized = TRUE` max-scales, a cograph convention. No
author-parity claim is made for either measure, and no claim is made about
spreading performance.

**163 public-test expectations** passed in
`tests/testthat/test-centrality-batch43.R`, including the four printed
figure1 panels, the printed figure2 example, the star and three-path
period-two cycles at both parities of `ira_max_iter`, the converging
four-path, analytic clique, ring and star families for both measures,
isolates, disconnected unions, empty graphs, labels, permutation,
input-projection, parameter validation and metadata. The broad
`^centrality|^kernels|coverage-centrality` filter has **3,588 passes, 0
failures and the same 21 warnings** batch41 and42 recorded; the complete
suite has **24,088 passes, 0 failures, 201 warnings and 42 existing skips**,
with no `cograph_no_converge` line anywhere in it. Package build and
`R CMD check` were not rerun.

## Batch42: neighborhood (neighbor distance) centrality

Added `neighbor_distance` / `centrality_neighbor_distance()`.
**52/160 original candidates implemented,108 pending**;180 runtime measures
and155 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.
This batch moves the ledger by **two** rows because Zoo2.279 and Zoo2.280 are
one definition at two parameter settings; see below.

Primary source: Liu, Tang, Zhou and Do2016,
[DOI10.1016/j.physa.2016.02.028](https://doi.org/10.1016/j.physa.2016.02.028),
Physica A452:289-298, section2.3 equation (1). **The paywalled version of
record was not read.** What was read is the sole author preprint,
arXiv:1511.00441v1 (2 Nov2015, 17pp.), whose PDF SHA256
`01fcbac501df07f022ff4ad588d6d5027c5ebba3f06dbcbc202e04c7618f6292` was
recomputed in this session and matches the hash recorded in
`candidate_review40`. The complete `pdftotext -layout` extraction was read for
sections1, 2.1-2.5, 3 and4, and **page4 was rendered and visually inspected**,
which is necessary because text extraction moves equation (1)'s summation index
sets `j in Gamma_i`, `l in Gamma_j\i`, `m in Gamma_l\j`, `s in Gamma_{s-1}\x`
onto a separate line where they are easy to misread. arXiv lists only v1, so no
author-side revision to the published text exists. The paper names no
repository, no supplementary material and no software.

Equation (1) is a chain of **nested** sums, each level excluding only the
immediately preceding node. The `k`-th term is therefore the benchmark
centrality summed over the endpoints of the **non-backtracking walks of length
`k`** starting at the focal node, **once per walk**; a walk may revisit any
earlier node, the focal node included. Defaults `nd_mass = "degree"`,
`nd_order = 2`, `nd_decay = 0.2` are the source's own recommendation and are
exactly what the Zoo calls neighbor distance centrality.

**Two Zoo labels, one implementation.** Zoo2.279 states that "the neighbor
distance centrality is a specific case of neighborhood centrality [236],
obtained by using degree centrality as the benchmark measure, a decay parameter
a =0.2, and considering neighbors up to two steps (n =2)", and Zoo2.280 defines
the general form and closes "When f is defined as the degree centrality with
n =2 and a =0.2, the measure is referred to as the neighbor distance
centrality". Both cite the same reference [236], the paper audited here. Both
labels are mapped to `neighbor_distance`, whose `nd_order`, `nd_decay` and
`nd_mass` cover the general form.

**Documented divergence from the Zoo.** Both Zoo entries paraphrase the measure
with sums over `N^(k)(i)`, "the set of k-hop neighbors" -- distance shells, not
the paper's nested non-backtracking sums. The two agree on trees and disagree on
any graph with a triangle or a cycle of length at most `2n`, and the gap is a
per-node offset rather than a rescaling. Recomputed in this session in exact
`Fraction` arithmetic: on the triangle-plus-pendant `A-B, A-C, B-C, A-D` the
implemented reading gives `4.16, 3.24, 3.24, 1.76` where shells give
`4.00, 3.04, 3.04, 1.76`; on `C4` the implemented reading gives 2.96 at every
node where shells give 2.88. Over four larger fixtures, 7 of33 nodes differ, the
largest absolute difference being1.48 (`zoo_shell_divergence.csv`).
**No shell variant is shipped.** The shell form appears only in this secondary
paraphrase, which is demonstrably a misreading of its own source: it also
attributes the naming to reference [235] (Liu, Tang, Zhou and Do2015,
*Sci. Rep.* 5:13172), a paper whose full text was downloaded and searched during
the research audit and contains no occurrence of "neighborhood centrality",
"neighbor distance" or the decay parameter `a`.

Production uses the Hashimoto-style vector recursion
`S_1 = A theta`, `S_2 = A S_1 - D theta`, `S_k = A S_{k-1} - (D - I) S_{k-2}`,
which is `O(n^2)` per step and never forms a walk. The recursion was derived
analytically before it was relied on (the `k =2` base case differs because a
length-zero walk has no penultimate node) and then checked numerically against
brute-force walk enumeration on **every labeled simple undirected graph through
five vertices at `k =1..6`, 6,594 matrix comparisons, zero mismatches**, plus
twelve `G(9, 0.4)` random graphs at `k =1..5`, zero mismatches.

Independent verification: `batch42/run_equivalence.R` crossed
`nd_order in {0,1,2,3,5}` x `nd_decay in {0.2, 1, -0.5}` x
`nd_mass in {degree, coreness}` -- 30 configurations -- on all5,532 retained
fixtures against four references that share none of production's algebra:
pure-Python dictionary propagation keyed by the last traversed directed edge;
depth-first enumeration of every individual non-backtracking walk;
brute-force enumeration of **all** walks with the backtracking ones filtered
out; and a dense `2m x 2m` Hashimoto edge matrix pushed with NumPy
object-dtype integers. The decay was passed as an exact rational string, so all
four are exact rational arithmetic; a fifth `math.fsum` route checks the
floating-point path. Core numbers came from `networkx.core_number`, additionally
asserted through five vertices against the maximum over induced subsets of their
minimum degree. **942,234 comparisons /4,347,880 scalar values, zero failures,
maximum scaled error `6.661338e-16` at `1e-11`.** The run also covered the public
wrapper, `normalized = TRUE`, a random node permutation and an input projection
(`weights x17`, diagonal9, `loops = TRUE`, `mode = "in"`, `cutoff = 1`,
`invert_weights = TRUE`).

**There is no published numerical fixture.** The paper prints imprecision
functions, Kendall tau and rank correlations against SIR simulations on six real
networks; it prints no table of node scores and shows no small figure graph with
values. `batch42/published_audit.R` therefore checks the paper's printed
*statements* -- **5 statements, 0 failures**: `a =0` returns the benchmark
centrality itself ("a =0 corresponds to k or kS"), the declared domain
(`theta` in {degree, coreness}, `n =1..4`, `a` in [0,1]) runs finite everywhere,
scores are nondecreasing in `a` for `a >=0`, `C^2(k)` reads structure three hops
away ("our way of considering the2-step neighbors encodes the information of the
3-step neighbors"), and the Zoo's neighbor-distance parameters are the cograph
defaults. It also reproduces the12 research-session worked values at a maximum
scaled error of `1.261617e-16`. Every number in this section was computed in
this session; none is printed in the paper.

Conventions: simple undirected unweighted skeleton (either arc makes one edge,
parallel edges count once, **loops are removed** because a loop makes "the node
the walk just came from" ambiguous); open neighbourhoods; endpoints counted with
walk multiplicity; `nd_order` a nonnegative whole number with zero returning
`theta`; `nd_decay` any finite number, which leaves the source's `[0,1]` domain
by explicit cograph extension; core numbers follow cograph's own `"coreness"`,
so isolates sit in the zero-shell; isolates score zero; raw scores are
component-local; empty graphs return no scores; large orders overflow because
walk counts grow geometrically. Weights, `mode`, `cutoff` and path-weight
inversion are ignored. There is no normalization in the source, so
`normalized = TRUE` max-scaling is labelled a cograph convention. There are no
undefined cases, so nothing is ever returned as a silent zero.

Not claimed: parity with author software (none exists), agreement with the
published Physica A text (not read), reproduction of the paper's SIR
spreading-performance results or its 2-step saturation effect (not attempted --
the six datasets were not obtained), and agreement with the Zoo's own numbers
(its per-node outputs are not published and its code is unreleased). The
de-duplicated "set" reading of the prose is excluded by the equation, not by the
prose; both are recorded in `neighbor-distance-centrality_worked.csv`.

Public tests: `tests/testthat/test-centrality-batch42.R`, **152 expectations**,
covering the source-versus-shell distinction, analytic star, clique, ring and
path families for both benchmarks and orders0-5, the internal step sums against
hand-counted walks, isolates, empty graphs, disconnected unions, normalization,
the parameter grid, input projection, permutation, label preservation,
parameter-validation errors and the `list_centralities()` metadata row.

## Batch41: relative-entropy integrated evaluation

Added `relative_entropy` / `centrality_relative_entropy()`.
**50/160 original candidates implemented,110 pending**;179 runtime measures
and153 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.

Primary and only source: Chen, Wang and Luo2016,
[DOI10.21629/JSEE.2016.06.10](https://doi.org/10.21629/JSEE.2016.06.10),
Journal of Systems Engineering and Electronics27(6):1219-1226, equations3,4,6,
8,9,10 and11 and Tables1-3. The complete `pdftotext -layout` extraction was read
end to end and PDF pages4,5 and6 (journal pp1222-1224) were rendered at200dpi
and visually inspected, confirming the summation limits of equations (4) and (6),
the `log_2` in equation (10), the closed form (11), Fig.1, and the two sentences
that declare each index positive or negative. PDF SHA256
`db3c3e8d8f5a115290f010e1b3b7a4ace809894096ca610a6a563c5a6880a414`, matching the
hash recorded in `candidate_review40`. The paper names no repository, no
supplementary material and no software.

Each index becomes a discrete distribution by equation (8) if positive or
equation (9) if negative, and the integrated score is the closed-form minimiser
of equation (10): the normalised geometric mean of those distributions,
equation (11). Output is unit-sum. The base of the logarithm appears only in
equation (10)'s objective and cancels from the closed form, so the
implementation is base-free.

`re_indexes` offers exactly the six indexes the paper both defines **and**
declares a direction for: degree, equation (3) closeness, equation (4)
betweenness and equation (6) constraint (the four "distinctiveness" indexes of
section4.1), plus the post-deletion component count and largest component size
(the two "destructiveness" indexes of section4.2). Equation (2) clustering and
equation (5) eigenvector are defined but never used and never given a
direction, and equation (7) average path length is infinite for every cut
vertex, so none of the three is offered. `re_negative` overrides the paper's
directions.

Two conventions had to be settled and are documented as reconciliations, not as
author-confirmed. Equation (6) is **not** Burt's constraint: its outer sum runs
over all of `V`, which is what gives Kite node1 the printed1.25 where
`igraph::constraint()` gives1; and the printed limit `j=1..|V|` would include
`j=i` and raise that node to1.5, so `j=i` is excluded as the only reading
reproducing Table1. Equation (4) betweenness counts ordered pairs, twice the
usual value, which equation (8) then cancels. One cograph extension is declared:
equation (3) sums distances over all of `V` and is identically zero on a
disconnected graph, so cograph sums the reachable partners instead; that is
exactly equation (3) on a connected graph, the paper's own domain.

An exact index zero annihilates a node exactly, as the paper's Table2 prints for
nodes1,6 and8. An index that is zero at every node, or equation (9)'s `|V|-1`
denominator on a single node, has no value and raises
`cograph_undefined_index`; nothing is returned as zero. Because
`relative_entropy` joins the `type = "all"` tier and its default set contains
betweenness, that strict error broke `centrality(K3, type = "all")` and two
existing tests. Batch41 therefore also added `.cg_tier_guard()`: a measure named
in `measures` or `include` still raises, while a measure a *tier* supplied has
that one condition turned into a `cograph_undefined_measure` warning and an `NA`
column. This generalises what the community-partition measures already do
without `membership`.

- **299,324 comparisons,1,081,324 values,zero failures**, maximum scaled error
  1.554312e-15 at tolerance1e-11. All5,532 retained graphs cross eight
  configurations: the paper's four-index default, the same four with no negative
  index, the same four with betweenness additionally declared negative,
  degree+betweenness, the two destructiveness indexes, all six, closeness alone
  and largest_component alone.
- **44,256 undefined classifications agree** between production and the
  reference, one per graph and configuration. Undefined counts by configuration:
  1,015 of5,532 for every configuration containing betweenness (1,003 graphs
  whose every component is complete, plus12 edgeless graphs),12 for closeness
  alone and2 for each destructiveness-only configuration (the single-node
  graphs). No undefined case is reported as zero on either side.
- References: NetworkX BFS distances and neighbour lists; shortest-path counts
  from an explicit BFS-layer integer recursion combined through the
  `sigma_jk(i) = sigma_ji * sigma_ik` pair identity, against igraph's Brandes
  accumulation in production; equation (6) as a literal nested `Fraction` sum
  over `j` and `q`, against a matrix square in production;
  `networkx.connected_components` after node deletion. Equations (8), (9) and
  the numerator of (11) are exact `Fraction`s, and the `m`-th root is taken
  three ways: `mpmath.root` of the exact rational product at dps60, an mpmath
  exp/log sum, and a NumPy double log sum. Small graphs additionally assert the
  betweenness against `networkx.betweenness_centrality` (n<=8) and against
  brute-force `all_shortest_paths` enumeration (n<=6). Main API, wrapper,
  maximum scaling, unit-sum, permutations and weight/loop/mode/cutoff
  projections are all checked.
- **Published example, three routes, all reported.** All **60** printed Table1
  and Table3 index entries reproduce exactly at the printed precision from the
  transcribed Fig.1 graph. The **30** printed Table2 integrated values computed
  from that exact graph differ by up to **3.569423e-05**; recomputing the same
  30 values from the paper's **own printed rounded** Table1/Table3 columns closes
  the gap to **4.774394e-07**, i.e. the printed six decimals. Route three
  explains route two: the paper integrated its rounded intermediates. Both gaps
  are asserted in the public tests. These are numbers printed in the paper.
- The ARPA case study (Fig.2, Tables4-5) is **not** used. Its Table4 closeness
  column (0.230-0.357 on21 nodes) is inconsistent with equation (3), which would
  give roughly0.02, and Fig.2 was not transcribed. The discrepancy is recorded,
  not worked around.
- **73 targeted expectations** pass. Broad centrality/kernel regression:
  **3,273 passing expectations**, zero failures/errors,21 warnings -- the20
  pre-existing ones plus the new `cograph_undefined_measure` warning that
  `centrality(K3, type = "all")` now legitimately raises in
  `test-centrality.R:595`. The complete suite: **23,773 passing expectations**,
  zero failures,201 warnings (200 pre-existing plus that one),42 existing
  skips. Package build/`R CMD check` was not rerun.

New files pass lint apart from the pre-existing single-file
`object_usage_linter` notes for internal `.cg_*` helpers, which batch39 and
batch40 files also produce; `R/centrality.R` has seven fewer lints than at
HEAD. The new Rd file passes checking; roxygen retains only the existing
`.cg_adjlist` link diagnostic. The catalogue count moves to179.

Complete evidence and retained fixtures are in
`local_testing_and_equivalence/batch41/SOURCE_AUDIT.md`. These are cograph
fixtures, not the paper's two case-study networks. No author-software parity
claim is made, and none is possible: the paper ships no code.

## Batch40: DK-based gravity model

Added `dkgm` / `centrality_dkgm()`.
**49/160 original candidates implemented,111 pending**;178 runtime measures
and152 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.

Primary and only source: Li and Huang2021,
[DOI10.1038/s41598-021-01218-1](https://doi.org/10.1038/s41598-021-01218-1),
equations1-3, equation4, Algorithm1, Figure1 and Tables1-5. Printed PDF pages
2,3 and4 were rendered and visually inspected; the Methods k-shell prose was
read from the text extraction. PDF SHA256
`99830c24e048e5ed0d975c1505b3421b30e94b640ebc390e6ae07f74538c1b98`. No author
software was located; the paper's data-availability GitHub link is a dataset
repository and is not author code.

The mass is the degree k-shell index `DK = k + k_s*`, where
`k_s* = k_s + p/(max_k q(k) + 1)` refines the shell by the stage at which the
node left it. Scores are the ordinary truncated squared-distance gravity sum
with that mass at both ends.

The source disagrees with itself about the peeling rule: Algorithm1 prints
"degree k" while its stage loop ends "until All remaining nodes in G have
degree > k", and the Methods define k-shell by removing degree "k <= 1". Strict
equality cannot terminate on a three-node path. cograph follows the at-most
reading, the only terminating one, and it reproduces every printed table.
Removal inside a stage is simultaneous. Because Algorithm1 starts the level at
one, isolates land in the one-shell rather than the zero-shell `coreness`
reports; they carry no edges, so nothing else changes. Stages restart per shell
while `max_k q(k)+1` is one global denominator, so adding a disconnected
component that peels in more stages changes every raw score. That is the
published definition, not a cograph choice, and it is tested.

Default `dkgm_radius = 2` is the printed Table5 setting and one of the two
values the discussion recommends generally. A dedicated parameter is used
rather than the shared `gravity_radius`, whose package default is3, because a
shared parameter cannot carry a measure-specific default and the wrapper must
agree with `centrality(measures = "dkgm")`; this mirrors `mcgm_radius`.
`"auto"` applies the paper's own equation4 `R* ~ <d>/2` with explicit cograph
rounding (ties to even, minimum one) over reachable pairs. Simple undirected
unweighted skeleton; loops and parallels removed; weights, mode, inversion,
cutoff and `gravity_mass` ignored; isolates and radii below one score zero.

- **257,700 comparisons,1,194,530 values,zero failures**, maximum scaled error
  7.7715611723761007e-16 at tolerance1e-11. All5,532 retained graphs cross nine
  radius settings:0,.5,1,1.9,2,3,Inf,NULL,auto.
- References: an independently written NetworkX node-deletion peeling loop,
  cross-checked against `core_number`; BFS distances; exact `Fraction` sums.
  Small graphs additionally use exhaustive induced-subset cores and exact
  integer-power distances. Main API, wrapper, maximum scaling, permutations
  and input projections are all checked.
- **46 published entries reproduce**: nine degrees, nine k-shells, nine removal
  stages, nine improved shell indices, nine DK values, nine R=2 scores and the
  printed node-3 worked example. These are numbers printed in the paper, not
  values newly computed on a published figure.
- **65 targeted expectations** pass. Broad centrality/kernel regression:
  **3,200 passing expectations**, zero failures/errors,20 existing warnings.
  The complete suite: **23,701 passing expectations**, zero failures,200
  existing warnings,42 existing skips. Package build/check was not rerun.

New files pass lint apart from the pre-existing single-file
`object_usage_linter` notes for internal `.cg_*` helpers, which batch39's file
also produces. The new Rd file passes checking; roxygen retains only the
existing `.cg_adjlist` link diagnostic. All49 ledger calls execute and the
catalogue count moves to178.

Complete evidence and retained fixtures are in
`local_testing_and_equivalence/batch40/SOURCE_AUDIT.md`. These are cograph
fixtures, not the paper's ten empirical networks. The SIR benchmark, Figures2-3
and Tables6-9 were not reproduced. No author-software parity or spreading
performance claim is made.

## Batch39: mixed and extended mixed gravitational centralities

Added `mixed_gravity` / `centrality_mixed_gravity()` and
`extended_mixed_gravity` / `centrality_extended_mixed_gravity()`.
**48/160 original candidates implemented,112 pending**;177 runtime measures
and151 Zoo mappings. Original160 rows/fields and CSV SHA256 are unchanged.

The pinned definition is Li and Huang2022's explicit reproduction,
[DOI10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3),
equations5-8 and reference19. Printed PDF page2 was visually inspected.
The original attribution is Wang, Li and Xia2018,
[DOI10.1016/j.amc.2018.04.028](https://doi.org/10.1016/j.amc.2018.04.028).
The original full equations and author software have not been inspected;
that limitation is explicit in the functions, catalogue and ledger.

MGC uses focal core-number mass and partner-degree mass with squared
hop-distance penalties. Default inner radius three follows the reproduced
research equations. EMGC sums immediate neighbors' raw MGC scores, so its
contributions can reach one hop farther. Radius one exposes the Zoo summary's
literal immediate-neighbor inner sum; it is not equivalent to the default.

Both use the simple undirected unweighted skeleton and original-graph
features, excluding loops and collapsing parallels. Weights, mode, inversion,
cutoff and gravity_mass are ignored. Isolates and radii below one give zero;
unreachable partners are omitted. Fractional radii are literal, NULL/Inf
includes all reachable nodes and auto is the explicit cograph rounded-half-mean
heuristic. Optional maximum normalization follows the complete raw score.

- **415,824 comparisons,1,917,190 values,zero failures**, maximum scaled error
  7.3295206653509e-16 at tolerance1e-11. All5,532 retained graphs cross both
  measures and nine radius settings:0,.5,1,1.9,2,3,Inf,NULL,auto.
- References: NetworkX core_number/BFS plus independently written direct
  and nested pair sums. Small graphs also use exhaustive induced-subset cores,
  exact integer-power distances and Fraction rational arithmetic. Main API,
  wrappers, maximum scaling, permutations and input projections are checked.
- **180 additional raw/normalized reference values** on the published
  nine-node Figure1 match exact rational calculations from768 enumerated
  simple paths and exhaustive induced subsets. These are newly calculated
  MGC/EMGC values, not a published numerical table. MCGM's table is not reused.
- **86 targeted expectations** pass. Broad centrality/kernel regression:
  **3,135 passing expectations**,zero failures/errors,20 existing warnings.

New files pass lint and both new Rd files pass checking; roxygen reports
only the existing `.cg_adjlist` link diagnostic. The standalone catalogue
renders177 measures with the source/radius conventions, and all48 ledger calls
execute. Package build rebuilt all vignettes. The scoped as-CRAN check passed
with **zero errors,zero warnings,2 existing NOTEs** (the MOTIFS planning file
and current-time verification). Tests ran separately; examples/manual and
vignette reruns were skipped in that check. The installed catalogue was also
verified for177 measures and the source/radius conventions.

Complete evidence and retained fixtures are in
`local_testing_and_equivalence/batch39/SOURCE_AUDIT.md`. These are cograph
fixtures, not Zoo's648 ICON networks. No original-author software parity,
Zoo-output parity or general spreading-prediction claim is made.

## Batch 38: multi-characteristics gravity model (historical counts)

Added `mcgm` / `centrality_mcgm()` with default radius two and the published
adaptive coefficient. **46/160 original candidates implemented,114 pending**;
175 runtime measures and149 Zoo mappings. All original160 rows/fields and
the original CSV SHA256 remain unchanged.

Primary: Li and Huang2022,
[DOI10.1038/s41598-022-14005-3](https://doi.org/10.1038/s41598-022-14005-3),
equations17-18, Algorithm1 and Tables1-2. Original PDF pages4-5 were visually
inspected. The complete source audit is in
`local_testing_and_equivalence/batch38/SOURCE_AUDIT.md`.

Mass combines globally maximum-scaled degree, coreness and eigenvector
features. The adaptive coreness coefficient uses their global medians.
Gravity contributions use squared hop-distance penalties. Other inputs
use a simple undirected unweighted skeleton. Radius and coefficient
parameters are explicit; coefficient one recovers equation16.

For disconnected graphs the explicit cograph eigenvector convention is a
uniform-vector projection onto the global dominant eigenspace. Weaker
components have zero eigenvector feature. Numerically tied roots share the
projection; the documented tolerance is64 machine epsilons times n times
max(1,rho). Global medians are retained. Automatic alpha errors when edges
exist but median coreness is zero; an explicit `mcgm_alpha` can override it.
Edgeless graphs and radii below one use a zero empty-interaction extension.
These conventions are independently checked and are not attributed to the
original authors. Optional maximum normalization follows the complete sum.

- All36 published feature/score entries and the printed coefficient match
  at their stated precision. Full precision and printed values are retained.
- **537,760 comparisons,2,500,254 values,zero failures**, maximum scaled error
  2.93856050603836e-11 at tolerance1e-10. All5,532 retained graphs cross
  radii0,.5,1,2,3,Inf and alpha auto,0,1,2. Another88 undefined-alpha
  configurations across22 graphs correctly raise the documented error.
- References: maintained NetworkX shifted eigenvector iteration, BFS and
  core_number; independent exact rational characteristic-root/mpmath100
  resolvent projection; exhaustive induced-subset cores and exact integer
  adjacency-power distances on small graphs. Public wrapper, maximum scaling,
  permutations and input projections are checked separately.
- **1,086 high-precision stress values** pass, maximum relative error
  6.431489e-15. Another24 configurations correctly reject overflowing raw
  scores. All1,272 decimal reference values and four input graphs are saved.
- **62 public expectations** pass. Broad centrality/kernel regression:
  **3,049 passing expectations**,zero failures/errors,20 existing warnings.

New files pass lint and the new Rd file passes checking. The existing main
centrality Rd still produces its non-ASCII em-dash diagnostic; roxygen also
reports the existing `.cg_adjlist` link warning. The standalone catalogue
renders175 measures with the new conventions, and all46 ledger calls execute.
Package build rebuilt all vignettes. The scoped as-CRAN check completed with
**zero errors,zero warnings,2 existing NOTEs** (the MOTIFS planning file and
current-time verification). Tests ran separately; examples/manual and vignette
reruns were skipped in the check. All package Rd checks passed. The installed
catalogue was also verified for175 measures and MCGM conventions.
The fixture collection is cograph's, not Zoo's648 ICON networks. No author
software parity or general predictive-performance claim is made.

`candidate_review38/SOURCE_AUDIT.md` records the accessible reproduction of
mixed and extended mixed gravitational centrality in this paper's equations
7-8. Their original full equations remain unread; both candidates stay pending.

## Batch 37: SpectralRank (historical counts)

Added `spectralrank` / `centrality_spectralrank()` with scalar or node-vector
`sr_prior`, default0. **45/160 original candidates implemented,115 pending**;
174 runtime measures and148 Zoo mappings. Original160 rows/fields and
SHA256 are unchanged.

Primary: Xu et al.2019 (online2018),
[DOI10.1109/TCYB.2018.2861568](https://doi.org/10.1109/TCYB.2018.2861568),
sectionIII-A equations4–8 and the diagonal-prior definition. The author PDF
was downloaded/read and pages3–4 visually inspected. Source hash and full
details are in `local_testing_and_equivalence/batch37/SOURCE_AUDIT.md`.

The score is the outgoing positive Perron vector after adding a unit-linked
ground node. Raw normalization includes the ground; optional package
normalization rescales ordinary nodes alone. Priors are nonnegative diagonal
information, distinct from the nonnegative interaction-weight extension.
Direction, loops, parallel aggregation, tiny-matrix direction inference,
empty/isolated graphs and numerical-error policies are explicit.

The audit retains two source discrepancies: literal unshifted iteration
oscillates on an edgeless graph, while its Perron vector is well-defined;
weighted Algorithm1 constructs Atilde+P but omits P in its update line.
The implementation follows the eigenvector definition and weighted prose.
No author-software or general predictive-performance parity is claimed.

- **115,404 comparisons,537,990 values,0 failures**, max scaled absolute
  error1.2858603e-12. References: NetworkX shifted iteration on a reversed
  augmented graph, exact rational characteristic roots plus a90-digit
  resolvent, and SciPy. All5532retained graphs have three prior/weight cases.
- **150 high-precision stress values** pass, max relative error2.975568e-11.
- **78 public expectations** pass. Figure2's two printed outgoing update
  equations are checked independently, and21 native scores on that graph
  agree with exact-root references. These are calculated reference scores,
  not a published numerical table. The fixtures are not Zoo's ICON networks.

Broad centrality/kernel regression: **2,987 passing expectations**,0 failures
or errors,20 existing warnings. New files pass lint and Rd checking.
The catalogue renders174 measures with SpectralRank's conventions, all45
ledger calls execute, and package build rebuilt all vignettes. The scoped
as-CRAN check completed with **0 errors,0 warnings,2 existing NOTEs**:
the MOTIFS planning file and current-time verification. Tests ran separately;
examples, manual and vignette reruns were skipped in that check. The installed
catalogue was checked for174 measures and SpectralRank's conventions.

The four MCDE-family candidates remain pending. `candidate_review37`
records that DBLP's apparent unpaywalled link is a hidden generic lookup;
OpenAlex currently supplies no full-text location, and the coauthor profile
offers only the abstract. The original entropy sign and other conventions
still need primary verification; these availability checks do not establish
that no accessible copy exists elsewhere.

## Batch 36: ControlRank (historical counts)

Added `controlrank` / `centrality_controlrank()`.
**44/160 original candidates implemented,116 pending**;173 runtime measures
and147 Zoo mappings. All original160 rows and fields remain unchanged,
including the original CSV's SHA256.

Primary: Zhou, Yu and Lu2019 (online2018),
[DOI10.1109/TCSII.2018.2845940](https://doi.org/10.1109/TCSII.2018.2845940),
Theorem3 and Figure1. The institutional published PDF was read and pages2–4
visually inspected. Its SHA256 is recorded in
`local_testing_and_equivalence/batch36/SOURCE_AUDIT.md`.

The score is the smallest eigenvalue after grounding each node in the
symmetric part of the row-Laplacian, retaining original degrees. Directed
input uses outgoing strength and can have negative scores. Weights are
nonnegative interactions; loops are removed and remaining parallels sum.
Undirected disconnected scores are all zero; singleton zero extends an
undefined empty minor. Optional normalization divides by a positive maximum,
otherwise preserves raw units. This O(n^4) measure is marked costly.

Independent evidence:

- All11 published bi-star values reproduced at printed precision, with
  SciPy eigensolves, exact-polynomial roots and incidence singular values.
- All5,532 retained batch30 graphs tested with binary and weighted variants:
  **79,578 comparisons,374,561 values,0 failures**, maximum scaled absolute
  error7.308779e-13. SciPy constructs row-Laplacians independently; SymPy
  rational characteristic-polynomial root intervals verify small graphs
  and larger ambiguous zeros; NetworkX incidence matrices verify undirected
  cases. These are cograph fixtures, not Zoo's648 ICON networks.
- **78 mpmath150 values** cover uniform scales1e-300 through1e300 and weak
  bridges. Maximum relative error8.2865371e-8 occurs at bridge weight1e-9;
  uniform scales remain below3e-16. Small eigenvalues lose relative precision;
  unresolved positive spectra and unrepresentable weight ranges error.
- **62 public expectations** cover analytical families, published values,
  orientation, signed normalization, weights, labels, empty/disconnected
  graphs, precision limits and explicit directed input at tiny weight scales.

The audit caught and fixed normalization of spurious tiny positive values
at exact structural zeros. Independent minor blocks now preserve those zeros;
initial failures and all six cases are retained. A separate stress failure
identified the shared parser's approximate symmetry detection: very small
asymmetric matrices need explicit `directed=TRUE` or a directed igraph object.
That input convention is documented and tested; the original failed artifact
is retained. No tolerance was loosened to pass these failures.

An exact commutator counterexample shows that adding diagonal feedback to
a normal directed Laplacian need not preserve normality. A separate
two-node example distinguishes the grounded score from finite-gain decay.
The implementation claims the defined spectral score, not a general directed
stability theorem, controller simulation, set optimization, or author-code
parity. Full details and reproducible counterexamples are in the batch audit.

Broad centrality/kernel regression: **2,909 expectations pass**,0 failures
or errors,20 existing warnings. New files pass lint and Rd checking.
The standalone catalogue renders173 selectable measures with ControlRank
and its conventions, and all44 implemented ledger calls execute. The
original160 CSV rows/fields and SHA256 remain unchanged. Package build
rebuilt the vignettes, and the installed catalogue was verified. The scoped
as-CRAN check finished with **0 errors,0 warnings,2 existing NOTEs** (the
MOTIFS planning file and current-time verification). Tests ran separately;
examples, manual and vignette reruns were skipped in that check.

## Batch 35: map equation centrality (historical counts)

Added `map_equation` / `centrality_map_equation()` with explicit flow and
coding conventions. **43/160 original candidates implemented,117 pending**;
172 runtime measures and146 Zoo mappings. The original160 CSV rows, fields
and SHA256 remain unchanged.

Primary: Blocker, Nieves and Rosvall2022,
[DOI10.1007/s41109-022-00477-9](https://doi.org/10.1007/s41109-022-00477-9),
published HTML and arxiv2201.12590v2. Pages4–6 visually inspected, including
eq2/4–12, Figures2–3 and TableI. Flow conventions additionally pinned to
Lambiotte-Rosvall2012 [DOI10.1103/PhysRevE.85.056107](https://doi.org/10.1103/PhysRevE.85.056107),
arxiv1112.5252v2 eq2–3/9–11, with page4 visually inspected.

The audit found a material source discrepancy. The equations include the
module-exit symbol in codebook usage; Infomap2.15.1's `modularCentrality()`
uses only its parent's node-visit flow. The public parameter makes this
choice explicit: `map_convention="paper"` is the exit-inclusive default;
`"infomap"` reproduces the visit-only implementation and all24 printed table
values. Both have independent numerical definitions and tests. The paper
convention reproduces Figure2's full silenced code lengths2.209812/1.988722
(rounding to the printed2.21/1.99), but only12 of24 table entries at printed
precision. These discrepancies are preserved in the audit, not hidden.
A four-node path example proves that choosing a convention can reverse ranks.
The Zoo summary subtracts from full original code length and consequently
adds a different self-information term; unreleased Zoo-code parity is not claimed.

Unrecorded link teleportation is the default flow; recorded uniform node
teleportation is also implemented. Damping defaults to0.85, with finite
values from0 inclusive to1 exclusive. Directed nonnegative interaction
weights are retained, undirected edges become reciprocal arcs, loops are
removed, and remaining parallel weights sum after the simplify rule.
Mode/inversion/cutoff are ignored. Dangling nodes teleport. Edgeless
unrecorded scores are zero by explicit extension; recorded flow is uniform.
Isolates can have positive recorded scores; singleton scores are zero.

Partitions stay fixed. NULL means one module; named membership is validated
and reordered. Globally unique leaf-module labels also support hierarchical
partitions, as checked against a real three-level Infomap codebook tree.
No partition optimization is performed. Scores are nonnegative bits, with
zero boundary values; optional maximum normalization changes their units.
Stable arithmetic retains tiny remaining-symbol masses instead of obtaining
them by subtraction. Extreme unrepresentable weight ranges or failed flow
solves raise errors. Dense O(n^3) time/O(n^2) memory, or O(n^2) time for
unrecorded undirected flow.

Evidence in `local_testing_and_equivalence/batch35/`:

- Pinned author notebook commit e943aefddf5266da6d859dbf54d16212a63571b2;
  executed Infomap2.15.1 wheel and matching source tarball checked against
  PyPI SHA256 digests. Full source/asset hashes are in `SOURCE_AUDIT.md`.
  The external package is installed in a temporary isolated environment;
  no author code was copied into production R.
- All5,532 saved batch30 inputs copied: exhaustive undirected graphs0–5,
  exhaustive directed graphs0–4, six canonical and260 random graphs up to50.
  Seed20260929 adds weighted inputs/partitions/damping configurations.
  Exact6,546 cases are saved, with matrices, membership, damping and labels.
  These are cograph fixtures, not Zoo's648 ICON benchmark.
- **123,060 comparisons,779,764 values,zero failures**. NetworkX PageRank,
  direct old/new silent code lengths, and external Infomap. Paper-oracle
  maximum error1.296963e-12; overall max4.379724e-11. Ordinary tolerance1e-10;
  external Infomap/node flows1e-9. Main API, wrapper, maximum normalization,
  named and permuted inputs verified across both models/conventions.
- All78,632 module-exit values independently agree with Infomap below1e-9,
  maximum4.655099e-11. Real three-level hierarchy:24 scores, max4.163336e-17.
- Published table: all24 values matched by the explicit Infomap convention;
  both equation/table results and full Figure2 code lengths retained.
  700-digit mpmath stress at weight ratios1e-10/1e-50/1e-250/1e-300:
  48 node values, max relative error1.997174e-16. Infomap's rounding to zero
  for some tiny nonzero values is retained separately; no bitwise parity claim.
- New public tests:63 expectations, zero warnings/errors. Broad regression:
  **2,845 passing expectations,zero failures/errors**,20 existing warnings.
  A malformed-input test also improved the shared damping scalar validator.
  New R/test/reference lint and Rd checks pass; only the existing roxygen
  `.cg_adjlist` warning remains. All43 implemented ledger calls execute.
- Standalone and installed catalogue contain172 measures, both conventions
  and both flow models. Source build generated all vignettes. CRAN-style
  package check: **0 errors,0 warnings,2 NOTEs**, the existing top-level
  planning file and an environment current-time verification failure.
  `/private/tmp/cograph-batch35-check/cograph.Rcheck/00check.log`.
  Tests ran separately; examples/manual/vignette reruns were skipped.

The full candidate goal remains active and incomplete.

## Batch 34: Node and Neighbor Layer Information (NINL)

Added `ninl` / `centrality_ninl()` with `ninl_order = 3` and
`ninl_radius = NULL`. This covers one original candidate: **42/160
implemented, 118 pending**, with171 runtime measures and145 Zoo mappings.
The original160 rows and all original field values remain unchanged;
SHA256 `4006d16ec1d0d40db43aab48cf0301cf2057604ffffff9fbedf4a3ea2173aae2`.

Primary definition: Zhu and Wang (2021), Symmetry13,1570,
[DOI10.3390/sym13091570](https://doi.org/10.3390/sym13091570).
Original publisher PDF pages3–4 visually inspected: section2.1 equations1–2,
Figure1, Table1 and Algorithm1. Initial score is the sum of original degrees
in the closed radius-ceiling(mean path length) neighborhood; order-p score
is A^p times that vector. Default p=3 follows the paper; all nonnegative
integer orders representable consecutively in doubles are accepted.
The original13-node Figure1 and **all52 printed values** at orders0–3 are
reconstructed and reproduced by both native and independent calculations.

The public graph contract uses simple undirected unweighted topology,
removes loops/parallel edges and ignores weights/mode/inversion/cutoff.
All distinct pairs enter mean path length. Disconnected inputs have infinite
mean and therefore automatic radiusInf, including only reachable nodes;
this is an explicit cograph extension, not a source-specified convention.
Isolates return0, empty graphs return no values. Numeric radius overrides
are also explicit extensions. Adding an isolate can change the automatic
radius within a previously connected component, as documented and tested.

Native stepwise propagation preserves finite walk multiplicities and
bipartite parity. Optional maximum normalization rescales each step to avoid
raw overflow. Exactly repeated floating-point states of period1 or2 allow
skipping with parity preserved; no approximate convergence is assumed.
Worst-case O(n^3+p*n^2) time/O(n^2) memory; huge orders may be slow without
an exact repeated state. Raw overflow errors; relative underflow is possible.
An extreme-order star test caught exponent-amplified drift in initial matrix
squaring; the final stepwise algorithm passes orders2048/2049 and
2^53-2/2^53-1 without replacing the recurrence by an eigenvector limit.

Independent evidence in `local_testing_and_equivalence/batch34/`:

- NetworkX BFS neighborhoods and original degrees; NumPy exact Python-integer
  matrix powers and Fraction normalization, separate floating powers, and
  explicit individual-walk enumeration through five nodes at orders0–3.
- Full1,236 saved batch27 inputs: all1,100 simple labeled undirected graphs
  through five nodes, six canonical graphs,130 random graphs6–50. These are
  cograph fixtures, not the Zoo ICON collection or the author's datasets.
  Projection/permutation seed20260928. Orders0/1/2/3/4/7 and automatic radius
  on every graph; radii0/1/2/Inf on every graph through four nodes and all
  larger canonical/random inputs. Published graph and six canonical fixtures
  additionally checked at128/2048/2049 against normalized exact integers.
- **80,669 comparisons, 656,554 values, zero failures**, maximum scaled error
  4.440892098500626e-16 at tolerance1e-10. Includes5,616 explicit-walk and21
  large-order comparisons. MainAPI, wrapper, maximum normalization,
  permutations, and arbitrary weighted/directed projections verified.
- R4.5.2, igraph2.3.3, NumPy2.4.2, NetworkX3.6.1. Exact graph inputs,
  manifests, versions, published table and per-comparison errors retained.
- New public tests:63 passing expectations, zero warnings/errors. Broad
  centrality/kernel/coverage suite:2,782 passing expectations, no failures
  or errors,20 existing warnings. New R/reference/test lint and Rd checks
  pass. Roxygen reports only the pre-existing unresolved `.cg_adjlist` link.
- Standalone and installed catalogue include NINL, its parameters, parity
  conventions and171 selectable measures. All42 implemented ledger calls
  execute. Package-check terminal result is recorded in `SOURCE_AUDIT.md`.

The detailed source audit distinguishes published facts, cograph extensions,
independent definition-based oracles and the original worked example.
No original author software or unreleased Zoo-code parity is claimed.
The overall candidate goal remains active and incomplete.

## Batch 33: localized bridging and Extended LBC

Added `localized_bridging` / `centrality_localized_bridging()` and
`extended_local_bridging` / `centrality_extended_local_bridging()`.
The two-hop function covers original candidate Extended LBC. Corrected the
Zoo Localized bridging centrality mapping to the new one-hop function:
the legacy `local_bridging` remains the inverse-degree product, which is
numerically different and is no longer counted as that Zoo measure.

Primary definitions: Macker2016,
[DOI10.1109/MILCOM.2016.7795393](https://doi.org/10.1109/MILCOM.2016.7795393),
original PDFpages2-3 visually read: equations3-5, Figures1-3, TableI and
the explicit absence of ego-size normalization. Nanda and Kotz2012,
[author chapter](https://cs.dartmouth.edu/~kotz/research/nanda-lbc-book/nanda-lbc-book.pdf),
sections7.2.4-7.2.6/equations7.7-7.8 restate their2008 LBC definition;
printedp205 visually read. Original2008 conferencePDF was not read.

Both multiply ego betweenness by the original-graph bridging coefficient.
The two-hop ego network includes every induced edge and permits paths up to
four edges long; this is not global betweenness cutoff2. Unordered pairs,
endpoints excluded, no ego-size scaling. Simple undirected unweighted
projection: either arc creates an edge; loops/parallels removed and weights,
mode/inversion/cutoff ignored. Isolates score0 as an explicit extension of
the undefined coefficient; leaves and cliques score0. Macker's separate
weighted link-quality/path-cost model is outside the implementation.
Both have dense worst-case O(n^4) time/O(n^2) memory; two-hop is marked costly.

Independent verification in `local_testing_and_equivalence/batch33/`:

- NetworkX induced ego graphs/Brandes versus native one-hop common-neighbor
  multiplication and two-hop BFS counts of paths through the focal vertex.
  A separate reference enumerates all shortest paths and uses exact Fraction
  shares on graphs through8vertices. Coefficients use original graph degrees.
- All1,100simple labeled undirected graphs through5vertices,6canonical
  fixtures and130random graphs through50:1,236retained inputs. These are
  cograph fixtures, not the Zoo ICON dataset. Seed20260927 for API projections.
- **14,638 comparisons,87,338 values,zero failures**, max scaled error
  1.110223e-15 at tolerance1e-10. Includes2,278explicit path comparisons,
  main API, wrappers, maximum normalization, permutations and asymmetric
  directed/weighted/loop projections.
- All11rows of Macker's original synthetic table reproduced. The figure's
  adjacency and scores are retained in macker_figure1.csv/published_table.csv.
  Table ego credits are published values; coefficient fractions are derived
  from the figure. NodeI's credit falls from1 to0.5 for two hops, as published.
- Public52expectations pass. Broad regression: **2,719passed,zero failures
  or errors**,20existing warnings. New R/test/reference files lint clean;
  both new Rd checks clean. Catalogue rendered and inspected for both new
  entries, legacy-score distinction and170selectable measures.
- Package check finished with zero errors, zero warnings and one existing
  note for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests executed separately; vignettes
  built during package build. Installed catalogue also verified. Log:
  `/private/tmp/cograph-batch33-check/cograph.Rcheck/00check.log`.
- All41implemented original-cohort calls run on karate. All160original
  fields and the original CSV hash are unchanged. Zoo mappings144;
  **41/160implemented,119pending**. The full goal remains active.

Sources, SHA256 hashes, conventions, an initial test-only matrix-dimension
error and reference limitations are recorded in `batch33/SOURCE_AUDIT.md`.
No original author implementation or unreleased Zoo code parity is claimed.

## Batch 32: BG-index and beta power (historical counts)

Added `beta_measure` / `centrality_beta_measure(x, beta_direction="positive")`.
The negative variant reverses the graph. One implementation covers two
original-cohort labels, BG-index and beta-measure.

Primary definition: van den Brink and Gilles1992, FEW565, definition2.1 and
example2.2, printedpp3-4 visually read. Published2000 definition2.1 agrees,
[DOI10.1016/S0378-8733(00)00019-8](https://doi.org/10.1016/S0378-8733(00)00019-8).
The original diamond edges12,13,24,34 yields(2,1/2,1/2,0), reproduced in the
public test. Boldi and Vigna2014's negative variant was visually read on
p26 of arxiv1308.2140, including footnote23 explaining graph reversal.

Positive beta sums reciprocal successor in-degrees; negative beta sums
reciprocal predecessor out-degrees. Uses simple unweighted directed graphs,
reciprocal arcs for undirected inputs; loops and parallel arcs removed.
Weights/mode/inversion/cutoff ignored. Isolates score zero; disconnected
components remain independent before maximum normalization. The original
weighted extension is not included. Dense O(n^2) time/memory after input
edge processing. Raw totals count vertices with positive in-degree (positive
variant) or out-degree (negative variant); normalization divides by maximum.

Independent verification in `local_testing_and_equivalence/batch32/`:

- 5,532 retained input graphs: all simple labeled undirected graphs through
  five vertices, directed graphs through four, canonical fixtures and random
  graphs through50. These are our fixtures, not the Zoo ICON benchmark.
- NetworkX predecessor/successor queries with exact Fraction arithmetic.
- A separate joint-choice oracle enumerates every possible selection of one
  predecessor per dominated node, then counts selections per actor. It never
  sums inverse neighbor degrees. 10,552 comparisons on graphs through five
  vertices cover174,888 complete joint outcomes across both orientations.
- Main API, wrapper, maximum normalization, permutations, dominance totals
  and weighted/loop projection: **76,936 comparisons,317,294 values,zero
  failures**, maximum scaled error4.84461e-16 at preselected tolerance1e-10.
- 39 public expectations pass; broad centrality/kernel regression has
  **2,665 passed,zero failures/errors**, with20 existing graph-domain warnings.
- New R, test and reference files lint clean; new Rd checks clean. Catalogue
  rendered and inspected for the new entry, parameter and168 selectable
  measures. All40implemented ledger calls ran on karate. The160original
  rows and every original field remain unchanged.
- Package check completed with zero errors, zero warnings and one existing
  note for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests ran separately; vignettes built
  during package build. Installed catalogue entry, direction parameter and
  168-measure count verified. Log:
  `/private/tmp/cograph-batch32-check/cograph.Rcheck/00check.log`.

Source URLs, downloaded-file hashes, conventions and reference distinctions
are in `batch32/SOURCE_AUDIT.md`; results and versions are retained. These
references establish numerical definition agreement, not author-software
parity or predictive performance. Coverage is now **40/160 implemented,
120pending**,168runtime measures and143Zoo mappings. Full goal remains active.

## Candidate review 32: unresolved source differences

Research artifacts in `local_testing_and_equivalence/candidate_review32/`
retain 40 values and graph fixtures; `counterexamples.py` passes all assertions.
Four candidates remain pending, with notes in the original-cohort ledger:

- Inward accessibility: Costa's earlier mean incoming probability differs
  from Zoo's incoming exponential entropy, already on one-step star graphs
  and isolates. The later paper cited by Zoo and stopped-walk semantics need
  resolution before implementation.
- Immediate Effects Centrality and Mediative Effects Centrality: Friedkin's
  bibliography is pinned, but institutional PDF retrieval returned zero
  bytes. The primary full text has not been read; direction, self-influence
  and disconnected-input conventions remain unresolved.
- DCC: the primary worked example rules out Zoo's probability-entropy
  reading. Its reported normalized entropy exceeds one, and its rounded
  degree-feature divisor cannot be an integer sum. The normalization radical
  and second-neighborhood figure still require visual inspection. L2 is an
  inference, not an implemented or verified replacement.

See `candidate_review32/SOURCE_AUDIT.md` for exact sources, counterexamples
and retrieval limitations. No implementation count is claimed for this review.

## Batch 31: Expected Force and modified Expected Force (historical counts)

Added `expected_force` / `centrality_expected_force()` and
`modified_expected_force` / `centrality_modified_expected_force()`.
The latter covers original candidate `Expected force (ExFm)`.
Pinned primary source: Lawyer2015,
[DOI10.1038/srep08665](https://www.nature.com/articles/srep08665),
published PDFpage2 equations1/2 visually read, plus its directed extension.
Full provenance and decisions: `local_testing_and_equivalence/batch31/SOURCE_AUDIT.md`.

**Corrected a false pre-existing mapping:** Zoo Expected Force was mapped
to `expected`, but that function sums neighbor degrees. It retains its
documented behavior; Zoo ExF now maps to the actual new `expected_force`.
The original candidate CSV remains unchanged.

The published ExF is Shannon entropy of normalized onward boundary-edge
counts after exactly two infected-to-susceptible transmissions without
recovery. Each event order and transmitting parent remains a distinct
outcome, even when three-node infected sets coincide. Boundary edges to
the same susceptible node count separately. Modified ExF multiplies by
log(alpha*degree), with defaultalpha2 and finitealpha>1. Directed inputs
retain outgoing transmission arcs; the modification uses outdegree.
Loops/parallels are removed; weights/mode/inversion/cutoff ignored.
No two-event outcomes or all-zero onward force returns0, with the latter
an explicit extension of undefined normalization. Weighted variants and
other transmission horizons are outside the verified scope.

Independent evidence:

- NetworkX explicit two-event transmission enumeration, individual boundary
  counts, normalization and Python math.fsum entropy. No native cluster
  multiplicity formula or histogram is used in this oracle.
- Unchanged author C++, compiled locally using Rcpp, pinned commit
  `00a5a8ee3c26ceeb3bdd872aee24499c8da42793`. Numerical parity is checked on
  connected undirected graphs of order>=4. The code remains in temporary
  storage and is not incorporated into the package.
- The author R example counts distinct outside nodes, differing from the
  paper/C++ boundary-edge definition. A retained diamond-with-leaf example
  gives1.366159 for ExF,1.329661 for the distinct-neighbor alternative, and4
  for the old `expected` function. The C++ returnsNaN on a triangle, where
  cograph's explicit zero-force extension returns0. No parity claim hides
  these differences.
- 5,532 retained matrices: all1,100 simple labeled undirected graphs through5,
  all4,166 directed graphs through4, six canonical graphs includingkarate,
  and260 random graphs5–50. These are our fixtures, not Zoo's ICON benchmark.
  Sixalpha values1.0001/1.5/2/4/16/1e308; public near-one test uses1+eps.
- **141,820 comparisons,676,311 values,zero failures**, maximum scaled
  error4.359077e-15 at preselected tolerance1e-10. Includes880 author-C++ ExF
  and2,640 author-force/published-modification comparisons. Initial runner's
  empty-vector R/Python conversion issue was corrected; final run exited0.

Public43 expectations pass. Broader regression:2,626passed,zero failures
or errors,20 existing warnings. New R lint and Rd checks pass; the catalogue
rendered with both new entries and167 selectable measures. Package check:
zero errors,zero warnings,one existing top-level-file NOTE. Tests ran
separately; vignettes built during build and the installed catalogue was
verified. All38 implemented ledger calls execute on karate; all160 original
source rows/fields remain preserved. Current coverage:
**38 implemented /122 pending**,167runtime measures,141Zoo mappings.
The full original cohort remains active work.

## Batch 30: proximal betweenness (historical counts)

Added `proximal_betweenness` / `centrality_proximal_betweenness()` for the
original CSV label `Proximal betwenness` (spelling preserved).
The pinned source is Brandes2008,
[DOI10.1016/j.socnet.2007.11.001](https://doi.org/10.1016/j.socnet.2007.11.001),
section3.2 Algorithm3, read in the
[author preprint](https://www.uni-konstanz.de/algo/publications/b-vspbc-08.pdf)
dated12November2007, printed pages7–8. Formula and algorithm visually checked.
Full source hash, decisions and limitations:
`local_testing_and_equivalence/batch30/SOURCE_AUDIT.md`.

The source role credits the last intermediary before the destination;
the target role credits the first after the origin. This agrees with Zoo's
terminology. `proximal_variant` exposes source(default), target, sum and union.
Sum counts both roles; union counts a two-edge intermediary once. Endpoints
and unreachable pairs are excluded. Raw scores preserve ordered pairs even
on undirected graphs, without halving. Maximum normalization is optional.
Uses the simple unweighted graph with direction retained. Weights, mode,
inversion and cutoff are ignored; loops and duplicate arcs are removed.
Weighted geodesics and edge-distinct multigraph paths are outside the
verified scope. Counts that overflow raise an error.

Independent verification:

- NetworkX `all_shortest_paths` with explicit first/last/set position credits
  and Python rational accumulation.
- Exact integer adjacency powers locate shortest distances and count
  first/last-edge contributions without BFS or dependency accumulation.
- 5,532 retained graphs: all1,100 labeled simple undirected graphs through
  five vertices, all4,166 directed graphs through four vertices, karate and
  five other canonical graphs, and260 random graphs of sizes5–50.
  These are our fixtures, not Zoo's648 ICON networks. Seed20260924.
- Four variants and seven reference/API categories: **154,896 comparisons,
  734,020 values, zero failures**, maximum scaled error2.320688e-15 against
  preselected tolerance1e-10. Final runner exited successfully. An earlier
  final-report error after passing all comparisons is retained and explained
  in the audit; no tolerance or arithmetic adjustment was needed.
- Analytic90-layer/270-node stress fixture has about2^134.5489 shortest paths
  between end-layer vertices. Eight raw/normalized comparisons,2,160 values,
  pass with maximum scaled error1.127196e-14. Separate public overflow test
  verifies an explicit error on1,030 binary branching layers.

These are locally written definition-based references using maintained
external path enumeration, not a claim of parity with unreleased Zoo/UCINET
implementations or evidence of spreading performance.

Public40 expectations pass. Broader regression:2,583 passed, zero failures
or errors,20 existing warnings. New R lint and Rd checks pass; catalogue
rendered and inspected. Package check:zero errors,zero warnings,one existing
top-level-file NOTE; tests ran separately and vignettes built during build.
All37 implemented ledger calls execute on karate; all160 original rows and
fields are preserved. Current coverage: **37 implemented /123 pending**,
165 runtime measures,140 Zoo mappings. The full cohort remains active work.

## Batch 29: bridging capital (historical counts)

Implementation: `R/centrality-batch29.R`; public tests:
`tests/testthat/test-centrality-batch29.R`. Primary source audit, complete
input matrices and pair values, independent numerical references and version
pins: `local_testing_and_equivalence/batch29/`.

Definition read: Jackson's author preprint arXiv1711.09504v3 (2019),
section3.3, printed page18, visually inspected. The later publication is
DOI10.1007/s00355-019-01189-3 (2020). The formula sums valued information
walk losses after deleting each outgoing matrix entry individually. It
deletes only p_ij, including on symmetric inputs; the reverse remains.
Expected walk counts EInf are used, not the alternative ever-heard
probabilities PInf. Repeated use of an entry counts a walk once in that
entry's deletion loss. This differs from a matrix-power derivative and
from simultaneous deletion of an undirected dyad or whole node.

Transmission entries must be in [0,1]; rows need not sum to one. Horizon
defaults to two; zero is an empty-sum extension. Source-destination values
default to all ones and may be finite nonnegative matrices, with labeled
dimensions reordered to the input. Loops remain after generic processing;
remaining parallel weights add into one probability entry. Mode, inversion
and cutoff are ignored. Isolates, zero horizon and zero values give zero.

Native code tracks walks before and after their first use of an entry,
avoiding subtraction of nearly equal powers. It has dense O(m T n³) time
and O(n²) memory and is marked costly. Nonrepresentable intermediate masses
raise errors. Maximum normalization scales values first to avoid raw valued
score overflow, but cannot resolve every intermediate range limitation.

Verification:

- 381 retained probability matrices and personalized value matrices,
  seed20260923. Three domains each include all76 labeled simple graphs
  through four vertices,30 random graphs n5–8,20 random graphs n10–20,
  and karate: binary undirected, weighted undirected and directed with loops.
  Probability weights are quarter steps; pair values are unit or integers0:3.
  Horizons0,1,2,3,4. These are our fixtures, not Zoo ICON datasets.
- 3,810 NetworkX deletion/NumPy matrix-power references;2,520 explicit
  distinct-arc walk enumerations;3,810 each wrapper, normalization and
  permutation/mode/inversion/cutoff checks. All17,760 comparisons and
  106,350 values pass, max scaled error3.331e-16 at tolerance1e-10.
- 40 public expectations pass, including analytic loop/pair cases that
  distinguish traversal multiplicity and undirected-edge deletion, named
  values, raw overflow normalization and explicit underflow rejection.
- The initial broad metadata test used weights1:9, outside this measure's
  probability domain. Its fixture now uses nonuniform probabilities .1:.9
  for bridging capital; production validation remains unchanged. The initial
  regression result is retained at `/private/tmp/cograph-batch29-regression-initial.rds`.
  Final broad rerun passes:2,543 expectations, zero failures/errors,
  20 existing warnings.
- New R files lint clean and new Rd checks pass. All36 ledger calls ran on
  karate, all160 original CSV rows/fields remain preserved, and the standalone
  catalogue includes bridging capital and164 selectable measures. Its example
  explicitly uses `weighted=FALSE` for interaction-count data and was executed.
- Installed catalogue verified. Package check completed with zero errors,
  zero warnings and one existing NOTE for `MOTIFS-AUDIT-FIXING-PLAN.md`.
  Tests ran separately; vignettes built during package build. Log:
  `/private/tmp/cograph-batch29-check/cograph.Rcheck/00check.log`.

Coverage:36/160 original candidates implemented,124 pending; runtime164
measures,139 Zoo mappings. The full cohort remains active work.

## Batch 28: Coleman-Theil hierarchy

Implementation: `R/centrality-batch28.R`; public regressions:
`tests/testthat/test-centrality-batch28.R`. Full source decisions, primary
URLs and fingerprints, retained matrices and numerical references:
`local_testing_and_equivalence/batch28/`.

Primary source actually read: Burt's STRUCTURE version 4.2 Reference Manual
(copyright 1991), printed pages 181–183, reproducing the definitions attributed
to Burt1992 equations 2.4/2.9. The original manual was downloaded and page183
visually inspected. It explicitly sets isolates to zero and one-contact nodes
to one. JUNG documents NaN for isolates; that behavior is not replicated.
The JUNG source was read but not executed, and no JUNG parity is claimed.

The index measures concentration of dyadic Burt constraints relative to their
mean over contacts. Investment proportions use mutual strengths z_ij+z_ji
and the full supplied graph, including alters' outside ties. Organizational
factors are fixed at one, matching the Zoo formula. The author's later
clustered-actor example uses other factors and is not claimed as a numerical
match. Contacts are distinct positive mutual neighbors; zero ties and loops
are removed, while remaining parallel weights sum after generic simplification.
Mode, shortest-path inversion and cutoff are ignored. Default values lie in
[0,1]; maximum normalization is an additional optional transformation.

Dense native O(n³) arithmetic uses a matrix product for indirect investment,
then a stable entropy expression near uniformity. Relative local-constraint
deviations within16 machine epsilons are treated as uniform. Global weight
scaling precedes mutual sums; loss of positive weights/investments raises an
error. Squared-constraint underflow uses the zero-log-zero limit.

Verification:

- 1,508 retained matrices, seed20260922: the full1,236 binary batch27
  cohort (including all1,100 labeled simple graphs through five vertices)
  plus136 weighted undirected and136 weighted directed graphs with loops.
  Complete copies are saved in this batch. These are our fixtures, not ICON.
- 1,508 NetworkX `local_constraint` references with separate direct entropy;
  1,100 exact Fraction constraint references;1,508 each of wrapper,
  normalized, permutation/mode/inversion and weight-scale comparisons.
  Six mpmath100-digit weighted-star checks supplement the entropy oracle.
- All8,646 comparisons/65,301 values pass. Ordinary tolerance1e-10,
  maximum scaled error2.843e-14. Near-uniform positive scores are checked
  by relative error at a preselected1e-7 tolerance; maximum1.221e-8 at a
  score of order1e-16. A zero production result would fail that check.
  No tolerance was relaxed after a failed comparison. The audit separates
  independent numerical references from API properties and explains the
  direct entropy oracle's small-value rounding limitation.
- Public40 expectations pass with no warnings. Broad centrality/kernel
  regression:2,501 passed,0 failures/errors,20 existing warnings. New R
  files lint clean and new Rd check passes. All35 ledger calls ran on
  karate; all160 original rows and source fields are preserved. Standalone
  catalogue includes Coleman-Theil and163 selectable measures. Installed
  catalogue also verified. Package check completed with zero errors, zero
  warnings and one existing NOTE for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests
  ran separately; vignettes built during package build. Log:
  `/private/tmp/cograph-batch28-check/cograph.Rcheck/00check.log`.

Coverage:35/160 original candidates implemented,125 pending; runtime163
measures,138 Zoo mappings. The full cohort remains active work.

## Batch 27: X-degree

Implementation: `R/centrality-batch27.R`; public regressions:
`tests/testthat/test-centrality-batch27.R`. Full audit, pinned author source
and MIT license, retained input matrices and independent reference scripts:
`local_testing_and_equivalence/batch27/`.

Definition: Torres et al. (2021), DOI10.1137/20M1352132, Proposition 3.8,
equation 3.15. The original published PDF was downloaded from an author's
site and read; page 661 was rendered and visually inspected. The score is
the squared sum of neighbor excess degrees minus their sum of squares,
using original degrees before focal deletion. It also equals the sum of
entries of DFE, using the nonbacktracking blocks defined by equations 2.1
and 3.1. Native code accumulates nonnegative pair products using prefix
sums; it does not subtract large squared quantities.

The source domain is simple, undirected and unweighted. Other inputs are
explicitly projected to that skeleton. Direction, weights, mode, inversion
and cutoff are ignored; loops and repeated connections are removed. The
formula gives zero on isolates, leaves and every node in a star. Components
are independent before maximum normalization. Empty graphs return no
scores, and an all-zero vector stays zero under normalization. Aggregation
is O(n+m) after neighbor construction, with O(n²) dense preparation. This
is the static score, not the paper's iterative immunization routine.

Verification:

- The published paper links author repository `leotrs/inbox`, pinned at
  commit `401bf028b56886ce67b15b2c5eacd7a28dbb4a48`. Complete `inbox.py`,
  README and MIT license are retained unmodified. The exact `x_degree`
  function is extracted via Python AST and executed unchanged with NumPy,
  avoiding an unused module-level `pqdict` dependency. The audit pins hashes.
- 1,236 retained graphs with seed 20260921: every one of the 1,100 labeled
  simple graphs through five vertices, six named fixtures including karate,
  and 130 random graphs of size 6–50. These are our fixtures, not Zoo ICON.
- 1,236 author-function comparisons; 1,139 independent nonbacktracking
  block-matrix products; 1,139 explicit four-edge nonbacktracking walk
  enumerations; 1,236 each of wrapper, normalization, permutation and
  directed/weighted/loop projection checks. Matrix and enumeration checks
  cover all fixtures of size at most eight. Walks may revisit vertices;
  immediate edge reversals are forbidden, so this is not simple-path counting.
- All 8,458 comparisons and 49,358 values pass with zero observed numerical
  difference. Raw checks require exact equality; normalized tolerance is
  1e-14. API properties are distinguished from independent numerical oracles.
- 34 public expectations pass without new warnings. Broad centrality/kernel
  regression: 2,461 passed, zero failures/errors, 20 existing warnings.
  New production, test and reference R files lint clean; new Rd check passes.
  All 34 implemented ledger calls ran on karate, and all original 160 rows
  and source fields remain preserved. Standalone catalogue includes X-degree
  and 162 selectable measures. Installed catalogue also verified.
- Package check completed: zero errors, zero warnings, one existing NOTE
  for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests ran separately; vignettes built
  during package build. Log:
  `/private/tmp/cograph-batch27-check/cograph.Rcheck/00check.log`.

The related X-nonbacktracking candidate remains pending: its exact definition
uses eigenvectors after each focal removal with v^T P v=1, while the author
default computes an original-graph approximation. Degenerate remaining
matrices and undefined normalization require further source/domain decisions.
The ledger records this distinction and does not substitute an approximation.

Coverage: 34/160 original candidates implemented, 126 pending; runtime 162
measures, 137 Zoo mappings. The full cohort remains active work.

## Batch 26: LineRank

Implementation: `R/centrality-batch26.R`.
Public regressions: `tests/testthat/test-centrality-batch26.R`.
Primary-source audit, independent references, retained graphs, source
discrepancy and convergence checks: `local_testing_and_equivalence/batch26/`.

Sources: Kang et al. (2011), DOI10.1137/1.9781611972818.11, Definitions 2–6
and Algorithm 2; Kósa et al. (2015), DOI10.2298/CSIS141101092K, section 4.
Both published PDFs were read; the latter's Figure 1 was visually inspected.
Exact URLs and SHA-256 fingerprints are in `batch26/SOURCE_AUDIT.md`.

The measure computes PageRank on edge states and sums at original endpoints.
Directed states connect when the first edge's target is the second's source.
Undirected inputs use one state per original edge and ordinary line adjacency,
following the later paper's clarification. Weighted directed line edges use
products of original weights; applying this to the undirected construction
is an explicit extension. The default `linerank_aggregation="probability"`
sums stationary probabilities. The `"weight"` option additionally multiplies
by original edge weight, following the original weighted incidence formula.
These choices differ on weighted inputs and are documented separately.

Uniform dangling redistribution, zero damping, loop incidences and retained
parallel states have explicit conventions. Finite nonnegative weights are
supported; zero edges are absent. Direction is retained; mode, inversion
and cutoff do not affect results. Dense native cost is O(m³) time/O(m²)
memory, so this measure is marked costly. This is not the authors'
distributed implementation. Unrepresentable positive transition ranges,
unstable systems and raw weighted-score overflow cause errors.

The original Algorithm 2 confuses row and column normalization. A retained
four-cycle-plus-chord counterexample has literal pseudocode stationary mass
1.029518 instead of one. Production follows the random-walk definition,
corroborated by the later primary source; no literal pseudocode parity is
claimed. Both literal and definition-based node values are retained.

Verification:

- Seed 20260920, 410 retained graphs: 136 binary undirected, 136 weighted
  undirected, 136 weighted directed, plus two loop/parallel fixtures. Each
  main group derives from empty, singleton, star8, clique6, cycle8, karate,
  100 Gnp graphs of size 3–8 and 30 of size 10–20. These are our fixtures,
  not Zoo's ICON collection. Damping 0, .2, .5, .85, .95; both aggregations.
- 4,100 external NetworkX line-graph/PageRank comparisons; 2,790 locally
  written Markov-tree cofactor comparisons; 4,100 each of wrapper,
  normalized-reference and mode/inversion checks. API consistency checks
  are distinguished from independent numerical references in the audit.
- All 19,190 comparisons, 142,730 values, pass at scaled tolerance 1e-10.
  Maximum error 1.842e-12. Five initial mismatches at NetworkX iteration
  tolerance 1e-13 are retained. Tightening iteration to 1e-15 resolved them
  without changing comparison tolerance. Six large cofactor checks on the
  three affected graphs independently pass, maximum error 6.971e-15.
  Final cofactors use log determinants from pivoted SciPy LU factors.
  NumPy's intermediate determinant code and matching results are retained
  because it emitted warnings; the final cofactor audit is warning-free.
- 41 public expectations pass. Broad centrality/kernel regression:
  2,427 passed, zero failures/errors, 20 existing warnings. Production and
  public-test files lint clean; new Rd checks passed. All 33 implemented
  ledger calls ran on karate; all 160 original CSV rows/fields are preserved.
- Standalone and installed catalogues contain LineRank and 161 selectable
  measures. Package check: zero errors, zero warnings, one existing NOTE
  for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests ran separately; vignettes built
  during package build. Check log:
  `/private/tmp/cograph-batch26-check/cograph.Rcheck/00check.log`.

Coverage: 33/160 original candidates implemented, 127 pending; runtime 161
measures, 136 Zoo mappings. The full cohort remains active work.

## Batch 25: random walk decay

Implementation: `R/kernels-batch25.R`, `R/centrality-batch25.R`.
Public regressions: `tests/testthat/test-centrality-batch25.R`.
Independent references, retained input matrices and masses, published
examples, source audit, versions and per-comparison errors:
`local_testing_and_equivalence/batch25/`.

Definition: Was, Rahwan and Skibski (2019), *Random Walk Decay Centrality*,
AAAI33(01),2197–2204, DOI10.1609/aaai.v33i01.33012197. Original publisher
PDF downloaded and read; pages3,4,7 visually checked. Definition1 eq6
counts only the first arrival at each target, including time0. Equation3
uses outgoing transition weights, and sinks lead to an external terminal
state. The preliminary domain includes directed multigraphs, loops and
nonnegative starting-node weights, with a footnote extending edge weights.
PDF URL and SHA-256 are recorded in `batch25/SOURCE_AUDIT.md`.

`rwd_decay` accepts finite [0,1), default0.5; zero is an explicit limit of
the source's open interval. `rwd_node_weights` defaults to ones and retains
raw starting mass. Named weights match and reorder by node labels;
unnamed weights follow node order. All-zero mass returns zero by linear
extension. Isolates score their own starting mass. Separate components
are independent before maximum normalization.

Direction and loops are retained. Sinks do not restart or redistribute.
Remaining weighted parallel edges sum; use `simplify=FALSE` to preserve
unweighted parallel multiplicity. Generic loops/simplify apply first;
mode, weight inversion and cutoff do not affect the method. A target's
outgoing edges do not affect its own raw score, as required by the source's
Lack-of-Self-Impact axiom. Maximum normalization can change that property.

The native algorithm solves each target's absorbing system after pruning
unreachable vertices. It costs O(n^4) time/O(n^2) memory and is marked
costly. Row scaling avoids outgoing-weight overflow. Transition-range loss,
unstable solves and raw score overflow raise explicit errors. Normalized
output scales starting mass before summation. If a first-hit probability
underflows before multiplication by a large mass, a forward-mass solve
and log-space target flux recover representable contributions. Tiny final
contributions below double range can still underflow.

Independent verification:

- Seed20260919,544 retained matrices:136 binary undirected,136 weighted
  undirected with loops,136 binary directed with loops,136 weighted directed
  with loops. Each136 includes empty,singleton,star8,clique6,cycle8,karate,
  100 Gnp graphs with3–8 vertices and30 with10–30 vertices. Edge weights1:9;
  personalized node masses0:5. These are our fixtures, not Zoo's ICON set.
- At decay0,.1,.5,.8,.95, with unit and personalized mass:5,440 NumPy
  full-resolvent ratios,4,200 explicit first-visit series,250 SymPy exact
  rational solves,5,440 wrappers,5,440 normalized references,5,400
  permutations/mode/inversion checks and5,400 Lack-of-Self-Impact checks.
  First-visit series have an explicit omitted-tail bound.
- 27 mpmath100-digit checks at1e-300,.999999 and the closest double below1;
  two more compare per-node ratios on a directed path with starting mass
  1e308 and decay1e-200/1e-300. This detects a tiny lost score even alongside
  much larger outputs. The forward-mass fallback passes these checks.
- Total31,601 comparisons and232,165 values; zero failures. Maximum scaled
  error6.151e-14; default tolerance1e-10, extreme ratio tolerance1e-12.
  Final kernel rerun passed. Versions retained: R4.5.2,igraph2.3.3,
  NumPy2.4.2,NetworkX3.6.1,SymPy1.14.0,mpmath1.3.0.
- Published Example2's seven first-visit probabilities agree. Examples4/5
  provide36 further node scores on four retained graphs; all match the
  published two-decimal values. Maximum difference from rounded values
  0.004870229, within0.005. Separate per-node results are retained.
- Example3 is internally inconsistent: its correct first-visit series,
  following rational expression and numerical values disagree. In Figure1
  at decay0.5, equation6 gives (u,v,w,t)=(1,13/20,6/5,12/35), corroborated
  independently. All four printed discrepancies are retained in
  `published_example3_discrepancy.csv`; no parity with that table is claimed.
- 61 public expectations passed. Broad centrality/kernel regression:
  2,384 passed,0 failures/errors,20 existing warnings. New R files lint clean;
  new Rd checks passed. Standalone catalogue rendered with RWD and160
  selectable measures. All32 implemented ledger calls ran on karate, and
  all original160 source rows and fields are preserved. Package check
  completed with0 errors,0 warnings,1 existing top-level-file NOTE.
  Tests ran separately; vignettes were built during package build. Installed
  catalogue verified with RWD and160 selectable measures. Check log:
  `/private/tmp/cograph-batch25-check/cograph.Rcheck/00check.log`.

Coverage:32/160 original candidates implemented,128 pending; runtime160
measures,135 Zoo mappings. The remaining cohort is still active work.

## Batch 24: graph regularization centrality

Implementation: `R/kernels-batch24.R`, `R/centrality-batch24.R`.
Public regressions: `tests/testthat/test-centrality-batch24.R`.
Independent references, exact graphs, author source files and licence,
versions, source decisions and numerical errors:
`local_testing_and_equivalence/batch24/`.

Definition: Dal Col and Petronetto (2023), Physica A628,129188,
DOI10.1016/j.physa.2023.129188. The publisher full text was unavailable;
the primary source actually read is the author implementation linked by
[Mendeley Data version1](https://data.mendeley.com/datasets/ns63f5dj86/1),
DOI10.17632/ns63f5dj86.1. Its GitHub repository redirects from GRC to
Graph-Regularization-Centrality. The retained MIT-licensed source is pinned
to commit89a0f8e6ede52a8d601779691185920d725ef910; SHA-256 fingerprints
are recorded in `batch24/SOURCE_AUDIT.md`.

The source computes an unnormalized weighted Laplacian, applies filter
1/(1+gamma*x) to each unit impulse and takes the reciprocal of its retained
value. The exact definition is therefore the reciprocal diagonal of
(I+gamma L)^-1, agreeing with Zoo's stated quadratic minimization. The
source's separate signal option is not the implemented unit-impulse
centrality. Gamma is finite and nonnegative, default1; isolates and gamma0
score1. Raw scores lie between1 and component size; separate components
are independent until optional global maximum normalization.

Unweighted inputs use the simple undirected skeleton. Weighted directed
opposite arcs and remaining parallel edges are summed; these are explicit
cograph projections for the source's undirected domain. Loops, mode,
shortest-path inversion and cutoff do not affect the measure. Native
component spectral calculations isolate the exact constant mode and use
log-space attenuation, supporting extreme finite gamma and weight scales.
Unresolvable weight ranges or positive spectra trigger explicit errors.

The author code fixes ten Chebyshev terms. Its default approximation is
**not** claimed numerically equivalent to the exact inverse: on its own
weighted10 example at gamma1, maximum relative difference is2.90%.
`author_approximation.csv` preserves every node output at10,64,256,1024
terms on four graphs and four gamma values. The1024-term results converge
to the direct reference within2.045e-13. Compatibility accommodations are
recorded: old-style SciPy csr_matrix, integer ARPACK maxiter and fixed
starting vector. Retained author files themselves are unchanged.

Independent verification completed at tolerance1e-10:

- 273 retained matrices:136 binary and136 corresponding weighted graphs,
  plus the exact author weighted10 edge list. Each136 includes empty,
  singleton, star8, clique6, cycle8, karate,100 Gnp graphs with3–8 vertices
  and30 with10–30 vertices. Seed20260918; weights exp(U[-3,3]). These are
  cograph verification fixtures, not Zoo's648-network ICON collection.
- Gamma0,.001,.1,1,10,100:1,638 SciPy Cholesky solves,1,260 LU determinant
  ratios,1,638 wrapper checks,1,638 normalized reference checks and1,626
  directed projection/loop/permutation checks.
- 16 author1024-term comparisons and12 mpmath700-digit inverse comparisons
  over extreme gamma and weight scales, including products above1e600.
- Total7,828 comparisons and65,420 values; zero failures, maximum scaled
  error2.995e-13. Versions and exact results retained alongside scripts.
- 49 public expectations passed, including analytic paths/stars/cliques,
  weighted triangle determinant ratios, components, scaling, labels,
  normalization, input projections and numerical/domain errors.
- Broad centrality/kernel regression:2,321 passing expectations, no failures
  or errors,20 existing all-measures warnings. New files lint clean. The
  standalone catalogue rendered with GRC and159 selectable measures.
  All31 implemented ledger calls ran on karate; every original160 source
  row and field is preserved. Package check completed with0 errors,
  0 warnings and1 existing top-level-file NOTE. Tests ran separately;
  vignettes were built during package build. Installed catalogue verified
  with GRC and159 measures. Log:
  `/private/tmp/cograph-batch24-check/cograph.Rcheck/00check.log`.

Coverage now31/160 implemented,129 pending; runtime159 measures and134 Zoo
mappings. Mapping entropy and MEB remain pending: the independent source
uses normalized degree where Zoo uses raw degree, and MEB leaves log-zero
betweenness handling unspecified. The exact source and unresolved details
are recorded in the ledger and batch24 source audit.

## Batch 23: adaptive LeaderRank

Implementation: `R/kernels-batch23.R`, `R/centrality-batch23.R`.
Public regressions: `tests/testthat/test-centrality-batch23.R`.
Independent numerical references, exact matrices, manifest, versions,
errors and source audit: `local_testing_and_equivalence/batch23/`.

Definition: Xu and Wang2017, DOI10.1016/j.physa.2016.11.034, section2.2,
equation3 and algorithm steps1–4. The published PDF was obtained from
[the author's archive](https://github.com/shuangxu96/My-Publication-Archive/blob/master/ALeaderRank2017.pdf),
linked from the author homepage. Its equation and algorithm were also
visually checked on page3. The source audit retains the retrieval URL,
date and SHA256 fingerprint.

Original H-indices are computed from neighbors' degrees, excluding the
focal node, before adding the ground. Ground H-index is one. Augmented
arcs have weight `a_ji*h_i`; each row is normalized into transition
probabilities. Initial scores are one on ordinary nodes and zero on ground,
giving total mass N. Returned stationary scores omit ground without
redistribution. This source H-index differs from the existing cograph
closed-neighborhood lobby convention, so it uses a separate helper.

The paper uses directed and undirected datasets without specifying a
directed H-index convention. Public `alr_h_mode` makes the choice explicit:
all (default) computes H on the undirected skeleton, while out/in use
outgoing/incoming neighbors' corresponding degrees. These directed
conventions are cograph choices; no author-directed-software parity is
claimed. Resource flow retains original arcs regardless of H-index mode.
Undirected edges become opposite arcs; all three H modes then coincide.
Input weights, generic mode, inversion and cutoff are ignored. Loops are
removed and parallel arcs count once.

Original H-zero nodes have zero stationary mass. If every H is zero, the
ground row is undefined and scores are NaN. This includes edgeless graphs
and some directed graphs under in/out H modes. Empty input returns an empty
vector. No pseudocount is added. A native ground-elimination linear solve
gives the unique stationary solution even for periodic chains, in O(N^3)
time and O(N^2) memory; ordinary-iteration convergence is not assumed.

Independent verification:

- Seed20260917;136 undirected graphs plus136 directed variants retaining
  each original arc with probability0.7. Base cases: empty, singleton,
  star8, clique6, cycle8, karate,100 Gnp graphs size3–8 and30 size10–30.
  Exact272 matrices, manifest and software versions are retained.
- All three H-index modes tested.4,296 comparisons passed scaled tolerance
  1e-10 across35,817 entries:33,716 finite values and2,101 checked NaNs.
- Reference H-indices use NetworkX degrees and neighbor sets with explicit
  threshold counting, independently of production sorting. The full
  augmented chain is constructed and lazified for NetworkX PageRank with
  damping1. All816 comparisons passed, max scaled error3.942e-12.
- A separate Markov-chain tree cofactor reference gives stationary weights
  from minors of the augmented Laplacian. All630 comparisons on original
  graphs up to8 nodes passed, max scaled error9.993e-16.
- For undirected graphs, an independent reversible-conductance identity
  uses ordinary augmented strength `h_i*(1+sum_neighbors h_j)` and ground
  strength `sum_i h_i`. Multiplying normalized strengths by N gives scores.
  All408 comparisons passed, max scaled error6.437e-16.
- Additional checks:816 wrappers,816 independently normalized comparisons,
  and810 weighted/loop/mode/permutation projections, with label order checked.
  Maximum scaled error across all checks was5.438e-12.
- 42 public API expectations cover analytical paths, rings, cliques and
  stars, open versus closed H-indices, directed H modes, resource direction,
  isolates and global initial mass, periodicity, all-zero H, invalid modes,
  labels, normalization, multiplicity and metadata.
- Versions: R4.5.2, igraph2.3.3, NumPy2.4.2, NetworkX3.6.1.

Broad centrality/kernel regression:2,272 passing expectations, no failures
or errors, and20 existing warnings from unsupported graph types or missing
memberships in the all-measures test. New files lint clean and generated
Rd checks passed. The catalogue rendered with ALR and158 selectable measures;
all30 implemented ledger calls ran on karate.
Package check completed with no errors or warnings and one existing NOTE
for `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests ran separately; vignettes were built
during package build. Installed catalogue verified with ALR and158 measures.
Log: `/private/tmp/cograph-batch23-check/cograph.Rcheck/00check.log`.

Original160 source rows and fields are preserved; the ledger records30
implemented and130 pending. Numerical references establish the specified
definition and conventions, not author-software parity or predictive
spreading superiority.

## Batch 22: weighted LeaderRank

Implementation: `R/kernels-batch22.R`, `R/centrality-batch22.R`.
Public regressions: `tests/testthat/test-centrality-batch22.R`.
Independent references, matrices, manifest, versions, errors and stress checks:
`local_testing_and_equivalence/batch22/`.

Definition: Li, Zhou, Lu and Chen (2014), DOI10.1016/j.physa.2014.02.041,
section 2, equations 1–2, read in the
[author preprint, version 2](https://arxiv.org/pdf/1306.5042).
Original directed arcs have weight one; an added ground node g receives
an edge of weight one from each original node. Its outgoing weight to i
is `(original in-degree_i)^alpha`. Row-normalized weights define the
resource transition matrix. Default alpha one is a source setting;
the public `wlr_alpha` parameter accepts every finite exponent, subject
to the mathematical zero-degree restriction below.

The original paper says to initialize all node scores to one, so this
implementation retains total mass N+1 over the augmented graph. The
ground score is omitted without redistribution. Zoo initializes the
ground to zero, giving total mass N and raw scores multiplied by N/(N+1).
This is a documented constant scale difference, not raw author-Zoo parity;
max-normalized scores coincide. Existing cograph `leaderrank` redistributes
the ground score and uses a different scale, so its raw outputs are not
used as a numerical oracle for alpha zero.

Directed arcs are retained, input weights ignored, loops removed and
parallel arcs collapsed. Undirected edges become opposite arcs by an
explicit extension. Mode, inversion and cutoffs are ignored. Alpha zero
sets every ground weight to one, including zero-in-degree nodes. Negative
alpha requires positive original in-degree at every node and otherwise
errors. Positive alpha gives zero stationary mass to zero-in-degree nodes;
if all original in-degrees are zero the ground transition row is undefined
and every score is NaN. Empty input returns an empty vector.

Production forms the strictly substochastic original-node transition block
`B=A/(outdegree+1)`. With ground outgoing probability vector q, it solves
`(I-transpose(B))*y=q`, then returns `(N+1)*y/(1+sum(y))`. The ground score
is `(N+1)/(1+sum(y))`. This gives the unique stationary solution even when
ordinary iteration is periodic; it does not claim an ordinary-iteration
limit on those inputs. Complexity is O(N^3) time and O(N^2) memory.
Ground probabilities use shifted log powers, with the subtraction before
multiplication to avoid overflow even at extreme finite exponents. Very
small probabilities can underflow to zero.

Verification completed:

- Seed20260916; 136 undirected configurations and 136 directed variants
  retaining each original arc with probability0.7. Base graphs: empty,
  singleton, star8, clique6, cycle8, karate, 100 Gnp size3–8 and 30 Gnp
  size10–30. Exact 272 matrices, graph manifest and versions are retained.
- Exponents -2, -0.5, 0, 0.2, 1, 2 and 5. 250 invalid negative-exponent
  configurations correctly raise the documented domain error. Remaining
  configurations yield 7,866 passing comparisons covering 68,086 output
  entries: 65,866 finite values and 2,220 explicitly checked NaNs.
- Primary independent reference constructs the full augmented chain and
  uses NetworkX PageRank with damping1 on its lazy version `(I+P)/2`.
  Laziness preserves the stationary distribution while removing periodicity.
  All1,654 comparisons pass scaled tolerance1e-10, maximum error5.219e-12.
- A separate Markov-chain tree reference calculates stationary weights from
  cofactors of the full augmented Laplacian, avoiding the production
  ground-elimination solve. All1,264 comparisons for original graphs up
  to8 vertices pass, maximum scaled error1.111e-15.
- Additional checks:1,654 public wrapper comparisons,1,654 normalized
  reference comparisons and1,640 weighted/loop/mode/permutation projections.
  Source node ordering is checked explicitly.
- 47 public API expectations include analytic directed rings, sinks,
  disconnected isolates, cliques, undirected stars, the alpha-zero
  undirected degree identity, period-two chains, invalid parameters,
  multiplicity, metadata and extreme finite exponents +/-1e308.
- A 100-digit mpmath reference solves the full augmented stationary system
  at alpha +/-1000 on a20-node star. Both comparisons pass absolute
  tolerance1e-11; maximum error3.553e-15. Ordinary double degree powers
  overflow or underflow in these cases; reference arithmetic remains finite.
- Versions: R4.5.2, igraph2.3.3, NumPy2.4.2, NetworkX3.6.1, mpmath1.3.0.

Broad centrality/kernel regression completed with2,230 passing expectations,
no failures or errors, and20 existing warnings from unsupported graph types
or missing memberships in the all-measures test. Generated Rd checks passed
and the catalogue rendered. All29 implemented ledger calls ran on karate.
New files lint clean. Package check completed with no errors or warnings
and one existing NOTE for `MOTIFS-AUDIT-FIXING-PLAN.md`; tests ran separately
and vignettes were built during package build. The installed catalogue
contains weighted LeaderRank and157 measures. Log:
`/private/tmp/cograph-batch22-check/cograph.Rcheck/00check.log`.

These references establish numerical agreement with the specified source
definition, scale and boundary conventions, not author-code parity or
predictive spreading superiority. Original160 source rows and fields are
preserved; the ledger now records29 implemented and131 pending.

Source review also retained two unresolved leads. The original neighborhood
centrality paper [Liu et al., equation1](https://arxiv.org/pdf/1511.00441)
uses neighbor-of-neighbor sums that exclude the immediately preceding node;
Zoo describes distinct shortest-distance shells. Whether multiplicities
and cycles are counted needs resolution before mapping neighborhood or
neighbor-distance centrality. Normalized local centrality's original text
is available at [ResearchGate](https://www.researchgate.net/publication/328766093_Identifying_Influential_Spreaders_in_Social_Networks_Via_Normalized_Local_Structure_Attributes),
DOI10.1109/ACCESS.2018.2879116, but equation3 extraction is garbled. Its
neighbor-set and zero-clustering normalization conventions still need
verification. These candidates remain pending, with notes in the ledger.

## Batch 21: global structure models

Implementation: `R/kernels-batch21.R`, `R/centrality-batch21.R`.
Public regressions: `tests/testthat/test-centrality-batch21.R`.
Independent references, exact matrices, graph manifest, versions and errors:
`local_testing_and_equivalence/batch21/`.

Three definitions are implemented, without treating their ranking
correlations as evidence of equivalence:

- GSM: Ullah et al. (2021), [equations 5–8](https://www.nature.com/articles/s41598-021-84684-x).
  `exp(core_i/N) * sum_j core_j/d_ij`, excluding the focal node.
- H-GSM: Mukhtar et al. (2023), [equations 6–8](https://www.nature.com/articles/s41598-023-37570-7).
  `s_i=exp(core_i*degree_i/N)`, `a=ceil(log2(mean(s)))`, and
  `s_i * sum_j s_j/d_ij^a`. Both the mean and N use all original nodes.
- IGSM: the exact definition reproduced in Mukhtar et al. (2023), equation 5,
  attributed to Zhu and Wang (2022), DOI10.1088/1674-1056/ac380d.
  `exp(degree_i/N) * sum_j degree_j/d_ij^a`, with
  `a=ceil(log2(mean(degree)))`. The original Zhu–Wang full text was
  unavailable; the formula was read in the later primary experimental paper.
  This implements IGSM itself, without the extended variant's additional
  nearest-neighbor aggregation.

All three use the simple undirected skeleton, hop distances and original
graph degrees/core numbers. Weights, mode, inversion and cutoffs are ignored;
loops are removed and parallel connections count once. Only reachable
partners contribute, an explicit extension to disconnected inputs. Isolates
and singletons score zero; empty input returns an empty vector. Global N
and means include isolates and other components. IGSM retains zero and
negative exponents for positive mean degree below one: distant reachable
partners then receive equal or greater weight. Edgeless IGSM scores are zero
by explicit extension because log2(mean degree) is otherwise undefined.

Production combines native coreness and all-pairs distances with logarithmic
summation, in O(N^3) worst-case time and O(N^2) memory. H-GSM computes the
global mean in log space. Raw scores outside double range error with guidance
to request normalization; normalized scores are computed from log-score
differences. Extremely small ratios may underflow to zero.

Independent verification completed:

- `run_equivalence.R`, seed 20260915, retains 136 exact graph matrices:
  empty, singleton, star8, clique6, cycle8, Zachary karate, 100 seeded Gnp
  graphs of size 3–10 and 30 of size 15–40. These are cograph verification
  fixtures, not the Zoo's 648-network ICON collection.
- 1,944 comparisons / 20,490 node values passed scaled tolerance 1e-11:
  408 NetworkX direct-formula comparisons, 315 exhaustive small-graph
  comparisons, 408 wrapper checks, 408 independently normalized comparisons
  and 405 weighted/directed/loop/permutation projection checks.
  Maximum scaled error was 3.609e-15. Largest absolute error was
  3.577e-7 on large raw values; it remained below the scaled tolerance.
- `reference.py` uses NetworkX core_number and BFS distances with ordinary
  exponential arithmetic and `math.fsum`. The second reference enumerates
  induced vertex subsets to obtain core numbers and uses NumPy
  Floyd–Warshall distances on graphs up to 10 vertices. It does not reuse
  production peeling, Dijkstra or logarithmic scoring. IGSM uses degree
  and the independent distances; it does not depend on the core oracle.
- 67 public API expectations include analytic stars, cycles, cliques,
  mean-degree ceiling cases, isolates, global-size effects, labels,
  permutations, multiplicity and metadata.
- Constructed graphs consistent with the papers' supplied core, degree and
  focal-distance information reproduce GSM's rounded focal result 21.833
  and H-GSM's rounded-intermediate result 152.877133 (absolute tolerance
  1e-4 for the latter). The original graph figures could not be retrieved
  for visual transcription; these are explicitly constructed fixtures,
  not claims of original author dataset reconstruction.
- `stress.R` compares against 100-digit mpmath closed-form clique scores.
  A disjoint clique380 + clique20 graph has raw log score 724.1425, beyond
  double range; the raw call errors and all 400 normalized values agree,
  including nonzero ratios of 3.728615e-313. A clique720 + clique5 graph
  has log self-influence 713.0497, also beyond double range; all 725
  normalized values agree, including the five correctly underflowed zeros.
  Reports: `stress.csv`, `stress_mass.csv`.
- Versions: R4.5.2, igraph2.3.3, NumPy2.4.2, NetworkX3.6.1, mpmath1.3.0.
- Broad centrality/kernel regression: 2,183 passing expectations, no failures
  or errors; 20 existing warnings from unsupported graph types or missing
  memberships in the all-measures test. New files lint clean, generated Rd
  checks passed, and the catalogue renders with all three new models and
  156 selectable measures. All 28 implemented cohort calls run on karate.
- Package check completed with no errors or warnings and one existing NOTE
  for the top-level `MOTIFS-AUDIT-FIXING-PLAN.md`. Tests ran separately;
  vignettes were built during package build. The installed catalogue
  contains all three models and 156 measures. Log:
  `/private/tmp/cograph-batch21-check/cograph.Rcheck/00check.log`.

These references establish numerical agreement with the stated definitions
and extensions. They do not establish author-software parity or superior
spreading predictions. The original 160 source rows and all source fields
remain unchanged; the ledger now records 28 implemented and 132 pending.

## Batch 20: exogenous centrality

Implementation: `R/kernels-batch20.R`, `R/centrality-batch20.R`.
Public regressions: `tests/testthat/test-centrality-batch20.R`.
Independent references, exact matrices, graph manifest, versions and errors:
`local_testing_and_equivalence/batch20/`.

Definition: [Everett & Borgatti (2010)](https://doi.org/10.1016/j.socnet.2010.06.004),
equations 3 and 8 and sections 3.1–3.3, read in the
[author-uploaded full text](https://www.researchgate.net/publication/248484610_Induced_endogenous_and_exogenous_centrality).
Exogenous centrality sums changes in all *other* nodes' base centrality
after deleting the focal node. Supported bases are degree, raw betweenness,
and adjusted reverse-closeness. The last is the default, an explicit cograph
choice. It sums N minus finite hop distances, with zero contribution for
unreachable partners, retaining the ORIGINAL N after deletion. Ordinary
normalized closeness and arbitrary normalized eigenvectors are not aliases.

Directed in/out modes refer to the base direction; all mode uses the
undirected skeleton. Degree therefore reverses direction in its exogenous
result. Weights are ignored, loops removed and parallel connections counted
once. Isolates and singletons score zero; adding an isolate can still change
other nodes' reverse-closeness scores because N includes every original node.
Betweenness contributions may be negative. Optional final max normalization
retains signs and does not normalize base scores. Repeated deletion makes
the measure costly, including when the degree base is selected.

Production uses column sums for the degree identity, native distances for
reverse-closeness, and a distance-sum identity for total betweenness after
deletion. Every reachable pair contributes distance minus one to total
betweenness, so repeated full betweenness solves are unnecessary. The
NetworkX reference instead recomputes all node base scores after every
deletion. A second reference explicitly enumerates shortest paths for
betweenness and simple paths for shortest-distance minima. These checks do
not establish UCINET or author-executable parity.

Seed 20260914: 136 binary undirected graphs and 136 directed configurations
obtained by independently retaining each arc with probability 0.7. Inputs
include empty, singleton, star8, clique6, cycle8, karate, 100 random graphs
sampled at 3–8 nodes, and 30 sampled at 10–25 nodes. Directed configurations
cover all/in/out modes; every configuration covers all three bases. Exact
matrices and software versions are retained: R 4.5.2, igraph 2.3.3,
NumPy 2.4.2, NetworkX 3.6.1.

All 4,301 comparisons / 33,628 node values passed at scaled tolerance
1e-10: 1,632 NetworkX base-recomputation cases, 1,044 explicit-path cases
(n<=7), 544 wrappers, 540 weight/loop/permutation checks, 540 normalization
checks, and one 16-node Florentine independent comparison. Maximum absolute
error 3.13e-13; scaled error 1.73e-13. The separate `source_audit.R` also
compares 16 Florentine betweenness scores against both NetworkX and explicit
path enumeration at absolute tolerance 1e-11.

**Published numerical discrepancies remain explicit.** On NetworkX's
Florentine graph plus isolated Pucci, cograph and NetworkX agree on all
adjusted reverse-closeness scores, but four differ from Table 5. Barbadori's
exogenous betweenness also differs from Table 1 beyond rounding uncertainty;
both independent references agree with cograph. Their origin remains
unresolved, so these are not passing published fixtures. See
`batch20/SOURCE_AUDIT.md`, `published_table5_audit.csv` and
`published_table1_audit.csv` for exact inputs, reported/computed values,
and limitations. Figure 2 was not retrieved and is not used as a fixture.

Integration: 48 public expectations passed. Broad centrality/kernel/coverage
suite: 2,116 passed, zero failures/errors and 20 existing all-tier warnings.
New R/test files lint clean; Rd check passes. Rendered catalogue includes
exogenous centrality and runtime count153. All25 candidate calls ran on
karate, and original160 source fields match the ledger. Progress:
25 implemented /135 pending;128 Zoo mappings overall. Package check finished
with zero errors, zero warnings and one existing top-level-file NOTE.
Tests ran separately and vignettes built during package build; the installed
catalogue includes the new entry and correct count. Log:
`/private/tmp/cograph-batch20-check/cograph.Rcheck/00check.log`.

## Batch 19: improved closeness (ICC)

Implementation: `R/kernels-batch19.R`, `R/centrality-batch19.R`.
Public regressions: `tests/testthat/test-centrality-batch19.R`.
Independent reference code, exact graph matrices, manifest, software
versions and per-configuration errors: `local_testing_and_equivalence/batch19/`.

Definition pinned to [Luan, Bao & Zhang (2021), equation 7](https://doi.org/10.1007/s11424-021-0111-7),
read in the [primary full text](https://www.researchgate.net/publication/352799385_Identifying_Influential_Spreaders_in_Complex_Networks_by_Considering_the_Impact_of_the_Number_of_Shortest_Paths).
ICC is (n-1) divided by the sum of hop distances divided by shortest-path
counts raised to alpha. The supported alpha interval is [0,1]; default
0.2 is a studied setting, not a universally optimal value. At alpha zero,
ICC equals ordinary normalized closeness on connected graphs. Every tree
is independent of alpha because shortest paths are unique. ICC can exceed
one, and optional final max normalization is separate from the n-1 factor.

The published domain is binary undirected graphs. cograph projects other
inputs onto the simple undirected skeleton, removing loops and collapsing
parallel connections. Weights, mode and path inversion are ignored.
Disconnected graphs score all zero under an explicit global infinite-distance
convention; singleton zero is a cograph extension of the undefined 0/0
expression. Empty input returns an empty vector. Within-component results
require supplying each component separately.

Production uses breadth-first traversal with logarithmic shortest-path
counts. Two definition-based independent references check the result:
(1) Python arbitrary-precision integer adjacency powers, taking the first
nonzero walk count for each pair, and (2) NetworkX explicit enumeration of
all shortest paths on graphs of at most ten nodes. The first does not use
BFS; the second explicitly materializes paths rather than accumulating log
counts. The alpha-zero limit also agrees with igraph normalized closeness.
These are numerical definition checks, not parity with author software or
evidence of superior epidemic predictions.

Seed 20260913: 136 graphs comprising empty, singleton, star8, clique6,
cycle8, Zachary karate, 100 random graphs sampled at 3–10 nodes, and 30
sampled at 15–40 nodes. All 3,148 comparisons / 30,626 node values passed
at scaled tolerance 1e-11: 1,496 integer-power comparisons, 1,155 explicit
path-enumeration comparisons, 91 igraph limiting cases, 136 wrapper checks,
135 directed/weighted/loop/permutation projections, and 135 final-normalization
checks. Both numerical oracles cover alpha 0 through 1 in steps of 0.1.
Maximum independent absolute error 8.88e-16; scaled error 6.20e-16.
Versions: R 4.5.2, igraph 2.3.3, NumPy 2.4.2, NetworkX 3.6.1.

Additional overflow stress: `stress.R` constructs 1,031 layers of two nodes,
with adjacent layers fully joined. End-to-end pairs have 2^1029 shortest
paths, exceeding double range. All 2,062 node scores agree with a closed
form based on layer separations at alpha 0.2. Maximum absolute error
2.84e-14, scaled error 1.88e-15; public API runtime 21.4 seconds locally.
The deterministic generator and results are retained in `stress.R` and
`stress.csv`. Very small effective-distance terms may underflow, while
direct-neighbor terms keep every connected nontrivial denominator positive.

Integration: 35 public expectations pass, covering analytic cycles, trees,
cliques and complete bipartite graphs, invalid parameters (including empty
input), disconnected conventions, labels, projection and normalization.
Broad centrality/kernel/coverage suite: 2,066 passing expectations, zero
failures/errors, 20 existing all-tier warnings. New R/test files lint clean;
Rd check passes. Rendered catalogue includes ICC and runtime count 152.
All 24 implemented candidate calls ran on karate, and all 160 original
source rows match the ledger. Coverage is 24 implemented / 136 pending,
with 127 Zoo mappings overall. Package check finished with zero errors,
zero warnings and one existing top-level-file NOTE. Tests ran separately;
vignettes built during package build, and the installed catalogue also
includes ICC and the correct runtime count. Check log:
`/private/tmp/cograph-batch19-check/cograph.Rcheck/00check.log`.

## Batch 18: weighted clustering degree algorithm (CDA)

Implementation: `R/kernels-batch18.R`, `R/centrality-batch18.R`.
Public regressions: `tests/testthat/test-centrality-batch18.R`.
Independent references and numerical artifacts:
`local_testing_and_equivalence/batch18/` (reference Python, R runner,
exact matrices, graph manifest, software versions, per-configuration errors).

Definition pinned to [Wang et al. (2018), equations 2–6](https://doi.org/10.1109/ACCESS.2018.2822844),
whose [primary full text is available here](https://www.researchgate.net/publication/324232657_CDA_A_Clustering_Degree_Based_Influential_Spreader_Identification_Algorithm_in_Weighted_Complex_Network).
CDA returns the propagation-capability score PC, comprising clustering
degree CD plus weighted neighbor CD contributions. CD combines degree and
strength with alpha, then applies the logistic of Barrat weighted clustering.
The neighbor contributions divide by the maximum edge weight in the entire
graph. This matches Zoo section 2.35; PC is not just the intermediate CD.

Default alpha 0.5 follows the paper. The supported convex family includes
zero and one. Input weights are finite and nonnegative; zero weights are
absent connections. Low-degree clustering is zero, consistent with the
source's leaf example; isolates score zero. Weighted directed arcs are added
into undirected edge weights, loops removed, and remaining parallel weights
added after simplify. Without weights, the simple skeleton is used. These
are explicit cograph projections. Weight units are retained; there is no
silent rescaling of strength. A remote component can change contributions
through the global maximum weight. Alpha-zero scores scale with uniform
weight scaling; alpha-one scores are invariant. Final normalization occurs
after all raw contributions. Overflow of strengths or scores raises an error.

Production counts triangles with a binary matrix product and row-normalized
weights. Independent references use (1) igraph Barrat transitivity and
(2) Python enumeration of unordered neighbor pairs and triangle closure,
using NetworkX adjacency. Both then evaluate the documented PC expression.
The Python oracle has no common-neighbor matrix multiplication or cograph
code. Neither reference establishes parity with the author's executable,
and no superiority claim for epidemic spreading is made.

Seed 20260912: 136 topologies, each binary and weighted, for 272 configurations.
Topologies include 100 random graphs sampled at 3–10 nodes, 30 sampled at
15–40 nodes, empty, singleton, star8, clique6, cycle8 and Zachary karate.
Nonzero random weights are uniform from 0.01 to 20. Versions: R 4.5.2,
igraph 2.3.3, NumPy 2.4.2, NetworkX 3.6.1.

All 3,802 comparisons / 44,912 node values passed at scaled tolerance
1e-11: 1,360 Python and 1,360 igraph comparisons across alpha
0/0.25/0.5/0.75/1, 540 endpoint-scaling checks, 272 wrappers and 270
directed/loop/permutation projections. Largest independent absolute error:
3.41e-13; largest independent scaled error: 4.41e-16. Public tests also
check analytical weighted triangles and cliques, parameter interpolation,
global component coupling, zero weights, invalid parameters and weights,
parallel edges, normalization, and finite/overflow cases at weights 1e307.

Integration: 38 focused public expectations passed. The broad
centrality/kernel/coverage suite passed 2,029 expectations with zero
failures/errors and the same 20 existing all-tier warnings; the final two
extreme-weight assertions were added and run in the focused suite afterward,
with production code unchanged. New R/test files lint clean, Rd checks
pass, and the rendered catalogue includes CDA. All 23 implemented candidate
calls ran on karate; all 160 original CSV source rows match the ledger.
Runtime151 measures;126 Zoo mappings. Package check: zero errors, zero
warnings, one existing top-level-file NOTE. Tests ran separately and
vignettes built during package build.

## Batch 17: extended neighborhood coreness and extended gravity

Implementation: `R/kernels-batch17.R`, `R/centrality-batch17.R`.
Public regression tests: `tests/testthat/test-centrality-batch17.R`.
Numerical references, graphs, manifest, versions and per-configuration
errors: `local_testing_and_equivalence/batch17/`.

| Zoo candidate | cograph measure | Pinned formula and provenance |
|---|---|---|
| Extended neighborhood coreness | `extended_coreness` | Bae & Kim (2014), DOI10.1016/j.physa.2013.10.047; the exact baseline equations read for this implementation are reproduced in Ma et al. (2016), equations 2 and 3 |
| Extended gravity centrality | `extended_gravity` | Ma et al. (2016), equations 6 and 7; original k-shell mass and radius three |

Primary experimental paper and accessible full text:
[Ma, Ma, Zhang & Wang (2016)](https://doi.org/10.1016/j.physa.2015.12.162),
[PDF](https://cdn.neusncp.com/public/picture/201909021541544541.pdf), section 2.
Zoo sections 2.129 and 2.119 agree with these formulas. Bae and Kim's
original full text was not obtained; attribution and the numerical
definition are corroborated by the explicit baseline equations used in
Ma et al.'s study. No author-software parity or general spreading-performance
claim is made.

Extended coreness is A squared times the original core-number vector.
All length-two walks contribute, including returns and repeated endpoints;
it is not a sum over distinct distance-two neighbors. Extended gravity
sums each immediate neighbor's raw k-shell gravity score. Its radius applies
around each such neighbor before the outer sum. Radius three follows the
original study; zero, one, two and unrestricted radii are also supported.
The optional automatic radius is explicitly a cograph heuristic: half the
mean finite positive hop distance, rounded ties-to-even with minimum one.
Both measures use the simple undirected unweighted skeleton and give
isolates zero. Weights, direction mode, loops and parallel multiplicity
are projected away. Extended gravity always uses core-number masses.

Production uses shell peeling for original core numbers, matrix-vector
aggregation, and native hop-distance/gravity kernels. The independent
Python reference uses NetworkX core numbers and BFS distances. Extended
coreness uses common-neighbor set intersections to count two-step walks;
extended gravity explicitly enumerates neighbor-source/partner pairs.
Graphs up to nine nodes also derive every core number through exhaustive
induced-subset maximin degrees, avoiding production's peeling algorithm.
That second oracle checks core decomposition independently; it shares the
Python score arithmetic with the NetworkX reference.

Seed20260911: 136 graphs, comprising 100 random graphs with 3–9 nodes,
30 with 15–40 nodes, empty, singleton, star8, clique6, cycle8 and Zachary
karate. R4.5.2, igraph2.3.3, NumPy2.4.2 and NetworkX3.6.1.
All 2,606 comparisons covering 24,613 node values passed at scaled
tolerance 1e-11:

- Extended coreness: 136 NetworkX and 105 exhaustive-core comparisons,
  136 wrappers and 135 weighted/directed/loop/permutation projections.
- Extended gravity: 952 NetworkX and 735 exhaustive-core comparisons
  across radius0/1/2/3/Inf/NULL/auto; 136 wrappers, 136 identities against
  the existing raw gravity function, and 135 input projections.

Extended coreness matched exactly. Largest extended-gravity error against
an independent reference was 3.64e-12 absolute /1.52e-15 scaled. Public
regressions include mixed-shell and analytical graphs, neighbor-centered
radius semantics, disconnected inputs, empty graphs, labels, normalization,
invalid radius values and weight/mode/mass controls. All 43 public
expectations passed; new R/test files lint clean and new Rd files pass
checkRd. The original 160 source rows match the ledger.

The broader centrality/kernel/coverage suite passed 1,993 expectations,
with zero failures or errors and the same 20 existing all-tier warnings.
All 22 implemented candidate calls executed successfully. Runtime150
measures;125 Zoo mappings. Final package check: zero errors, zero warnings,
one existing top-level-file NOTE. Tests ran separately and vignettes built
during the build. Both new catalogue entries were verified in the rendered
HTML and the built/installed package. The catalogue now rejects unknown
section names instead of silently omitting those records.

INK remains pending rather than being declared implemented at alpha one.
Zoo section 2.189 includes an exponent on neighbor core numbers. The
original publisher abstract, DOI10.1016/j.physleta.2014.09.054, confirms
a tunable parameter, but the full original exponent convention was not
obtained in this batch. Other research leads now have pinned citations:
DST's original source is Chen et al. (2003), DOI10.1109/ICNNSP.2003.1281204;
CDA is Wang et al. (2018), DOI10.1109/ACCESS.2018.2822844. CDA's primary
full text was subsequently found on ResearchGate (publication324232657):
equations2–6 confirm weighted degree, Barrat clustering, logistic mapping
and the neighbor-contribution sum. Its implementation, edge-case conventions
and independent numerical checks remain future work; DST still needs its
full original definition reviewed.

## Batch 16: node resistance curvature

Implementation: `R/kernels-batch16.R`, `R/centrality-batch16.R`.
Public regression tests: `tests/testthat/test-centrality-batch16.R`.
Numerical references and results: `local_testing_and_equivalence/batch16/`,
including `reference.py`, `run_equivalence.R`, `equivalence.csv`, exact input
matrices, a graph manifest and software versions.

Definition: [Devriendt & Lambiotte (2022), definition 1, equation 2](https://arxiv.org/pdf/2201.06385),
[published paper](https://doi.org/10.1088/2632-072X/ac730d). Node curvature is
one minus half the sum of incident conductance times effective resistance.
This matches Zoo section 2.335. It is distinct from link resistance curvature
and from the differently scaled resistance-distance curvature of later work.

Production solves grounded Laplacian systems using Cholesky factors and
squared edge-incidence solution norms. References are:

- Maintained NetworkX `resistance_distance`, with `invert_weight=False`
  because input weights are conductances. Its pseudoinverse calculation
  differs from production's grounded triangular solves.
- A locally written exhaustive spanning-tree oracle, independent of
  Laplacian calculations. Enumerate every acyclic set of n-1 edges,
  weight each tree by its conductance product, and return one minus half
  the weighted expected degree. This uses the paper's Appendix A.1,
  Theorem 2, and applies component by component. It does not establish
  parity with the authors' own software.

Validation uses 136 topologies, each unweighted and weighted: 100 seeded
random graphs of 3–7 nodes, 30 random graphs of 15–40 nodes, empty, singleton,
star8, clique6, cycle8 and Zachary karate. Seed: 20260910; nonzero random
conductances are powers of two from 1/32 through 32. R 4.5.2, igraph 2.3.3,
NumPy 2.4.2, NetworkX 3.6.1.

All 1,566 comparisons passed at scaled tolerance 1e-10, covering 13,028
node/scalar values: 272 NetworkX, 210 exhaustive-tree, 272 component-sum,
272 wrapper, 270 directed-projection/permutation and 270 uniform-scale
checks. The exhaustive comparisons enumerate 55,398 component spanning
trees; graphs with components above seven nodes are omitted from that
oracle. Largest absolute difference from NetworkX: 2.70e-13; from tree
enumeration: 6.95e-14. Public regression tests cover analytical path, star,
cycle and clique scores, an explicitly enumerated weighted triangle,
disconnected graphs, isolates, labels, zero weights, parallel arcs,
normalization, weight inversion, invalid weights and overflow guards.

Finite nonnegative weights are conductances; zero weights are absent
connections. The weighted directed projection sums opposite arcs, while
unweighted inputs use the simple skeleton. Loops are removed, subject to
the finite nonnegative weight validation when retained by input settings.
Isolates score one. Raw scores can be negative and sum to the component
count. Max normalization preserves signs but changes that sum. This is a
geometric descriptor, not a universal ranking of influence. Dense solves
are costly and excluded from the default all tier; extreme weight ranges
can raise numerical singularity or precision errors.

Integration validation: 35 public regression expectations and 1,950 broad
centrality/kernel/coverage expectations passed, with zero failures or errors
and the same 20 existing generic all-tier warnings. New R/test files lint
clean; Rd checks and catalogue rendering passed. Package check finished with
zero errors, zero warnings and the existing top-level-file NOTE. Tests ran
separately; vignettes built during package build. The original 160 source
rows match the status ledger, and all 20 implemented candidate calls ran
on karate. Runtime: 148 measures; Zoo mapping count: 123.

### ArticleRank source discrepancy found during batch 16

ArticleRank remains pending. [Li & Willett's original paper, equation 2](https://eprints.whiterose.ac.uk/id/eprint/10323/1/Willett_10323.pdf)
uses a contribution factor mean(NR)/(mean(NR)+NR(i)), while Zoo section 2.11
uses 1/(mean outdegree+outdegree(i)). The numerator difference affects
propagation, not merely final score normalization. The paper's reported
reference counts also require care when constructing an input contract
from an induced citation network. A full implementation must resolve which
variant is exposed and how reference counts are supplied or derived.
Ultipa's current documentation additionally describes base-rank division
by n and dangling-rank redistribution, so its example is not a direct
numerical reference for either raw equation without matching conventions.

## Batch 12: five new measures and one existing equivalence

Implementation: `R/kernels-batch12.R`, `R/centrality-batch12.R`.
Public API regression tests: `tests/testthat/test-centrality-batch12.R`.
Numerical evidence: `local_testing_and_equivalence/batch12/run_equivalence.R`
and `reference.py`, with per-configuration errors in `equivalence.csv`.
Heavy equivalence tooling stays local, following the existing repository policy.

| Zoo label | cograph measure | Definition | Numerical reference and limits |
|---|---|---|---|
| k-truss number | `truss` | [Malliaros et al. 2016](https://doi.org/10.1038/srep19307), node maximum incident-edge truss | NetworkX `k_truss`, using k-2 triangles; isolates explicitly return zero |
| Mixed Degree Decomposition (MDD) | `mdd` | [Zeng & Zhang, method steps 1–4](https://arxiv.org/pdf/1204.4497) | Locally written exhaustive induced-subset optimization for four lambda values; independent of peeling. igraph coreness and degree check endpoints |
| Bridging coefficient | `bridging_coefficient` | [Hwang et al., original technical report, eq. 3](https://www.cse.buffalo.edu/tech-reports/2006-05.pdf); [2008 paper](https://doi.org/10.1145/1401890.1401934) | Locally written Python neighbourhood arithmetic using NetworkX degree; no claim of author-code parity |
| Godfather index | `godfather` | [Jackson, section 3.2](https://arxiv.org/pdf/1711.09504), unconnected unordered neighbour pairs | Independent Python pair enumeration versus production triangle subtraction |
| Support | `support` | [Jackson, section 3.4](https://arxiv.org/pdf/1711.09504), number of supported relationships | NetworkX 3-truss vertex degree versus production common-neighbour matrix |
| LocalRank | existing `semilocal` | [Chen et al. 2012](https://doi.org/10.1016/j.physa.2011.09.017), two nested neighbour sums of two-hop reach | `centiserve::semilocal`, plus an independent matrix/distance expression in the shipped test |

Validation run: 106 graphs (100 seeded random graphs of 3–10 vertices,
empty, singleton, star, clique, cycle and Zachary karate). 2,216 comparisons,
zero failures. Largest absolute numerical discrepancy: 2.22e-16 for the
bridging coefficient; integer measures and MDD matched exactly. MDD's
exhaustive oracle covers graphs up to ten vertices; its limiting cases also
cover karate. Public regression file: 50 passing expectations.

The MDD oracle computes, for every nonempty vertex subset S, its minimum
mixed degree `lambda * original_degree + (1-lambda) * degree_in_S`.
Each member's score is the largest such threshold over subsets containing it.
This independently expresses generalized core membership without reproducing
the production shell-removal procedure. It requires nonnegative lambda <= 1.

## Batch 13: volume and maximal clique centrality

Implementation: `R/kernels-batch13.R`, `R/centrality-batch13.R`.
Public regression tests: `tests/testthat/test-centrality-batch13.R`.
Numerical runner and independent Python reference:
`local_testing_and_equivalence/batch13/`. The runner writes per-configuration
errors, a graph manifest, software versions and the exact graph objects.

| Zoo label | cograph measure | Definition | Numerical reference and limits |
|---|---|---|---|
| Volume centrality | `volume` | [Wehmuth & Ziviani, eq. 1](https://arxiv.org/pdf/1108.1067v1); [DACCER](https://doi.org/10.1016/j.comnet.2013.05.001) | NetworkX BFS neighbourhoods and original-graph degrees, radii 0, 1, 2, 3 and infinity. Closed neighbourhood includes its centre; boundary edges contribute to degree. Infinite radius excludes unreachable vertices |
| MCC | `mcc` | [Chin et al., Methods A.4](https://doi.org/10.1186/1752-0509-8-S4-S11) | NetworkX maximal cliques, plus independently testing every vertex subset for clique completeness and maximality when n<=10. Excludes singleton cliques by explicit cograph convention, so isolates score zero |

Validation: 136 graphs (100 seeded random graphs with 3-10 vertices,
30 random graphs with 15-45 vertices, empty, singleton, star8, clique6,
cycle8 and Zachary karate). All 1,463 comparisons passed with zero numerical
discrepancy. Of these, 136 MCC comparisons use NetworkX; 105 also use
exhaustive subset recognition. Volume has 680 graph/radius comparisons.
The rest exercise wrappers and the documented projection with labels and
permuted node order. Public regression tests: 35 passing expectations.

MCC uses maximal cliques of size at least two. The paper says MCC reduces
to degree when a node's neighbours have no mutual edges; treating a
singleton as a maximal clique would instead give an isolate 0! = 1.
The zero convention is recorded explicitly rather than presented as
unambiguous author-software behavior. These checks establish parity with
the pinned mathematical definition and stated convention, not with the
cytoHubba executable or its biological prediction results. Maximal clique
enumeration has exponential worst-case cost; MCC is excluded from the
default all tier. Values are double precision and overflow raises an error,
including clique sizes above 171. This is tested through the public API.

Broader centrality, kernel and coverage regression tests also passed, with
the same 20 existing warnings from the generic all-tier fixture. Package
check: zero errors, zero warnings and one existing top-level-file NOTE;
tests ran separately, and vignettes built during package build. The updated
catalogue was also rendered and inspected separately. All 15 implemented
candidate calls in the ledger ran successfully on karate; all 160 original
source rows remain unchanged.

## Batch 14: finite-horizon diffusion and dynamical importance

Implementation: `R/centrality-batch14.R`, `R/kernels-batch14.R`.
Public regression tests: `tests/testthat/test-centrality-batch14.R`.
Independent references and saved input matrices, graph manifest, software
versions and per-configuration errors: `local_testing_and_equivalence/batch14/`.

| Zoo label | cograph measure | Definition | Independent numerical evidence |
|---|---|---|---|
| Diffusion centrality | `diffusion_centrality` | [Banerjee et al. 2013, eq. 5](https://doi.org/10.1126/science.1236498); [2019, section 3.1.2](https://doi.org/10.1093/restud/rdz008) | Explicit weighted walk enumeration, permitting revisits, and NumPy matrix powers. Production uses matrix-vector recurrence |
| Dynamical importance | `dynamical_importance` | [Restrepo et al. 2006, eq. 2](https://arxiv.org/pdf/cond-mat/0606122); also Zoo section 2.85 | NumPy full-matrix eigenvalues after deletion; small binary graphs use exact SymPy characteristic polynomials and rational real-root intervals. Production evaluates strong-component eigenspectra in R |

Diffusion accepts finite nonnegative weighted directed or undirected graphs.
It computes outgoing walks, with q in [0, 1] and integer horizon T >= 0.
The defaults q=1 and T=3 are explicit cograph choices; Zoo's experimental
settings are not inferred. If entries of qA exceed one, the result is a
weighted-walk extension without the source's probability interpretation.
Loops follow the input option, parallel edges follow `simplify` and remaining
edges add to the adjacency, and path-weight inversion does not apply.
T=0 and q=0 return zero. T=1 gives q times outgoing strength. Overflow raises
an error, even when normalization is requested.

Dynamical importance uses the actual spectral-radius decrease, not the
eigenvector-product approximation. It accepts nonnegative directed weights,
removes loops and uses the whole graph's radius when disconnected. Zero
original radius makes the ratio undefined: the result is NaN, including on
DAGs. Isolates in a graph with positive radius score zero. Repeated
eigendecomposition is classified as costly. The strong-component calculation
avoids spurious eigenvalues from nilpotent parts; final roundoff is clipped
to [0, 1]. Analytic clique, star, cycle and tied-component examples verify
these conventions through the public API.

Validation: 104 input matrices (100 seeded random networks of 3-7 nodes,
empty, isolated singleton, weighted singleton loop, and karate), seed
20260908. All 4,242 comparisons passed at tolerance
`abs(actual-reference) / max(1, abs(reference)) <= 1e-11`:

- Diffusion: 1,664 NumPy matrix-power comparisons across q=0/.1/.5/1 and
  T=0/1/2/4; 1,648 also use explicit walk enumeration on n<=7. Another
  412 cover wrappers, permutations, unweighted input and the TNA limit.
- Dynamical importance: 208 Python definition comparisons (weighted and
  binary), plus 310 wrapper/permutation/transposition checks. Undefined-value
  masks must match exactly; the CSV records 595 undefined node values across
  these configurations rather than treating them as numerical deltas.
- Largest diffusion discrepancy versus an independent oracle: 1.71e-12
  absolute and 1.06e-14 scaled. TNA limiting comparisons reach 2.91e-11
  absolute on larger scores, but only 4.57e-16 scaled. Largest dynamical
  discrepancy is 7.05e-15.
- Public tests: 74 passing expectations. References: NumPy 2.4.2,
  NetworkX 3.6.1, SymPy 1.14.0; R 4.5.2 and igraph 2.3.3.

The first floating-point reference run split a repeated eigenvalue on two
reciprocal dyads connected in one direction, producing a false 5.98e-9 loss.
Every node deletion leaves another dyad, so the exact loss is zero. The
small-binary-graph reference now obtains the largest real root from exact
characteristic polynomials with rational intervals narrower than 1e-30;
this does not repeat production's component decomposition. The six-node
counterexample is a permanent public regression fixture. No tolerance was
relaxed to accommodate the inaccurate reference.

The tests also exposed an existing power-series bug on unweighted igraph
inputs. It now uses binary adjacency when the weight attribute is absent,
as documented. Weighted TNA behavior is retained. Generic normalization
preserves wholly undefined or empty vectors without an empty-maximum warning.
None of these checks claims parity with the original authors' software or
reproduces the predictive experiments in their papers.

Broader centrality/kernel/coverage tests passed with the same 20 existing
all-tier warnings. Package check finished with zero errors, zero warnings
and one existing top-level-file NOTE; tests ran separately and vignettes
built during package build. New help pages pass checkRd, and the catalogue
renders successfully. All 17 implemented-candidate calls ran on karate; the
original 160 source rows remain unchanged.

## Batch 15: dynamics-sensitive and Malatya centrality

Implementation: `R/kernels-batch15.R`, `R/centrality-batch15.R`.
Public regression tests: `tests/testthat/test-centrality-batch15.R`.
Independent numerical references and exact saved graph inputs, manifest,
software versions and per-configuration errors:
`local_testing_and_equivalence/batch15/`.

| Zoo label | cograph measure | Published definition | Independent numerical reference |
|---|---|---|---|
| Dynamics-sensitive (DS) centrality | `dynamics_sensitive` | [Liu et al. 2016](https://doi.org/10.1038/srep21380), [preprint eq.5/eq.7](https://arxiv.org/pdf/1504.06672) | Binomial expansion of the recovery matrix into powers of binary adjacency, evaluated with NumPy matrix powers and explicit walk enumeration; mu=1 checked against finite diffusion |
| Malatya centrality | `malatya` | [Karci, Yakut & Oztemiz 2022, eq.1](https://dergipark.org.tr/en/download/article-file/2734758), DOI10.53070/bbd.1195501 | NetworkX neighbour degree ratios, plus the reciprocal identity with raw bridging coefficient on nonisolated nodes |

Both use the simple undirected unweighted skeleton, with no loops. Either
direction creates an edge, parallel edges count once and weights are ignored;
projection is an explicit input convention, not a claimed directed or weighted
generalization of the papers. Isolates score zero.

DS implements the full finite-time recovery family:
`sum_{r=0}^{T-1} beta*A*(beta*A+(1-mu)*I)^r*1`. The Zoo prints the mu=1
special case, which equals finite diffusion. The implementation also supports
mu=0 (SI) and intermediate recovery rates. Defaults beta=.1, mu=1 and T=5
select a setting studied by the source, not an estimated optimum. T=0 and
beta=0 return zero; the initial seed is not added. The score is a linearized
cumulative approximation and may exceed the network size; it is not a bounded
infection probability. Finite horizons require no spectral convergence test,
but numerical overflow raises an error.

The independent DS reference expands each recovery power and groups terms by
walk length k. Its coefficient of `A^k * 1` is
`beta^k * sum_{r=k-1}^{T-1} choose(r,k-1)*(1-mu)^(r-k+1)`.
This evaluates a different algebraic expression from the production vector
recurrence. For graphs of at most eight nodes, DFS enumerates every walk of
length up to five (including revisits) and independently verifies the walk
counts used by the expansion. The time horizon does not include a zeroth-order
seed contribution.

Malatya returns static scores on the original graph. It does not run the
vertex-cover selection procedure from the source. Its raw score is
`degree(i) * sum_{j in N(i)} 1/degree(j)`, hence it is the reciprocal of raw
bridging coefficient away from isolates. This is a mathematical identity;
the Zoo's high correlations with leverage play no role in verification.

Validation: 106 graphs (100 random graphs with 3-8 vertices, empty, singleton,
star8, clique6, cycle8 and karate), seed20260909. All 15,834 comparisons passed
at scaled tolerance 1e-11:

- DS: 6,784 binomial/matrix comparisons over beta=0/.1/.5/1,
  mu=0/.25/.7/1 and T=0/1/2/5; 6,720 corresponding enumerated-walk checks;
  1,696 finite-diffusion limits,106 wrapper and105 projection checks.
- Malatya: 106 independent NetworkX checks,106 reciprocal-identity checks,
  106 wrapper and105 projection checks.
- Maximum DS discrepancy: 7.28e-12 absolute and 4.03e-16 scaled. Maximum
  Malatya discrepancy: 1.42e-14 absolute and 2.67e-16 scaled.
- Public tests: 49 expectations passed, including recovery limits, a
  nonregular graph with intermediate recovery, overflow, labels, normalization,
  loops, parallel edges, directed input projection, empties and isolates.

References are locally written mathematical oracles using independent
libraries and enumeration. They do not establish parity with the authors'
software or reproduce empirical spreading/vertex-cover performance.

The broader centrality/kernel/coverage regression suite passed 1,913
expectations with zero failures or errors and 20 existing all-tier warnings.
Results are saved in `/private/tmp/cograph-batch15-regression.rds`.
Package check: zero errors, zero warnings and one existing top-level-file
NOTE; tests ran separately and vignettes built during package build. New help
pages pass checkRd, and the updated catalogue rendered and was inspected.
All 19 implemented-candidate calls ran on karate, and all 160 original source
rows remain unchanged. The ledger retains 141 pending candidates.

## Historical evidence requiring care

- Seven original candidate rows were already mapped before batch 12. Their
  evidence comes from batch 11, not new implementations in this batch.
- Local gravity's `auto` radius is checked against NetworkX distances and
  degrees. Its half-mean-distance heuristic comes from Li et al. (2019);
  nearest-integer rounding (ties to even), minimum 1 and exclusion of
  infinite distances are explicit cograph conventions.
- The original coverage lookup omitted necessary arguments for gravity
  variants and bounded-distance betweenness. The generator now emits them.
- The Zoo correlation matrix compares Zoo implementations, not cograph
  outputs. Rounded tau values and high average correlation prove neither
  exact ranking nor mathematical equivalence. Generated wording now says so.
- The 19-network Zoo consistency report and cross-package near-duplicate
  outputs are historical diagnostics until rerun; they are not verification
  evidence for this batch.
