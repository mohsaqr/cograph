# cograph 2.4.8

## New features

### Centrality Batch 51 — trust-PageRank

- Added `centrality_trust_pagerank()`, Sheng, Zhu, Wang, Wang and Hou's
  trust-PageRank. PageRank's even split of a node's score among its
  neighbours is replaced by a *trust-value* mixing a similarity ratio with
  a degree ratio: `T(i,j) = (1-k) s(i,j)/sum_{l in N_j} s(j,l) +
  k d_i/sum_{l in N_j} d_l`, fed to
  `TPR_i = (1-alpha)/n + alpha sum_{j in N_i} T(i,j) TPR_j`, with `s` the
  fixed point of SimRank restricted to the lines of the graph. New
  parameters `tpr_alpha` (0.85), `tpr_k` (0.85), `tpr_decay` (1),
  `tpr_tol` (1e-14) and `tpr_max_iter` (1000).
- **The Centrality Zoo cites the wrong paper.** Its entry 2.381
  attributes Trust-PageRank to Sheng et al., *Physica A* 541:123262,
  which defines the unrelated global-and-local-structure index of entry
  2.149. The formulas it prints are equations (2), (4), (5), (6) and (7)
  of a different, fully open-access Sheng et al. paper, *Algorithms*
  13(11):280, `doi:10.3390/a13110280`, which is what cograph implements.
  A reader following the Zoo's reference lands on a different measure.
- **The similarity recursion runs on the lines of the graph only, and
  that is what makes it converge.** Algorithm 1 quantifies over
  *connected* pairs and table 3 marks every non-adjacent cell with a
  dash, so a non-adjacent pair contributes zero rather than the `0.1`
  that initialises the lines. The base case `s(a,a) = 1` is then the only
  inhomogeneous term, and it reaches a line exactly through the triangles
  that line carries, so the recursion contracts even at the source's
  `C = 1`. Pinning non-adjacent pairs at `0.1` instead reproduces neither
  published fixture.
- **Both published fixtures reproduce.** Table 3 on page 6 prints seven
  similarities, and all seven round to their printed two decimals; its
  `S_v` column is the sum of the paper's own rounded cells rather than
  the rounded sum, a convention node 5 alone separates. Table 5 on page
  10 prints two top-ten rankings: the karate club's reproduces in order
  at all ten positions, and the kite's up to three exact ties forced by
  its own automorphism group.
- **A component with lines but no triangle has no value, and cograph
  returns `NA` there rather than inventing one.** The recursion is then
  homogeneous, its least non-negative fixed point is zero, and the
  similarity ratio is `0/0`. Every path, tree, star, even cycle and
  complete bipartite graph is in that class, and so is the Petersen
  graph. Unlike `centrality_dil()` and `centrality_lhc()`, the quotient
  is not determined by its numerator, so the `1/d_j` fallback would
  silently turn the measure into a degree-ratio PageRank over the whole
  class; this follows `centrality_iec()` instead. An isolate is *not* in
  the class and keeps the bare `(1 - alpha)/n`.
- **The trust matrix is column-stochastic**, so the scores sum to one on
  a graph without isolates and the iteration count is a convergence
  tolerance rather than a modelling choice. Both recursions stop on a
  *relative* change, because the similarities on one graph span twenty
  orders of magnitude; a recursion still moving at `tpr_max_iter` raises
  `cograph_no_converge`.
- **The source's claim that its decay constant `C` does not matter is
  false for the converged recursion.** It holds for a homogeneous
  recursion; the base case makes this one affine, so `C` enters the
  resolvent. Moving it from 1 to 0.5 moves the karate club's similarity
  ratios by up to 0.141.
- Direction, weights, loops and parallel edges are projected onto the
  simple undirected skeleton the source defines on. Marked costly, so
  `type = "all"` excludes it unless asked for.

### Centrality Batch 50 — degree and importance of lines

- Added `centrality_dil()`, Liu, Xiong, Shi, Shi and Wang's degree and
  importance of lines. A node's degree is corrected by the share it can
  claim of the importance of the lines touching it:
  `I_e(m,n) = (k_m - p - 1)(k_n - p - 1) / (p/2 + 1)` is the importance of
  a line, `p` being the number of triangles carrying it;
  `W(i,j) = I_e(i,j) (k_i - 1)/(k_i + k_j - 2)` is the endpoint's share of
  it; and `DIL(i) = k_i + sum over the open neighbourhood of W(i,j)`. No
  new parameter.
- **`lambda` is `p/2 + 1`, and a text-layer reading gets it wrong.** The
  stacked fraction extracts from the published PDF as `lambda = 2p + 1`,
  in the original as much as in the Almasi and Hu (2019) reproduction of
  it. The equations were read from 300 dpi page images, and the paper's
  own worked example settles it in printed prose: at `p = 1` it writes
  `lambda = 1/2 + 1 = 1.5` and `I_e45 = 8/3`.
- **No Zoo divergence.** Unlike several recent batches, Zoo entry 2.62
  transcribes the source correctly, and the original agrees with the
  Almasi and Hu reproduction symbol for symbol — only the symbol names
  differ.
- **All three published fixtures reproduce, 29 printed values in total.**
  Figure 1 on page 210 prints `I_e45 = 9` and `8/3`; figure 2 on page 211
  prints four edge importances and then `L_v2 = 26/9` and
  `L_v5 = 52/15`; and table 3 on page 217 prints a value for all 21 nodes
  of the ARPA network. The figure-1 and figure-2 values match as exact
  fractions with no tolerance at all, and all 21 table-3 values match
  under the paper's own four-decimal rounding, in the descending order it
  prints them. The edge list read off figure 6 is corroborated by the
  paper's own degree column at all 21 nodes.
- **An isolated pair of nodes is the one undefined split, and it
  resolves.** The share's denominator `k_i + k_j - 2` vanishes only when
  both degrees are one, and there the line's importance is exactly zero
  as well, so every admissible share of it gives the same contribution.
  The share is written as zero with the test taken before the division,
  and both nodes score 1. The source is silent on the case; the choice
  follows `centrality_lhc()`, not `centrality_iec()`, and the help page
  says why.
- **A score is never below its node's degree**, because `p` can never
  exceed either endpoint's degree minus one, and the network's total
  excess over degree is exactly the total importance of its lines — the
  two endpoint shares of a line sum to one. Raw scores are
  component-local: the measure never reaches past a node's second
  neighbours.
- Direction, weights, loops and parallel edges are projected onto the
  simple undirected skeleton the authors state their domain to be. Not
  costly, so `type = "all"` includes it.

### Centrality Batch 49 — immediate effects centrality

- Added `centrality_iec()`, Friedkin's immediate effects centrality. A
  node is scored by how quickly the rest of the network's influence
  reaches it: `c_IEC(j) = (n - 1) / sum_{i != j} m_ij`, where
  `M = (I - Z + E Z_dg) diag(1/c)` is the mean first passage time matrix
  of the influence chain `W = A / rowSums(A)`, `c` is `W`'s left
  eigenvector at eigenvalue one and `Z = (I - W + 1 c')^-1` is the
  fundamental matrix. The sum runs *down* column `j`, so a high score
  marks a node the network reaches fast. No new parameter.
- **The influence matrix carries a unit self-loop, and the self-loop is
  load-bearing.** The source sets `a_ii = 1` before row-normalising, a
  construction it attributes to French (1956) and states twice on page
  1494. Its footnote 10 gives the reason — a strong network with
  `w_ii > 0` must be regular, meaning aperiodic — and its footnote 9 the
  periodic counterexample a zero diagonal admits.
- **This is not cograph's `centrality_markov()`, and the difference is not
  a rescaling.** The package's own candidate ledger recorded for several
  rounds that the two differed only in an `n` versus `n - 1` numerator.
  That is wrong: `markov` *also* omits the self-loop. The numerator is a
  constant factor and cannot reorder anything; the self-loop can and does.
  On the five-node star `markov` gives `1.25, 0.161, ...` where `iec`
  gives `0.5, 0.08, ...`, and the two rank the nodes differently on 2 of
  the 21 connected five-node graphs. `markov` is unchanged; the two ship
  side by side, and each help page now points at the other.
- **A reducible influence chain is refused, not extended.** Without
  irreducibility the eigenvector of equation (9) has one dimension per
  closed class, so `c` is undetermined and `diag(1/c)` undefined. Worse,
  the closed form does not announce the failure: for `i` and `j` in
  different classes `z_ij = 0`, and equation (11) returns the entirely
  finite `m_ij = z_jj / c_j` where the true mean first passage time is
  infinite. Rather than publish a finite wrong number, `iec` tests the
  chain by boolean closure before any solve and returns `NA` at every node
  with a `cograph_undefined_measure` warning. In practice the requirement
  is a connected undirected graph or a strongly connected digraph. A
  one-node graph is `NA` too, equation (20) dividing by `n - 1 = 0`; an
  empty graph returns no scores.
- Direction is kept, because `W` is a matrix of directed influence and row
  `i` is what actor `i` attends to; there is no in/out/all variant, so
  `mode`, `cutoff` and `invert_weights` are ignored. Weights are dropped
  deliberately — `a_ii = 1` is calibrated against `a_ij = 1`, so a
  rescaling of the weights would silently re-weight each actor's
  self-reliance against the network — and loops in the input are absorbed
  by the mandated diagonal while parallel edges collapse. The measure is
  marked costly and is therefore held back from `type = "all"`.
- **The source prints a complete numerical fixture and all 105 values
  reproduce.** Table 1, pages 1492-1494, gives the measure to three
  decimals for every node of all 21 connected non-isomorphic five-node
  graphs. The published audit enumerates those 21 graphs from scratch,
  recovers each printed row's node labelling from the table's own TEC and
  IEC columns, and matches every value exactly against one of two stated
  printing rules rather than against a tolerance: 103 of 105 under
  round-half-away-from-zero at three decimals and 2 under truncation, the
  two being the exactly halfway `0.1875` the paper prints as `.187`. The
  105 printed values of the companion TEC column reproduce as well, three
  of them needing the same truncation rule.

### Centrality Batch 48 — the Lhc index

- Added `centrality_lhc()`, the Lhc index of Wang, Yang, Liu and Ma. A
  node's *influence* is `C(v) = sum_{u in Phi(v)} k_u (1 + TP(u)) /
  d^2(uv)`, a sum over the ball `Phi(v)` of radius `lhc_radius` in which
  each member contributes its degree, inflated by its share of the
  network's triangles, discounted by the square of its distance; the index
  is `Lhc(v) = sum_{w in tau(v)} C(w)`, that influence summed over the
  open neighbourhood. The triangle share is `TP(u) = NTS(u) / TNTS` with
  `NTS(u)` the number of triangles containing `u`. New parameter
  `lhc_radius`.
- **The denominator is `TNTS`, not the number of triangles, and the paper
  settles it rather than the Zoo.** Immediately after defining `TNTS` the
  source writes that "the total number of triangle structure exists in the
  network are `1/3 * TNTS`", so `TNTS = 3 * Delta` and the share sums to
  exactly one over the nodes. Entry 2.221 of the Centrality Zoo
  transcribes the structure of both equations correctly but names the
  denominator "`Delta`, the total number of triangular structures in the
  network", which read literally is three times too small: on the
  Krackhardt kite that reading scores node 1 at 125.45 where the paper
  gives 100.15. cograph follows the paper.
- `lhc_radius` is the source's own parameter, exposed with the source's
  default. The paper writes it `d`, states on page 4 that it "is set to be
  2", and sweeps it in section 3 over eleven real networks, reporting "the
  optimal value of d is about 2-3". At `lhc_radius = 1` the ball collapses
  to the neighbours; at or above the graph's radius the score stops
  moving. The domain is a whole number of at least one and anything else
  raises `cograph_bad_parameter`.
- **Triangle-free graphs are an explicit cograph decision.** Every tree,
  star, path, even cycle and bipartite graph has `TNTS = 0`, making
  `TP(u)` a `0/0` at every node, and the source never mentions the case.
  Since `TNTS` is a sum of nonnegative counts it vanishes exactly when
  every numerator does, so there is no share to distribute: `TP` is
  written as zero and the index reduces to the pure
  degree-over-squared-distance sum. The test is made before any division,
  so no `0/0` is evaluated.
- **Raw scores are not component-local.** `TNTS` is a global sum, so
  attaching a disconnected component that carries a triangle rescales
  every score, while attaching one with no triangle -- an isolate
  included -- changes nothing. An isolate scores zero because its
  neighbourhood is empty; a singleton and every node of an edgeless graph
  score zero for the same reason.
- Direction, weights, loops and parallel edges are dropped to the simple
  undirected skeleton the source defines on; `mode`, `cutoff` and
  `invert_weights` are ignored, and `normalized = TRUE` max-scales, the
  source stating no normalization.
- **The source prints no numerical example**, so there is no published
  per-node table to reproduce. Acceptance rests on three independent
  reference routes and on hand-derived closed forms for stars, complete
  graphs, rings and paths.

### Centrality Batch 47 — randomized shortest paths betweenness

- Added `centrality_rsp_betweenness()`, the simple randomized shortest
  paths betweenness of Kivimaki, Lebichot, Saramaki and Saerens. A
  Boltzmann distribution over the absorbing walks from `s` to `t` is
  tilted by an inverse temperature away from the unbiased random walk and
  towards low-cost walks, and a node scores the expected number of visits
  it receives summed over every ordered source-target pair,
  `bet_i = sum_{s,t} (z_si / z_st - z_ti / z_tt) z_it` with
  `Z = (I - W)^-1` and `W = (D^-1 A) o exp(-beta C)`. New parameters
  `rsp_beta` and `rsp_cost`.
- **The published closed form is defined only on a strongly connected
  graph, and the source says what to do otherwise.** Equation (15) divides
  by every entry of `Z`, and Algorithm 1 takes a strongly connected graph
  as its input, but the text below equation (9) settles the general case:
  the derivation "holds only if there exists a path from `s` to `t`.
  Otherwise, naturally, `eta_ij(s, t) = 0`." cograph evaluates the closed
  form masked by reachability, which reproduces equation (15) to machine
  precision whenever the graph is strongly connected and applies the
  source's own zero rule when it is not.
- **Scores are therefore component-local.** Two disjoint triangles score
  exactly what one triangle scores, and adding a disconnected component --
  an isolate included -- leaves every existing score untouched.
- **A zero out-degree gives a derived zero, not an imputed one.** `D^-1`
  is undefined there, so cograph writes that row of `P^ref` as zero, which
  is the paper's own killed random walk read at a node where the walker
  dies at once; `Z` then has `z_ii = 1` and the arithmetic gives exactly
  `1 - 1 = 0`. An isolate, a singleton graph and every node of an edgeless
  graph score zero for that reason. `NetworkToolbox::rspbc()` raises an
  error on such input and `plot`-side `current_flow_betweenness` returns
  `NA` on disconnected input; this measure can answer where those cannot,
  because `(I - W)` stays nonsingular whatever the connectivity.
- Records a divergence from the CRAN reference implementation:
  `NetworkToolbox::rspbc()` masks only the reciprocal half of the term and
  leaves the `n Diag(Z')` half counting every source, so the two agree
  exactly on strongly connected input and part company on a disconnected
  graph. That function additionally rounds to zero decimals and shifts so
  its minimum is one, post-processing that is nowhere in the paper and is
  not copied here.
- `rsp_beta` defaults to 0.01, which is **not** the source's number: the
  paper fixes no default and treats it as a modelling choice. 0.01 is the
  value `NetworkToolbox::rspbc()` recommends, adopted so the two are
  comparable out of the box; it sits near the random-walk end, so raise it
  towards 1 and beyond to move the reading towards shortest paths. The
  domain is `rsp_beta > 0` and values outside it raise
  `cograph_bad_parameter`.
- `rsp_cost` chooses how a weight becomes a cost, which the source leaves
  free: `"inverse"` (default) sets `C = 1 / w`, reading a weight as an
  affinity, as the CRAN reference hard-codes; `"weight"` sets `C = w`,
  reading it as a distance. The two coincide on a binary graph. Negative
  and non-finite weights raise `cograph_bad_input`, Algorithm 1 requiring
  a non-negative cost matrix.
- Direction is read from the graph rather than from `mode`, so a reversed
  input generally scores differently. Marked **costly**: one dense `n x n`
  inverse, which the source itself calls the computational bottleneck at
  `O(n^3)` time and `O(n^2)` memory.
- The paper prints no table of node scores on a small graph, so there is
  no published per-node example to reproduce. What is checked against the
  paper instead is its printed page 9 limit claim, that the score becomes
  proportional to degree as `beta` approaches zero from above on an
  undirected network; it holds, and does so at first order in `beta`.

### Centrality Batch 46 — hybrid characteristic centrality (HCC and EHCC)

- Added `centrality_hcc()` and `centrality_ehcc()`, the hybrid
  characteristic centrality of Liu and Zheng and its extension. HCC adds
  two normalised halves: the *extended degree*
  `delta * k + (1 - delta) * sum of the neighbours' degrees` over its
  largest value, and the round in which an *E-shell* peel removes the node
  over the number of rounds. EHCC is the closed-neighbourhood sum of HCC,
  the focal node counted once. New parameter `hcc_delta`, default the
  source's 0.5.
- **The source's printed algorithm contains a typo, and cograph implements
  the correction its own tables require.** Step 3 of the E-shell procedure
  prints `S_p = arg max` while the same sentence calls `S_p` "the set of
  minimum nodes" and the paper's table 2 heads its column "Minimum
  extended degree" with the increasing values 2, 2.5, 3, 4.5, 5, 6. The
  minimum reading reproduces every printed row; the literal maximum peel
  deletes a different set first and finishes in four rounds instead of
  six.
- **The peel recomputes but equation (4) does not.** Step 6 updates the
  extended degrees on the residual graph, which is what the printed table 2
  minima require, while equation (4)'s `k^ex` and `k^ex_max` are the
  original-graph values. The paper's node `d` settles it: its original 9.5
  gives the printed 1.86, its residual 6 gives 1.55.
- **Raw scores are not component-local.** `k^ex_max` and `pos_max` are
  single global constants, so a disconnected addition rescales the two
  halves independently rather than by one common factor.
- Records an unresolved disagreement with the source: table 3 prints
  `EHCC(g) = 10.01` for its figure 1, where the exact value `661/66` is
  10.015151… and rounds to 10.02. That printed cell is the truncation, but
  six other printed cells require rounding, so no single convention
  reproduces all twenty table 3 entries. The other 85 printed values
  reproduce.
- Degenerate cases are stated on the help pages: an isolate has extended
  degree zero, the global minimum for every `hcc_delta` in `[0, 1]`, so it
  always leaves in the first round; an edgeless graph makes equation (4)'s
  first term `0/0`, written as zero, so every node of an edgeless graph — a
  singleton included — scores exactly 1. `hcc_delta` outside the source's
  stated `[0, 1]` raises `cograph_bad_parameter` rather than being
  extended.
- Documents a divergence from the Centrality Zoo: it calls the E-shell
  decomposition "a variant of k-shell decomposition", which it is not —
  there is no outer level loop and no repeat-until-stable inner loop. On a
  four-node path the k-shell reading puts all four nodes in one shell where
  the E-shell peel takes two rounds.

### Centrality Batch 45 — KED method

- Added `centrality_ked()`, the KED method of Chen, Xiao, Zeng and Zhang:
  the degree, weighted by one plus the normalised entropy of the
  neighbours' degrees, times `exp(K / N)` for the neighbour-degree sum `K`
  and the whole graph's order `N`. Two nodes with the same degree and the
  same number of second neighbours are separated by how evenly their
  neighbours carry the onward paths. It takes no parameters: equation (6)
  is a bare product.
- **The logarithm base is not a convention to choose.** Equation (2)
  divides the neighbour-degree entropy by the entropy of the uniform
  distribution on `k` outcomes, so the base cancels top and bottom. A full
  base-ten reading gives identical scores, which is asserted to 60 digits
  on every verification fixture.
- **Raw scores depend on the whole graph's order, and can reorder.** `N` is
  the vertex count of the whole network, and `exp(K / N)` shrinks a large
  neighbour-degree sum more than a small one, so adding a disconnected
  component is not a rescaling: on a seven-node example in the tests, one
  extra isolate swaps two nodes' places.
- Records that the source's stated range `1 <= D <= e` is not general. It
  needs `K <= N`, which holds on the sparse toy networks of its figure 1 and
  on only 1,598 of the 5,532 verification fixtures; every node of the
  five-clique has `K = 16` against `N = 5`. cograph implements the formula,
  not the range claim, and raises an error rather than returning `Inf` if
  the exponent ever leaves the range of `exp()`.
- Documents a divergence from the Centrality Zoo: its section 2.215 drops
  the `1 +` from `E`, divides `K` by the largest cluster degree instead of
  by `N`, and adds tunable exponents that appear nowhere in the paper. On
  the source's own figure 1 that reading gives 13.5914 and 6.5672 where the
  paper prints 25.9187 and 19.2212. cograph implements the paper and offers
  no Zoo variant.
- Two cases the source never mentions are cograph decisions, stated on the
  help page: a node with one neighbour has `0/0` for its normalised entropy
  and takes zero, and an isolate scores zero.

### Centrality Batch 44 — Local neighbor contribution (LNC)

- Added `centrality_lnc()`, the local neighbor contribution of Dai, Wang,
  Sheng, Sun, Khawaja, Ullah, Dejene and Duan: the chance that a node
  picking a neighbour at random picks a given one and misses the rest,
  scaled by its degree, multiplied by the sum of its neighbours' degrees
  weighted by their degree centralities. It takes no parameters, which the
  source advertises as one of its contributions.
- **Raw scores depend on the whole graph's order.** The `1 / (n - 1)` comes
  from the source's degree centrality, where `n` counts every node in the
  network rather than in the component, so adding a disconnected component
  multiplies every score by `(n - 1) / (n' - 1)`. The ranking is untouched;
  the raw values are not.
- Implements the source's printed numbers, not its printed equations, and
  says so. Equations (4) and (5) sum over `j = 1` to `k`, and `k` is
  described three incompatible ways: the prose calls it the number of
  nearest and next nearest neighbours, Algorithm 1 sets it to the degree,
  and equation (5) read literally carries one factor of the degree too many
  and returns 6.75 where the paper prints 1.6875. Inverting each of the
  eleven printed influences gives `k = d^2` in (4) and `k = d` in (5), the
  reading implemented here; the alternative literal split of the two factors
  gives the same product, so the measure itself is unambiguous.
- Documents a divergence from the Centrality Zoo: its section 2.238 replaces
  the focal node's contribution probability with each neighbour's and the
  binomial count with the size of the two-hop neighbourhood. That formula
  reproduces none of the eleven printed values and inverts the paper's
  headline ranking, so no Zoo variant is offered.
- Isolates and the single node of a singleton graph score zero as an
  explicit cograph extension: the source has no value where the degree, and
  hence `1 / d`, is undefined, and no result is silently `NaN`.

### Centrality Batch 43 — Iterative resource allocation (IRA and IIRA)

- Added `centrality_ira()`, the iterative resource allocation of Ren, Zeng,
  Chen, Liao and Liu: every node starts with one unit of resource and hands
  it repeatedly to its neighbours in proportion to the receiver's centrality
  until the amounts stop moving. `ira_mass`, `ira_alpha`, `ira_tol` and
  `ira_max_iter` expose the source's `theta`, `alpha`, `epsilon` and
  iteration bound. Every non-empty column of the allocation matrix sums to
  one, so a component's scores sum to its vertex count.
- Added `centrality_iira()`, the improved variant of Zhong, Liu and Shang,
  which scales each share by `1 - (1 - beta)^k`. That factor is strictly
  below one, so the resource decays geometrically; the source fixes the step
  count instead of a tolerance. `iira_beta` and `iira_steps` default to the
  source's 0.2 and 50, the raw `I(50)` is returned so the printed example is
  reproducible, and `normalized = TRUE` max-scales it. Raw IIRA scores from
  different connected components are on different exponential scales and must
  not be compared.
- **`ira` reports non-convergence instead of hiding it.** The allocation
  matrix is a reversible walk, so on a bipartite component it has an
  eigenvalue of exactly -1 whose coefficient in the all-ones start is the
  difference between the two class sizes. When those differ, the resource
  settles into a period-two cycle and no tolerance is ever met: the
  three-star alternates for ever between `3, 1/3, 1/3, 1/3` and `1, 1, 1, 1`.
  `centrality_ira()` then stops at `ira_max_iter`, raises a classed
  `cograph_no_converge` warning naming the largest remaining change, and
  returns that parity-dependent iterate. Neither source mentions this case.
- Documents two divergences from the Centrality Zoo: its IRA entry states the
  transpose and asks for an eigenvector, which sidesteps rather than reports
  the bipartite case, and its IIRA formula pairs the numerator index with the
  wrong neighbourhood, giving a matrix that is stochastic in neither
  direction and reproduces neither printed quantity. cograph implements the
  papers.
- Records that one printed table entry is reproduced only from the exact
  steady state: the source rounds `15/8` up to `1.88`, while the iteration
  approaches that limit from below and returns `1.8749998`, which rounds to
  `1.87`. No author software exists for either measure.

### Centrality Batch 42 — Neighborhood (neighbor distance) centrality

- Added `centrality_neighbor_distance()`, the neighborhood centrality of Liu,
  Tang, Zhou and Do: a benchmark centrality plus its decayed sums over the
  non-backtracking walks that leave the node. `nd_order`, `nd_decay` and
  `nd_mass` expose the source's `n`, `a` and `theta`, and the defaults
  (degree, two steps, 0.2) are the setting the Centrality Zoo calls neighbor
  distance centrality.
- Implements the source's nested sums, in which each level excludes only the
  node the walk just came from, so an endpoint reached by several walks is
  counted once per walk. This is **not** a sum over distance shells. The Zoo's
  paraphrase writes k-hop neighbour sets instead; the two agree on trees and
  differ on any graph with a triangle or a short cycle, and the difference is
  documented rather than offered as a variant.
- Documents that the source prints no table of node scores, so there is no
  published numerical fixture, and that no author software exists.

### Centrality Batch 41 — Relative-entropy integrated evaluation

- Added `centrality_relative_entropy()`, which turns several indexes into
  discrete distributions and returns the unit-sum distribution with the
  smallest total relative entropy to all of them: the normalised geometric
  mean of the index distributions.
- `re_indexes` chooses the constituents from six the source both defines
  and gives an evaluating direction (degree, closeness, betweenness,
  constraint, and the two post-deletion destructiveness indexes
  `n_components` and `largest_component`); `re_negative` overrides which of
  them are read as "smaller is more important". The default reproduces the
  published Kite study's four-index column, and all three of its printed
  integrated columns are reproduced.
- Documents the two non-standard conventions the source uses -- its
  constraint sums over every node rather than over the neighbours, so it is
  not Burt's constraint, and its betweenness counts ordered pairs -- along
  with the reachable-partner closeness extension used outside its connected
  domain. An index that is zero at every node leaves the definition without
  a value and raises a classed `cograph_undefined_index` error rather than
  returning zeros.
- `centrality()` no longer lets one undefined measure end a whole tier. A
  measure named in `measures` or `include` still raises, but when
  `type = "basic"`, `"extended"` or `"all"` supplied it, an undefined result
  becomes a `cograph_undefined_measure` warning and an `NA` column, matching
  what the community-partition measures already do without `membership`.

### Centrality Batch 40 — DK-based gravity model

- Added `centrality_dkgm()`, whose mass is the degree k-shell index: the
  original degree plus a shell number refined by the stage at which the
  node left that shell during the k-shell peeling.
- Default `dkgm_radius = 2` matches the paper's printed nine-node example,
  which the implementation reproduces along with its removal stages,
  improved shell indices and DK values. `"auto"` applies the paper's own
  half-mean-distance rule with cograph rounding.
- Documents the published pseudocode/prose disagreement over removing
  nodes of degree exactly `k` versus at most `k`, the one-shell placement
  of isolates, and the single global stage denominator, which makes raw
  scores depend on disconnected components.

### Centrality Batch 39 — Mixed gravitational centralities

- Added `centrality_mixed_gravity()` and
  `centrality_extended_mixed_gravity()`, using focal core numbers and partner
  degrees, with an optional outer sum over immediate neighbors.
- Default radius three follows the explicit published reproduction;
  radius one exposes the Zoo summary's literal interpretation. Both functions
  document source provenance, skeleton projections and normalization.

### Centrality Batch 38 — Multi-characteristics gravity model

- Added `centrality_mcgm()` with the published adaptive coefficient and
  default radius two, plus explicit radius and coefficient overrides.
- Reproduces the nine-node published example. Uses a simple undirected
  skeleton and documents the eigenvector convention for disconnected graphs.
  Undefined automatic coefficients require an explicit override.

### Centrality Batch 37 — SpectralRank

- Added `centrality_spectralrank()` with optional scalar or node-specific
  diagonal priors. Scores use outgoing neighbors in an augmented graph.
- Preserves the paper's normalization over original and ground nodes;
  optional package normalization rescales original nodes alone. Uses the
  Perron eigenvector also on edgeless graphs where plain iteration oscillates.

### Centrality Batch 36 — ControlRank

- Added `centrality_controlrank()`, the smallest eigenvalue of each grounded
  symmetric row-Laplacian, retaining original degrees after node deletion.
- Supports nonnegative weighted graphs, an explicit outgoing orientation
  for directed inputs, and signed directed scores. Reproduces the published
  bi-star example and is marked costly because it solves one spectrum per node.

### Centrality Batch 35 — Map equation centrality

- Added `centrality_map_equation()` for fixed one-level or leaf-module
  partitions, with recorded node and unrecorded link teleportation.
- Coding conventions are explicit: `paper` includes module-exit flow as
  defined in the equations; `infomap` reproduces the author's visit-only
  implementation and published table. Both conventions have independent
  numerical verification. Stable arithmetic retains extremely small scores.
- Invalid missing, nonfinite, nonscalar or nonnumeric damping inputs now
  receive the centrality argument error instead of an incidental R error.

### Centrality Batch 34 — Node and Neighbor Layer Information

- Added `centrality_ninl()` with the full finite iteration family (default
  three) and optional radius overrides. The initial score sums degrees
  within the ceiling of average path length, then propagates through neighbors.
- Reproduces all 52 entries in Zhu and Wang's published example. Uses simple
  undirected unweighted topology, with explicit disconnected-graph and
  normalization conventions. Preserves bipartite alternation at large orders.

### Centrality Batch 33 — Localized bridging and Extended LBC

- Added `centrality_localized_bridging()` and
  `centrality_extended_local_bridging()`: one-hop and two-hop ego betweenness
  multiplied by the original-graph bridging coefficient. Uses simple,
  undirected, unweighted topology; the two-hop variant is marked costly.
- Corrected the Zoo localized-bridging mapping. `local_bridging` retains
  its existing inverse-degree product, which is a different score.

### Centrality Batch 32 — BG-index and beta power

- Added `centrality_beta_measure()` with positive and negative directed
  variants. Successors share one unit equally among their predecessors;
  the negative variant reverses the graph. Loops and duplicate arcs are
  removed, weights ignored, and isolates score zero.
- Covers both Zoo labels BG-index and beta-measure. Independent predecessor
  choice enumeration and the original published diamond example verify
  numerical scores, with direction and normalization checked through the API.

### Centrality Batch 31 — Expected Force and its degree adjustment

- Added `centrality_expected_force()` and `centrality_modified_expected_force()`
  from Lawyer's two-event definition, with explicit sequence multiplicity,
  boundary-edge counting, direction and exhausted-force conventions.
- Corrected the Zoo Expected Force mapping: `expected` computes neighbor-degree
  sums and remains available under that definition. ExF now maps to
  `expected_force`; the original ExFm candidate maps to the modified function.
- Verified against independent event enumeration and pinned author C++ on
  its connected undirected domain. No author code is included in the package.

### Centrality Batch 30 — Proximal betweenness

- Added `centrality_proximal_betweenness()` with source, target, sum and
  union variants from Brandes. Uses directed unweighted shortest paths,
  excludes endpoints, and explicitly preserves ordered-pair raw scaling.
- Independent path enumeration and exact integer adjacency powers verify
  both orientations and overlap counting. Nonfinite path counts raise errors.

### Centrality Batch 29 — Bridging capital

- Added `centrality_bridging_capital()` with a finite walk horizon and optional
  source-destination information values. It follows Jackson's single-entry
  deletion definition using transmission probabilities between zero and one.
- Independent matrix-power and walk-enumeration checks verify repeated-edge
  counting, direction, loops and valued information. Native tracking avoids
  cancellation; this dense, costly measure must be requested explicitly.

### Centrality Batch 28 — Coleman-Theil hierarchy

- Added `centrality_coleman_theil()`, measuring concentration of Burt's dyadic
  constraints using mutual tie weights. Follows the author's explicit
  isolate-zero and single-contact-one conventions, with organizational
  multipliers fixed at one.
- Verified against NetworkX local constraints, exact rational calculations
  and high-precision entropy checks. Stable arithmetic preserves small
  departures from uniformity; weights, direction and parallel ties have
  documented handling.

### Centrality Batch 27 — X-degree

- Added `centrality_x_degree()`, counting four-edge nonbacktracking walks
  centered at each node using original neighbor excess degrees. It uses
  the simple undirected unweighted skeleton and supports maximum normalization.
- Verified against pinned author code, independent nonbacktracking matrix
  blocks and explicit walk enumeration, including all simple labeled graphs
  through five vertices. This adds a static score; it does not perform the
  paper's iterative immunization strategy.

### Centrality Batch 26 — LineRank

- Added `centrality_linerank()`, using directed line-graph walks or the
  ordinary undirected line graph clarified by Kosa et al. (2015).
- Exposes probability aggregation and the original pseudocode's additional
  edge-weight aggregation as explicit `linerank_aggregation` choices.
  Supports loops and remaining parallel edge states with documented
  conventions, uniform dangling redistribution and damping in [0,1).
- Uses a native dense stationary solve and is marked costly. Verification
  compares NetworkX line graphs/PageRank and Markov-chain tree cofactors.
  The original pseudocode's inconsistent normalization is not replicated.

### Centrality Batch 25 — random walk decay

- Added `centrality_random_walk_decay()` with `rwd_decay` and optional
  `rwd_node_weights`. Scores sum discounted first-arrival probabilities,
  including the starting node. Retains directed flow, weighted transitions
  and loops; sinks terminate the walk without restarting it elsewhere.
- Supports personalized starting mass, zero decay as a limit, disconnected
  graphs and normalized scores when raw sums overflow. Per-target absorbing
  solves make the measure costly; request it explicitly.
- Independently verified using full-resolvent ratios, explicit first-visit
  series, exact rational arithmetic and high-precision calculations.
  Reproduces the published Example 4 and 5 tables; the inconsistent
  Example 3 values are retained as a source discrepancy.

### Centrality Batch 24 — graph regularization centrality

- Added `centrality_graph_regularization()` with finite nonnegative
  `grc_gamma`, default one. Computes reciprocal diagonal entries of the
  inverse regularized weighted Laplacian. Isolates and zero regularization
  score one; disconnected components are independent before normalization.
- A component spectral calculation supports extreme regularization and
  uniform weight scales without overflowing their product. Unresolvable
  weight ranges or positive spectra raise explicit numerical errors.
- Verified against SciPy direct solves, determinant ratios, high-precision
  arithmetic and convergence of the retained author implementation.
  The author's default ten-term approximation is audited separately and
  is not claimed to give identical values.

### Centrality Batch 23 — adaptive LeaderRank

- Added `centrality_adaptive_leaderrank()`, weighting every destination by
  its original open-neighborhood H-index and adding a ground node with
  H-index one. Retains source total mass N and omits the ground score
  without redistribution. Zero H-indices receive zero stationary scores;
  an all-zero H-index vector yields NaN.
- `alr_h_mode` selects the H-index convention: all (default), out or in.
  The paper leaves its directed H-index choice unspecified; these choices
  are explicit cograph conventions, while resource flow retains input arcs.
  The focal node is excluded from the H-index calculation.
- Verified against independent NetworkX iteration, Markov-chain tree
  cofactors and a reversible-conductance identity for undirected inputs.

### Centrality Batch 22 — weighted LeaderRank

- Added `centrality_weighted_leaderrank()` with finite `wlr_alpha`, default
  one. Original directed arcs retain unit weight and ground-to-node weights
  depend on original in-degree. Input edge weights are ignored. Undirected
  edges are represented as opposite arcs.
- Uses the original paper's N+1 initial mass and omits the ground score
  without redistribution. Zoo's N-mass initialization differs by a constant
  factor; max-normalized scores agree. Negative exponents require positive
  in-degrees; positive exponents on edgeless graphs return NaN.
- A native stationary solve handles periodic chains and shifted logarithms
  avoid overflow in degree powers. Verification uses NetworkX lazy-chain
  iteration, Markov-chain tree cofactors and 100-digit stress references.

### Centrality Batch 21 — global structure models

- Added `centrality_global_structure()` (GSM),
  `centrality_hybrid_global_structure()` (H-GSM) and
  `centrality_improved_global_structure()` (IGSM). GSM uses coreness;
  H-GSM combines degree and coreness; IGSM uses degree. The latter two
  use their published adaptive distance exponents. All three use the simple
  undirected skeleton, with explicit zero contributions for unreachable
  partners and global size/means that include isolates.
- IGSM follows the equation reproduced in Mukhtar et al. (2023); the
  original Zhu and Wang (2022) full text was unavailable. Its zero or
  negative distance exponents on sparse disconnected graphs are retained.
- Verified against independent NetworkX calculations, exhaustive small-graph
  core enumeration, and published focal calculations. H-GSM uses logarithmic
  sums so normalized results remain available when raw scores or
  self-influence exceed double precision; 100-digit reference stress checks
  cover both overflow and very small normalized values.

### Centrality Batch 20 — exogenous centrality

- Added `centrality_exogenous()` with degree, betweenness and adjusted
  reverse-closeness bases. Measures the contribution to other nodes'
  centrality when the focal node is deleted. Supports directed base
  directions and retains negative betweenness contributions.
- Reverse-closeness retains the original graph size after deletion.
  All bases use simple binary topology and raw scores before any optional
  final normalization. Repeated graph deletion places this measure in
  the costly tier. Verification includes independent NetworkX scores,
  explicit path enumeration and a documented audit of discrepancies in
  the original paper's Florentine table.

### Centrality Batch 19 — improved closeness

- Added `centrality_improved_closeness()` with `icc_alpha` in [0,1],
  default 0.2, following Luan et al.'s shortest-path multiplicity formula.
  Uses the simple undirected skeleton; alpha zero recovers ordinary
  normalized closeness on connected graphs. Disconnected graphs and
  singletons score zero under explicit cograph conventions.
- Verified against exact integer adjacency powers, NetworkX shortest-path
  enumeration and igraph's closeness limit. Logarithmic path counting
  avoids overflow on graphs with more shortest paths than doubles can hold.

### Centrality Batch 18 — weighted clustering degree algorithm

- Added `centrality_cda()` with `cda_alpha`, default 0.5. Returns Wang
  et al.'s propagation-capability score using degree, strength, Barrat
  clustering and weighted neighbor contributions. The full weighted
  calculation retains original weight units and a global maximum weight.
- Verified against igraph Barrat clustering and independent Python
  neighbor-pair enumeration across mixing parameters, including endpoint
  scaling, disconnected inputs, labels and input projections.

### Centrality Batch 17 — extended coreness and gravity

- Added `centrality_extended_coreness()`, the two-step aggregation of
  original core numbers, and `centrality_extended_gravity()`, the sum of
  immediate neighbors' raw k-shell gravity scores. Both use the simple
  undirected skeleton and assign isolates zero.
- Extended gravity supports `gravity_radius`, default three as in Ma et al.
  The radius applies around each neighbor before the outer sum. Independent
  checks use NetworkX cores and distances and an exhaustive core-number
  oracle on small graphs, covering multiple radii and input projections.

### Centrality Batch 16 — node resistance curvature

- Added `centrality_resistance_curvature()`, implementing Devriendt and
  Lambiotte's conductance-weighted node curvature component by component.
  Raw scores can be negative; isolates score one. Directed projection,
  zero conductances, normalization and numerical limits are documented.
- Independent checks use NetworkX effective resistance and exhaustive
  weighted spanning-tree degree expectations on small graphs. Dense
  electrical solves make this an explicitly requested costly measure.

### Centrality Batch 15 — dynamics-sensitive and Malatya centrality

- Added `centrality_dynamics_sensitive()` with `ds_beta`, `ds_mu` and
  `ds_steps`, including the full recovery-rate family from Liu et al.
  Recovery rate one recovers the finite-diffusion formula listed by Zoo;
  recovery zero supports the paper's SI case. These are linearized scores,
  with their interpretation and numerical limits documented explicitly.
- Added `centrality_malatya()`, the static degree-ratio sum. On nonisolated
  nodes it is exactly the reciprocal of the bridging coefficient. Both
  additions use the simple undirected skeleton and assign isolates zero.
- Verification uses independently expanded matrix/walk formulas and
  NetworkX neighbour-degree arithmetic, with documented parameter limits.

### Centrality Batch 14 — finite diffusion and dynamical importance

- Added `centrality_diffusion_centrality()` with `diffusion_q` and
  `diffusion_steps`. It evaluates Banerjee et al.'s finite weighted walk
  sum, separate from existing diffusion degree and the TNA power series.
  Directed edges follow their outgoing orientation; weights, loops and
  repeated walks are supported. Probability interpretation and default
  parameter choices are documented explicitly.
- Added `centrality_dynamical_importance()`: relative spectral-radius loss
  on node deletion, recomputed directly. It supports nonnegative directed
  weighted graphs, removes loops and returns NaN when the original radius
  is zero. This costly measure is held back from the default all tier.
- Independent numerical checks use explicit walk enumeration, NumPy matrix
  powers and eigenspectra, and SymPy exact characteristic polynomials.
- Fixed the existing TNA power-series method's missing binary fallback for
  unweighted igraph inputs. Normalization now preserves entirely undefined
  or empty result vectors without an empty-maximum warning.

### Centrality Batch 13 — volume and maximal cliques

- Added `centrality_volume()` with `volume_radius` and `centrality_mcc()`,
  also available through `centrality(measures = )`. Both use the simple
  undirected skeleton. Volume sums original degrees over closed hop
  neighbourhoods; MCC sums factorial contributions from maximal cliques.
- MCC explicitly excludes singleton cliques (isolates score zero), reports
  numerical overflow, and is held back from the default `type = "all"` tier
  because clique enumeration has exponential worst-case cost.
- Verification uses NetworkX neighbourhoods and maximal cliques, plus
  exhaustive recognition of maximal cliques on graphs of up to ten nodes.
- Corrected the catalogue's existing diffusion entry: default diffusion
  degree and the TNA power series are separate formulas. The Banerjee
  diffusion-centrality candidate remains pending.

### Centrality Batch 12 — verified parameter candidates

- Added `centrality_truss()`, `centrality_mdd()`,
  `centrality_bridging_coefficient()`, `centrality_godfather()` and
  `centrality_support()`, also available through `centrality(measures = )`.
  They use the simple undirected skeleton. `mdd_lambda` controls the
  exhausted-degree weight; truss numbers use the k-2 triangle convention.
- Documented LocalRank as the existing `semilocal` measure on simple
  undirected graphs. Corrected parameterized Zoo lookup calls and replaced
  unsupported equivalence claims based solely on rank correlation.
- Numerical checks cover NetworkX, centiserve and independent combinatorial
  references. The original 160 candidate rows remain tracked in
  `docs/zoo/parameter_candidate_status.csv`.

### Centrality Batch 7 — Centrality Zoo comparison batch

Five measures chosen from the Centrality Zoo correlation study (Shvydun
2025; 349 measures, 648 ICON networks, average Kendall tau) as the ones
with the **lowest rank redundancy** against what `centrality()` already
computed (maximum tau with any existing measure in parentheses). All are
implemented in base R on matrices (`R/kernels-batch7.R`) with thin igraph
glue and one exported verb each (`R/centrality-batch7.R`).

- `centrality_distance_entropy()` (tau 0.30) — Stella & De Domenico
  (2018). Normalised Shannon entropy of a node's hop-distance profile;
  closeness is the mean of that profile, this is its spread. The
  normaliser is `log(M - m + 1)` so a uniform profile scores exactly 1
  (the printed formula's `log(M - m)` is undefined for two distances).
- `centrality_local_dimension()` (tau 0.50) — Pu et al. (2014). OLS slope
  of `ln B(r)` on `ln r`, ball including the centre. Reproduces the worked
  example of Wen & Deng (2019) exactly (0.9231). Lower = more influential.
- `centrality_local_information_dimension()` (tau 0.38) — Wen & Deng
  (2020). Entropy-weighted local dimension over boxes up to half the
  eccentricity. Higher = more influential. Single-box nodes use the
  paper's discretised derivative.
- `centrality_modularity_vitality()` (tau 0.40) — Magelinski, Bartulovic &
  Carley (2021). `Q(G, C) - Q(G - i, C \ i)` under a fixed partition;
  positive = community hub, negative = bridge. Closed-form vectorised
  update (one matrix product for all nodes); matches brute-force
  `igraph::modularity()` after deletion on random directed, undirected and
  weighted graphs. Requires `membership`; wrong-length input raises
  `cograph_bad_membership`.
- `centrality_neighborhood_connectivity()` (tau 0.64) — Maslov & Sneppen
  (2002). Mean neighbour degree, isolates 0; equals
  `igraph::knn(weights = NA)`, with `mode` support.

The three distance-scaling measures are hop-count measures and ignore edge
weights (as `gravity` and `collective_influence` already do); they share one
unweighted all-pairs matrix per `centrality()` call.

### Centrality Batch 8 — the Zoo's "on the way" measures

The twelve measures the batch 7 lookup listed as "on the way" are now
implemented (thirteen `centrality()` measures), each verified against an
exact brute-force definition or the source paper's own numbers, plus an
independent Python reference written from the paper (kept in
`local_testing_and_equivalence/batch8/`, not shipped). Base-R kernels in
`R/kernels-batch8.R`, verbs in `R/centrality-batch8.R`.

- `centrality_shapley_game1()`, `_game2()`, `_game3()` — Michalak et al.
  (2013) closed-form Shapley values of the one-hop, `shapley_k`-neighbour
  and `shapley_cutoff`-hop coverage games. Equal to exact Shapley values from
  full coalition enumeration on random graphs of up to 8 nodes (with
  isolates, loops, several components), directed extension included.
- `centrality_access_information()`, `centrality_hide_information()` —
  Rosvall et al. (2005) / Sneppen et al. (2005) search information, averaged
  from and to each node; shortest-path DAG accumulation, no path
  enumeration. Equal to explicit all-shortest-paths enumeration; reproduces
  the papers' star and complete-bipartite values. Disconnected graphs
  average over each node's reachable set.
- `centrality_rumor()` — Shah & Zaman (2011) rumor centrality on each node's
  BFS tree, log scale. `exp()` of it equals brute-force spreading-order
  counts on trees; reproduces the paper's Fig. 5 (8, 12, 2, 3, 3).
- `centrality_community_hub_bridge()` — Ghalmane, El Hassouni & Cherifi
  (2019) raw hub-bridge score (needs `membership`; `cograph_bad_membership`
  on bad input).
- `centrality_entropy_variation(of = "degree" | "betweenness")` — Ai (2017)
  signed entropy drop on node deletion; degree variant in closed form.
  Equal to the author's own R code path to 1e-15 and to the paper's Table 2
  quantiles on its 4234-node network.
- `centrality_s_shell()` — Liu, Tang, Do & Hui (2017) strength-based shell
  index with asymmetric topological weights (`s_shell_a`, default 0.5). The
  Zoo's "s-shell index" is this measure, not the Eidsaa-Almaas s-core.
  Shells verified against the maximal-subgraph definition; `a = 0` gives
  k-core dense ranks.
- `centrality_degree_discount()`, `centrality_single_discount()` — Chen,
  Wang & Yang (2009) greedy seed-selection orders (`discount_p`), scored
  like `voterank` (first selected = 1).
- `centrality_ncvoterank()` — Kumar & Panda (2020) neighbourhood-coreness
  VoteRank (`ncvote_theta`). The original article could not be obtained;
  the definition follows the Zoo encyclopedia and three restatements, and
  the coreness normalisation (by its maximum) is a documented choice. Its
  VoteRank limit reproduces `networkx.voterank`.

The Centrality Zoo lookup article and coverage document were regenerated:
82 Zoo measures are now available in cograph and nothing is "on the way".

### `centrality()` tiers, and a catalogue of the measures

- **`type = "all"` no longer runs the measures whose cost grows steeply
  with network size.** Four are held back: `infection`, `two_way_rw`,
  `node_contraction_improved` and `entropy_variation_betweenness`. On an
  81-node graph `infection` alone took 611 seconds while every other
  measure together took about five, so a single `type = "all"` call could
  take minutes by accident. `type = "basic"` (the default) and
  `type = "extended"` are unchanged.
- **New `include` argument** puts them back: `include = "costly"` for all
  four, or name the ones you want. Naming a measure in `measures = `
  always computes it whatever its cost, so nothing became unreachable.
- **`list_centralities()`** (new export) is a tidy table of every measure
  with the facts you need before reading a column: `orientation` (which end
  of the scale marks a prominent node), `mode_aware`, `needs_membership`,
  `uses_weights` and `costly`. Twelve measures are oriented so that a *low*
  value marks the more central node, including `eccentricity`,
  `constraint`, `heatmap` and the local-dimension family; sorting their
  column the usual way puts the periphery on top.
  `list_centralities(orientation = "lower")` lists them. The measure lists
  now live in one place that both `centrality()` and `list_centralities()`
  read, and a test asserts the two agree.

## Bug fixes

- `katz` now warns with a `cograph_katz_diverged` condition when
  `katz_alpha` is too large for the graph. Katz converges only for
  `alpha < 1 / rho(A)`; above it the linear solve still returns numbers,
  but they are not Katz scores and can be negative. The default 0.1 is
  invalid on any graph whose spectral radius exceeds 10, which includes
  many weighted networks. The check costs nothing on the happy path and
  the warning names the valid bound.
- `alpha` and `power` now raise a classed `cograph_singular_system` error
  instead of surfacing a bare LU factorization message from igraph when
  `I - alpha A` is singular.
- `?centrality_dmnc` gains a "Divergence from centiserve" section.
  `centiserve::dmnc()` counts the largest component's edges in the wrong
  index space: its membership vector indexes the neighbourhood subgraph
  but is used to subset the original graph. The two disagree on 14 of the
  34 karate nodes even at a matched epsilon, and reproducing that indexing
  exactly reproduces centiserve's output. cograph counts the edges of the
  component it actually found. The catalogue's equivalence claim was
  corrected.

### Centrality Batch 9 — the remaining Zoo measures with a pinned definition

Twenty more measures (`R/kernels-batch9.R`, `R/centrality-batch9.R`),
each researched from its source paper by a dedicated agent and verified
against published tables, brute-force definitions and an independent Python
reference (kept in `local_testing_and_equivalence/batch9/`, not shipped).

- Community-aware (need `membership`): `centrality_community_based()`
  (Zhao et al. 2015; reproduces the paper's Table 1 and Tulu et al.'s
  Table 1), `centrality_comm_centrality()` (Gupta, Singh & Cherifi 2016;
  `comm_r`), `centrality_community_mediator()` (Tulu, Hou & Younas 2018;
  base-2 entropy reproduces its Table 1).
- Dimension family: `centrality_local_dimension_fixed()` (Silva & Costa
  2013; `ld_radius`), `centrality_fuzzy_local_dimension()` (Wen & Jiang
  2019; reproduces its kite Table 1 and karate top ten in order),
  `centrality_local_volume_dimension()` (Li & Deng 2021; definition from the
  authors' later preprint, flagged).
- VoteRank family: `centrality_wvoterank()` (Sun et al. 2019; reproduces
  all sixty numbers of its Figure 1), `centrality_enrenew()` (Guo et al.
  2020; reproduces its Figure 1; `enrenew_depth`), `centrality_voterank_plus()`
  (Liu et al. 2021; matches the authors' code; `voterank_lambda`).
- `centrality_node_contraction()` and `centrality_node_contraction_improved()`
  (Tan, Wu & Deng 2006; Wang et al. 2011; reproduce Table 1 and the path
  closed forms; `contraction_rho`). The Zoo entry's "removal" wording is
  wrong; the sources contract.
- `centrality_two_way_rw()` (Curado et al. 2022; reproduces the paper's toy
  example including every fraction; O(n^4)).
- Local measures: `centrality_heatmap()` (Duron 2020; reproduces Table 1;
  lower = more central), `centrality_flow_coefficient()` (Honey et al. 2007,
  BCT form; equals one minus clustering on undirected graphs),
  `centrality_local_entropy()` (Nie et al. 2016), `centrality_weighted_h_index()`
  (Gao et al. 2019), `centrality_redundancy()` (Burt 1992; Borgatti's
  worked example).
- `centrality_weighted_kshell()` (Garas, Schweitzer & Havlin 2012;
  `wks_alpha`, `wks_beta`; Figure 1 example and Table 2 core size),
  `centrality_renewed_coreness()` (Liu, Tang, Zhou & Do 2015; Figure 1 and
  all twelve percentages of its Table S1; the Zoo's transcription is off by
  one), `centrality_geodesic_kpath()` (Borgatti & Everett 2006; paths counted
  with multiplicity; `centiserve::geokpath` counts nodes instead).

Not shipped, with reasons recorded in the coverage document: DegreePunishment,
improved WVoteRank and local degree dimension (source articles unobtainable,
definitions rest on the Zoo alone) and multi-local dimension (a rescaling of
local dimension for every q outside (0, 1)).

New pkgdown article **Centrality Zoo lookup** answers "is the Zoo measure I
want in cograph?": every Zoo measure listed once under Available, Almost
identical (tau >= 0.99, with the cograph measure to use), Near-duplicate
(0.90 <= tau < 0.99), On the way, or Not available. `docs/CENTRALITY-ZOO-COVERAGE.md` records the full intersection of
the Zoo matrix with cograph: which Zoo measures are rank-identical to an existing
cograph measure (and therefore not worth adding), which are near-duplicates,
and the remaining ranked candidates.

### Centrality Batch 10 — the gaps against the other centrality packages

`docs/CENTRALITY-CROSS-COVERAGE.md` counted, in both directions, what each
R and Python centrality package reaches of the Zoo. The five node measures
that other packages offered and `centrality()` did not are now implemented
(`R/kernels-batch10.R`, `R/centrality-batch10.R`), each verified against
the package whose gap it closes:

- `centrality_local_efficiency()` — Latora & Marchiori (2001). Global
  efficiency of the subgraph induced on a node's neighbours, the node
  removed. Matches `brainGraph::efficiency(type = "local")` and the
  networkx induced-subgraph form on 25 random graphs. Note that
  `igraph::local_efficiency()` measures those distances *through the rest
  of the network* and so reports larger values; `network_local_efficiency()`
  keeps its igraph parity and its help page now says so.
- `centrality_s_core()` — Eidsaa & Almaas (2013). The weighted k-core: the
  largest strength threshold whose core still contains the node. Matches
  `igraph::coreness()` on unweighted graphs and a brute-force reading of
  the definition on weighted ones. `brainGraph::s_core()` returns the
  peeling round instead, which is documented as a divergence.
- `centrality_fragmentation()` — Borgatti (2006). Distance-weighted
  fragmentation after deleting the node; matches `keyplayer::fragment()`.
  This is the Zoo's "Distance-weighted fragmentation", taking cograph's Zoo
  coverage to 103 of 349.
- `centrality_kpath()` — Sade (1989). Simple paths of length at most
  `kpath_len` that the node lies on; matches the per-vertex counts of
  `sna::kpath.census()` for k = 2 and 3, directed and undirected.
- `centrality_epc()` — Lin et al. (2008), the cytoHubba edge percolated
  component. Recovers the exact bond-percolation mean on small graphs and
  matches the normalisation of `centiserve::epc()`. Monte Carlo: pass
  `epc_seed` for a reproducible value; the caller's random stream is
  restored.

`fragmentation` and `epc` join the costly list, so `type = "all"` holds
them back (`include = "costly"` or naming them restores them).

Three of the reported gaps turned out not to be gaps at all, and the
document now says so with the evidence: `centiserve::closeness.latora()` is
cograph's `harmonic` exactly, `centiserve::communibet()` is
`communicability_betweenness` exactly, and `brainGraph::efficiency(type =
"nodal")` is `harmonic` over `n - 1`. Two remain unimplemented and are
listed with the reason: the link-community centrality of Kalinka &
Tomancak (its reference package `linkcomm` is archived, so no equivalence
check is possible) and `keyplayer::kpset()` (a set search, not a node
measure).

### Centrality Batch 11 — tuning the families cograph already had

160 of the Zoo's measures sit at a rank correlation of 0.90 or better with
something `centrality()` already computes
(`docs/zoo/parameter_candidates.csv`), which suggests many are the same
family at a different setting rather than different ideas. The first five
investigated bear that out, and four new measures plus two arguments cover
seven more Zoo labels (coverage 103 -> 110 of 349):

- `centrality_length_scaled_betweenness()` — Borgatti & Everett (2006),
  Brandes (2008) Algorithm 5. Betweenness with each separated pair weighted
  by `1 / d(s,t)`.
- `centrality_delta_betweenness()` — Agneessens, Borgatti & Everett (2017).
  Betweenness with the pair weight `(d - 1)^-delta`
  (`betweenness_delta`, default 1); `delta = 0` is ordinary betweenness.
- `centrality_ego_betweenness()` — Everett & Borgatti (2005). Betweenness
  inside the node's own ego network. Close to `effective_size`, and a test
  pins that it is not a function of it.
- `centrality_delta_closeness()` — Agneessens et al. (2017) eq. 2.
  `sum_j d_ij^-delta / (n-1)` (`closeness_delta`, default 1). One exponent
  spans the family: `delta = 1` is `harmonic` over `n-1`, `delta = 2` is
  `harary` over `n-1`, a large `delta` approaches degree, `delta = 0`
  counts the reachable set.
- **Bounded-distance ("k-") betweenness needs no new measure**:
  `centrality(x, measures = "betweenness", cutoff = k)` already computes
  it, verified against a brute-force reading of the definition on directed
  and undirected graphs. It is now mapped as covered.

All four are exact under a brute-force enumeration of weighted geodesic
pairs (`local_testing_and_equivalence/batch11/run_equivalence.R`, 6 blocks,
6 PASS).

## Bug fixes

- **`gravity` computed a formula that appears in no paper.** It summed
  `deg(j) * kshell(j) / d(i,j)^2` over every reachable `j`: the product of
  two masses on the partner, none on the focal node, and no truncation. Its
  help page cited Li et al. (2019), whose formula is `k_i k_j / d^2`.
  Dropping the focal mass changes the ranking, not just the scale. The
  measure now computes `m_i m_j / d^exponent` and gains two arguments:
  `gravity_mass` (`"kshell"` default, `"degree"`, or `"legacy"`) and
  `gravity_radius` (a number, default 3, `"auto"` for half the mean
  distance, or `NULL`). The default is now Ma, Ma, Zhang & Wang (2016);
  `gravity_mass = "degree", gravity_radius = NULL` is Li et al.'s gravity
  model and `gravity_radius = "auto"` their local gravity model, so one
  measure covers three Zoo labels. **`gravity` returns different values
  than in 2.4.7 and earlier**; `gravity_mass = "legacy"` with
  `gravity_radius = NULL` reproduces the old numbers exactly, and a test
  pins that.

# cograph 2.4.6

## New features

- `splot()` on a `netobject` with `method = "entropy"` (Nestimate's
  `entropy_network()`) now receives TNA styling — oval layout, TNA palette,
  initial-probability donuts — instead of falling through to psych styling,
  so the entropy re-weighting of a transition network renders comparably
  with its source.

- `splot()` now accepts `label_abbrev`, matching `mcml`: use an integer for
  a fixed maximum label length, `"auto"` for node-count-aware abbreviation,
  or `NULL` to retain full labels.

- **Producer-supplied splot metadata** (`x$meta$splot`): packages that create
  cograph-plottable objects can now attach a small rendering contract —
  `renderer` (resolved through a cograph-maintained whitelist of existing
  renderers; arbitrary function names are never evaluated), `weight` (which
  stored edge quantity to render: an edge column keeps the producer's edge
  set, a matrix redefines the drawn network from its nonzero cells, aligned
  by dimnames), and `defaults` (renderer arguments). Precedence is always
  `user arguments > meta$splot$defaults > cograph defaults`; on the regular
  network path this includes deprecated argument aliases (a user-supplied
  `positive_color` still beats a metadata `edge_positive_color` default).
  See `?splot`, section "Producer-Supplied splot Metadata".

## Bug fixes / changes

- **Motif subsystem overhaul** following an adversarial review (13 findings,
  each verified against igraph before fixing):
  - **`motif_census()` mislabeled 13 of the 16 directed triad classes**: it
    attached MAN-order names to `igraph::motifs()` output, which is in
    igraph's isomorphism-class order (a pure 021U triad was reported as
    `102`). The directed 3-node census now uses `igraph::triad_census()`,
    whose ordering *is* MAN order. Counts were internally consistent —
    z-scores compared like with like — but carried the wrong names.
    `motifs()`, `subgraphs()` and `triad_census()` were never affected.
  - The undirected census gains the missing one-edge class
    (`empty`/`edge`/`wedge`/`triangle`), and self-loops are stripped before
    counting (they are not part of any 3-node class).
  - The `"configuration"` null model now uses exact degree-preserving edge
    rewiring. The old stub-matching + `simplify()` silently changed degrees,
    and the undirected `"vl"` sampler errored on graphs with isolates and
    restricted the ensemble to connected graphs (two disconnected triangles
    got `sd = 0, z = 0, p = 1` for an observation the null could never
    produce).
  - All motif p-values are now **empirical permutation p-values**
    (add-one corrected) instead of Gaussian approximations, and a degenerate
    null (sd = 0) yields `z = NA` when the observation differs from it —
    never a silent `z = 0`. Zero-variance handling was previously
    inconsistent across the three engines (forced 0 / sd := 1 / sd := 0.1).
    `n_random` / `n_perm` below 2 is now an error.
  - `motifs(pattern = "all")` now actually includes the `003` class; a full
    census sums to `choose(n, 3)` and matches `igraph::triad_census()`
    class by class.
  - Instance-mode significance (`subgraphs()`) now tests the null
    probability that a triple instantiates **the row's own MAN type**; the
    old null counted "any of the six edges exists" (for ten subjects each
    with a 3-cycle, expected was 8.33 instead of the correct 3.33).
  - Instance mode reports one row per (triple, MAN type) instead of
    collapsing to a dominant type, so per-type totals now agree with census
    mode on identical data.
  - Instance-mode significance on aggregate input (a single matrix) now
    warns and reports `params$significance = FALSE` instead of silently
    returning results without the promised `z`/`p` columns.
  - Census significance on a symmetric directed matrix now runs a directed
    null (previously the undirected `empty`/`wedge`/`triangle` names never
    matched a MAN row, yielding all-NA statistics).
  - `extract_motifs(level = "aggregate")` now actually pools the
    per-individual transition matrices (previously only metadata changed),
    and `min_transitions` applies per-triad at aggregate level as
    documented.
  - `motif_census(x, directed = ...)` conflicting with an igraph input's
    own directedness is now an error instead of relabeling without
    converting.
  - `edge_method = "percent"` thresholds above 1 are percentages and the
    comparison is `>=` as documented (the old `> total * 1.5` default could
    never classify anything); fractional edge weights are rounded, not
    truncated, when building permutation stubs.
  - `plot.cograph_motifs(type = "network")` forwards `...` to the per-motif
    igraph plots as documented, and pattern-panel significance decoration
    is suppressed for legacy per-triple results where a per-type lookup
    would be ambiguous.
  - A Monte Carlo equivalence suite now validates the whole subsystem on
    1000 generated datasets per run — 18 topologies (random, scale-free,
    small-world, rings, stars, tournaments, DAGs, bipartite blocks,
    disconnected, isolates, empty), 3-60 nodes, directed / weighted /
    undirected / multi-actor inputs — against `igraph::triad_census()`,
    brute-force triple enumeration, and per-actor reference censuses.
  - The legacy `extract_motifs()` individual-level path now retains one row
    per `(node triple, MAN type)` and tests that exact type under the same
    weighted stub-matching null as individual-level `subgraphs()`. It no longer
    collapses mixed-type triples to a dominant label or counts any permuted
    type as a match.
  - Fractional transition data now produce balanced integer in/out stubs;
    low-activity units (including one-transition and empty units when allowed)
    participate correctly in instance nulls, and one-element stub vectors are
    shuffled without R's `sample(x)` length-one ambiguity.
    Unit eligibility is frozen from the original loopless weighted activity,
    and every positive edge retains support during integerization.
  - `sig`, printing, and motif plot colors now consistently follow the
    empirical permutation decision (`p < .05`) instead of mixing it with
    `|z| > 1.96` or `|z| > 2` cutoffs. Parallel edges are simplified before
    `motif_census()` so observed and null graphs use the same simple-graph
    projection.
  - Instance results now include structured `node1` / `node2` / `node3`
    columns in addition to the existing `triad` display label, so node names
    containing `" - "` remain unambiguous in statistics and plots.
  - Directed size-4 motif rows now use one consistent `M1`–`M218` naming
    sequence covering all igraph isomorphism slots (199 are connected), and
    `directed=` conflicts are rejected consistently for both igraph and
    cograph-network inputs.
  - Degenerate-null rows (`z = NA` with the smallest possible empirical p,
    emitted when the observation lies outside a zero-variance null) are now
    treated as the strongest findings everywhere: they rank first in sorted
    results, survive `top = n` cuts instead of being silently truncated, and
    the significance plots report them with a message instead of silently
    dropping them (a z bar cannot be drawn for them).
  - `plot(x, type = "triads")` no longer errors on fractional weighted
    counts (e.g. probability matrices at aggregate level); whole numbers
    keep the plain `n=3` caption.
  - Weight validation for permutation nulls applies only to null-eligible
    units, so a malformed cell in a unit excluded by `min_transitions` no
    longer aborts the whole significance run (malformed cells in eligible
    units still error loudly).

- `plot_transitions()` with a multi-column data frame (the consecutive
  multi-step branch) no longer silently drops styling arguments: `value_min`,
  `label_color`, `label_fontface`, `label_nudge`, `title_color`,
  `title_fontface`, `value_halo`, `value_fontface`, `value_nudge`, and
  `total_fontface` are now forwarded, so both multi-step input forms (list
  of matrices, data frame) respond to the same arguments identically.

- Motif pattern plots (`plot(motifs(x), type = "network")`, triad glyph
  panels) drew the wrong structure for five of the sixteen MAN triad classes:
  the `021D` and `021U` glyphs were swapped (transposed matrices), the `120D`
  and `120U` glyphs both drew a `120C`-isomorphic triad, and the `210` glyph
  drew a `120`-class triad — so the `120U` and `210` structures were never
  drawn at all. Only the drawn glyphs were wrong: motif *counts*,
  significance tests, and `triad_census()` were always computed from a
  separate, correct canonical pattern set (verified against
  `igraph::triad_census()`). All sixteen visual patterns are now verified
  against igraph by a regression test that also pins the visual and
  canonical sets to each other. Thanks to Mengli Zhang for reporting
  (reconstructing the structures from the package source and spotting that
  three "distinct" glyphs were isomorphic).

- `plot_bootstrap_forest()`, `extract_motifs()`, `motif_census()`,
  `triad_census()` and `extract_triads()` are now listed in the package index
  and the reference site. All five are exported and user-facing, but carried
  `@keywords internal`, which hid them from `help(package = "cograph")` — you
  could only find them if you already knew the name. `mcml()` remains hidden;
  it is a deprecated alias of `csum()`. The `n` and `...` arguments of
  `print.cograph_motif_analysis()` and `print.cograph_motifs()` are now
  documented (previously exempt from checking by the `internal` keyword).

- `plot_difference()`'s new `difference` argument moved to the end of the
  signature, after `combined`. It had been inserted *before* `combined`, which
  shifted the positional argument order relative to the released 2.3.6
  signature. Because `plot_compare()` is `function(x, ...)` and forwards to
  `plot_difference()`, a caller passing 14 positional arguments had their 14th
  silently rebound from `combined` to `difference` — making the function treat
  `x` as an already-subtracted matrix, discard `y`, and draw the wrong network
  with no error. `difference` was introduced after the last CRAN release, so no
  released behaviour changes. Named calls were never affected.

- `plot_bootstrap_forest()` and `plot_edge_diff_forest()` no longer emit a
  `geom_errorbarh()` deprecation warning under ggplot2 4.0.0. The four
  horizontal error-bar layers now use
  `geom_errorbar(orientation = "y")`; the rendered output is unchanged.
  `DESCRIPTION` now declares the `ggplot2 (>= 3.4.0)` requirement the package
  already had (it uses the `linewidth` aesthetic throughout).

- `plot_edge_diff_forest(layout = "chord")` no longer emits a spurious
  "row names were found from a short variable and have been discarded"
  warning for every node arc it draws.

- `aggregate_layers()`, `supra_adjacency()`, `layer_similarity_matrix()` and
  `plot_motifs()` now ship runnable examples. Their `\examples` sections were
  previously commented out (or entirely `\dontrun`), so they demonstrated
  nothing and were never checked. The remaining `\dontrun` blocks in
  `motifs()` and `extract_motifs()` are now `\donttest`, so they are executed
  under `R CMD check --run-donttest`.

- `detect_communities()` with the `"louvain"` (the default) or `"leiden"`
  method no longer errors on a **directed** graph. These igraph algorithms
  are undirected-only, so `detect_communities(tna_object)` — a tna model is
  always directed — aborted with "Multi-level community detection works for
  undirected graphs only". It now collapses the directed edges to undirected
  (mean, as the `"fast_greedy"` method already did) with a message, so the
  package's primary object type works with the default algorithm. This also
  fixes `plot_htna(x, community = "louvain")` and other internal callers that
  ran community detection on a directed model.

- `splot()` on a Nestimate `netdifference` (from `subtract_networks()` /
  `as_netdifference()`) now routes to `plot_difference()`. Previously it fell
  through to the `netobject` path, which styles by `$method` — "difference"
  is not a TNA-family method, so the asymmetric difference matrix was drawn
  with undirected psych styling: no arrowheads and one triangle of each
  asymmetric edge pair silently dropped. `splot(d, minimum = 3)` is now the
  straightforward call for a signed difference network.

- The `netdifference` routing excludes `net_permutation`-family objects:
  `net_bayes` carries both classes and must keep reaching
  `splot.net_permutation`, whose per-edge CI/star arrays are aligned by
  `Nestimate::plot.net_bayes` to that renderer's edge ordering.

- `plot_difference()` on a `netdifference` now draws the display matrix
  (`$weights` — e.g. only the credible differences when coerced with
  `as_netdifference(b, significant_only = TRUE)`), falling back to
  `$difference_matrix`. For `subtract_networks()` results the two are
  identical, so nothing changes there.

- `plot_permutation()` / `splot.net_permutation()`: the `title` and `layout`
  defaults now use exact `[[` indexing. `args$title` on a dots-list holding
  `title_size` (but no `title`) partially matched `title_size`, so the
  default title was silently skipped and no title was drawn — this is why
  `Nestimate::plot.net_bayes()` output had no title. Same latent hazard
  fixed for `layout` / `layout_scale`.

- Edge label templates gain a `{p_diff}` placeholder (probability of the
  difference, for Bayesian comparisons), fed by the new `edge_label_p_diff`
  argument — a per-edge vector or a full node-by-node matrix (the matrix is
  indexed at each drawn edge, so it survives `minimum`/`threshold`
  filtering, and is aligned by dimnames so it may be supplied in any node
  order). Filled automatically from `$p_difference` by
  `splot.net_permutation` and by `plot_difference()` on Bayesian
  `netdifference` coercions. Template example:
  `edge_label_template = "{est} (P={p_diff})"`.

- `splot.netobject()` styling classifier: `"edge_betweenness"` networks are
  now styled by their directedness. A directed edge-betweenness network
  previously fell into psych styling — drawn undirected, silently losing one
  direction of each asymmetric pair; it now gets the TNA presets with arrows.
  An undirected one (from a correlation-family source — Nestimate preserves
  the source's directedness) keeps the psych look.

- Nestimate producers now use the `meta$splot` contract: `netdifference`
  objects carry `renderer = "difference"` and `net_bayes` carries
  `renderer = "permutation"`, so metadata routing (which runs before class
  dispatch) selects the renderer; the `netdifference` class branch remains
  as a fallback for objects built without metadata.

# cograph 2.4.3

## Bug fixes / changes

- `plot_difference()` no longer hides small difference edges: it defaults
  `minimum = 0` (the style presets otherwise injected `minimum = 0.01`,
  silently dropping edges with `|x - y| < 0.01`). An explicit `minimum`
  still wins.

- `plot_difference(x, y, difference = TRUE)` now warns that `y` is ignored
  and uses `x` as the difference network, instead of silently computing
  `x - y`.

# cograph 2.4.2

## Bug fixes / changes

- `plot_compare()` is **no longer deprecated** — it is a plain alias of
  `plot_difference()`. `tna::plot_compare()` delegates to it by name
  (`cograph::plot_compare(x, y, ...)`), so deprecating it wrongly made every
  `tna::plot_compare()` call emit a warning; the warning is removed. Both
  names call the same implementation; `plot_difference()` is the preferred
  spelling for new cograph code.

- `plot_difference()` also auto-detects a Nestimate `netdifference` object
  (or any object exposing `$difference_matrix`), alongside `tna_comparison`.

# cograph 2.4.1

## New features

- `plot_difference()` can now consume a **pre-computed difference network**:
  a `tna_comparison` object (from `tna::compare()`) is detected automatically
  and its `$difference_matrix` is plotted, and `difference = TRUE` treats `x`
  as an already-subtracted matrix/network (no `y` needed). The two-network
  `plot_difference(x, y)` path is unchanged.

# cograph 2.4.0

## New features

- Two focal-node flow layouts, usable anywhere a layout name is accepted
  (`splot(x, layout = "target")` / `layout = "saqr"`):
  - `layout_target()` ports qgraph's `flow()` — places one node of
    interest (`target =`) on the left and every other node in columns by
    unweighted BFS distance (hops). Unlike qgraph it handles disconnected
    graphs (isolated nodes go to a trailing column) instead of erroring.
  - `layout_saqr()` ports the Dynalytics Desktop "saqr" transition layout
    (Saqr et al., LAK25): Start on top, End on bottom, middle nodes ranked
    by outgoing weight from Start and split into 2–3 sine-enveloped rows
    with a zig-zag first row (`start =`, `end =`, `jitter =`).

## Bug fixes / changes

- `plot_difference()` now **styles the difference network automatically**
  instead of drawing bare default-blue nodes: an undirected difference
  gets the psychometric look (Okabe-Ito node palette, no arrows, thin
  edges), a directed difference gets the TNA look (TNA palette, arrows).
  Node size uses the calibrated preset (previously nodes could render
  near-invisible), and edges stay coloured by the sign of the difference.
  Explicit `node_*` arguments still override the preset.

- `plot_difference()` is added as the preferred name for the
  difference-network plotter. `plot_compare()` remains a first-class alias
  of it (`tna::plot_compare()` delegates to `cograph::plot_compare()` by
  name, so the name must keep working).

- `plot_difference()` (the renamed difference-network plotter) now treats
  an S3 `cograph_network` (which is itself a list — e.g. a `psychnet`
  fit, a Nestimate `netobject`, or any `as_cograph()` result) as a single
  network. Previously such an object fell into the "plain list of
  networks" branch and was misread as a list of sub-networks, failing
  with "x must be a matrix, cograph_network, tna, or igraph object".
  Comparing two psychnet/netobject networks with
  `plot_difference(net1, net2)` now works.

# cograph 2.3.11

## New features

- `dyad_census()` classifies every dyad of a directed network into
  mutual (M), asymmetric (A), or null (N), returning a tidy
  one-row-per-type data.frame with counts and proportions and a
  dyad-based reciprocity (`2M / (2M + A)`) attribute. It is the
  dyad-level companion to `triad_census()`. Undirected input counts
  every edge as a mutual dyad.

- `ego_networks()` reports tidy per-ego personal-network metrics — size,
  ego/alter tie counts and densities, and Burt's structural-hole
  measures (`effective_size`, `constraint`, `order = 1` only) — with one
  row per ego. The structural-hole columns reuse the same
  implementations as `centrality()`, so they match
  `centrality(x, measures = c("effective_size", "constraint"))` exactly.

# cograph 2.3.10

## Bug fixes / changes

- Bootstrap plots of undirected co-occurrence networks
  (`splot.net_bootstrap`) now default to the `"oval"` layout instead of
  the force-directed `"spring"` layout, matching `splot.tna_bootstrap`.
  Pass `layout = "spring"` to restore the previous behavior.

- Bootstrap plots now auto-suppress the `".00"` decimal tail on
  integer-valued weight matrices (co-occurrence counts, raw
  frequencies): `266.00**` renders as `266**`. Detection mirrors
  `splot.netobject` — when every nonzero weight is a whole number and the
  user has not set `weight_digits`, both `weight_digits` and
  `edge_label_digits` default to `0`. Applies to both `splot.net_bootstrap`
  and `splot.tna_bootstrap`. Non-integer (correlation/GLASSO) networks are
  unaffected, and an explicit `weight_digits` always wins.

# cograph 2.3.9

## New features

- `plot_mcml()` gains a `theme` argument: `"classic"` (default — the
  established pie-node / straight-edge look, now with thinner node and
  shell borders and slightly larger detail nodes), `"rich"` (donut nodes
  on both layers plus curved summary edges and splot self-loops), and
  `"light"` (`"rich"` with no shell outline and a softer fill). Granular
  overrides `node_donut`, `node_donut_inner_ratio`,
  `summary_donut_inner_ratio`, `summary_donut_show_value`, `curved_edges`,
  and `summary_curve` win over the preset.

- `plot_mcml()` now colors edges by weight sign on every layer
  (within-cluster, between-cluster, summary, and self-loops) via
  `edge_color_by`: `"auto"` (default) keeps cluster coloring for
  non-negative transition networks but switches to sign coloring when any
  negative weight is present (correlation / association networks),
  `"cluster"` and `"sign"` force either mode. Positive edges use
  `edge_positive_color` (`"#2E7D32"`, green) and negative edges
  `edge_negative_color` (`"#C62828"`, red), matching `splot()`. Edge
  visibility thresholding and width scaling now use the absolute weight,
  so negative edges are drawn rather than silently dropped, and a
  positive/negative key is added to the legend when sign coloring is
  active.

- `plot_mcml()` summary-node labels are now placed "on the clock": each
  label sits just outside its node in the cardinal direction the node
  points from the arrangement center (top at 12, bottom at 6, left at 9,
  right at 3), anchored at the node boundary so it always clears the node
  regardless of `summary_size`. An explicit `summary_label_position` still
  overrides this.

# cograph 2.3.8

## New features

- `plot_mcml()` and `splot()` accept `mcml_pc` objects
  (`Nestimate::build_mcml_pc()`, experimental psychometric MCML) and
  render them undirected via their `meta$directed` flag.

# cograph 2.3.7

## Breaking changes

- The exported names `cluster_summary()` and `build_mcml()` are removed
  to end, permanently, the collision with `Nestimate::cluster_summary()`
  and `Nestimate::build_mcml()` — different functions that silently
  masked each other depending on package attach order (the same disease
  as the `cluster_network()` alias removed in 2.3.6, where load order
  silently flipped results between raw counts and row-normalized
  probabilities). Migration is name-for-name with identical behavior:
  - `cluster_summary(...)` → `csum(...)` (the existing short alias is
    now the canonical exported name; same arguments, same
    `cluster_summary` return object).
  - `build_mcml(...)` → `summarize_clusters(...)` (same arguments, same
    `mcml` return object).
  In sessions where both packages are attached, the bare names
  `cluster_summary()` / `build_mcml()` now always refer to Nestimate's
  data-layer verbs, regardless of attach order. The `as_tna()` generic
  is intentionally exported by both packages: the definitions are
  identical (`function(x) UseMethod("as_tna")`), so masking is harmless
  and S3 methods from both packages dispatch correctly.

## New features

- `plot_mcml()` gains a `directed` argument (default `NULL` =
  auto-detect). Undirected rendering suppresses arrowheads on all three
  edge layers (within-cluster, between-cluster, summary), draws each
  symmetric edge pair once instead of twice (previously a symmetric
  matrix produced overplotted reciprocal arrows), and moves edge labels
  to the edge midpoint. Auto-detection reads `$meta$directed` from
  `cluster_summary`/`mcml` input (e.g., co-occurrence aggregations such
  as `Nestimate::build_mcml(type = "cooccurrence")` now render
  undirected with no extra flag), the `$directed` field of network
  objects, or matrix symmetry for plain matrices — the same contract as
  `splot()`, which forwards `directed` when dispatching
  `mcml`/`cluster_summary` objects.
- `plot_mcml()` undirected matrix input is aggregated with
  `cluster_summary(type = "cooccurrence")` (symmetrized counts) instead
  of the row-normalized `type = "tna"`, whose output is asymmetric even
  for symmetric input and cannot be represented by undirected drawing.
  When `directed = FALSE` is forced on weights that are not symmetric,
  `plot_mcml()` now warns that only the upper triangle is drawn.

## Bug fixes

- `cluster_summary()` and the sequence path of `build_mcml()` now record
  the *effective* directedness in `$meta$directed`: `FALSE` when
  `type = "cooccurrence"` (which symmetrizes the weights), instead of
  echoing the `directed` argument unchanged.

# cograph 2.3.6

## Bug fixes

- Removed the `cluster_network()` alias for `summarize_network()`. It
  collided with `Nestimate::cluster_network()` — a completely different
  function (PAM clustering on sequence data, one network per cluster) —
  and the two silently masked each other depending on package attach
  order, producing confusing `unused arguments (k = ..., cluster_by =
  ...)` errors. Use `summarize_network()` (or its remaining short form
  `cnet()`) for matrix-to-cluster aggregation in cograph.

# cograph 2.3.5

## Documentation

- Added Sonsoles López-Pernas as co-copyright holder in `LICENSE`.
- README / docs wording fixes (e.g. "hyper order" → "higher-order").
- Introduction vignette no longer asserts a fixed centrality-measure
  count, which kept drifting as the measure set grew.

# cograph 2.3.4

## Bug fixes

- `.smooth_blob()` (used by `plot_simplicial()` and
  `overlay_communities()`) now guards `grDevices::chull()` against
  non-finite anchor coordinates. Previously a node lacking layout
  coordinates (NA/Inf) aborted the blob with "finite coordinates are
  needed"; such anchors are now dropped before the convex-hull step.

# cograph 2.3.3

## Documentation

- Aligned the `motifs()` / `subgraphs()` roxygen documentation with the
  post-audit behavior shipped in 2.3.2 (census `type_summary` counts,
  `min_count` handling, and corrected plot legend descriptions).

# cograph 2.3.2

## Bug fixes

- Full audit pass over the motifs subsystem: `type_summary` now holds
  real MAN-type counts in census mode, `min_count` is honored in census
  mode, and the swapped source/target color description in
  `plot.cograph_motif_result()` is corrected.
- Unbroke `motifs()` and `plot_simplicial()` on Nestimate-backed
  workflows (HON / HYPA sequence inputs).
- `panel_layout()`: tightened dimension validation and made the
  restoration claim honest — it now restores only the `par()` settings
  it actually changed.

# cograph 2.3.1

## Multi-panel layout control

- New `combined` argument (default `TRUE`) on every multi-panel plot
  function: `splot()` group-cascade, `plot_netobject_group()`,
  `plot_netobject_ml()`, `plot_net_bootstrap_group()`,
  `plot_group_permutation()`, `plot_compare()`, `splot.net_mlvar()`,
  `plot_network_evolution()`, `plot.cograph_motifs()`,
  `plot.cograph_motif_result()`, `plot.cograph_motif_analysis()`, and
  `plot.tna_disparity()`. With `combined = FALSE` these functions draw
  panels into the active device without calling `graphics::par(mfrow=...)`,
  so callers can drive their own layout (e.g. `graphics::layout()` or
  the new `panel_layout()` helper). Default `TRUE` preserves prior
  behavior — every existing call site renders identically.
- New `panel_layout()` helper sets up a custom multi-panel device layout
  for use with `combined = FALSE`. Accepts either a uniform-grid
  `c(nrow, ncol)` or a `graphics::layout()` matrix for non-uniform
  layouts (e.g. one wide panel + two narrow ones). Returns a `par()`
  snapshot for restoration via `on.exit()`.

## Test suite hygiene

- `test-coverage-splot-{41,42}.R`: bumped `n_nodes` from 4 to 10 in
  seven per-edge attribute tests so the seed=42 sampler does not
  produce duplicate (1,2) pairs that trip cograph's
  undirected-duplicate-edge detector.
- `test-coverage-class-network-41.R`: aligned the
  `set_layout_coords()` mismatched-row-count test with the strict
  input validation already enforced by `R/class-network.R`.
- `test-overlay-communities.R`: prefixed two `communities()` calls
  with `cograph::` to avoid `tna` masking when both packages are
  loaded in the suite (per CLAUDE.md "namespace masking" gotcha).

# cograph 2.3.0

## Documentation

- Audited every R/*.R function file for roxygen/Rd accuracy. Corrected
  stale defaults (`cr_color` `#D4820A` -> `#D4829A` in `plot-forest.R`;
  `show_value` default `FALSE` -> `TRUE` in `splot-nodes.R`), corrected
  dataset dimensions in `data-hai.R` (`302` -> `429 x 287`), corrected a
  reference to the nonexistent `igraph::is_bipartite()` (now
  `bipartite_mapping()`), expanded `centrality()` `@param` measure
  lists for `mode`, `cutoff`, `invert_weights`, and `membership` to
  match the implementation, dropped baked-in measure counts that rot
  on each addition, and removed nonexistent themes from `sn_theme`
  documentation. No runtime behavior changes from the documentation
  pass itself.

## Bug fixes

- `plot_simplicial()` now warns when `anomaly` is set on an input that
  has no anomaly concept (HON, association rules, link prediction,
  character pathways, `method = "hon"` / `"rules"`). Previously the
  argument was silently dropped, so calls like
  `plot_simplicial(hon, anomaly = "over")` and
  `plot_simplicial(hon, anomaly = "under")` produced byte-identical
  plots. `anomaly` is honored only for `net_hypa` inputs and
  `method = "hypa"` auto-builds.

## Centrality

- `centrality()` gains an umbrella argument `tna_network` (logical or
  NULL). When `TRUE` (or auto-detected from a `tna`/`group_tna`/`ctna`/
  `ftna`/`atna` input), all measures shared with `tna::centralities()`
  match byte-for-byte: `loops = FALSE`, `invert_weights = TRUE`,
  `diffusion_method = "power_series"`, `transitivity_type = "onnela"`.
  Side-by-side audit confirms zero divergence on `OutStrength`,
  `InStrength`, `ClosenessIn/Out/All`, `Betweenness`, `Diffusion`,
  `Clustering` (`max|diff| = 0`). Any per-argument override the user
  passes explicitly always wins over the umbrella.
- `centrality()` (and `centrality_diffusion()`) gain a
  `diffusion_method = c("kandhway_kuri", "power_series")` argument. The
  default `NULL` auto-detects: `"power_series"` for tna inputs (matches
  `tna::centralities(., measures = "Diffusion")` byte-for-byte when
  `loops = FALSE`), `"kandhway_kuri"` (the existing 1-hop binary-degree
  formula, Kandhway & Kuri 2014) for everything else. Previously
  cograph's diffusion silently disagreed with tna's because cograph used
  an unweighted neighborhood-degree sum while tna uses
  `rowSums(P + P^2 + ... + P^n)` on the diagonal-zeroed weighted matrix
  — the same name covered two different statistics. Set explicitly to
  override the auto-detect.

## Tests

- Added a regression test in
  `tests/testthat/test-validate-nestimate-bootstrap-permutation.R`
  asserting that `centrality()` on a Nestimate `netobject` agrees with
  `centrality()` on its `$weights` matrix when the diagonal is
  non-zero. Locks in the upstream Nestimate fix to
  `.extract_edges_from_matrix()` (Nestimate >= 2026-05-02) which now
  preserves self-loops in `$edges`. Without that fix, loop-bearing
  netobjects (e.g. `Nestimate::build_mcml() |> Nestimate::as_tna()`)
  silently under-counted node degree by 2.

## Plotting — edge-label cex coupling (Phase 2)

- Default `edge_label_size` is now coupled to the node label cex at a
  fixed 0.55 fraction (`edge_cex = 0.55 * mean(node_label_cex)`) so the
  node-to-edge-label ratio stays a stable ~1.82x across canvases. This
  replaces the previous `EDGE_LABEL_SCALE_CAP`-based compensation, which
  let the ratio drift from 2.5x at reference to 3.6x at poster canvases
  because edge labels were clamped to a tighter 1.6 ceiling while node
  labels scaled freely to 2.3. The visible effect: edge weight
  annotations are now readable at poster sizes instead of shrinking
  relative to node labels. User-explicit `edge_label_size` still wins
  and receives the same (capped) visual-scale compensation as before;
  only the default path changed.
- Edge-label visual_scale resolution moved from `render_edges_splot()`
  into `splot.R` so the final cex is produced in a single place.

## Plotting — device-aware visual scaling

- `splot()` now applies device-dependent compensation to text, line, and
  point sizes so visual ratios (label-to-node, legend-to-plot, edge
  thickness) stay consistent when the output device changes. This fixes the
  long-standing "labels too big at high DPI" and "legend desynchronised from
  the plot" issues when saving PNGs at `res = 300` or `res = 600` with
  pixel-default `width`/`height`, and when resizing the RStudio plot pane.
  Implementation: a single `compute_visual_scale()` reads the active
  device's canvas size (`dev.size("in")`) and returns multipliers keyed off
  a 5.9-inch reference (matching the default RStudio 7×5" pane so
  backward-compatible behaviour at the default canvas is preserved).
  Multipliers are clamped to `[0.55, 1.9]` to keep thumbnails and posters
  legible. See the new `R/visual-scale.R`.
- New `scaling = "fixed"` mode on `splot()` — and corresponding global
  option `options(cograph.visual_scaling = FALSE)` — disables device
  compensation for reproducibility-sensitive workflows that calibrated
  against the previous behaviour.
- `splot()` return value now carries two attributes for downstream tooling:
  `cograph.visual_scale` (the multiplier list) and `cograph.node_diam_in`
  (the representative node diameter in inches at the rendered device).
- The splot-internal `render_legend_splot()` plus the new shared
  `.render_legend_base()` (`R/render-legend-shared.R`) replace the ad-hoc
  legend cex/pt.cex handling with a single compensated path. `plot_htna`,
  `plot_mtna`, `plot_mlna`, `plot_mcml` still use their historical scale
  multiplier arguments; Phase 2 will migrate them to the shared helper.

## Plotting

- `splot.netobject` now routes on the Nestimate `$method` slot rather than
  just direction. Undirected sequence-based networks from `build_cna()` and
  `wtna(method = "cooccurrence")` get oval TNA-family styling (layout,
  palette, donuts) with arrows and dotted edge starts automatically dropped
  because the matrix is symmetric. Glasso / cor / pcor / ising networks
  still get `psych_styling = TRUE` (spring layout, Okabe-Ito palette).
- `from_tna()` auto-detects integer-valued weight matrices (ftna, ctna, raw
  counts) and sets `weight_digits = edge_label_digits = 0` so edge labels
  render as `2304` rather than `2304.00`. Fractional weights still format
  to two decimals. Explicit user-supplied `weight_digits` still wins.
- `psych_styling = TRUE` is now exported as a first-class styling preset
  (undirected counterpart of `tna_styling`) — Okabe-Ito palette, spring
  layout, no arrows — applied by default to `splot.netobject` on
  correlation-family input and to the `$contemporaneous` / `$between`
  constituents of `net_mlvar`.
- Expanded `splot()` dispatch coverage across the tna and Nestimate class
  hierarchies, ensuring `tna`, `ftna`, `ctna`, `group_tna`, `tna_bootstrap`,
  `group_tna_bootstrap`, `tna_permutation`, `group_tna_permutation`,
  `netobject`, `netobject_group`, `netobject_ml`, `net_mlvar`, `wtna_mixed`,
  `net_bootstrap`, `net_permutation`, `boot_glasso`, `mcml`, `net_hon`,
  `net_hypa`, and `simplicial_complex` all reach the correct renderer.
- Self-loops are now preserved in every plot function.

## Correctness fixes (audit-driven)

- `detect_duplicate_edges()`, `aggregate_duplicate_edges()`,
  `simplify.cograph_network()`, and the internal `check_duplicate_edges()`
  helper now respect directed vs undirected semantics. Previously the
  canonical (min/max) endpoint key collapsed `A -> B` and `B -> A` into one
  edge even on directed graphs, matching `igraph::simplify()` ground truth.
- `.compute_modularity()` replaces a nested for loop with cluster-wise
  vectorization (`sum(A[idx, idx]) - sum(k_out[idx]) * sum(k_in[idx]) / m`),
  per the project "no for loops" rule. Results verified bit-exact against
  `igraph::modularity()`.
- `is_directed()` now recognises `CographNetwork` R6 objects — previously
  only the `cograph_network` list format dispatched correctly.
- `compute_layout_for_cograph()` uses `layout$get_type()` instead of the
  removed `$name` field on `CographLayout`.
- `network_small_world()` returns `0` (valid: no triangles means
  definitively not small-world) instead of `NA_real_` when the observed
  clustering coefficient is zero but path length is finite.
- `simplify.cograph_network()` threads the directed flag through to edge
  aggregation so directed multigraphs collapse correctly.

## Performance & documentation

- `simplify()` performance refactor for large networks plus a cleaner
  title-composition path.
- `motifs()`, `extract_motifs()`, and `plot.cograph_motif_analysis`
  examples reworked to use `n_perm = 10L` (or `significance = FALSE`) and
  promoted from `\dontrun` to CRAN-runnable (optional tna branches stay in
  `\donttest`). Retires 320 seconds of latent CRAN timing risk — every
  example now runs in under 4 seconds.

## New tests

- `test-audit-fixes.R` — ground-truth regressions for the directed edge
  semantics, modularity vectorization, and small-world behaviour changes.
- `test-integer-weight-labels.R` — locks `from_tna()` integer-weight
  auto-detect behaviour and precedence of explicit `weight_digits`.
- `test-equiv-{assortativity, cluster-quality, communities, disparity,
  edge-centrality, network-summary, robustness, standalone-measures}.R` —
  numerical equivalence against igraph, sna, centiserve, brainGraph,
  influenceR, tidygraph, and NetworkX. Gated by
  `skip_coverage_tests() + skip_on_cran()`, so they do not run on the
  CRAN pipeline.

# cograph 2.1.0

## New Features

### Batch 6 — new-API graph-level / set-level / pair-level measures

These measures don't fit the per-node `centrality()` data frame, so they live as standalone functions:

- `estrada_index()` — graph-level spectral invariant: \eqn{EE(G) = \sum_i e^{\lambda_i}}, equal to the trace of the matrix exponential of the adjacency. Equivalently, the sum of `subgraph_centrality()` across all nodes. Matches `networkx.estrada_index` at machine epsilon (max relative diff ~5e-15 across random test graphs).
- `trophic_incoherence()` — graph-level food-web stability measure (Johnson et al. 2014). Defined as the population standard deviation of per-edge trophic differences \eqn{s_v - s_u} where \eqn{s_i} is the trophic level of node \eqn{i}. Zero for perfectly coherent DAGs (e.g., a pure chain). Matches `networkx.trophic_incoherence_parameter` at machine epsilon. Directed-only; reuses the existing `trophic_level` calculator.
- `group_centrality(x, nodes, measure = c("betweenness", "closeness", "degree"))` — Everett-Borgatti (1999) group centrality for a *set* of nodes. Returns a scalar. Supports `mode = "in"/"out"` for directed-degree variants. **Group closeness and group degree** match `networkx.group_*_centrality` bit-exact. **Group betweenness** implements the textbook Everett-Borgatti / Puzis 2008 definition (fraction of shortest paths passing through at least one node in the group), which diverges from `networkx.group_betweenness_centrality` on some graphs due to a known quirk in NetworkX's Puzis-Yahalom-Elovici iterative algorithm. Verified via an independent Python brute-force: cograph matches the textbook definition; NX produces larger values on graphs with many overlapping shortest paths. Documented in the roxygen "Divergence from NetworkX" section.
- `dispersion(x, u = NULL, v = NULL, normalized = TRUE, alpha = 1, b = 0, c = 0)` — Backstrom-Kleinberg (2014 Facebook) pair-level measure of tie strength. Counts the number of "well-dispersed" mutual friends of `u` and `v` (pairs of common neighbors that are not directly connected and share no common neighbor inside `u`'s ego network other than `u` and `v`). Matches `networkx.dispersion` bit-exact across all 156 edges on the karate club graph. Returns a scalar, named vector, or data frame depending on which of `u`, `v` are specified.

### Centrality Batch 5 — Gould-Fernandez brokerage (5 roles)

Added the five Gould-Fernandez (1989) brokerage role counts, a foundational measure in social network analysis (~1500 citations). Each role is a separate per-node measure requiring a `membership` argument (following the same pattern as `participation`, `within_module_z`, `gateway`), and counts open directed 2-paths `a -> v -> c` through broker `v`:

- `centrality_brokerage_coordinator()` — all three in broker's group (w_I)
- `centrality_brokerage_itinerant()` — endpoints same group, broker different (w_O, "consultant")
- `centrality_brokerage_representative()` — broker + source same, target different (b_IO)
- `centrality_brokerage_gatekeeper()` — broker + target same, source different (b_OI)
- `centrality_brokerage_liaison()` — all three in different groups (b_O)

Bit-exact match against `sna::brokerage$raw.nli` for all five roles across 20 random directed graphs. Implemented natively (no runtime dependency on sna). Key implementation detail: the Gould-Fernandez counting rule requires **open 2-paths only** — triads where a direct edge `a -> c` already exists are excluded. This matches sna's C implementation exactly and was derived empirically (sna's `.C("brokerage_R", ...)` has no R-level source).

Directed-only; warns and returns `NA` on undirected input.

### Centrality Batch 4 — directed prestige family (Wasserman-Faust / sna)

- `centrality_prestige_domain()` — directed-graph prestige measure: for each node \eqn{v}, the number of other nodes that can reach \eqn{v} via a directed path. Classical Wasserman-Faust (1994) measure from `sna::prestige(cmode = "domain")`. Bit-exact match against sna, implemented natively via `igraph::distances(mode = "out")` + `colSums(is.finite(D)) - 1` (no runtime dependency on sna). Directed-only; returns NA with a warning on undirected input.
- `centrality_prestige_domain_proximity()` — distance-weighted variant: `R_v^2 / (D_v * (n - 1))` where `R_v` is the number of reachers and `D_v` is the sum of their geodesic distances to `v`. Bit-exact match against `sna::prestige(cmode = "domain.proximity")` on strongly connected directed graphs. On graphs with any unreachable pair, sna has a known bug (`FALSE * Inf = NaN` collapses the denominator, producing all-zero output); cograph's `is.finite()`-masked formula produces mathematically correct values on any directed graph. Directed-only.

### Centrality Batch 3 — classical measures with reference-package validation

- `centrality_katz()` — Katz (1953) status index. Bit-exact match against `centiserve::katzcent` (cograph mirrors centiserve's exact LAPACK call sequence). Also matches `igraph::alpha_centrality(exo = 1)` and `networkx.katz_centrality_numpy` at machine epsilon. New `katz_alpha` parameter (default 0.1).
- `centrality_hubbell()` — Hubbell (1965) input-output centrality. Bit-exact match against `centiserve::hubbell` (cograph mirrors centiserve's full-inverse LAPACK call path). Note: centiserve's default (`weights = NULL`) silently ignores `E(g)$weight`; to reproduce cograph's behavior with centiserve on weighted graphs, pass `weights = igraph::E(g)$weight` explicitly. New `hubbell_weight` parameter (default 0.5).
- `centrality_information()` — Stephenson-Zelen (1989) information centrality. Bit-exact match against `sna::infocent` on connected undirected graphs (cograph mirrors sna's exact construction and `solve()` call sequence).
- `centrality_pairwisedis()` — Pairwise disconnectivity (Potapov et al. 2008). Directed-only; fraction of reachable ordered pairs that become unreachable when each node is removed. Bit-exact match against `centiserve::pairwisedis`. Warns and returns `NA` on undirected input, matching the convention used by `salsa`, `leaderrank`, and `trophic_level`.
- `centrality_reaching_local()` / `reaching_global()` — Local and global reaching centrality (Mones, Vicsek & Vicsek 2012). Bit-exact match against `networkx.local_reaching_centrality` across the directed unweighted, undirected unweighted, and weighted branches. Undirected unweighted LRC coincides with `igraph::harmonic_centrality(normalized = TRUE)` (documented). `reaching_global()` is a graph-level hierarchy statistic in [0, 1].

# cograph 1.8.2

## New Features

- `plot_simplicial()` now accepts `tna`, `netobject`, `net_hon`, and `net_hypa` objects directly — higher-order pathways are auto-built and visualized with proper state labels, no manual extraction needed. New parameters: `method` (`"hon"` / `"hypa"`), `max_pathways`, `ncol`. Dismantled mode uses `gridExtra` grid layout with scaled nodes
- `print.cograph_network()` now shows a structured summary: node/edge counts, density, reciprocity, weight range, and top-degree nodes — replacing the minimal R6 default output
- Added `mcml` S3 class with `as_mcml()` generic for type-safe handling of Markov Chain Multi-Level models — enables `print()`, `plot()`, and method dispatch on MCML objects
- Added local `%||%` operator for R 4.1 compatibility (no longer requires R 4.4+)

## Breaking Changes

- MCML field names renamed for clarity: `$between` → `$macro`, `$within` → `$clusters`
- `as_tna()` on MCML objects now returns a flat `group_tna` list instead of a nested structure

## Bug Fixes

- `plot_mcml()` now suppresses zero-weight edges instead of drawing invisible lines, and strips leading zeros from edge labels (`.32` instead of `0.32`)
- Self-loops in `cluster_summary()` are now preserved in the macro diagonal, reflecting intra-cluster retention rates
- Sequence data is properly propagated through the full tna → macro → cluster pipeline, so downstream models can use bootstrap and permutation tests

# cograph 1.8.0

## New Features

- Added `overlay_communities()` for drawing community blob overlays on any network plot — accepts method names, membership vectors, or pre-computed community objects
- Added `plot_simplicial()` for higher-order pathway visualization, rendering simplicial complexes as smooth blobs with flexible separators and a dismantled view option
- Added `value_nudge` parameter to `plot_transitions()` for controlling the distance between flow labels and nodes
- Added bundle legend label controls: `bundle_legend_size`, `bundle_legend_color`, `bundle_legend_fontface`, `bundle_legend_position`
- Added per-function label controls (`label_size`, `label_color`, `label_fontface`, `label_hjust`) to `plot_transitions()`, `plot_trajectories()`, and `plot_alluvial()`

## Bug Fixes

- Fixed spiky text halo artifacts in transition and heatmap plots by increasing circular offset directions from 8 to 16 (22.5° spacing for smooth outlines)

# cograph 1.7.0

## New Features

### Cluster Analysis
- Added `cluster_summary()` for aggregating network weights at the cluster level, producing between-cluster and within-cluster matrices from raw transition data
- Added `build_mcml()` for constructing Markov Chain Multi-Level models from edge lists or sequence data with automatic cluster detection
- Added `cluster_quality()` for modularity-based cluster quality metrics and `cluster_significance()` for permutation-based significance testing
- Added `as_tna()` to convert cluster summaries to TNA objects for bootstrapping, permutation testing, and plotting with `splot()`

### Network Operations
- Added `simplify()` for pruning weak edges from networks, with configurable weight threshold and aggregation method
- Added `disparity_filter()` for backbone extraction (Serrano et al. 2009), with methods for matrices, tna, igraph, and cograph_network objects
- Added `robustness()` for network robustness analysis with targeted (betweenness, degree) and random attack strategies, plus `ggplot_robustness()` for faceted ggplot2 output
- Added `temporal_edge_list()` for converting sequence data to timestamped edge lists
- Added `supra_adjacency()`, `supra_layer()`, `supra_interlayer()` for multilayer supra-adjacency matrix construction
- Added `layer_similarity()`, `layer_similarity_matrix()`, and `layer_degree_correlation()` for comparing layers in multilayer networks
- Added `aggregate_weights()` and `aggregate_layers()` for weight aggregation across layers
- Added `verify_with_igraph()` for cross-validating cograph centrality and network metrics against igraph

### Motif Analysis
- Added `motifs()` / `subgraphs()` as a unified API for triad census (node-exchangeable counts) and instance extraction (named node triples), with auto-detection of actor/session columns, rolling/tumbling window support, and exact configuration model significance testing

### Visualization
- Added `plot_mcml()` for Markov Chain Multi-Level visualization showing between-cluster summary edges alongside within-cluster detail, with pie charts, self-loops, and 22 customization parameters
- Added `plot_chord()` for native chord diagrams with automatic weight-based arc sizing
- Added `plot_time_line()` for cluster membership timeline visualization
- Added `plot_htna()` orientations: `"facing"` (tip-to-tip columns) and `"circular"` (two semicircles), plus `intra_curvature` for drawing intra-group edges as dotted bezier arcs
- Added `threshold` parameter to all plot functions for filtering edges/cells below a minimum absolute weight
- Added `value_fontface`, `value_fontfamily`, and `value_halo` parameters to `plot_heatmap()` for text styling control
- Added directional shorthands for `scale_nodes_by`: `indegree`, `outdegree`, `instrength`, `outstrength`, `incloseness`, `outcloseness`, `inharmonic`, `outharmonic`, `ineccentricity`, `outeccentricity`
- Added `scale_nodes_scale` parameter to `splot()` for dampening (< 1) or exaggerating (> 1) centrality-based node sizing differences
- Added qgraph argument translation in `splot()`: when plotting tna objects, qgraph-style parameters (`vsize`, `asize`, `edge.color`, `lty`, `shape`) are automatically mapped to cograph equivalents

### Transition Plot Enhancements
- Added intermediate labels with `node_label_format` (e.g., `"{state} (n={count})"`) for showing counts on transition plot nodes
- Added line bundling via `bundle_size` for aggregating individual trajectories into weighted summary lines in large datasets
- Added flow value labels via `show_values` / `value_position` for displaying transition counts on flow lines
- Added `label_position` consistency across ALL columns (first, middle, last) in trajectory plots

### Data & Infrastructure
- Added example datasets: `gamer_data`, `group_engagement`, `srl_data`
- Added `set_node_groups()` / `get_node_groups()` for managing cluster assignments on cograph_network objects
- Consolidated cograph_network metadata under `$meta` with getter/setter functions
- Added `group_tna` support to `splot()` for direct plotting of grouped TNA models
- Gave each `centrality_*` wrapper its own focused help page

## Bug Fixes

- Fixed load and percolation centrality computation: the BFS assumed unit edge weights, causing infinite loops on weighted graphs; directed graphs now transpose correctly (matching sna convention); disconnected nodes no longer contribute spurious centrality
- Fixed self-loop and edge clipping in `splot()` viewport calculation
- Fixed argument forwarding in splot dispatch for bootstrap/permutation objects — named parameters (minimum, threshold, layout, title) were consumed by `splot()`'s signature and silently dropped when dispatching
- Fixed overlapping flow value labels in multi-step alluvial plots
- Fixed alluvial label halo rendering producing spike artifacts (8 → 16 offset directions)
- Fixed viridis palette direction in `plot_heatmap()` so high values get dark colors
- Fixed `build_mcml()` density method crash when weight vector had no names
- Fixed display label priority resolution (labels > label > identifier)
- Removed zero-value labels that appeared after rounding in transition plots

## Improvements

- Simplified splot dispatch: extracted `.collect_dispatch_args()` helper to replace 6 copy-paste dispatch blocks, using `match.call()` + `mget()` for reliable argument capture

# cograph 1.6.0

## New Features

### Centrality
- Added `centrality()` with 23 measures and individual wrappers: degree, strength, betweenness, closeness, eigenvector, pagerank, harmonic, authority, hub, alpha, power, kreach, diffusion, percolation, eccentricity, transitivity, constraint, coreness, load, subgraph, leverage, laplacian, current-flow betweenness, current-flow closeness, voterank
- Added `edge_betweenness()` for edge-level centrality
- Added automatic weight inversion for path-based measures when working with tna transition matrices (where higher weight = stronger connection, not shorter distance)

### Community Detection
- Added `detect_communities()` with 11 algorithms: louvain, walktrap, fast_greedy, label_propagation, leading_eigenvector, infomap, spinglass, leiden, optimal, edge_betweenness, multilevel — plus `com_*` shorthand aliases
- Added consensus clustering and `cluster_significance()` for permutation-based validation

### Network Metrics
- Added `network_summary()` and `summarize_network()` for computing comprehensive network-level statistics (density, reciprocity, transitivity, diameter, components, degree distribution)

### Visualization
- Added `plot_transitions()` for alluvial/Sankey flow diagrams, with `plot_alluvial()` and `plot_trajectories()` wrappers
- Added `plot_bootstrap()` and `plot_permutation()` for significance-styled visualization of bootstrap and permutation test results — significant edges rendered solid on top, non-significant edges dashed behind
- Added `plot_mixed_network()` for overlaying symmetric (undirected, straight) and asymmetric (directed, curved) edges on the same network
- Added `plot_heatmap()` for adjacency matrix heatmaps with optional hierarchical clustering and `plot_ml_heatmap()` for multilayer 3D perspective heatmaps
- Added `plot_compare()` for difference network visualization showing edge-weight changes between two networks
- Added `splot()` S3 methods for `tna_bootstrap` and `tna_permutation` objects

### Motif Analysis
- Added `motif_census()`, `triad_census()`, and `extract_motifs()` for triad motif analysis with pattern filtering, significance testing, and network diagram visualization

### Network Utilities
- Added `filter_edges()`, `subset_edges()`, `select_nodes()`, `select_edges()` for flexible network subsetting
- Added `set_groups()` for storing cluster assignments on cograph_network objects with automatic dispatch to `plot_htna()` / `plot_mtna()`

### Infrastructure
- All plot functions now accept `cograph_network` objects as input, in addition to matrices, igraph objects, and tna objects
- Layout computation is now lazy — coordinates are only calculated when first needed
- Improved `layout_spring` and `layout_gephi_fr` algorithms: vectorized attraction forces, edge aggregation for dense networks
- Renamed package from Sonnet to cograph

## Bug Fixes

- Fixed `par(pin)` error on exit when plot device state was corrupted
- Fixed motif plot scaling and margins for different device sizes

# cograph 1.5.2

## Breaking Changes

- Standardized first parameter name to `x` across all plotting functions:
  - `plot_tna()`: `input` → `x`
  - `plot_htna()`: `input` → `x` (was `model`)
  - `plot_mtna()`: `input` → `x` (was `model`)
  - `splot()` already used `x`

## Bug Fixes

- Fixed `tplot()` default margins causing tiny plots compared to `splot()`

## Documentation

- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.md`)

# cograph 1.5.1

## Breaking Changes (with backwards compatibility)

The following parameters have been renamed for consistency. The old names still work but emit deprecation warnings:

| Old Name | New Name | Reason |
|----------|----------|--------|
| `esize` | `edge_size` | Add `edge_` prefix, expand abbreviation |
| `cut` | `edge_cutoff` | Add `edge_` prefix, clarify meaning |
| `usePCH` | `use_pch` | Fix camelCase to snake_case |
| `positive_color` | `edge_positive_color` | Add `edge_` prefix (matches theme storage) |
| `negative_color` | `edge_negative_color` | Add `edge_` prefix (matches theme storage) |
| `donut_border_lty` | `donut_line_type` | Expand `lty` abbreviation |

## Improvements

- `edge_label_fontface` now accepts string values ("plain", "bold", "italic", "bold.italic") in addition to numeric values

# cograph 1.4.0

## New Features

- Added `mlna()` for multilevel network visualization with 3D perspective
- Added `mtna()` for multi-cluster network visualization with shape-based cluster containers
- Added `plot_htna()` for hierarchical multi-group network layouts with polygon and circular arrangements
- Added `tplot()` as a qgraph drop-in replacement with automatic parameter translation
- Added `arrow_angle` parameter for customizable arrowhead geometry

## Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines rendering when border width is 0

# cograph 1.3.1

## New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edge starts indicating direction
- Added direct support for tna objects via `from_tna()` — no manual matrix extraction needed
- Added direct support for statnet `network` and `qgraph` objects as input
- Added auto-conversion of `pie_values` vector to `donut_fill` when all values are in [0,1]

## Bug Fixes

- Fixed TNA visual defaults being silently overwritten in `splot()` when other parameters were specified
- Fixed self-loop edge labels overlapping the loop arc
- Fixed `donut_shape` validation rejecting custom SVG shapes
- Fixed title clipping when title text exceeded plot margins
- Fixed edge rendering crash on certain edge/node configurations
- Removed underscore prefix requirement for custom SVG shape names

# cograph 1.2.7

## Bug Fixes

- Fixed oversized nodes in `from_qgraph()` when a layout override was provided
- Fixed oval layout using independent axis scaling, which distorted aspect ratios — now uses uniform scaling via `normalize_coords()`
- Fixed edge label alignment in `from_qgraph()` by using a matrix intermediary for per-edge vector reordering
- Fixed `nrow(el)` crash: qgraph's Edgelist is a list, not a data.frame
- Fixed oval layout node distortion and donut fill values when converting from qgraph

# cograph 1.2.6

## New Features

- Added `donut_empty` parameter for rendering unfilled donut nodes
- Added `from_qgraph()` for converting qgraph objects to cograph format, reading resolved `graphAttributes` for accurate parameter extraction

## Bug Fixes

- Fixed oval `layout_info` guard causing errors on certain device configurations
- Fixed curvature extraction passing vector values instead of scalars

# cograph 1.2.0

## New Features

- Added `soplot()` for grid/ggplot2-based network plotting — full feature parity with `splot()` using a different rendering backend
- Added `layout_oval()` for oval/elliptical node arrangements
- Added `layout_scale` parameter to expand or contract the network layout, with `"auto"` mode for node-count-based scaling
- Added Gephi-style Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for visually indicating edge direction via styled start segments (dashed, dotted)

## Bug Fixes

- Fixed `soplot()` curve direction and edge defaults diverging from `splot()` behavior
- Fixed `rescale_layout` distorting oval aspect ratios by switching to uniform scaling
- Fixed edge scaling producing abnormally thick edges on small networks
- Fixed `par(pin)` restoration error on plot device exit

# cograph 1.1.0

## New Features

- Added `splot()` — a base R graphics engine for network visualization using `polygon()`, `lines()`, and `xspline()`, providing better performance than grid-based rendering for large networks
- Added polygon-shaped donut nodes, custom SVG node shapes, and AI-generated shape support
- Added shadow/halo labels and fine-grained text control (fontface, fontfamily, hjust, vjust, angle)
- Added double donut nodes with separate inner/outer border controls
- Added edge CI (confidence interval) underlays and template-based edge labels
- Added comprehensive legend support: groups, edge color scales, and node size scales
- Added high-resolution output via `sn_save()` with configurable DPI
- Added edge curve modes, bidirectional arrows, self-loop rotation, and per-edge curve control

## Bug Fixes

- Fixed donut rendering producing artifacts and simplified the `donut_color` API to accept 1 color (fill), 2 colors (fill + background), or n colors (per-node)
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed self-loop rendering to use qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation causing edge crossings

# cograph 1.0.0

- Initial release of cograph network visualization package
