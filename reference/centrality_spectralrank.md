# SpectralRank with optional diagonal prior information

Xu et al.'s SpectralRank is the positive right eigenvector belonging to
the largest real eigenvalue of the augmented adjacency \\B =
\left(\begin{smallmatrix}A+P&\mathbf{1}\\
\mathbf{1}^T&0\end{smallmatrix}\right)\\. A ground node connects
bidirectionally to every original node with unit edge weight. The
diagonal P is zero for ordinary SpectralRank; a nonnegative prior gives
the paper's weighted SpectralRank family.

## Usage

``` r
centrality_spectralrank(x, sr_prior = 0, ...)
```

## Arguments

- x:

  Network input accepted by
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

- sr_prior:

  Nonnegative finite scalar or one value per original node. Default zero
  selects SpectralRank; one selects a uniform unit prior.

- ...:

  Additional arguments to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector in input node order.

## Details

Raw scores are scaled by the maximum over ALL nodes, including the
ground node, which is then omitted from the output. Its score is not
redistributed. Consequently the largest returned score can be below one.
Optional `normalized = TRUE` additionally divides by the maximum over
original nodes, changing this source-defined scale.

The paper uses binary adjacency and outgoing neighbors: an edge i to j
contributes j's score to i. The function preserves this orientation;
transpose the graph to use incoming neighbors. Finite nonnegative edge
weights extend the same matrix definition; they are interaction weights,
separate from the diagonal-prior meaning of weighted SpectralRank. Unit
ground edges stay fixed, so scaling original edge weights generally
changes scores. For tiny asymmetric matrix weights, set
`directed = TRUE` or use a directed igraph object because the shared
parser otherwise uses approximate symmetry detection.

Loops are removed and remaining parallel edges sum after the generic
simplify rule; unweighted remaining edges count once each. Zero weights
are absent. Mode, path-weight inversion and cutoff are ignored. Named
vector priors are matched to node names. Scalar priors broadcast; the
ground prior is always zero. Supply externally computed degree, H-index
or coreness scores as a vector to select those prior families.

All nodes, including isolates, receive positive spectral scores because
of the ground links. Without edges or priors, each of n original nodes
scores \\1/\sqrt{n}\\. For an edgeless graph the paper's unshifted power
iteration oscillates, although the Perron eigenvector is unique. This
function explicitly uses that eigenvector definition, without claiming
convergence of the published iteration. A singleton scores one; an empty
graph returns no scores. Adding disconnected nodes generally changes
other scores because all share the ground node.

For nonzero priors the implementation follows section III-A2's
\\B=\widetilde A+P\\. Algorithm 1 constructs that matrix but its update
line prints \\\widetilde A\\, omitting P; this inconsistency is retained
in the verification audit. No author-software parity is claimed.

Dense eigendecomposition takes O(n cubed) time and O(n squared) memory.
Extreme weight/prior ranges or unresolved positive eigenpairs raise
errors. The score defines a spectral ranking, not a spreading
probability or a general guarantee of predictive performance.

## References

Xu, S., Wang, P., Zhang, C.-X. and Lu, J. (2019; online 2018). Spectral
Learning Algorithm Reveals Propagation Capability of Complex Networks.
IEEE Transactions on Cybernetics, 49(12), 4253-4261. Section III-A,
equations 4-8 and Algorithm 1.
[doi:10.1109/TCYB.2018.2861568](https://doi.org/10.1109/TCYB.2018.2861568)
.

## Examples

``` r
centrality_spectralrank(igraph::make_ring(5))
#>         1         2         3         4         5 
#> 0.6898979 0.6898979 0.6898979 0.6898979 0.6898979 
centrality_spectralrank(igraph::make_star(5), sr_prior = 1)
#>         1         2         3         4         5 
#> 0.4620543 0.6755484 0.6755484 0.6755484 0.6755484 
```
