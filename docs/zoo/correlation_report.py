"""Build the self-contained correlation explorer and exportable figure."""
from pathlib import Path
import json
import csv
import html
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

HERE = Path(__file__).resolve().parent / 'correlation-audit'
data = json.loads((HERE / 'explorer_data.json').read_text())
s = data['summary']
validation = json.loads((HERE / 'validation.json').read_text())
assert validation['passed'], 'Do not publish an unverified correlation report'
nearest_rows = ''.join('<tr><td>'+html.escape(r['zoo_label'])+'</td><td>'+html.escape(r['nearest_baseline'])+
                       f"</td><td>{r['mean_tau_b']:.3f}</td><td>{r['min_tau_b']:.3f} to {r['max_tau_b']:.3f}</td><td>{r['networks']}</td></tr>"
                       for r in data['nearest'])
with (HERE / 'zoo_comparison.csv').open() as stream:
    gaps = list(csv.DictReader(stream))
gap_rows = ''.join('<tr><td>'+html.escape(r['zoo_label_1'])+' / '+html.escape(r['zoo_label_2'])+
                   f"</td><td>{float(r['native_mean_tau_b']):.3f}</td><td>{float(r['zoo_reported_tau']):.3f}</td><td>{float(r['native_minus_zoo']):+.3f}</td><td>{r['networks']}</td></tr>"
                   for r in gaps[:20])
groups = ''.join('<li>'+', '.join('<code>'+html.escape(m)+'</code>' for m in group)+'</li>'
                 for group in s['mean_correlation_groups'])
template = r'''<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Centrality correlation audit</title>
<style>
:root{color-scheme:light;font-family:system-ui,sans-serif;color:#15283d;background:#f3f6fa}body{max-width:1180px;margin:0 auto;padding:32px 22px}h1{font-size:32px;margin:8px 0}h2{font-size:22px;margin:0 0 14px}p{line-height:1.55}.muted{color:#51657c}.eyebrow{font-size:13px;font-weight:650;letter-spacing:.08em;color:#205d93}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin:24px 0}.card,section{background:white;border:1px solid #dce4ed;border-radius:12px;padding:20px}.card b{display:block;font-size:28px}.card span{font-size:13px;color:#51657c}section{margin:20px 0}.controls{display:flex;gap:14px;flex-wrap:wrap;align-items:end}label{display:grid;gap:5px;font-size:13px}select,input{font:inherit;border:1px solid #bdcad7;border-radius:6px;padding:8px;background:white}canvas{display:block;width:100%;height:auto;margin:16px 0;touch-action:manipulation}.detail{background:#f0f5fa;padding:14px;border-radius:8px;min-height:48px;font-size:14px;line-height:1.6}.legend{display:flex;gap:10px;align-items:center;font-size:12px}.gradient{height:12px;width:180px;background:linear-gradient(90deg,#ba3d4d,#fff,#2878b5)}table{border-collapse:collapse;width:100%;font-size:13px}td,th{padding:9px 10px;text-align:left;border-bottom:1px solid #e4eaf1}th{position:sticky;top:0;background:#edf3f9}td:nth-last-child(-n+3){font-variant-numeric:tabular-nums}a{color:#1b659d}.scroll{max-height:540px;overflow:auto}li{line-height:1.7;margin:8px 0}code{font-size:12px}small{line-height:1.55;display:block}.warning{border-left:4px solid #c99131;padding-left:14px}button{border:1px solid #9eb5ca;border-radius:6px;background:white;padding:8px 14px;color:#153b5a;cursor:pointer}@media(max-width:650px){.cards{grid-template-columns:1fr 1fr}body{padding:18px 12px}section{padding:14px}h1{font-size:27px}}
</style>
<div class="eyebrow">IMPLEMENTATION PAUSED · 48 OF 160 CANDIDATES COVERED</div>
<h1>How similarly do the measures rank nodes?</h1>
<p class="muted">A fresh audit of native outputs, with an exploratory comparison against the Zoo’s published matrix. Correlation describes ranking similarity; it does not establish numerical equivalence.</p>
<div class="cards"><div class="card"><b>177</b><span>native measures considered</span></div><div class="card"><b>19</b><span>packaged benchmark networks</span></div><div class="card"><b>__USABLE__</b><span>native measures usable somewhere</span></div><div class="card"><b>__CLOSE__</b><span>native pairs with |mean τ| ≥ .99</span></div></div>
<section><h2>What the audit shows</h2><p>Of the 68 native pairs with |mean τ-b| ≥ .99, 57 stay above |τ-b| ≥ .99 on every observed network. Of those original 68 pairs, 61 retain the mean threshold with equal domain weights and 66 retain it after excluding the illustrative kite. The closest other baseline for Coleman–Theil reaches only |mean τ-b| = .626; for k-truss it reaches .810.</p><p>Independent SciPy/NumPy calculations verified all __VALIDPAIRS__ pair observations and __VALIDVALUES__ correlation values within 10⁻¹². This validates the correlation calculation, not the centrality definitions. <a href="validation.json">Validation evidence</a> · <a href="README.md">Findings and limitations</a></p><p class="warning">Rounding near-ties can matter: on the Rhode foodweb, alpha versus power changes from raw Kendall .687 to rounded Kendall 1.000. Both are retained. There are 75 costly skips, 54 constant vectors, 240 nonfinite outputs, and 12 errors (10 time limits, 2 singular systems). Eleven directed-only measures are unavailable under this undirected projection; Hubbell is unavailable at its default attenuation.</p></section>
<section><h2>Explore the correlations</h2><div class="controls">
<label>Measures<select id="scope"><option value="candidates">48 candidate labels</option><option value="native">Native measures with usable scores</option></select></label>
<label>Statistic<select id="stat"><option value="mean_tau_b">Native mean Kendall τ-b</option><option value="mean_spearman">Native mean Spearman ρ</option><option value="zoo">Zoo reported Kendall correlation</option><option value="gap">Native Kendall minus Zoo</option></select></label>
<label>Minimum native networks<input id="minimum" type="number" min="1" max="19" value="10" style="width:70px"></label>
<label>Highlight a measure<input id="search" placeholder="e.g. gravity" type="search"></label>
</div><p class="muted"><small>Hover or tap a cell for full names, network count and range. Ordering uses complete linkage on absolute mean Kendall correlations; cell colors preserve the sign. Gray cells lack the selected native coverage. Some candidate labels share a native configuration.</small></p>
<div class="legend"><span id="leftLegend">−1</span><div class="gradient"></div><span id="rightLegend">+1</span><span>Red: negative · blue: positive</span></div>
<canvas id="matrix" width="1100" height="970" aria-label="Interactive centrality correlation heatmap"></canvas><div id="detail" class="detail" aria-live="polite">Select a cell to inspect a pair.</div>
<p><a href="candidate_heatmap.png">Exportable candidate heatmap</a> · <a href="pairs.csv">All pair summaries (CSV)</a> · <a href="per_network_pairs.csv.gz">Per-network correlations (CSV.gz)</a></p></section>
<section><h2>What this benchmark covers</h2><p>All networks are projected to simple, undirected, unweighted graphs, retaining the largest connected component. The collection includes 12 ecological networks, social and neural networks, a literary network, and the illustrative Krackhardt kite graph. It is not a representative sample of all network types.</p><p>Native defaults are preserved. Three additional parameter configurations represent candidate calls for degree-mass gravity and bounded betweenness. Community-dependent measures use saved, seeded Louvain partitions. The map-equation default remains one module. Costs above 40 nodes follow the earlier audit’s recorded exclusion rule; individual calls also have a 30-second elapsed-time limit.</p><p>Only complete, finite, nonconstant score vectors enter each network’s correlation matrix. Rank inputs divide by the largest absolute score and round to 12 decimal places to handle numerical near-ties. Unrounded Kendall and raw-score Pearson correlations are retained for sensitivity checks. Means weight networks equally; each pair has its own availability count and range.</p><p><a href="network_manifest.csv">Network manifest</a> · <a href="configurations.csv">Exact configurations</a> · <a href="availability.csv">Availability, warnings and errors</a> · <a href="memberships.csv">Community assignments</a> · <a href="candidate_map.csv">Candidate call mapping</a> · <a href="versions.csv">Versions</a></p></section>
<section><h2>Each candidate’s closest other baseline</h2><p class="muted">Sorted from lower to higher absolute mean Kendall correlation. Baselines exclude configurations belonging to the candidate cohort. At least 10 shared networks are required. Negative values indicate reversed rankings. These results suggest where measures differ on this benchmark, not universal novelty.</p><div class="scroll"><table><thead><tr><th>Candidate</th><th>Closest baseline</th><th>Mean τ-b</th><th>Network range</th><th>N</th></tr></thead><tbody>__NEAREST__</tbody></table></div><p><a href="candidate_nearest.csv">Download all candidate comparisons</a></p></section>
<section><h2>Groups with close mean rankings</h2><p class="muted">Complete-link groups at |mean τ-b| ≥ .99, requiring at least 10 networks for every pair. This groups means, not identities or guaranteed agreement on every network.</p><ul>__GROUPS__</ul><a href="mean_correlation_groups.csv">Download groups</a></section>
<section><h2>Exploratory comparison with Zoo</h2><p class="warning">Zoo reports correlations across 648 ICON networks. This audit uses a different, smaller collection, and Zoo’s exact parameter, sign and tie conventions have not been established. Its retained matrix has no negative entries, whereas our signed correlations include reversed rankings. Differences below cannot isolate implementation errors.</p><p>Across __ZPAIRS__ candidate pairs with at least 10 native networks (excluding aliases of the same native configuration), matrix-entry Pearson correlation is <b>__ZPEARSON__</b>, Spearman correlation is <b>__ZSPEARMAN__</b>, and mean absolute gap is <b>__ZGAP__</b>. As a sensitivity check, comparing mean absolute native Kendall values against Zoo gives Pearson <b>__ZMPEARSON__</b>, Spearman <b>__ZMSPEARMAN__</b>, and mean absolute gap <b>__ZMGAP__</b>. This does not establish Zoo’s convention. These are descriptive comparisons of dependent matrix entries; no significance test is claimed.</p><div class="scroll"><table><thead><tr><th>Largest signed-value gaps</th><th>Native τ-b</th><th>Zoo</th><th>Gap</th><th>N</th></tr></thead><tbody>__GAPS__</tbody></table></div><p><a href="zoo_comparison.csv">All Zoo comparisons, including magnitude sensitivity</a> · <a href="https://centralityzoo.github.io/comparison/">Zoo benchmark description</a></p></section>
<p class="muted"><small>Reproduce with docs/zoo/correlation_audit.R, correlation_export.R, correlation_validate.py, correlation_summary.py and correlation_report.py. <a href="score_fixtures.json.gz">Exact score and graph fixtures</a> are exported beside this report; RDS originals and checkpoints remain in local_testing_and_equivalence/correlation_audit. Implementation coverage remains 48/160; no measure was added during this audit.</small></p>
<script>
(() => {
'use strict';
const DATA=__DATA__;
const $=id=>document.getElementById(id), canvas=$('matrix'),ctx=canvas.getContext('2d'),base=document.createElement('canvas');base.width=canvas.width;base.height=canvas.height;const bc=base.getContext('2d');
const left=220,top=60,side=860;let active=DATA.candidates,cell=side/active.ids.length;
const fmt=x=>x===null||x===undefined?'unavailable':Number(x).toFixed(3);
function color(value,limit){const x=Math.max(-1,Math.min(1,value/limit)),target=x<0?[186,61,77]:[40,120,181],t=Math.abs(x);return `rgb(${target.map(v=>Math.round(255+(v-255)*t)).join(',')})`;}
function value(i,j){let stat=$('stat').value;if(stat==='gap')return active.mean_tau_b[i][j]===null?null:active.mean_tau_b[i][j]-active.zoo[i][j];return active[stat][i][j];}
function render(){active=DATA[$('scope').value];cell=side/active.ids.length;const candidate=$('scope').value==='candidates';for(const option of $('stat').options)if(['zoo','gap'].includes(option.value))option.disabled=!candidate;if(!candidate&&['zoo','gap'].includes($('stat').value))$('stat').value='mean_tau_b';const limit=$('stat').value==='gap'?2:1;$('leftLegend').textContent='−'+limit;$('rightLegend').textContent='+'+limit;bc.fillStyle='#fff';bc.fillRect(0,0,base.width,base.height);const minimum=Number($('minimum').value)||1;for(let i=0;i<active.ids.length;i++)for(let j=0;j<active.ids.length;j++){let v=value(i,j);bc.fillStyle=v===null||active.networks[i][j]<minimum?'#e5e9ef':color(v,limit);bc.fillRect(left+j*cell,top+i*cell,Math.ceil(cell),Math.ceil(cell));}bc.fillStyle='#314a63';bc.font='11px system-ui';bc.textAlign='right';const every=Math.max(1,Math.ceil(active.ids.length/55));for(let i=0;i<active.ids.length;i+=every){const label=active.labels[i];bc.fillText(label.length>31?label.slice(0,29)+'…':label,left-8,top+(i+.65)*cell);}bc.textAlign='left';bc.font='14px system-ui';bc.fillText(active.ids.length+' labels / configurations',left,30);ctx.drawImage(base,0,0);highlight();}
function highlight(){const term=$('search').value.trim().toLowerCase();if(!term)return;ctx.strokeStyle='#132d42';ctx.lineWidth=2;active.labels.forEach((label,i)=>{if(label.toLowerCase().includes(term)){ctx.strokeRect(left,top+i*cell,side,cell);ctx.strokeRect(left+i*cell,top,cell,side);}});}
function inspect(event){const r=canvas.getBoundingClientRect(),x=(event.clientX-r.left)*canvas.width/r.width,y=(event.clientY-r.top)*canvas.height/r.height,j=Math.floor((x-left)/cell),i=Math.floor((y-top)/cell);ctx.drawImage(base,0,0);highlight();if(i<0||j<0||i>=active.ids.length||j>=active.ids.length)return;ctx.strokeStyle='#142e44';ctx.lineWidth=2;ctx.strokeRect(left+j*cell,top+i*cell,cell,cell);const detail=$('detail');detail.textContent='';const title=document.createElement('strong');title.textContent=active.labels[i]+' ↔ '+active.labels[j];detail.append(title,document.createElement('br'));detail.append(document.createTextNode('Native Kendall τ-b '+fmt(active.mean_tau_b[i][j])+' · Spearman '+fmt(active.mean_spearman[i][j])+' · '+active.networks[i][j]+' shared networks · Kendall range '+fmt(active.min_tau_b[i][j])+' to '+fmt(active.max_tau_b[i][j])));if(active.zoo){detail.append(document.createElement('br'),document.createTextNode('Zoo reported correlation '+fmt(active.zoo[i][j])+' · different benchmark and potentially different conventions'));}}
for(const id of ['scope','stat','minimum','search'])$(id).addEventListener('input',render);canvas.addEventListener('mousemove',inspect);canvas.addEventListener('click',inspect);render();
})();
</script></html>'''
replacements = {'__USABLE__':str(s['usable_native_measures']), '__CLOSE__':str(s['native_pairs_abs_mean_tau_ge_099']),
                '__NEAREST__':nearest_rows,'__GROUPS__':groups,'__GAPS__':gap_rows,
                '__ZPAIRS__':str(s['zoo_comparable_pairs']),'__ZPEARSON__':f"{s['zoo_matrix_pearson']:.3f}",
                '__ZSPEARMAN__':f"{s['zoo_matrix_spearman']:.3f}",'__ZGAP__':f"{s['zoo_mean_absolute_gap']:.3f}",
                '__ZMPEARSON__':f"{s['zoo_magnitude_matrix_pearson']:.3f}",
                '__ZMSPEARMAN__':f"{s['zoo_magnitude_matrix_spearman']:.3f}",
                '__ZMGAP__':f"{s['zoo_magnitude_mean_absolute_gap']:.3f}",
                '__VALIDPAIRS__':f"{validation['pair_observations']:,}",
                '__VALIDVALUES__':f"{validation['comparisons']:,}",
                '__DATA__':json.dumps(data,allow_nan=False).replace('<','\\u003c')}
for key,value in replacements.items():
    template=template.replace(key,value)
(HERE/'index.html').write_text(template)
# Standalone export: signed correlations with unavailable cells gray.
m=np.array([[np.nan if v is None else v for v in row] for row in data['candidates']['mean_tau_b']])
counts=np.array(data['candidates']['networks']);m[counts<10]=np.nan
labels=data['candidates']['labels']
cmap=plt.get_cmap('RdBu').copy();cmap.set_bad('#e5e9ef')
fig,ax=plt.subplots(figsize=(17,15));im=ax.imshow(m,cmap=cmap,vmin=-1,vmax=1)
ax.set_xticks(range(len(labels)),labels,rotation=90,fontsize=7)
ax.set_yticks(range(len(labels)),labels,fontsize=7)
ax.set_title('Candidate centralities: mean Kendall τ-b across 19 benchmark networks\nMinimum 10 shared networks; signed colors, ordering by absolute correlation',fontsize=15,pad=20)
fig.colorbar(im,ax=ax,fraction=.025,pad=.025,label='Mean Kendall τ-b')
fig.text(.015,.01,'48 labels / 47 distinct candidate configurations. Different benchmark from Zoo; correlation is not numerical equivalence.',fontsize=9)
fig.tight_layout(rect=[0,.025,1,1]);fig.savefig(HERE/'candidate_heatmap.png',dpi=150);plt.close(fig)
print('Self-contained explorer and exportable heatmap created')
