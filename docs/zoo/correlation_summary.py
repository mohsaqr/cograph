"""Summarize retained cross-correlations without changing measure coverage."""
from pathlib import Path
from collections import defaultdict, Counter
import csv
import gzip
import json
import math
import statistics as st
import numpy as np
from scipy.cluster.hierarchy import linkage, leaves_list, fcluster
from scipy.spatial.distance import squareform

HERE = Path(__file__).resolve().parent / 'correlation-audit'


def read(name):
    path = HERE / name
    opener = gzip.open if path.suffix == '.gz' else open
    with opener(path, 'rt') as stream:
        return list(csv.DictReader(stream))


def write(name, rows):
    if not rows:
        return
    with (HERE / name).open('w') as stream:
        writer = csv.DictWriter(stream, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)


def finite(value):
    try:
        result = float(value)
        return result if math.isfinite(result) else None
    except (ValueError, TypeError):
        return None


configs = read('configurations.csv')
config_ids = [r['config'] for r in configs]
native = [r['config'] for r in configs if r['native_default'] == 'TRUE']
candidates = read('candidate_map.csv')
availability = read('availability.csv')
manifest = read('network_manifest.csv')
per_network = read('per_network_pairs.csv.gz')
grouped = defaultdict(list)
for row in per_network:
    grouped[tuple(sorted([row['config_1'], row['config_2']]))].append(row)
summary = []
for (a, b), records in grouped.items():
    tau = [float(r['tau_b']) for r in records]
    family = defaultdict(list)
    for r in records:
        family[r['family']].append(float(r['tau_b']))
    sensitivity = [abs(float(r['tau_b'])-float(r['tau_b_raw'])) for r in records
                   if finite(r['tau_b_raw']) is not None]
    pearson = [float(r['pearson']) for r in records if finite(r['pearson']) is not None]
    without_kite = [float(r['tau_b']) for r in records if r['network'] != 'kite']
    summary.append(dict(config_1=a, config_2=b, networks=len(records),
                        mean_tau_b=st.mean(tau), mean_abs_tau_b=st.mean(abs(v) for v in tau),
                        median_tau_b=st.median(tau),
                        min_tau_b=min(tau), max_tau_b=max(tau),
                        min_abs_tau_b=min(abs(v) for v in tau),
                        mean_spearman=st.mean(float(r['spearman']) for r in records),
                        mean_pearson=st.mean(pearson) if pearson else None,
                        domain_balanced_tau_b=st.mean(st.mean(v) for v in family.values()),
                        mean_tau_without_kite=st.mean(without_kite) if without_kite else None,
                        networks_abs_tau_ge_099=sum(abs(v) >= .99 for v in tau),
                        mean_raw_tie_sensitivity=st.mean(sensitivity) if sensitivity else None,
                        max_raw_tie_sensitivity=max(sensitivity) if sensitivity else None))
summary.sort(key=lambda r: -abs(r['mean_tau_b']))
write('pairs.csv', summary)
lookup = {tuple(sorted([r['config_1'], r['config_2']])): r for r in summary}
valid_networks = defaultdict(set)
for row in availability:
    if row['status'] == 'ok':
        valid_networks[row['config']].add(row['network'])


def pair(a, b):
    if a == b:
        n = len(valid_networks[a])
        return dict(networks=n, mean_tau_b=1., mean_abs_tau_b=1., mean_spearman=1., min_tau_b=1.,
                    max_tau_b=1., min_abs_tau_b=1., mean_raw_tie_sensitivity=0.) if n else None
    return lookup.get(tuple(sorted([a, b])))


candidate_configs = {r['config'] for r in candidates}
baseline = sorted(set(native)-candidate_configs)
nearest = []
for c in candidates:
    options = [(b, pair(c['config'], b)) for b in baseline if b != c['config']]
    options = [(b, r) for b, r in options if r and r['networks'] >= 10]
    if options:
        b, r = max(options, key=lambda br: abs(br[1]['mean_tau_b']))
        nearest.append(dict(zoo_label=c['zoo_label'], config=c['config'],
                            nearest_baseline=b, mean_tau_b=r['mean_tau_b'],
                            mean_spearman=r['mean_spearman'], networks=r['networks'],
                            min_tau_b=r['min_tau_b'], max_tau_b=r['max_tau_b'],
                            min_abs_tau_b=r['min_abs_tau_b']))
nearest.sort(key=lambda r: abs(r['mean_tau_b']))
write('candidate_nearest.csv', nearest)
with gzip.open(HERE.parent / 'correlation.json.gz', 'rt') as stream:
    zoo = json.load(stream)
zix = {name: i for i, name in enumerate(zoo['labels'])}
zoo_pairs = []
for i, a in enumerate(candidates):
    for b in candidates[i+1:]:
        r = pair(a['config'], b['config'])
        if not r:
            continue
        z = zoo['matrix'][zix[a['zoo_label']]][zix[b['zoo_label']]]
        zoo_pairs.append(dict(zoo_label_1=a['zoo_label'], zoo_label_2=b['zoo_label'],
                              config_1=a['config'], config_2=b['config'],
                              native_mean_tau_b=r['mean_tau_b'], zoo_reported_tau=z,
                              native_minus_zoo=r['mean_tau_b']-z,
                              native_mean_abs_tau_b=r['mean_abs_tau_b'],
                              native_mean_abs_tau_minus_zoo=r['mean_abs_tau_b']-z,
                              networks=r['networks'],
                              native_min_tau_b=r['min_tau_b'], native_max_tau_b=r['max_tau_b'],
                              same_native_configuration=a['config'] == b['config']))
zoo_pairs.sort(key=lambda r: -abs(r['native_minus_zoo']))
write('zoo_comparison.csv', zoo_pairs)


def matrix(ids, field='mean_tau_b', minimum=1):
    result = np.full((len(ids), len(ids)), np.nan)
    for i, a in enumerate(ids):
        for j, b in enumerate(ids):
            row = pair(a, b)
            if row and row['networks'] >= minimum:
                result[i, j] = row[field]
    return result


def ordered(ids):
    m = matrix(ids, minimum=10)
    distance = 1-np.nan_to_num(abs(m), nan=0.)
    distance = np.clip((distance+distance.T)/2, 0, 1)
    np.fill_diagonal(distance, 0)
    tree = linkage(squareform(distance, checks=True), method='complete')
    return [ids[i] for i in leaves_list(tree)], tree


usable_native = [m for m in native if valid_networks[m]]
native_order, tree = ordered(usable_native)
cluster_labels = fcluster(tree, t=.01, criterion='distance')
clusters = defaultdict(list)
for i, label in enumerate(cluster_labels):
    clusters[int(label)].append(usable_native[i])
groups = [sorted(v) for v in clusters.values() if len(v) > 1]
groups.sort(key=lambda v: (-len(v), v))
write('mean_correlation_groups.csv', [dict(group=i+1, size=len(group), measure=m)
                                      for i, group in enumerate(groups) for m in group])
strong = [r for r in summary if r['config_1'] in native and r['config_2'] in native
          and r['networks'] >= 10 and abs(r['mean_tau_b']) >= .99]
constant_everywhere = [m for m in native if not valid_networks[m]]
status_counts = Counter(r['status'] for r in availability)
# Matrix-entry similarities are descriptive; entries are not independent samples.
zvalid = [r for r in zoo_pairs if r['networks'] >= 10 and not r['same_native_configuration']]
z_native = np.array([r['native_mean_tau_b'] for r in zvalid])
z_reference = np.array([r['zoo_reported_tau'] for r in zvalid])
z_magnitude = np.array([r['native_mean_abs_tau_b'] for r in zvalid])
from scipy.stats import spearmanr
stats = dict(native_measures=len(native), configurations=len(config_ids),
             candidate_labels=len(candidates), networks=len(manifest),
             usable_native_measures=len(usable_native),
             candidate_configurations=len(candidate_configs),
             status_counts=dict(status_counts), unavailable_native=constant_everywhere,
             native_pairs_abs_mean_tau_ge_099=len(strong), mean_correlation_groups=groups,
             zoo_comparable_pairs=len(zvalid),
             zoo_matrix_pearson=float(np.corrcoef(z_native, z_reference)[0, 1]),
             zoo_matrix_spearman=float(spearmanr(z_native, z_reference).statistic),
             zoo_mean_absolute_gap=float(np.mean(abs(z_native-z_reference))),
             zoo_median_absolute_gap=float(np.median(abs(z_native-z_reference))),
             zoo_magnitude_matrix_pearson=float(np.corrcoef(z_magnitude, z_reference)[0, 1]),
             zoo_magnitude_matrix_spearman=float(spearmanr(z_magnitude, z_reference).statistic),
             zoo_magnitude_mean_absolute_gap=float(np.mean(abs(z_magnitude-z_reference))),
             zoo_magnitude_median_absolute_gap=float(np.median(abs(z_magnitude-z_reference))),
             strong_pairs_abs_tau_ge_099_every_observed_network=sum(r['min_abs_tau_b'] >= .99 for r in strong),
             strong_pairs_domain_balanced_ge_099=sum(abs(r['domain_balanced_tau_b']) >= .99 for r in strong),
             strong_pairs_without_kite_ge_099=sum(abs(r['mean_tau_without_kite']) >= .99 for r in strong),
             min_nodes=min(int(r['n']) for r in manifest), max_nodes=max(int(r['n']) for r in manifest))
(HERE / 'summary.json').write_text(json.dumps(stats, indent=2))
# Compact payload for a self-contained interactive explorer.
label_by_config = {r['config']: r['zoo_label'] for r in candidates}
candidate_order_unique, _ = ordered(sorted(candidate_configs))
candidate_order = sorted(candidates, key=lambda c: (candidate_order_unique.index(c['config']), c['zoo_label']))

def payload(ids, labels):
    result = dict(ids=ids, labels=labels)
    for field in ['mean_tau_b', 'mean_spearman', 'min_tau_b', 'max_tau_b']:
        result[field] = [[None if not math.isfinite(v) else round(float(v), 5) for v in row]
                         for row in matrix(ids)]
    result['networks'] = [[pair(a, b)['networks'] if pair(a, b) else 0 for b in ids] for a in ids]
    return result


explorer = dict(summary=stats, nearest=nearest,
                native=payload(native_order, native_order),
                candidates=payload([r['config'] for r in candidate_order],
                                   [r['zoo_label'] for r in candidate_order]))
explorer['candidates']['zoo'] = [[zoo['matrix'][zix[a['zoo_label']]][zix[b['zoo_label']]]
                                for b in candidate_order] for a in candidate_order]
(HERE / 'explorer_data.json').write_text(json.dumps(explorer, allow_nan=False))
print(json.dumps(stats, indent=2))
print('Most distinct candidate configurations relative to other native baselines:')
for r in nearest[:12]:
    print(r['zoo_label'], r['nearest_baseline'], round(r['mean_tau_b'], 4), r['networks'])
print('Largest dataset-dependent gaps versus Zoo:')
for r in zoo_pairs[:8]:
    print(r['zoo_label_1'], '/', r['zoo_label_2'], round(r['native_mean_tau_b'], 3), r['zoo_reported_tau'])
