"""Independently recompute every retained correlation using SciPy/NumPy."""
from pathlib import Path
from collections import defaultdict, Counter
import csv
import gzip
import hashlib
import json
import math
import platform
import numpy as np
import scipy
from scipy.stats import kendalltau, rankdata

HERE = Path(__file__).resolve().parent / 'correlation-audit'
with gzip.open(HERE / 'score_fixtures.json.gz', 'rt') as stream:
    fixtures = json.load(stream)
with gzip.open(HERE / 'per_network_pairs.csv.gz', 'rt') as stream:
    pairs = defaultdict(list)
    for row in csv.DictReader(stream):
        pairs[row['network']].append(row)
availability = list(csv.DictReader((HERE / 'availability.csv').open()))
assert len(availability) == 19*180
assert len({(r['network'], r['config']) for r in availability}) == len(availability)
assert len(fixtures) == 19
maximum = dict.fromkeys(['tau_b', 'spearman', 'pearson', 'tau_b_raw'], 0.)
comparisons = 0
network_checks = []
failures = []
tolerance = 1e-12

def as_float(v):
    try:
        return float(v)
    except (ValueError, TypeError):
        return float('nan')

def hex_float(v):
    return float('nan') if v in ('NA', 'NaN') else float.fromhex(v)

for name, f in fixtures.items():
    ids = f['configs']
    nodes = f['nodes']
    raw = np.array([[hex_float(v) for v in row] for row in f['raw_hex']])
    ranked = np.array([[as_float(v) for v in row] for row in f['rank_input']])
    adjacency = np.asarray(f['adjacency'])
    assert raw.shape == ranked.shape == (len(nodes), 180)
    assert len(set(nodes)) == len(nodes)
    assert np.array_equal(adjacency, adjacency.T)
    assert set(np.unique(adjacency)) <= {0, 1} and np.trace(adjacency) == 0
    valid = np.array([np.isfinite(v).all() and len(np.unique(v)) > 1 for v in ranked.T])
    observed = {r['config'] for r in availability if r['network'] == name and r['status'] == 'ok'}
    assert observed == {ids[i] for i in np.flatnonzero(valid)}
    assert len(pairs[name]) == int(valid.sum()*(valid.sum()-1)//2)
    assert len({(r['config_1'], r['config_2']) for r in pairs[name]}) == len(pairs[name])
    for i in np.flatnonzero(valid):
        scale = np.max(abs(raw[:, i]))
        assert np.allclose(np.round(raw[:, i]/scale, 12), ranked[:, i], atol=1e-12, rtol=0)
    ix = {ids[i]: j for j, i in enumerate(np.flatnonzero(valid))}
    raw_valid, rank_valid = raw[:, valid], ranked[:, valid]
    # Scaling avoids overflow; Pearson is invariant to a positive scale.
    pearson = np.corrcoef(raw_valid/np.max(abs(raw_valid), axis=0), rowvar=False)
    spearman = np.corrcoef(rankdata(rank_valid, axis=0, method='average'), rowvar=False)
    for r in pairs[name]:
        i, j = ix[r['config_1']], ix[r['config_2']]
        expected = dict(tau_b=kendalltau(rank_valid[:, i], rank_valid[:, j], variant='b').statistic,
                        tau_b_raw=kendalltau(raw_valid[:, i], raw_valid[:, j], variant='b').statistic,
                        spearman=spearman[i, j], pearson=pearson[i, j])
        for field, value in expected.items():
            actual = float(r[field])
            error = abs(value-actual)
            comparisons += 1
            if not math.isfinite(error) or error > tolerance:
                if len(failures) < 20:
                    failures.append(dict(network=name, a=r['config_1'], b=r['config_2'],
                                         field=field, expected=value, actual=actual))
            maximum[field] = max(maximum[field], error)
    network_checks.append(dict(network=name, nodes=len(nodes), usable=int(valid.sum()),
                               pairs=len(pairs[name])))
    print(f'{name}: {len(pairs[name])} pairs independently checked', flush=True)

original = HERE.parent / 'parameter_candidates.csv'
assert hashlib.sha256(original.read_bytes()).hexdigest() == '4006d16ec1d0d40db43aab48cf0301cf2057604ffffff9fbedf4a3ea2173aae2'
result = dict(passed=not failures, tolerance=tolerance, comparisons=comparisons,
              pair_observations=sum(len(v) for v in pairs.values()), maximum_absolute_error=maximum,
              failures=failures, network_checks=network_checks,
              availability_counts=dict(Counter(r['status'] for r in availability)),
              versions=dict(python=platform.python_version(), numpy=np.__version__, scipy=scipy.__version__),
              raw_transfer='IEEE doubles exported as hexadecimal strings; no decimal near-tie loss',
              scope='Correlation calculation and retained fixture integrity; not centrality function equivalence')
(HERE / 'validation.json').write_text(json.dumps(result, indent=2, allow_nan=False))
print(json.dumps({k: v for k, v in result.items() if k != 'network_checks'}, indent=2))
assert not failures, 'Independent correlation validation failed; inspect validation.json'
