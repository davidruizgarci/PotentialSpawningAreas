# Positive mean validation: 12 October 2026

The nested experiment is complete. Neither genus should yet be treated as having a definitive validated abundance map. Scyliorhinus has weak geographic abundance performance; Raja has useful predictive improvement but unstable family selection. The saved full-data fits are provisional candidates, not replacement final maps.

## Reproducible workflow

Run script `4_2_2_3_validate_mean.R` from the repository root with the local data root and a new output directory. The README specifies the complete grid, seeds, calibration and output files. The run used R 4.4.3 and gbm 2.2.3, five prespecified predictors, 194 hauls per genus, and 40 outer assessments: two genera, three geographic partitions plus one random partition, and five folds each. Scyliorhinus has 102 positive hauls; Raja has 60. Input hashes were unchanged at completion.

Both candidates fit Gaussian BRTs to positive observations. One uses BPUE directly; the other uses log1p(BPUE) with a correction estimated from cross-fitted training residuals. The complete hurdle prediction is occurrence probability times the positive arithmetic mean. Simple inverse transformation alone is not this mean. The global correction is a tested candidate approximation; it does not establish that the residual distribution is constant across environmental conditions.

Geographic partitions are coordinate-based bands with three reproducible random rotations. They contain approximately equal numbers of locations, retain co-located hauls together, and have no exclusion buffer. The random reference also groups locations. Neither observed nor predicted abundance constructs the partitions. Post-fitting diagnostics separately report zero, ordinary positive and high positive catches, using the training-positive 75th percentile as the high-catch threshold.

Tuning and retransformation occur inside each outer training set. The positive family is chosen by inner complete-hurdle MSE in original units; occurrence parameters use Brier score. Calibration residuals have a further cross-fitting layer. This is conditional validation of the fixed predictor recipe, not a repeat of historical variable screening. It evaluates spatial transfer within the sampled study setting, not temporal transfer or unsampled environmental extremes.

## Held-out results

RMSE and mean are in capsules/km². Lower RMSE is better. Geographic values pool three assessments of the same 194 hauls: 582 prediction records are not 582 independent observations. No confidence interval or significance claim is inferred from that count.

| Genus | Method | Geographic RMSE | Geographic predicted mean | Random RMSE |
|---|---|---:|---:|---:|
| Scyliorhinus | Direct BPUE | 168.21 | 59.14 | 149.54 |
| Scyliorhinus | Corrected log1p | 165.12 | 55.08 | 160.34 |
| Scyliorhinus | Inner-selected family | 169.88 | 52.42 | 151.02 |
| Scyliorhinus | Training-mean baseline | 168.60 | 53.35 | 169.84 |
| Raja | Direct BPUE | 94.92 | 27.98 | 67.33 |
| Raja | Corrected log1p | 108.87 | 37.52 | 81.90 |
| Raja | Inner-selected family | 102.11 | 36.47 | 74.65 |
| Raja | Training-mean baseline | 120.54 | 33.42 | 117.52 |

Observed sample means are 53.38 for Scyliorhinus and 33.47 for Raja. Agreement of a pooled mean with these sample means does not establish accurate spatial allocation or an unbiased area-wide total.

For Scyliorhinus, corrected log1p reduces geographic RMSE by only 2.1% relative to the constant baseline. The complete family-selection procedure performs slightly worse than that baseline. For Raja, direct BPUE reduces geographic RMSE by 21.3%; the selection procedure improves by 15.3%. Random validation gives more favourable errors, particularly for Raja, and therefore cannot replace the geographic assessment of transfer to reserved areas.

## Selection stability and deployment candidates

Across the 15 geographic outer fits, Scyliorhinus selects log1p eight times and direct BPUE seven times; Raja selects log1p six times and direct BPUE nine times. The log correction varies from 1.98 to 2.73 for Scyliorhinus and 1.64 to 8.53 for Raja. Direct positive-component predictions are clipped at zero in 24/582 and 25/582 geographic prediction records respectively; corrected-log predictions require no clipping in this run.

The separate full-data selection averages internal scores across three geographic partitions. It selects:

| Genus | Family | Tree complexity | Learning rate | Trees | Correction |
|---|---|---:|---:|---:|---:|
| Scyliorhinus | Corrected log1p | 1 | 0.005 | 750 | 2.09294 |
| Raja | Corrected log1p | 5 | 0.005 | 1500 | 4.85327 |

Raja's full-data selection disagrees with the better-performing family on outer assessment. This is evidence of instability, not a reason to silently override the prespecified selector after seeing its test results. Direct BPUE is the stronger comparative result for Raja in this experiment; choosing it post hoc would require reporting that selection as exploratory. Scyliorhinus provides little evidence that the tested mean models reliably allocate abundance among reserved geographic areas.

## Decision and remaining work

Retain both candidates and all diagnostics; keep the automatically selected objects provisional. For Scyliorhinus, investigate geographic bias and influential high catches before interpreting abundance-based PSA boundaries. For Raja, investigate correction variability and the inner/outer selection disagreement, with a limited prespecified comparison of more regularised or alternative positive-mean formulations. Any further experiment informed by these outcomes must be labelled exploratory; a new independent dataset would provide stronger confirmation.

Once a final formulation is defensible, integrate it into prediction with a joint bootstrap of occurrence, positive fit and any retransformation correction. Generate joint-replicate intervals, rebuild the annual stack series and recompute annual integration. Script 4.6 remains deliberately blocked: this experiment does not repair the historical multiplication of separate component interval widths. Manuscript numerical claims and reviewer responses require updating after those final results; no final annual total is claimed here.

## Verification

Automated checks cover response-independent folds, row-order invariance, co-located sampling, seed variation and arithmetic retransformation. A synthetic nested-fitting test and a separate real-data reduced-grid test change reserved response magnitudes while requiring predictions, selected families and correction factors to remain identical. An independent Python calculation reproduced every pooled RMSE, checked finite nonnegative predictions, and confirmed 40 outer jobs and one assessment per haul per partition and method. Row-level data, fitted models and credentials are excluded from the PR.
