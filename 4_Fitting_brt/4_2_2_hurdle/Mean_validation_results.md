# Positive mean validation: 12 October 2026

The original nested experiment and a limited exploratory follow-up are complete. The operational decision is an additive corrected-log BRT for Scyliorhinus and an additive direct-BPUE BRT for Raja. This closes the formulation choice for production, while retaining weak geographic abundance performance for Scyliorhinus and the exploratory provenance of the decision. Full-year maps and real annual totals have not yet been regenerated.

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

## Decision after the original experiment

The original experiment motivated a limited comparison of more regularised and alternative positive-mean formulations. The automatically selected objects from script 4.2.2.3 remain historical candidates; production uses the separate locked fits from 4.2.2.5, based on the follow-up below. Because the follow-up was informed by these outcomes, it is exploratory. A new independent dataset would provide stronger confirmation.

Production now has a paired-location bootstrap and a joint prediction route, described below. Script 4.6 remains deliberately blocked because its historical multiplication of component interval widths is invalid. The replacement entry points are 4.5.1 and 6.4.1. Manuscript numerical claims and reviewer responses require updating after the full annual regeneration; no real annual total is claimed here.

## Focused follow-up and locked decision

The follow-up retains the original training-only occurrence configurations and the identical 40 outer assessments. Positive BRT complexity is fixed at 1, learning rate at 0.005, and tree count chosen internally from 250, 750 and 1500. A positive Gamma GLM with a log link and the same five linear predictors provides a simpler skew-compatible alternative. Its training-only standardisation and convex Gamma mean likelihood are fitted with a checked BFGS line search, because standard IRLS failed to converge in some small, skewed subsets even with a larger iteration allowance. The fitted means agree with standard Gamma GLM on a well-conditioned synthetic test. No catch was removed, capped or winsorised. These reused assessment results are exploratory, not an independent validation of the final decision. See [R's family documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/family.html) for Gamma log-link models.

| Genus | Positive formulation | Geographic RMSE | Geographic mean | Random RMSE |
|---|---|---:|---:|---:|
| Scyliorhinus | Additive direct BPUE | 165.70 | 56.35 | 152.47 |
| Scyliorhinus | Additive corrected log1p | **164.52** | 53.49 | 159.92 |
| Scyliorhinus | Gamma log-link GLM | 198.32 | 61.11 | 146.18 |
| Raja | Additive direct BPUE | **94.70** | 28.77 | 66.95 |
| Raja | Additive corrected log1p | 101.29 | 36.30 | 85.06 |
| Raja | Gamma log-link GLM | 115.83 | 31.89 | 100.11 |

The fixed corrected-log family wins over fixed direct BPUE in all three geographic partitions for Scyliorhinus; fixed direct BPUE wins in all three for Raja. Gamma does not improve geographic prediction for either genus, although its random-partition error for Scyliorhinus is lower. The expanded inner family selector itself has geographic RMSE 200.57 for Scyliorhinus and 97.75 for Raja. Therefore, a reliable automatic family selector has not been demonstrated; the operational choice is explicitly a post-hoc fixed-family decision, not a new claim of unbiased final-model validation. Differences between the two BRT candidates for Scyliorhinus remain small.

The five largest Raja BPUE observations account for 48.44% of the sum of observed haul densities and lie between 40.35 and 40.47 degrees N. The author identifies this broader area as a known high-catch setting. This is consistent with aggregation, but coordinates and density concentration alone do not establish a biological mechanism. Five hauls explain 74.85% of direct-model squared error and 72.44% of corrected-log squared error in the original geographic assessment. For Scyliorhinus, the five largest densities account for 43.82% of the density sum, and five hauls explain 75.51% and 81.89% of those respective errors. Density-sum shares are descriptive sample statistics, not shares of area-wide abundance or annual captures. The detailed local audit also reports captured-count shares and latitude-band performance.

Lock Scyliorhinus to corrected log1p and Raja to direct BPUE, with additive positive BRTs. Within-family full-data tuning chooses 750 trees at learning rate 0.005 for both. The final Scyliorhinus correction is 2.09294; Raja needs no inverse-log correction. Retain all high catches. Do not seek further candidate families merely to improve these reused assessment scores. Describe Scyliorhinus abundance allocation as weakly supported: the additive log model improves geographic RMSE by only 2.4% over the training-mean baseline. Raja's additive direct model improves by 21.4%, but underpredicts the pooled geographic mean (28.77 versus 33.47).

## Production implementation and actual verification

Script 4.2.2.5 fits the locked models and paired-location bootstrap, conditional on fixed predictors, families and tuned parameters. The same location remapping is used for both components and genera, with the retransformation correction refitted per replicate. Two hundred joint replicates per genus have been generated locally. They do not account for model-family selection, unmodelled spatial dependence or effort/conversion uncertainty.

Script 4.5.1 writes daily joint means, conditional mean intervals, occurrence, complete replicate rasters and domain/identity audits. Both genera have been predicted on the real rebuilt 1 January 2021 stack with all 200 replicates, and point, mean and percentile values checked against direct model predictions. Script 6.4.1 integrates by cell and date within each replicate before calculating period intervals. The integration checks include known small-grid arithmetic, missing/negative effort and shifted grids. A real-map test uses synthetic swept area and must not be interpreted as a real bycatch estimate.

On the tested day, 3499 of 3531 depth-domain cells have complete predictors; 32 coastal cells lack one or more predictors. The domain audit retains that distinction. Production integration stops if any such cell has positive swept area, rather than silently treating missing BPUE as zero. This coverage must be checked against the real effort series before certifying annual totals.

The annual stack builder now supports an explicit data root, year and new derived output folder, retaining original inputs and requiring a complete unique calendar before writing its manifest. A full calendar-year stack/map rebuild and real effort integration remain pending. Follow [Final results workflow](../../Final_results_workflow.md) for the exact execution order and remaining PSA, figure and manuscript work.

## Verification

Automated checks cover response-independent folds, row-order invariance, co-located sampling, seed variation and arithmetic retransformation. A synthetic nested-fitting test and a separate real-data reduced-grid test change reserved response magnitudes while requiring predictions, selected families and correction factors to remain identical. An independent Python calculation reproduced every pooled RMSE, checked finite nonnegative predictions, and confirmed 40 outer jobs and one assessment per haul per partition and method. Row-level data, fitted models and credentials are excluded from the PR.
