# Producing results after the mean-model decision

Do not run every historical script from the beginning. Start from the final corrected model tables and the original environmental inputs. The mean comparison is complete, and the fixed decision is documented in `4_Fitting_brt/4_2_2_hurdle/Mean_validation_results.md`. Scyliorhinus uses an additive log1p BRT with cross-fitted mean correction; Raja uses an additive direct-BPUE BRT. This is an exploratory decision informed by the spatial assessments, with weak spatial abundance skill for Scyliorhinus.

Run all commands from the repository root. Install the packages imported by each script; `Rscript` must be available. Paths below are placeholders and must be quoted if they contain spaces. Outputs use new folders to avoid overwriting previous runs. Keep data, models and credentials local.

## 1. Completed analyses; normally no need to repeat

The original nested comparison is script `4_2_2_3_validate_mean.R`. The focused follow-up is `4_2_2_4_mean_sensitivity.R`. Run the follow-up only after the original comparison has its `validation_complete.rds` marker:

```text
Rscript 4_Fitting_brt/4_2_2_hurdle/4_2_2_4_mean_sensitivity.R DATA_ROOT ORIGINAL_VALIDATION NEW_SENSITIVITY
```

Outputs include held-out errors, high-catch concentration, influential hauls and latitude-band diagnostics. The same assessment data are reused, so this is exploratory sensitivity rather than a new independent validation. High captures remain in every applicable training and assessment set.

## 2. Final fit and joint uncertainty

```text
Rscript 4_Fitting_brt/4_2_2_hurdle/4_2_2_5_fit_final_hurdle.R DATA_ROOT ORIGINAL_VALIDATION COMPLETED_SENSITIVITY NEW_MODELS 500
```

Families, predictors, positive tree complexity and learning rate are locked. Only tree count is selected within the locked positive family using full-data internal CV; occurrence uses the original full-data tuning. The script refits both components on all final observations and creates paired-location bootstrap ensembles. The same sampled locations are used for occurrence, positive abundance and both genera; duplicate copies remain together when cross-fitting the log correction. No failed replicate is silently discarded or replaced.

A 200-replicate fit has already been generated locally and tested. Five hundred replicates improve percentile-tail resolution. These intervals describe conditional fitted-mean uncertainty: they do not include family-selection uncertainty, unmodelled spatial dependence, effort uncertainty or uncertainty in conversion from effort to swept area. They are not intervals for a new individual catch. File `final_fit_complete.rds` confirms completion.

## 3. Rebuild the annual environmental stacks

Old stacks with negative depth cannot be reused. The batch route reads the signed source bathymetry, builds positive ocean depth, writes into a new derived folder and verifies full calendar coverage:

```text
Rscript 2_enviro/2_11_enviro_stack_daily.R DATA_ROOT 2021 NEW_STACKS
```

The script reads `DATA_ROOT/input/summary_static` and `DATA_ROOT/input/cmems_predict_3d/2021`. It retains source netCDFs and historical stacks. Required inputs must be present for all dates. It writes `NEW_STACKS/stack_manifest.csv` with unique dates and absolute stack paths. Missing days stop the run; do not remove the coverage check.

## 4. Generate joint daily maps

```text
Rscript 4_Fitting_brt/4_5_1_predict_hurdle_joint.R COMPLETED_MODELS STACK_MANIFEST SIGNED_BATHYMETRY NEW_MAPS
```

Use the original signed `Bathy.tif` as `SIGNED_BATHYMETRY`. Stack filenames must match manifest dates. The prediction extent is the historical map domain (-1.5 to 5 degrees longitude, 37 to 43 latitude), restricted to ocean depth greater than 20 and at most 700 m. Depth sign and ocean membership are checked independently. Model predictors are matched by name.

Each genus/day produces a summary raster (full-data fitted mean, bootstrap mean, 2.5th and 97.5th percentiles, occurrence probability), the complete joint-replicate raster, a depth-domain mask and an audit file. Replicate IDs persist across every cell and date; a file hash links their ordered identity to the saved raster. Missing predictor cells are counted in the audit. Do not multiply separate component intervals. A real 1 January 2021 stack has been tested for both genera with 200 replicates; that is not a full-year regeneration.

The historical component predictor `4_5_predictMap_boot.R` and merge `4_6_merge_predictions.R` are not the production route for these final formulations. The historical merge remains blocked. Likewise, the old variable-influence/partial plots need to read these final fitted models before being reused in the manuscript.

## 5. Prepare consistent effort, then integrate by replicate

The effort series must cover the entire requested period using one dataset version. Missing month-end days must not be patched with a different GFW version. The final effort preparation must produce **km² swept per cell per day**, conservatively aligned to the prediction grid, with explicit zeros where effort is zero. Hours and hours/km² are different inputs and cannot be passed directly. Do not use bilinear interpolation to transfer extensive cell totals. The complete effort-series regeneration, conversion and conservative alignment have not yet been executed or certified by this mean-model experiment.

Create a CSV with `date` (YYYY-MM-DD), `swept_area_path` (absolute raster path), and `dataset_version` (one non-empty version label for the period). Then:

```text
Rscript 6_bycatchEstimate/6_4_1_integrate_joint_hurdle.R COMPLETED_MAPS EFFORT_MANIFEST NEW_TOTALS 2021-01-01 2021-12-31
```

This entry point rejects missing/duplicate dates, mixed declared versions, unaligned grids, negative/missing effort within the domain, changed replicate files and positive effort where BPUE is missing. It multiplies each density by swept area, sums cells and dates **within each replicate**, then computes period percentiles. It reports effort outside the prediction domain separately. The point total uses the full-data fitted mean. Do not sum daily interval endpoints to obtain an annual interval. A version label is a declaration, not proof that the source download is consistent: retain download provenance.

The script is verified using known small-grid totals and a real-map synthetic-effort smoke test. No real annual bycatch total has yet been recomputed. It does not add uncertainty in effort, gear width or speed. PSA polygons, protected-area overlaps, seasonal maps and figures must be regenerated/adapted to these outputs before the manuscript numbers are final; the existing downstream scripts should not be run blindly on old filenames or median maps.

## 6. Update the manuscript only after the numerical products are complete

Use the full-data fitted arithmetic mean for the main map/total, show the joint mean intervals, retain the geographic validation limitations, and label the final family choice as exploratory. Record the effort dataset version, date range, conversion assumptions and output provenance. Update the reviewer response alongside the manuscript. Do not describe abundance-based PSA boundaries or regional totals as independently validated, especially for Scyliorhinus.

## Checks before production

```text
Rscript tests/check_model_inputs.R
Rscript tests/check_mean_validation.R
Rscript tests/check_mean_sensitivity.R
Rscript tests/check_joint_integration.R
```

Operationally: the mean analysis and local joint fit are complete. Full-year stacks/maps, certified annual swept area, real annual totals, downstream PSA products and final manuscript updates remain to be produced. The author does not need to choose a statistical model or rerun data preparation from scratch.
