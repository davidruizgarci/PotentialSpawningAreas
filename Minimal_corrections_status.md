# Minimal corrections to the original workflow — 12 October 2026

All edits are in the Desktop PotentialSpawningAreas checkout. The historical Manuscript_Code_Data/EggCase_Distribution folder is a read-only reference. Existing model-fitting scripts, parameters, predictor selection, folds and saved models were not changed or refitted. Pre-existing differences in PSA and other figure scripts were retained.

## Applied corrections

1. **2_enviro/2_11_enviro_stack_daily.R:** convert negative source seabed elevation to positive ocean depth; keep land missing; independently mask ocean at final cell centres; inspect every depth block before and after saving. Keep the original grid, filenames and daily output folders. Back up existing grd/gri files before regeneration. Also correct the netCDF filename pattern, the missing vo name and date extraction from absolute folder paths, and stop if no dynamic grid exists. These are operational fixes, not new predictors or grid choices.

2. **4_Fitting_brt/4_5_predictMap_boot.R:** create the ocean/depth mask after loading the daily stack, instead of using an undefined or stale s. Preserve the historical 1–700 m domain and the existing 100-model ensembles. Reject old negative-depth stacks.

3. **2_enviro/2_3_GFW_download.R and 6_bycatchEstimate/6_1_rasterFE.R:** query through the exclusive first day of the next month. Complete only absent dates in the original monthly CSV folders and names. Preserve every existing corrected row and create a backup before appending. Empty or incompatible responses cannot replace a file. A missing date is not interpreted as zero effort.

4. **Training-residual retransformation, explicitly chosen by the author:** estimate S = mean(exp(log1p(observed BPUE) - fitted log prediction)) using the training data stored inside each existing Gaussian model. Each bootstrap uses its own saved resample and correction. Recover positive mean as exp(mu) * S - 1 before multiplying by occurrence. No auxiliary fits. This in-sample estimate can understate the correction; a global factor also assumes residual retransformation is sufficiently similar across predictor values. The original bootstrap medians and percentile summaries remain in use.

5. **4_6_merge_predictions.R:** require complete dates and verified units before multiplying corrected positive BPUE by occurrence. This prevents independently shortened date lists from misaligning and prevents old log maps from entering the corrected workflow. Component and point hurdle maps have small scale/fingerprint sidecars; map filenames remain unchanged.

6. **6_3_cropBPUE.R and fig/2_PredMaps.R:** remove the later expm1 operations, which would otherwise transform already-corrected BPUE again. The crop script defaults to point BPUE, not CI-width products. Plot colours, layouts and display scales are retained.

Three small shared helpers contain depth checks, monthly completion and training-residual retransformation. The missing laptop-only 0_setup.R was recreated from the historical setup with a code_dir variable while retaining its data working directory. It has a local Git exclusion and must not be committed; credentials remain local.

## Verification completed

- Synthetic checks cover positive depth, land exclusion, month ends including leap years, preservation of corrected effort rows, backups, and prevention of duplicate downloads.
- A real 1 January 2021 stack was rebuilt only in temporary storage and successfully predicted with all four original selected models. This was a smoke test, not a regenerated annual result.
- Both original positive selected models and all 200 existing positive bootstrap models yielded valid training-residual correction factors; bootstrap model fingerprints were unchanged. Selected-model S values are approximately 1.370401 (Raja, n=60) and 1.637846 (Scyliorhinus, n=102). These are retransformation factors, not new abundance estimates.
- Model-fitting script fingerprints remain unchanged. No annual production maps, totals or manuscript figures have been regenerated.

## Dates: audit only, no data replacement

The final Raja table contains 119 dates that disagree with its extraction reference: 111 have 2020 instead of 2021 and eight have 2020 instead of 2022. Month and day agree in all 119 cases. Scyliorhinus has no such differences. Extraction dates agree with encoded haul dates for both genera. The saved selected models use physical predictors, not date, year or season; this date discrepancy alone does not justify refitting these models. Fix keyed dates before any subsequent date-dependent observed-data analysis. Existing data, model metadata and validation results were not rewritten.

## Fishing-effort completion is still pending

Each 2021 monthly CSV omits its last day: 12 missing dates in total. The installed gfwr 3.0 requests public-global-fishing-effort:latest. A temporary probe for the already-recorded 30 January 2021 returned 1817 rows versus 1779 in the historical file, with matching column names but different values. Compatibility is therefore NOT verified. No actual missing-date data were appended and no original effort values were changed. The scripts stop on an incomplete historical file until source compatibility is established; changing the verification flag alone does not establish compatibility.

Next: locate a compatible historical GFW release. If unavailable, consult the author before using a different release or regenerating a complete effort series. Preserve the original corrected effort values and the original static fishing-effort predictor meanwhile. Do not run the annual integration on a mixture of releases.

## Hurdle uncertainty is still pending

The historical product of occurrence and abundance CI widths is not a hurdle confidence interval. 4_6 generates the approved point maps and then stops before this invalid interval calculation. No new joint-bootstrap design has been introduced. Interval computation needs a separate agreement; existing CI-derived annual results and figures must not be treated as corrected. The unchanged extensive-effort resampling and downstream alignment logic also need review before certifying annual captures.

## What to run now

Run the laptop-only setup, then inspect 2.11. Rebuild its stacks when ready; it retains a backup of each replaced raster pair. Configure each original component in 4.5 to predict from the existing model ensembles with corrected depth and positive-response units. Generate point hurdle maps in 4.6 only after both components have been regenerated. Expect the explicit stop before the unresolved uncertainty section. Do not rerun model fitting or assume annual totals and figure outputs are ready. No commit, push or historical-folder restoration was performed.
