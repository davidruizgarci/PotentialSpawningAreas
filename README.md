# PotentialSpawningAreas

The Lopez et al. comparison is deferred while the numerical workflow is corrected and evaluated.

This repository contains a series of scripts designed to extract environmental variables, process 4D netCDF data, fit Boosted Regression Tree (BRT) models, predict egg case bycatch occurrence and abundance, identify Potential Spawning Areas (PSAs) and estimate total bycatch in overlap with fishing effort.

If you use this repository, please cite it using the following DOI:
[![DOI](https://zenodo.org/badge/930585604.svg)](https://doi.org/10.5281/zenodo.14847186)

### Available Scripts:
*1_prep_df*: Organizes and prepares data for analysis.

*2_enviro*: 
- Download environmental data from CMEMS.
- Extract environmental variables at surveyed points.
- Select depth layers from 4D environmental rasters.
- Generate raster stacks for later predictions.
  
*3_Subsetting_Checking_data*: Performs data validation and pre-modeling checks.

*4_Fitting_brt*: 
- Fit BRT models to predict egg case bycatch occurrence and abundance.
- Implement bootstrap methods to handle stochasticity.
- Generate spatial predictions.
- Identify Potential Spawning Areas (PSAs).

*5_calcPSAs*: 
- Analyze PSA overlap with fishing effort, marine protected areas (MPAs), trawling exclusion zones (TEZs), and important shark and ray areas (ISRAs).
- Assesses seasonal variations in predicted BPUE.
- Assess presistence in PSA distribution.

*6_bycatchEstimate*: Estimates annual total bycatch of egg cases based on daily BPUE predictions and fishing effort maps from Global Fishing Watch (GFW).

*PSA_polygon*: Contains the final PSA polygon as a shapefile.

*fig*: Generates various visual outputs, including:
- Data validation plots.
- Study area maps.
- Predicted BPUE maps weighted by occurrence.
- Egg case density maps.
- PSA overlap maps with MPAs, TEZs, and ISRAs.
- Maps of environmental predictor variables.

*fun*: Provides a function to extract environmental data from 4D (lon, lat, depth, time) or 3D (lon, lat, depth) rasters at a specified depth value.

### Reproducing the numerical revision

Run scripts from the repository root, using the existing `input_data`, `temp_data` and `output_data` configuration. Install the packages listed at the top of each script. Local data and credentials are not stored in Git.

1. `2_enviro/2_3_GFW_download.R`: download effort with an exclusive end date at the start of the following month. Keep dataset versions consistent across the year.
2. `2_enviro/2_11_enviro_stack_daily.R`: rebuild daily stacks. Signed seabed elevation is converted to positive ocean depth before stacking; land is excluded. Every saved depth layer is read back and checked.
3. `3_Subsetting_Checking_data/3_7_validate_model_inputs.R`: validate final inputs and match dates to the environmental extraction by haul code. Other final values are retained. This can also be run with the local data root as its only command-line argument.
4. `3_Subsetting_Checking_data/3_8_summarise_ECEME_content.R`: summarise capsule content, with the workbook and output CSV as arguments. Uncertain development states and rotten content are non-empty; this does not imply viability.
5. `4_Fitting_brt/4_1_set_folds.R`: create geographic folds without splitting hauls. The output is a separate `ALL2` table, which the fitting scripts now read by default. Run for each genus using the same seed and haul coordinates.
6. `4_Fitting_brt/4_2_2_hurdle/`: dates are parsed explicitly and predictor columns are renamed by name, never by position. Run `4_2_2_3_validate_mean.R` to compare and select positive-mean formulations before final mapping. The historical component-fit scripts remain available for their original diagnostics; they do not replace the nested evaluation.
7. `4_Fitting_brt/4_5_predictMap_boot.R`: check depth before prediction and apply the 20–700 m ocean domain on the prediction grid. Negative-depth legacy stacks are rejected and must be rebuilt with script 2.11.
8. Complete positive-mean selection and joint uncertainty before rerunning final hurdle maps and annual integration. Script 4.6 explicitly stops until this is implemented: the historical merge of separate component interval widths is not a valid hurdle interval. Existing map totals remain provisional until this step is replaced.

Run `Rscript tests/check_model_inputs.R` before rebuilding maps. Checks cover keyed dates, column identities, positive depth, land exclusion and reproducible geographic grouping. Shared functions live in `fun/`; there is no separate revision workflow folder.

### Positive mean and validation

Run from the repository root:

```text
Rscript 4_Fitting_brt/4_2_2_hurdle/4_2_2_3_validate_mean.R <local_data_root> <new_output_folder>
Rscript tests/check_mean_validation.R
```

The comparison evaluates a Gaussian BRT fitted directly to positive BPUE and a Gaussian BRT fitted to log1p(BPUE) with a cross-fitted retransformation correction. Both use the same prespecified physical predictors: depth, slope, log fishing effort, bottom temperature and salinity. The evaluation is conditional on this predictor recipe, rather than a validation of the historical screening of all environmental variables. Candidate tree complexities are 1, 3 and 5; learning rates are 0.005 and 0.02; tree counts are 250, 750, 1500 and 3000. Bag fraction is 0.7; minimum node size is at most 5 and decreases in small inner training sets. Negative positive-component predictions are clipped at zero and clipping is recorded.

Five outer geographic bands hold out entire sampling locations. Three fixed random rotations (seeds 123, 456, 789) change their boundaries; an additional random location-group partition (seed 2026) provides an interpolation reference. Folds use coordinates, not observed or predicted abundance, and locations rounded to six decimal places remain together. All taxa use the same locations and partition seeds. Geographic boundaries have no exclusion buffer, so this assesses geographic transfer without guaranteeing independence across every boundary. No fold is silently replaced because its result is poor.

Within each outer training set, three-fold tuning chooses occurrence parameters by Brier score and positive-mean parameters/family by complete-hurdle MSE in capsules/km2. For each log candidate, a further three-fold cross-fit estimates the retransformation correction within the inner training data. Each calibration observation is excluded from the model that predicts its residual. The outer assessment responses do not enter tuning, calibration or family selection. The mathematical target is `p * E(BPUE | positive, X)`; inverse-log predictions alone need not equal that arithmetic mean.

Outputs include outer predictions, region/repeat/abundance-stratum errors, the training-mean baseline, parameter choices, correction factors, source hashes and software versions. Abundance strata are diagnosed after fitting: zero, positive up to the training positive-BPUE 75th percentile, and above it. These diagnostics do not alter the folds. RMSE is the prespecified selection criterion, with MAE and regional mean bias reported alongside it. Repeated assessments of the same hauls are not independent new observations and must not be used as such to construct confidence intervals.

The `nested_selected` outer predictions evaluate the entire family-selection procedure. Full-data deployment selection averages inner scores across the three geographic partitions, then refits the chosen family and parameters. Its fitted model is a candidate for mapping, not a new independent performance estimate. Final joint bootstrap maps and annual integration remain separate pending steps; do not substitute this object into the historical separate-bootstrap merge. All row-level outputs and fitted models stay in the specified local output folder.

Methodological context: [Roberts et al. 2017 on structured cross-validation](https://www.wsl.ch/lud/biodiversity_events/papers/Roberts_et_al-2017-Ecography.pdf), [blockCV documentation](https://search.r-project.org/CRAN/refmans/blockCV/html/blockCV.html).

The completed experiment and its limitations are documented in [Mean validation results](4_Fitting_brt/4_2_2_hurdle/Mean_validation_results.md). Scyliorhinus shows weak geographic abundance performance, and Raja shows unstable family selection. The full-data fits remain provisional; the completed comparison does not establish final maps or annual totals.
