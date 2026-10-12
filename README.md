# PotentialSpawningAreas

Revision preparation: [comparison with LÃ³pez et al. 2025 and the value of abundance and Raja](7_compare_Lopez/README.md). This is an analysis protocol and input audit, not completed spatial comparison results.

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
6. `4_Fitting_brt/4_2_2_hurdle/`: fit occurrence and positive BPUE. Dates are parsed explicitly and predictor columns are renamed by name, never by position.
7. `4_Fitting_brt/4_5_predictMap_boot.R`: check depth before prediction and apply the 20–700 m ocean domain on the prediction grid. Negative-depth legacy stacks are rejected and must be rebuilt with script 2.11.
8. Complete positive-mean selection and joint uncertainty before rerunning final hurdle maps and annual integration. Script 4.6 explicitly stops until this is implemented: the historical merge of separate component interval widths is not a valid hurdle interval. Existing map totals remain provisional until this step is replaced.

Run `Rscript tests/check_model_inputs.R` before rebuilding maps. Checks cover keyed dates, column identities, positive depth, land exclusion and reproducible geographic grouping. Shared functions live in `fun/`; there is no separate revision workflow folder.

### Positive mean and validation

Compare two prespecified positive-BPUE candidates: a Gaussian BRT fitted directly to BPUE, and a Gaussian BRT fitted to log1p(BPUE) with a cross-fitted retransformation correction. Evaluate both through the complete hurdle prediction in original units, using identical geographic holdouts. Select tree parameters and estimate any correction inside the training data only. Report RMSE, MAE and observed versus predicted mean by held-out area; give each area's results rather than relying on a pooled score alone. Check negative predictions from the untransformed Gaussian model and report any clipping rule. No candidate has yet been selected for final mapping.

Geographic folds alone do not make the existing parameter search a nested evaluation. The outer geographic holdout must stay out of all tuning and calibration. A log model with a correction factor is a candidate, not an automatic fix. For annual totals, use `p * E(BPUE | positive, X)`; inverse-log predictions alone need not equal that arithmetic mean. Combine occurrence and positive predictions within the same resampled-haul replicate before calculating intervals.
