# PotentialSpawningAreas

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
