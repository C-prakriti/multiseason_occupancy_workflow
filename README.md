# Multiseason Occupancy Modeling Workflow for Wildlife Monitoring

This repository documents a complete workflow for preparing detection histories, survey covariates, spatial site covariates and model building for multiseason occupancy anlaysis using camera trap data, QGIS, Goolge Earth Engine and R.

## Objectives:

-   Prepare Encounter history matrices for occupancy modeling
-   Generate observation covariates (effort, year)
-   Develop spatial site covariates:
    -   Forest cover
    -   Forest loss
    -   NDVI
    -   Distance to settlement
    -   TRI
- Build candidate models and validate them using unmarked package in R

## Repository Structure

``` mermaid
flowchart TD

A[README.md]

A --> B[data]
B --> B1[raw]
B --> B2[processed]

A --> C[docs]

C --> C1[01_encounter_history.md]
C1 --> C2[02_survey_covariates.md]
C2 --> C3[03_forest_cover.md]
C3 --> C4[04_ndvi.md]
C4 --> C5[05_distance_to_settlement.md]
C5 --> C6[06_tri.md]
C6 --> C7[07_umf_object.md]
C7 --> C8[08_model_selection.md]

A --> D[scripts]

D --> D1[001_extract_metadata.R]
D1 --> D2[002_data_parsing.R]
D2 --> D3[003_assign_siteID.R]
D3 --> D4[004_encounter_history.R]
D4 --> D5[005_survey_covariates.R]
D5 --> D6[006_gee_ndvi.js]
D6 --> D7[007_datacleaning_umfobject.R]
D7 --> D8[008_model_selection.R]

A --> E[qgis]

E --> E1[occupancy_project.qgz]
E --> E2[sample_sites.gpkg]
E --> E3[siteID.gpkg]
E --> E4[study_area.gpkg]

A --> F[styles]

A --> G[outputs]
```

## Workflow Overview

-   Prepare camera station metadata
-   Generate encounter history matrices in R
-   Create observation covariates
-   Prepare spatial covariates in QGIS and Google Earth Engine
-   Extract stie-level covariates
-   Organize outputs and create unmarkedMultFrame object for occupancy modeling
-   Build candidate models for detection, occupancy, colonization and Extinction using AICc
-   Select the best models for each parameter
-   Check the fitness of the model using Goodness of Fit
-   In case of moderate over dispersion, use QAICc to identify the best model for each parameters
-   Estimate final prediction probability

## Site covariates

| Covariate           | Ecological Relevance      |
|---------------------|---------------------------|
| Forest cover        | Habitat availability      |
| Forest loss         | Habitat disturbance       |
| NDVI                | Proxy for prey base index |
| Settlement distance | Human disturbance         |
| TRI                 | Habitat suitability       |

## Software Used

-   R 4.5.3
-   QGIS 3.44.9
-   Google Earth Engine
-   MODIS MOD13Q1 NDVI product
-   Hansen Global Forest Change dataset
-   GHSL settlement layer

## Documentation

-   [Encounter history matrix preparation](docs/01.encounter_history.md)
-   [Survey covariates](docs/02.survey_cov.md)
-   [Forest covariates](docs/03.forest_cover.md)
-   [NDVI covariates](docs/04.ndvi.md)
-   [Distance to settlement](docs/05.distance_to_settlement.md)
-   [Terrain ruggedness index](docs/06.tri.md)
-   [Umf Object Creation](docs/07.umf_object.md)
-   [Model Selection](docs/08.model_selection.md)

## Outputs

-   Encounter history matrix
-   Effort matrix
-   Year matrix
-   Forest cover and loss covariate
-   NDVI covariae
-   Distance to settlement covariate
-   Terrain ruggedness index covariate
-   Maps of different site covariates
-   dynamic covariates
-   Scaled covariates
-   umf object.rds

## Future additions

-  Estimate occupancy dynamics through time
-  Create final Parameter table
-  Visualize the model relationships
