# Multiseason Occupancy Modeling Workflow for Wildlife Monitoring

This repository documents a complete workflow for preparing detection
histories, survey covariates ad spatial site covariates for multiseason
occupancy anlaysis using camera trap data, QGIS, Goolge Earth Engine and
R.

## Objectives:

-   Prepare Encounter history matrices for occupancy modeling
-   Generate observation covariates (effort, year)
-   Develop spatial site covariates:
    -   Forest cover
    -   Forest loss
    -   NDVI
    -   Distance to settlement
    -   TRI

## Repository Structure
```mermaid
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

A --> D[scripts]

D --> D1[001_extract_metadata.R]
D1 --> D2[002_data_parsing.R]
D2 --> D3[003_assign_siteID.R]
D3 --> D4[004_encounter_history.R]
D4 --> D5[005_survey_covariates.R]
D5 --> D6[006_gee_ndvi.js]

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
-   Organize outputs for occupancy modeling

## Site covariates

| Covariate           | Ecological Relevance      |
|---------------------|---------------------------|
| Forest cover        | Habitat availability      |
| Forest loss         | Habiatat disturbance      |
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

-   [Encounter history matrix
    preparation](D:/R_projects/multiseason_occupancy/docs/01.encounter_history.md)
-   [Survey
    covariates](D:/R_projects/multiseason_occupancy/docs/02.survey_cov.md)
-   [Forest
    covariates](D:/R_projects/multiseason_occupancy/docs/03.forest_cover.md)
-   [NDVI
    covariates](D:/R_projects/multiseason_occupancy/docs/04.ndvi.md)
-   [Distance to
    settlement](D:/R_projects/multiseason_occupancy/docs/05.distance_to_settlement.md)
-   [Terrain ruggedness
    index](D:/R_projects/multiseason_occupancy/docs/06.tri.md)

## Outputs

-   Encounter history matrix
-   Effort matrix
-   Year matrix
-   Forest cover and loss covariate
-   NDVI covariae
-   Distance to settlement covariate
-   Terrain ruggedness index covariate
-   Raste outputs
-   Maps of different site covariates

## Future additions

-   Correlation analysis
-   Model fitting in R
-   Detection probability analysis
-   Model selection Workflow
-   Habitat suitability visualization
