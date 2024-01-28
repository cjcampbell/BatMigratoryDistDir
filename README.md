# BatMigratoryDistDir

Data and code used in analyses associated with _Unusual migratory strategy a key factor driving interactions at wind energy facilities in at-risk bats_.

## Repository contents
### /R/
Scripts used to read and clean data, run analyses, plot figures.
| Script                                  | Description |
| -----------                             | ----------- |
| 00_Setup.R                              | Load libraries, functions, set navigation objects |
| 01_LoadDeuteriumData.R                  | Load data used in this study |
| 02_LoadSpatialData.R                    | Load spatial data used in subsequent analyses (e.g., isoscapes, GADM boundaries, IUCN rangemaps) |
| 03_getIsoscapeValuesAtSampleSites.R     | Prepare to fit transfer functions |
| 04_FitApplyTransferFunctions.R          | Fit transfer functions |
| 05_MakeProbabilityOfOriginMaps.R        | Generate proobability of origin maps |
| 06_MakeConvertSampleSiteCoordinateDF.R  | Prepare for clustering (script 7) |
| 07_ComparePoOMaps.R                     | Clustering to group individuals by similar probability of origin (PoO) maps |
| 08_MetricsConnectingPoOToSampleSite.R   | Estimate distance and direction of travel metrics for each individual|
| 09_glm_sex.R                            | Model fitting for sex identification |
| 10_glm_distTraveled.R                   | Model fitting for distance traveled |
| 11_glm_direction.R                      | Model fitting for direction of travel |
| 12_plotDistanceAndDirectionTogether.R   | Plotting model outputs from distance and direction of travel analyses (Figure 3, S3) |
| 13_plotd2HHistogram.R                   | Plot histogram of adjusted d2H values (Figure S1) |
| 14_plotSampleInventoryMap.R             | Plot sample acquisitions (Figure 2) |
| 15_TableSummarizeTransferFunctions.R    | Generate data for table summarizing transfer functions |
| 16_plotClusterOutputs.R                 | Make plots of trees and clusters with sample sizes (Figure S2) |
| 17_plotIsoscapeGlobe.R                  | Plot isoscape on globe used in conceptual figure (Fig 1) |
