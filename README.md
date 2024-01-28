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

### /data/
Data used in this study.

| File / directory                        | Description |
| -----------                             | ----------- |
| assignR_d2h_world/isoscape.tif, assignR_d2h_world/se.tif | Isoscape accessed through the assignR R package; original source detailed at [wateriso.utah.edu](https://wateriso.utah.edu/waterisotopes/pages/data_access/ArcGrids.html); see Bowen, G.J. and Revenaugh, J., 2003. Interpolating the isotopic composition of modern meteoric precipitation. _Water Resources Research_, 39(10) and Bowen, G.J., Wassenaar, L.I. and Hobson, K.A., 2005. Global application of stable hydrogen and oxygen isotopes to wildlife forensics. _Oecologia_, 143, pp.337-348.|
| /66098_caitjcampbell_JJA_NoAm_Map_1980_2009/, /66100_caitjcampbell_Annual_NoAm_Map_H_1980_2010/ | Candidate isoscapes generated through [IsoMAP](https://isomap.rcac.purdue.edu/) |
| /Lit_data/Baerwald data.csv             | Published data from Baerwald, E. F., W. P. Patterson, and R. M. R. Barclay. Origins and migratory patterns of bats killed by wind turbines in southern Alberta: evidence from stable isotopes. _Ecosphere_ 5, no. 9 (2014): 1-17. |
| /Lit_data/cryan data.csv             | Published data from Cryan, P.M., Bogan, M.A., Rye, R.O., Landis, G.P. and Kester, C.L., 2004. Stable hydrogen isotope analysis of bat hair as evidence for seasonal molt and long-distance migration. _Journal of Mammalogy_, 85(5), pp.995-1001. |
| /Lit_data/Fraser data.csv            | Published data from Fraser, E.E., Brooks, D. and Longstaffe, F.J., 2017. Stable isotope investigation of the migratory behavior of silver-haired bats (_Lasionycteris noctivagans_) in eastern North America. _Journal of Mammalogy_, 98(5), pp.1225-1235. |
| /Lit_data/Pylant data.csv | Published data from Pylant, C.L., Nelson, D.M., Fitzpatrick, M.C., Gates, J.E. and Keller, S.R., 2016. Geographic origins and population genetics of bats killed at wind‐energy facilities. _Ecological Applications_, 26(5), pp.1381-1395. |
| /Lit_data/Pylant_LABO_data.csv | Published data from Pylant, C.L., Nelson, D.M. and Keller, S.R., 2014. Stable hydrogen isotopes record the summering grounds of eastern red bats (_Lasiurus borealis_). _PeerJ_, 2, p.e629.|
| CASIF_BatMigratoryStructure_dat.csv | New stable hydrogen isotope measurements generated for this study. |
| alldat.csv | Data used in this study after georeferencing, adjusting to unified reference scale |

### /bin/
Data products generated as part of this study. Shared here to increase reproducibility of scripts within the pipeline.
| Script                                  | Description |
| -----------                             | ----------- |
| Fraserdata-georeferenced.csv            | georeferenced data from Fraser, E.E., Brooks, D. and Longstaffe, F.J., 2017. Stable isotope investigation of the migratory behavior of silver-haired bats (_Lasionycteris noctivagans_) in eastern North America. _Journal of Mammalogy_, 98(5), pp.1225-1235. |
| NoAm_boundary_aea.rds | Summary object of the land mass of North America, from [gadm.org](https://gadm.org/data.html). |
| my_isoscapes.RData | Candidate isoscapes used in this study, cropped to study extent |
| range_raster.Rdata | Species ranges from IUCN used in this study, from IUCN Bat Specialist Team. |
