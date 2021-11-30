This repository contains all necessary scripts and functions to reproduce the analyses in the manuscript entitled "Exposure to Unconventional Oil and Gas Development and All-cause Mortality in Medicare Beneficiaries" by Longxiang Li, Francesca Dominici, Annelise J. Blomberg, Falco J. Bargagli-Stoffi, Joel D. Schwartz, Brent A. Coull, John D. Spengler, Yaguang Wei, Joy Lawrence, Petros Koutrakis. 

Code:
- 01_Handle_Raw_Well_Data.R contains code to extract and filter the raw well production data downloaded from Enverus. The county-specific files were previously downloaded via the Direct Access platform. The script aggregates, filters and cleans the raw data for follow-up analysis. Please contact the corresponding author if you have difficulties in downloading the files. 
- 02_Predict_Unknown_Drilling_Type.R contains function to impute the drilling type information for wells without valid drilling type. The drilling type information is not available for each well. We fit a random forest model, primarily using the drilling type of nearby wells to impute the missing record.
- 03_Exposure_Assessment.R contains functions to estimate PE/DE defined in the manuscript. Please note that, this code has been adjusted for the Harvard-based research computing environment and may need to be modified to run of different computing environments. The PE is a first-order inverse distance weighted and the DE is the contribution of PE by upwind wells.
- 04_Merge_Exposure_Data.R contains scripts to merge all exposure data (including PM2.5, temperature) together.
- 05_Prepare_Cox_Dataset.R contains code to prepare the exposure outcome data specifically for the Cox Proportional Hazard Models (Analysis Set I).
- 06_Full_Cox_Model.R contains the code to perform Model I in Analysis Set I.
- 07_Cox_Model_Wind.R contains the code to perform Model II in Analysis Set I.
- 08_Dif_in_Dif_Mort.R contains the code to perform the Difference-in-Differences (DiD) and the Difference-in-Difference-in-Differences (DDD) analyses in Analysis Set II.

Three scripts beginning with 00_ are functions that are called in the following analysis thus cannot run independently.
- 00_Function_Calculate_Area_Based_Density.R contains a function to calculate the population weighted PE and DE for each ZIP Code. It is called in 03_Exposure_Assessment.R.
- 00_Function_Get_Neghbour_Features.R contains the definition of a function to extract the drilling type information from nearby wells. It is called in 02_Predict_Unknown_Drilling_Type.R
- 00_Function_Prepare_Wind.R contains the definition of a function to extract the wind direction & velocity data from NARR data. It is called in 03_Exposure_Assessment.R

Data:
- UOGD Data: We obtained UOGD data from Enverus (https://app.drillinginfo.com/production/#/default) via its built-in API (Direct Access). A subscription is needed to download the data used in our study.
- Medicare Data: We purchased Medicare Plan A data from Centers of Medicare and Medicaid (https://www.cms.gov/).

Contact:
- If you have any question regarding the code, please send an email to the corresponding author Longxiang Li (lol087@mail.harvard.edu).

Term of Use:
- Authors/funders retain copyright (where applicable) of code on this Github repo and the article in Nature Energy. Anyone who wishes to share, reuse, remix, or adapt this material must obtain permission from the corresponding author. By using the contents on this Github repo and the article, you agree to cite:

*** [To be updated]

This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Exposure to Unconventional Oil and Gas Development and All-cause Mortality in Medicare Beneficiaries." The analyses rely upon subscription-only and publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.
