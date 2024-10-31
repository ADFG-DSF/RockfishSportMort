<img align="left" src="https://github.com/commfish/cr_brf/blob/main/figures/SRIlogo.jfif" width="140">

# Gulf of Alaska Rockfish Sport Fish Harvest Reconstruction

#### Author: Phil Joy (philip.joy@alaska.gov)

#### Last updated: Novemeber 2024

#### Local Directory
The local directory will contain files that are not available in the git repository:
* Juneau: Phil's H:\Documents\Rockfish_SF_moretality\
* Anchorage S:\RTS\FishSci1_groundfish\catch_accounting\

## About this repository

This repository houses the estimation procedure for harvest, release, and total sport fish mortality of rockfish in coastal Alaska developed as part of the [ADF&G Statewide Rockfish Initiative](https://stateofalaska.sharepoint.com/teams/DFGSPFStatewideRockfishInitiativeTeam). This is where the raw data is processed and formatted and houses the estimation proceedure. There are two methods/approaches for estimating rockfish harvest, release an mortality estimates. The method used to date (through 2023 estimates) has been the Howard method ([Howard et al. 2020](https://www.adfg.alaska.gov/FedAidPDFs/FDS20-25.pdf)) originally conducted by running SAS code followed by excel based calculations. Those methods were transitioned to R in 2024 for reproducibility and efficiency. One limitation of the Howard approach was a decision-tree process whereby data and information was borrowed from other areas when estimates were unavailable or unreliable due to limited sample sizes. A second limitation is that the proceedure could only produce estimate back through 1998 when the logbook program was initiated

The second method is under development and comprises a Bayesian version of the Howard methods, orginally recomended in Howard et al. (2020), that allows for more appropriate and defensible sharing of information between areas, handles missing data in a more appropriate manor, accurately propogates uncertainty throughout the estimation proceedure and thus does not rely on the decision tree approach in the original Howard methods. Furthermore, the Bayesian approach should provide sport fish harvest, catch and mortality estimates back to 1978 when the SWHS was implemented. The original Bayesian methods were developed by Adam Reimer and has been further developed by the curret author. For communciation purposes we will refer to Howard methods and Reimer methods to distinguish the two approaches. 

## Data reports for dissemination:

In development...

## Data Sources:

1. **SWHS data**: from Jake Bozzini (jake.bozzini@alaska.gov): Available in late September when SWHS estimates are finalized. Save to [`data/raw_dat/YEAR/`] folder.
   1. *rf_byMgmtUnit_sentDATE.xlsx*
   2. *IPHC_YEAR_guipri_all_sentYEAR.xlsx* (NOTE: This is currently in Southcentral port sample data folder and needs to be moved) 
3. **eLogbookdata**: *YEARLogbookDataDATE.csv* or *statewide_YEAR_DATA.csv* depending on what they are calling from Kyla Buster (kayla.buster@alaska.gov) and saved to [`data/raw_dat/YEAR/`] folder. 
4. **Southcentral port sampling data** from Clay Mckean (clay.mckean@alaska.gov) saved to [`data/raw_dat/Species_comp_SC/`]:
   1. *Spcomp_guided_SENTDATE*
   2. *Spcomp_unguided_SENTDATE*
6. **Southeast port sampling data** from Diana Tersteeg (diana.tersteeg@alaska.gov) and Chris Hinds (chris.hinds@alaska.gov) and saved to [`data/raw_dat/Species_comp_SE/`]:
   1. *Species_comp_Region1_forR_YEAR.Final*

## Estimation instructions

#### Due date: early October

### Work flow

1. Collect data from various sources.
   1. Species compostional data and logbook data are available anytime after the new year.
   2. Last year's SWHS data are available in late September of the following year. You should have all of the other data sources in place when the SWHS data is released so that estimates can be generated in the fall for Board of Fish meetings. 
3. Process the statewide harvest survey data in [`swhs_processing.R`].
4. Process the logbook data in [`lb_processing.R`].
5. Weight and process the SC port sampling data in [`SC_apportionment_calcs.R`].
   * Note that the southeast group has developed code for apportionments in Region 1.
6. Generate Howard estimates in [`BRF_Howard.R`], [`YE_Howard.R`], [`DSR_Howard.R`], [`SLOPE_Howard.R`], and [`PEL_Howard.R`].
   * Note that DSR, SLOPE and PEL are for Southeast Region only unless otherwise requested by Region 2. 
7. Prepare the data for the Bayesian model using [`scripts/bayes_data_prep.R`]. 
8. Generate Bayesian Reimer estimates in [`scripts/bayes_est.R`].

## Repository Directory

1. [`data/`]: Data folder:
   1. [`data/raw_dat/`]: Despite the name, this folder contains both the raw data as described above, and processed data and calculations generated during the course of the analysis. Files in these folders that are not included in the source list above are generated in the process of running the scripts. 
      1. [`data/raw_dat/YEAR/`]: The year folders will contain the raw SWHS and logbook data for the year as well as files that will be generated by the code each year. 
      2. [`data/raw_dat/Species_comp_SE/`]: This folder contains the updated port sampling data as generated by Chris and Diana each year. Chris has developed code to calculate the apportionment numbers so this file contains those results. 
      3. [`data/raw_dat/Species_comp_SC/`]: This folder contains the updated port sampling data as generated by Clay each year. Other files in this folder are generated when running the code [`SC_apportionment_calcs.R`] code.
      4. [`data/raw_dat/Archive/`]: This folder contains old data used prior to transitioning to R in 2024.
   2. [`data/bayes_dat/`]: This folder contains the processed data that is prepared for the Reimer model.
2. [`scripts/`]: Scripts for running the analysis
   * *Data processing*:
     1. [`scripts/swhs_processing.R`]: Processes and archives the swhs data.
     2. [`scripts/lb_processing.R`]: Processes and archives the logbook data and adds it to the swhs data.
     3. [`scripts/SC_apportionment_calcs.R`]: Apportions the raw Region 2 port sampling data to generate species apportionment estimates. Note that Region 1 began doing this task in house in 2024.
     4. [`scripts/bayes_data_prep.R`]: Prepares raw data for the Reimer model. 
   * *Howard estimates*: Produces rockfish harvest and release estimates using the Howard methods. 
     1. [`scripts/BRF_Howard.R`]: Black rockfish estimates.
     2. [`scripts/YE_Howard.R`]: Yelloweye rockfish estimates.
     3. [`scripts/DSR_Howard.R`]: Demersal shelf rockfish assemblage estimates (Southeast region only).
     4. [`scripts/SLOPE_Howard.R`]: Slope rockfish assemblage estimates (Southeast region only).
     5. [`scripts/PEL_Howard.R`]: Pelagic rockfish assemblage estimates (Southeast region only).
   * *Reimer estimates*: Produces rockfish harvest and release estimates using the Reimer methods. *Under development*
     1. [`scripts/bayes_est.R`]: This is the script for running the model
     2. [`scripts/functions.R`]: Functions for use in the bayes_est.R script. 
4. [`models/`]: This contains jags models for the Bayesian Reimer model.
5. [`figures/`]: This folder contains figures produced in the analysis.
6. [`output/`]: This folder contains files and results for dissemination.
   1. [`output/reports/`]: This folder contains area specific reports for dissemination.
7. [`markdown`]: This contains text files and rmarkdown files.



##### Old readme notes below while develoiping this repo:

Replicated an excel based version of the harvest reconstruction prepared by Katie Howard. Currently this repository contains a Bayesian version of the same estimates although the objective was to add earlier SWHS data to the analysis to extend the time series of estimates.

The working directory for this repository is located at S:\\RTS\\Reimer\\RockfishSportMort.

The file/folder structure for this analysis is as follows:

-   data/: R data sets created by readdata.R.

-   data-raw/: raw data for the analysis.

-   functions/: helper functions for the analysis.

-   markdown/: RMarkdown document  and html output associated with this analysis. 

-   models/: Jags code associated with this analysis.

-   posts/: Posterior samples from the model. These files are large and not available on GItHub (stored in the network location indicated above). Posterior samples from earlier commits follow the naming convention postH_{8digitsha}.rds

-   scripts/: R code to run analysis.
