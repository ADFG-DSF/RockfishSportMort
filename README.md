<img align="left" src="https://github.com/commfish/cr_brf/blob/main/figures/SRIlogo.jfif" width="140">

# GOA Rockfish Sport Fish Harvest Reconstruction

### Author: Phil Joy (philip.joy@alaska.gov)

This repository houses the estimation procedure for harvest, release, and total sport fish mortality of rockfish in coastal Alaska developed as part of the [ADF&G Statewide Rockfish Initiative](https://stateofalaska.sharepoint.com/teams/DFGSPFStatewideRockfishInitiativeTeam). This is where the raw data is processed and formatted and houses the estimation proceedu. There are two methods/approaches for estimating rockfish harvest, release an mortality estimates. The first method is the Howard methods (Howard et al. 2020) which was originally conducted by running SAS code from Scott Meyers followed by calculations performed in a number of excel spreadsheets. Those methods have been converted to R format for reproducibility and to speed up the process. 

The second method is under development and comprises a Bayesian version of the Howard methods that allows for more appropriate and defensible sharing of information between areas, handles missing data in a more appropriate manor, accurately propogates uncertainty throughout the estimation proceedure and thus does not rely on the decision tree approach in the original Howard methods. Furthermore, the Bayesian approach should provide sport fish harvest, catch and mortality estimates back to 1978 when the SWHS was implements. The original Bayesian methods were developed by Adam Reimer and has been further developed by the curret author. For communciation purposes we will refer to Howard methods and Reimer methods to distinguish the two approaches. 

Data Sources:

1. SWHS
2. eLogbook 
3. Southcentral port sampling data
4. Southeast port sampling data

Steps for right now:

1. Process the statewide harvest survey data in swhs_processing.R
2. Process the logbook data in lb_processing.R
3. Weight and process the SC port sampling data in SC_apportionment_calcs.R
4. Generate estimates in BRF_Howard.R, YE_Howard.R, DSR_Howard.R, SLOPE_Howard.R, and PEL_Howard.R.

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
