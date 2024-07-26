# GOA Rockfish Sport Fish Harvest Reconstruction

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
