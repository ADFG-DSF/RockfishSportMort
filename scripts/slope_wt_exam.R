##############################################################################
## Checking out some slope rf weight data...
##############################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(janitor)
library(readxl)
###############################################################################
REP_YR <- 2024
##

slope <- read.csv("data/raw_dat/Species_comp_SE/SE_Slope_RF_2006_2024_AWL_LOGBOOK_02OCT25.csv") %>%
  clean_names() %>%
  #mutate(across(c(1, 2, 4), ~ ifelse(!is.na(as.numeric(.)), as.numeric(.), .))) %>%
  mutate_at(vars(length, len_cm,weight_kg,wt_lbs), as.numeric) %>%
  filter(!is.na(weight_kg)) %>%
  mutate(user_group = tolower(user_group))

str(slope)
unique(slope$rf_grp)
unique(slope$analysis_grp)

rbind(slope %>% group_by(analysis_grp) %>%
        summarize(n = n(),
                  mean_wt = mean(weight_kg, na.rm = T),
                  median_wt = median(weight_kg, na.rm = T),
                  var_wt = var(weight_kg, na.rm =T)),
      slope %>% 
        summarize(n = n(),
                  mean_wt = mean(weight_kg, na.rm = T),
                  median_wt = median(weight_kg, na.rm = T),
                  var_wt = var(weight_kg, na.rm =T)) %>%
        mutate(analysis_grp = "all")) -> sumstats; sumstats



ggplot(slope,
       aes(weight_kg, col = analysis_grp, fill = analysis_grp)) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) + theme_bw() +
  geom_vline(data=sumstats, aes(xintercept = mean_wt),col = c("red","blue","black")) +
  geom_vline(data=sumstats, aes(xintercept = median_wt),col = c("red","blue","black"),
             linetype = 2)

library(boot)

bootstrap_stat <- function(data, indices, stat = "mean") {
  resampled_data <- data[indices] # Resample the data using the provided indices
  if (stat == "mean") {
    return(mean(resampled_data)) # Return the mean of the resampled data
  } else if (stat == "median") {
    return(median(resampled_data)) # Return the median of the resampled data
  } else {
    stop("Invalid statistic. Use 'mean' or 'median'.")
  }
}

unique(slope$analysis_grp)

# Perform bootstrapping for the mean
bmean_all <- boot(slope$weight_kg, statistic = function(data, indices) bootstrap_stat(data, indices, stat = "mean"), R = 1000)
bmean_big <- boot(slope$weight_kg[slope$analysis_grp == "Slope_LG_BOC_SR_RHE"], 
                  statistic = function(data, indices) bootstrap_stat(data, indices, stat = "mean"), R = 1000)
bmean_sm <- boot(slope$weight_kg[slope$analysis_grp == "Slope_SM"], 
                  statistic = function(data, indices) bootstrap_stat(data, indices, stat = "mean"), R = 1000)

# Perform bootstrapping for the median
bmed_all <- boot(slope$weight_kg, statistic = function(data, indices) bootstrap_stat(data, indices, stat = "median"), R = 1000)
bmed_big <- boot(slope$weight_kg[slope$analysis_grp == "Slope_LG_BOC_SR_RHE"],, 
                    statistic = function(data, indices) bootstrap_stat(data, indices, stat = "median"), R = 1000)
bmed_sm <- boot(slope$weight_kg[slope$analysis_grp == "Slope_SM"], 
                    statistic = function(data, indices) bootstrap_stat(data, indices, stat = "median"), R = 1000)

# Print results
cat("Bootstrap Mean:\n")
print(bootstrap_mean)

cat("\nBootstrap Median:\n")
print(bmed_all)

# Plot the bootstrap distributions
par(mfrow = c(3, 2)) # Set up a 1x2 plotting area
plot(bmean_all, main = "Bootstrap Mean")
plot(bmean_big, main = "Bootstrap Mean")
plot(bmean_sm, main = "Bootstrap Mean")
plot(bmed_all, main = "Bootstrap Median")
plot(bmed_big, main = "Bootstrap Median")
plot(bmed_sm, main = "Bootstrap Median")

#-------------------------------------------------------------------------------
  
#get relative number of slopes.
se_int <-  
  read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_MHS_Region1_forR_",REP_YR,"_RUN_30-Sep-2025.xlsx"), 
            range = c("A1:R1000"),
            sheet = "MHS num Fish")  %>%
  rename_all(.funs = tolower) %>%
  mutate(user = tolower(user)) %>%
  rename(area = rpt_area) %>% 
  #filter_all(any_vars(!is.na(.))) %>%
  select(-c("totalrf_n_rel","totalrf_n_res","totalrf_n_nonres")) %>%
  mutate_at(c("totalrf_n","ye_n","black_n","pelagic_n","nonpel_n",
              "notye_nonpel_n","dsr_n","slope_n",
              "pelnbrf_n","dsrnye_n","slope_lg_n","slope_sm_n"),as.numeric) %>%
  select(year,user,area,slope_n,slope_lg_n,slope_sm_n) %>%
  filter(!is.na(slope_n))

str(se_int)
ggplot(se_int ,
       aes(x = as.numeric(year), col = user, fill = user)) +
  geom_point(aes(y = slope_n)) +
  geom_line(aes(y = slope_n)) +
  #geom_col(aes(y = slope_lg_n)) +
  facet_wrap(~area, scale = "free")

slope_bd <- se_int %>%
  pivot_longer(cols = c(slope_n,slope_lg_n,slope_sm_n),
               names_to = "slope_cat",
               values_to = "n")

ggplot(slope_bd %>% filter(user == "charter" & slope_cat != "slope_n"),
       aes(x = as.numeric(year), y = n, col = slope_cat, fill = slope_cat)) +
  geom_col() +
#  geom_line(data = slope_bd %>% filter(slope_cat == "slope_n",
#                                       user == "charter"),
#            col = "black") +
  facet_wrap(~area, scale = "free")

ggplot(slope_bd %>% filter(user == "private" & slope_cat != "slope_n"),
       aes(x = as.numeric(year), y = n, col = slope_cat, fill = slope_cat)) +
  geom_col() +
#  geom_line(data = slope_bd %>% filter(slope_cat == "slope_n",
#                                       user == "private"),
#            col = "black") +
  facet_wrap(~area, scale = "free")

slope %>% filter(gf_area != "") %>%
  group_by(gf_area,year,user_group,analysis_grp) %>%
  summarize(n = n(),
            mean = mean(weight_kg),
            median = median(weight_kg)) %>%
  mutate(group = ifelse(analysis_grp == "Slope_SM","small","large")) %>%
  select(-analysis_grp) %>%
rbind(slope %>% filter(gf_area != "") %>%
        group_by(gf_area,year,user_group) %>%
          summarize(n = n(),
                    mean = mean(weight_kg),
                    median = median(weight_kg)) %>%
        mutate(group = "all")) %>%
  arrange(gf_area,user_group,year) %>% 
  pivot_wider(names_from = group,
              values_from = c(n,mean,median)) %>%
  rename(area = gf_area,
         user = user_group) %>%
  mutate(user = tolower(user)) -> wt_sum

#-------------------------------------------------------------------------------
# Need to see if the length/weight data proportions are the same as for the species
# comp samples
library(dplyr)

slope %>% group_by(year,gf_area,user_group,analysis_grp) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = analysis_grp,
              values_from = n) %>%
  rename(area = gf_area,
         user = user_group,
         small_wt_n = Slope_SM,
         big_wt_n = Slope_LG_BOC_SR_RHE) %>%
  filter(area != "") %>%
  full_join(se_int %>% mutate(year = as.integer(year)),
            by = c("year","area","user")) %>%
  pivot_longer(cols = c(small_wt_n, big_wt_n, slope_sm_n, slope_lg_n, slope_n),
               names_to = "variable", values_to = "value") %>%
  mutate(type = ifelse(grepl("wt", variable), "wts", "comp")) %>%
  pivot_wider(
    names_from = variable, values_from = value
  ) %>%
  mutate(
    n_small = ifelse(type == "wts", small_wt_n, slope_sm_n),
    n_big   = ifelse(type == "wts", big_wt_n, slope_lg_n),
    n_tot   = ifelse(type == "wts", small_wt_n + big_wt_n, slope_n)
  ) %>%
  select(year, area, user, type, n_small, n_big, n_tot) %>%
  mutate(prop_sm = n_small / n_tot,
         prop_lg = n_big / n_tot,
         lo95_sm = pmax(0,prop_sm - 1.96 * sqrt(prop_sm*(1-prop_sm)/(n_tot))),
         hi95_sm = pmin(1,prop_sm + 1.96 * sqrt(prop_sm*(1-prop_sm)/(n_tot))),
         lo95_lg = pmax(0,prop_lg - 1.96 * sqrt(prop_lg*(1-prop_lg)/(n_tot))),
         hi95_lg = pmin(1,prop_lg + 1.96 * sqrt(prop_lg*(1-prop_lg)/(n_tot)))) -> rep_ex
 
ggplot(rep_ex %>% filter(!is.na(prop_sm) & !area %in% c("EYKT") & user != "unknown"),
       aes(x =year, y = prop_sm, col = type, fill = type)) +
  geom_line() +
  facet_wrap(~area+user) +
  geom_ribbon(aes(ymin = lo95_sm, ymax = hi95_sm),alpha = 0.2, colour = NA) +
  theme_bw() +
  ggtitle("Comparing sample sizes for species comp and length (wts) sampled by year and area") +
  ylab("Proportion of slope rockfish that were small")

ggplot(rep_ex %>% filter(!is.na(prop_lg) & !area %in% c("EYKT") & user != "unknown"),
       aes(x =year, y = prop_lg, col = type, fill = type)) +
  geom_line() +
  facet_wrap(~area+user) +
  geom_ribbon(aes(ymin = lo95_lg, ymax = hi95_lg),alpha = 0.2, colour = NA) +
  theme_bw() +
  ggtitle("Comparing sample sizes for species comp and length (wts) sampled by year and area") +
  ylab("Proportion of slope rockfish that were large")


View(rep_ex)







