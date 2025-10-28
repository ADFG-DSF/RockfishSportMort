################################################################################
# Weight Sample Size evaluation
################################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(janitor)
library(readxl)
###############################################################################
datSC <- read.csv("data/raw_dat/Species_comp_SC/sample_size_rf_SC_Port_Sampling.csv") %>%
  clean_names() %>% 
  rename(sp_grp = sp) %>%
  mutate(sp_grp = ifelse(sp_grp == 142,"Black",
                         ifelse(sp_grp == 145,"Yelloweye","IDK")),
         freq = NA)


datSE <- 
  read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\SE_2011_2025_number of vessels with sampled RF_19SEP25.xlsx"), 
            sheet = "By MHS Grouping",
            range = "A4:I974") %>% clean_names() %>%
  rename(cfmu = gf_area,
         user = classn,
         sp_grp = rf_mhs_grp) %>%
  select(-c(obs,type))

head(datSC)
head(datSE)

unique(datSC$sp_grp)
unique(datSE$sp_grp)

cfmus <- c("CI","NG","PWSI","PWSO",
           "WESTSIDE","AFOGNAK","EASTSIDE","NORTHEAST",
           "CSEO","EYKT","IBS","NSEI","NSEO","NSEO/CSEO","SSEI","SSEO")

dat <- rbind(datSC,datSE) %>%
  filter(!is.na(cfmu),
         !is.na(sp_grp)) %>%
  mutate(cfmu = factor(cfmu, 
                          levels = cfmus),
         smp_per_boat = sampled_fish / unique_boats)

range(dat$smp_per_boat)

dat %>% filter(smp_per_boat < 1)

ggplot(dat,aes(unique_boats,col = sp_grp, fill = sp_grp)) +
#  geom_histogram() +
  geom_density(alpha = 0.2) +
  facet_wrap(~cfmu, scale = "free") + 
  theme_bw()

ggplot(dat,aes(smp_per_boat,col = sp_grp, fill = sp_grp)) +
  #  geom_histogram() +
  geom_density(alpha = 0.2) +
  facet_wrap(~cfmu, scale = "free") + 
  theme_bw()

dat %>% filter(user != "Unknown") %>%
  group_by(cfmu,sp_grp,user) %>%
  summarize(min_boats = min(unique_boats),
            med_boats = median(unique_boats),
            mean_boats = mean(unique_boats),
            max_boats = max(unique_boats),
            prop_gte_10 = sum(unique_boats >= 10) / n(),
            prop_gte_5 = sum(unique_boats >= 5) / n(),
            prop_gte_3 = sum(unique_boats >= 3) / n()) -> boat_stats

bs <- boat_stats %>% select(-c(max_boats,prop_gte_5,prop_gte_3, prop_gte_10)) %>%
  pivot_longer(cols = c(min_boats,med_boats,mean_boats),
               names_to = "var",
               values_to = "stat") %>%
  mutate(var = factor(var,levels = c("mean_boats", "med_boats", "min_boats"))) %>%
  arrange(var)

ggplot(bs %>% filter(user != "Unknown"),
       aes(x = cfmu,col = var, fill = var)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  scale_color_manual(values = wes_palette("IsleofDogs2")) +
  geom_col(aes(y = stat),
           position = "identity",
           alpha = 1) +
  geom_hline(yintercept=5, col = "red", linetype = 2) +
  facet_wrap(~sp_grp+user, scale = "free") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("Minimum, median and mean number of boats sampled per year") +
  ylab("Minimum, median and mean number of boats sampled per year") +
  xlab("CFMU")

bs2 <- boat_stats %>% select(-c(max_boats,mean_boats,med_boats,min_boats)) %>%
  pivot_longer(cols = c(prop_gte_10, prop_gte_5,prop_gte_3),
               names_to = "proportion_gte",
               values_to = "stat") %>%
  mutate(proportion_gte = factor(proportion_gte,levels = c("prop_gte_3", "prop_gte_5", "prop_gte_10"))) %>%
  arrange(proportion_gte)

ggplot(bs2,aes(x = cfmu,col = proportion_gte, fill = proportion_gte)) +
  scale_fill_manual(values = wes_palette("Moonrise2")) +
  scale_color_manual(values = wes_palette("Moonrise2")) +
  geom_col(aes(y = stat),
           position = "identity",
           alpha = 1) +
  facet_wrap(~sp_grp+user, scale = "free") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("Proportion of years with at least 3 or 5 boats sampled for species") +
  ylab("Proportion of years with at least 3 or 5 boats sampled") +
  xlab("CFMU")


#-------------------------------------------------------------------------------
# Raw SE weights
bootstrap_ci <- function(x, n = 1000, conf = 0.95) {
  boot_means <- replicate(n, mean(sample(x, replace = TRUE)))
  lower <- quantile(boot_means, (1 - conf) / 2)
  upper <- quantile(boot_means, 1 - (1 - conf) / 2)
  c(mean = mean(x), lower = lower, upper = upper)
}

bootstrap_sd <- function(x, n_boot = 1000) {
  boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  sd(boot_means)
}

wts <- read.csv("data/raw_dat/Species_Comp_SE/SE_RF_2006_2025_MHS_AWL_IntvID.csv") %>%
  clean_names() 

str(wts)

wts %>%
  mutate(date = as.Date(date, format = "%d-%b-%y"),
         length = as.numeric(length),
         len_cm = as.numeric(len_cm),
         weight_kg = as.numeric(weight_kg)) -> wts

unique(wts$analysis_grp)

# Notes from Diana Tersteeg:
#Starting in 2017 with the database, we had a shiftid and interviewid variable which 
# identified individual shifts (a unique day worked by an individual technician at 
# a port and harbor) and individual interviewid (a unique boat interview during a shiftid).  

# To recreate this for earlier years, I concatenated the SHSITE for location, the 
# date, harbor, and sampler type (catch or creel).  For the Interview Id I concatenated 
# the new shiftID and Boat Number/interview number).  For 2006-2016 I was able to 
# recreate a unique Shift Id and for 2011-2016 I was able to create a unique 
# InterviewID (however there were 2 records in 2011 in Sitka, and 23 records in 
# 2011 in Ketchikan where there was not an interview number recorded – these were 
# all lumped into one interview for Sitka and two interviews for Ketchikan based 
# on day and harbor).

# 2017: interviewid
# 2011-2016 also interviewid
# 2006-2010: nada:

wts %>% filter(!is.na(weight_kg)) %>%
  group_by(year,analysis_grp,gf_area,user_group) %>%
  summarise(n_samps = n(),
            n_ints = n_distinct(interview_id),
            mean_wt = mean(weight_kg),
            sd_weight = sd(weight_kg),
            bootstrap_sd = bootstrap_sd(weight_kg)) %>%
  mutate(samp_p_int = n_samps / n_ints) %>%
  filter(user_group %in% c("Charter","Private"),
         gf_area != "",
         gf_area != "UNKN") -> samps

unique(samps$gf_area)

ggplot(samps,# %>% #filter(analysis_grp == "BLACK"),
       aes(x= year,y = n_samps,col = analysis_grp)) +
  geom_line() +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

ggplot(samps,# %>% #filter(analysis_grp == "BLACK"),
       aes(x= year,y = n_ints,col = analysis_grp)) +
  geom_line() +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

ggplot(samps %>% filter(year > 2010),
       aes(x= year,y = samp_p_int,col = analysis_grp)) +
  geom_line() +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

samps %>% filter(year > 2010) %>% 
  group_by(analysis_grp,gf_area,user_group) %>%
  mutate(mean_n = mean(n_samps),
         mean_int = mean(n_ints),
         mean_spi = mean(samp_p_int)) -> int_sum

ggplot(int_sum,
       aes(x= year,y = samp_p_int,col = analysis_grp)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_spi, col = analysis_grp), lty = 2) +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

ggplot(int_sum,
       aes(x= year,y = n_ints,col = analysis_grp)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_int, col = analysis_grp), lty = 2) +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()












