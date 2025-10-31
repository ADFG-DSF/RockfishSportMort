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

wts1 <- read.csv("data/raw_dat/Species_Comp_SE/SE_RF_2006_2025_MHS_AWL_IntvID.csv") %>%
  clean_names() 

str(wts1)

wts1 %>%
  mutate(date = as.Date(date, format = "%d-%b-%y"),
         length = as.numeric(length),
         len_cm = as.numeric(len_cm),
         weight_kg = as.numeric(weight_kg)) -> wts1

unique(wts1$analysis_grp)

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

wts1 %>% filter(!is.na(weight_kg)) %>%
  mutate(gf_area = ifelse(gf_area %in% c("IBS","EYKT"),
                          "EWYKT",gf_area)) %>%
  group_by(year,analysis_grp,gf_area,user_group) %>%
  summarise(n_samps = n(),
            n_ints = n_distinct(interview_id),
            mean_wtkg = mean(weight_kg),
            sd_wt = sd(weight_kg),
            boot_sd = bootstrap_sd(weight_kg)) %>%
  mutate(samp_p_int = n_samps / n_ints) %>%
  filter(user_group %in% c("Charter","Private"),
         gf_area != "",
         gf_area != "UNKN") -> samps1

# Region 2 Data (weight in kgs)
wts2 <- read.csv("data/raw_dat/Species_Comp_SC/rf_mean_wt_SC.csv") %>%
  clean_names() %>%
  mutate(samp_p_int = n_fish / boats)

#Put Region 1 and 2 together
wts <- rbind(samps1 %>% ungroup() %>%
               mutate(spec = tolower(analysis_grp)) %>%
               select(year,user = user_group, cfmu = gf_area, 
                      spec, 
                      n_samps, n_ints, samp_p_int,
                      mean_wtkg, sd_wt, boot_sd),
             wts2 %>% filter(mean_wt != "") %>%
               mutate(spec = ifelse(sp == 145, "yelloweye",
                                    ifelse(sp == 142,"black","other")),
                      boot_sd = bootstrap_sd) %>%
               select(year, user, cfmu,spec,
                      n_samps = n_fish, n_ints = boats, samp_p_int,
                      mean_wtkg = mean_wt, sd_wt, boot_sd))

unique(wts$cfmu)
unique(wts$spec)

with(wts,table(year,cfmu))

ggplot(wts,# %>% #filter(analysis_grp == "BLACK"),
       aes(x= year,y = n_samps,col = spec)) +
  geom_line() +
  facet_wrap(~cfmu + user, scale = "free") +
  theme_bw()

ggplot(wts,# %>% #filter(analysis_grp == "BLACK"),
       aes(x= year,y = n_ints,col = spec)) +
  geom_line() +
  facet_wrap(~cfmu + user, scale = "free") +
  theme_bw()

ggplot(samps %>% filter(year > 2010),
       aes(x= year,y = samp_p_int,col = analysis_grp)) +
  geom_line() +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

# get mean sample sizes from 2011 onward 

wts %>% filter(year > 2010) %>% 
  group_by(spec,cfmu,user) %>%
  mutate(mean_n = mean(n_samps, na.rm = T),
         mean_int = mean(n_ints, na.rm = T),
         mean_spi = mean(samp_p_int, na.rm = T)) -> wtsX

wtsX %>% select(user, cfmu,spec,
                   mean_n,mean_int,mean_spi) %>%
  unique() -> int_sum

# Add the mean sample sizes to full data set and for pre2011 data we will 
# guestimate the number of interviews based on the mean number of samples
# per interview (mean_spi) in the post2010 data

full_join(wts,int_sum,
          by = c("user","cfmu","spec")) %>%
  mutate(n_ints2 = ifelse(year > 2010, n_ints,
                          n_samps / mean_spi)) -> wts

ggplot(wts, aes(n_ints2, colour = spec, fill = spec)) +
  geom_histogram() +
  facet_wrap(~cfmu + user, scale = "free") +
  geom_vline(xintercept = 5, color = "red") +
  theme_bw()
  
ggplot(wts, aes(n_samps, colour = spec, fill = spec)) +
  geom_histogram() +
  facet_wrap(~cfmu + user, scale = "free") +
  geom_vline(xintercept = 10, color = "red") +
  theme_bw()

ggplot(wts,aes(x = year, y = n_ints2, col = spec)) +
  #geom_line() +
  geom_point() +
  geom_smooth(size = 1, se = FALSE) +
  facet_wrap(~cfmu+user, scale = "free") + 
  theme_bw()

wts %>%
  group_by(user,cfmu,spec) %>%
  summarise(psamp_gt10 = sum(n_samps >= 10) / n(),
            pint_gt4 = sum(n_ints2 >= 4) / n(),
            pint_gt5 = sum(n_ints2 >= 5) / n()) ->ss_eval


ggplot(ss_eval, aes(x = cfmu, col = psamp_gt10, fill = psamp_gt10)) +
  geom_col(aes(y = psamp_gt10),
           position = "identity",
           alpha = 1) +
  facet_wrap(~spec + user, scale = "free") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(ss_eval, aes(x = cfmu, col = pint_gt4, fill = pint_gt4)) +
  geom_col(aes(y = pint_gt4),
           position = "identity",
           alpha = 1) +
  facet_wrap(~spec + user, scale = "free") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(ss_eval, aes(x = cfmu, col = pint_gt5, fill = pint_gt5)) +
  geom_col(aes(y = pint_gt5),
           position = "identity",
           alpha = 1) +
  facet_wrap(~spec + user, scale = "free") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90))

# Cutoff: 10 fish and 4 boats:

wts_for_mod <- wts %>%
  filter(!is.na(mean_wtkg),
         n_samps > 9,
         n_ints2 > 3)

unique(wts$cfmu)
unique(wts$spec)

with(wts_for_mod, table(year,cfmu))

ggplot(wts_for_mod %>% mutate(user_sp = paste0(spec," - ",user)) %>%
         filter(spec == "black"), 
       aes(x= year, y = mean_wtkg, col = user, fill = user)) +
  geom_ribbon(aes(ymin = mean_wtkg - 1.96 * boot_sd,
                  ymax = mean_wtkg + 1.96 * boot_sd),
              alpha = 0.2, color = NA) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean_wtkg - 1.96 * sd_wt,
                    ymax = mean_wtkg + 1.96 * sd_wt),
                alpha = 0.2) +
  facet_wrap(~cfmu) +
  xlim(2005,2025) + theme_bw() +
  ylim(0,4.2)

ggplot(wts_for_mod %>% mutate(user_sp = paste0(spec," - ",user)) %>%
         filter(spec == "yelloweye"), 
       aes(x= year, y = mean_wtkg, col = user, fill = user)) +
  geom_ribbon(aes(ymin = mean_wtkg - 1.96 * boot_sd,
                  ymax = mean_wtkg + 1.96 * boot_sd),
              alpha = 0.2, color = NA) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean_wtkg - 1.96 * sd_wt,
                    ymax = mean_wtkg + 1.96 * sd_wt),
                alpha = 0.2) +
  facet_wrap(~cfmu) +
  xlim(2005,2025) + theme_bw() +
  ylim(0,10)

ggplot(wts_for_mod %>% mutate(user_sp = paste0(spec," - ",user)) %>%
         filter(spec == "dsr_less_ye"), 
       aes(x= year, y = mean_wtkg, col = user, fill = user)) +
  geom_ribbon(aes(ymin = mean_wtkg - 1.96 * boot_sd,
                  ymax = mean_wtkg + 1.96 * boot_sd),
              alpha = 0.2, color = NA) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean_wtkg - 1.96 * sd_wt,
                    ymax = mean_wtkg + 1.96 * sd_wt),
                alpha = 0.2) +
  facet_wrap(~cfmu) +
  xlim(2005,2025) + theme_bw() +
  ylim(0,3)

ggplot(wts_for_mod %>% mutate(user_sp = paste0(spec," - ",user)) %>%
         filter(spec == "slope"), 
       aes(x= year, y = mean_wtkg, col = user, fill = user)) +
  geom_ribbon(aes(ymin = mean_wtkg - 1.96 * boot_sd,
                  ymax = mean_wtkg + 1.96 * boot_sd),
              alpha = 0.2, color = NA) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean_wtkg - 1.96 * sd_wt,
                    ymax = mean_wtkg + 1.96 * sd_wt),
                alpha = 0.2) +
  facet_wrap(~cfmu) +
  xlim(2005,2025) + theme_bw() +
  ylim(0,10)

ggplot(wts_for_mod %>% mutate(user_sp = paste0(spec," - ",user)) %>%
         filter(spec == "pelagic_less_blk"), 
       aes(x= year, y = mean_wtkg, col = user, fill = user)) +
  geom_ribbon(aes(ymin = mean_wtkg - 1.96 * boot_sd,
                  ymax = mean_wtkg + 1.96 * boot_sd),
              alpha = 0.2, color = NA) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean_wtkg - 1.96 * sd_wt,
                    ymax = mean_wtkg + 1.96 * sd_wt),
                alpha = 0.2) +
  facet_wrap(~cfmu) +
  xlim(2005,2025) + theme_bw() +
  ylim(0,3.5)


write.csv(wts_for_mod, "data/bayes_dat/wt_dat_processed.csv")



#------------------------------------------------------------------------------

ggplot(wts,
       aes(x= year,y = samp_p_int,col = spec)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_spi, col = spec), lty = 2) +
  facet_wrap(~cfmu + user, scale = "free") +
  theme_bw()

ggplot(int_sum,
       aes(x= year,y = n_ints,col = analysis_grp)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_int, col = analysis_grp), lty = 2) +
  facet_wrap(~gf_area + user_group, scale = "free") +
  theme_bw()

unique(samps$analysis_grp)

ggplot(samps %>% filter(!is.na(sd_weight),
                        !is.na(bootstrap_sd),
                        !is.na(mean_wt),
                        analysis_grp == "BLACK"),
       aes(x= year,y = mean_wt ,col = user_group, fill = user_group)) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * bootstrap_sd,
                  ymax = mean_wt + 1.96 * bootstrap_sd),
              colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * sd_weight ,
                  ymax = mean_wt + 1.96 * sd_weight ),
              colour = NA, alpha = 0.2) +
  geom_line() +
  facet_wrap(~gf_area) +
  theme_bw() #+ ylim(0,)


ggplot(samps %>% filter(!is.na(sd_weight),
                        !is.na(bootstrap_sd),
                        !is.na(mean_wt),
                        analysis_grp == "YELLOWEYE"),
       aes(x= year,y = mean_wt ,col = user_group, fill = user_group)) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * bootstrap_sd,
                  ymax = mean_wt + 1.96 * bootstrap_sd),
              colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * sd_weight ,
                  ymax = mean_wt + 1.96 * sd_weight ),
              colour = NA, alpha = 0.2) +
  geom_line() +
  facet_wrap(~gf_area) +
  theme_bw()

ggplot(samps %>% filter(!is.na(sd_weight),
                        !is.na(bootstrap_sd),
                        !is.na(mean_wt),
                        analysis_grp == "DSR_less_YE"),
       aes(x= year,y = mean_wt ,col = user_group, fill = user_group)) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * bootstrap_sd,
                  ymax = mean_wt + 1.96 * bootstrap_sd),
              colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * sd_weight ,
                  ymax = mean_wt + 1.96 * sd_weight ),
              colour = NA, alpha = 0.2) +
  geom_line() +
  facet_wrap(~gf_area) +
  theme_bw() + ylim(0,4)

ggplot(samps %>% filter(!is.na(sd_weight),
                        !is.na(bootstrap_sd),
                        !is.na(mean_wt),
                        analysis_grp == "Slope"),
       aes(x= year,y = mean_wt ,col = user_group, fill = user_group)) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * bootstrap_sd,
                  ymax = mean_wt + 1.96 * bootstrap_sd),
              colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = mean_wt - 1.96 * sd_weight ,
                  ymax = mean_wt + 1.96 * sd_weight ),
              colour = NA, alpha = 0.2) +
  geom_line() +
  facet_wrap(~gf_area) +
  theme_bw() + ylim(0,10)




