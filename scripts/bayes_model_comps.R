################################################################################
# ROCKFISH HARVEST AND RELEASE ESTIMATION WITH REIMER / BAYESIAN METHODS
#
# This code allows for comparisons between contemporary models (> 2019) and 
# historical reconstruction models
#
# Developed by  Phil Joy (philip.joy@alaska.gov)
#
# Lats updated: July 2026
#
################################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(jagsUI)
library(rjags)
library(scales)

#functions, including data and parameter specifications
source(".\\scripts//bayes_data_param_load.R")
source(".\\scripts//functions.R")

#year to run model through

end_yr <- 2024
start_yr <- 1977

#most recent Howard estimates: 
REP_YR <- 2024 #for bringing in Howard estimats

#load the data:
list2env(readinData_alt(spl_knts = 7,
                        start_yr = 1977,
                        end_yr = end_yr,
                        SE06 = "exclude"), #SE06 = "exclude"
         .GlobalEnv)

#load parameters
params <- jags_params()

#area codes
area_codes <- comp %>% select(area,area_n) %>% unique() %>%
  add_row(area = "BSAI", area_n = 5) %>%
  add_row(area = "SOKO2SAP", area_n = 6) %>%
  add_row(area = "WKMA", area_n = 7) %>%
  mutate(area_n = as.character(area_n)) %>% arrange(as.numeric(area_n))

set.seed(8645)

histresults <- "Gen4int_indcomp_swhsR_FULL_pHB4pars_re0full_altwt_2xcvSEo_finaltuning7_thru2024_2e+06_SE06ex_2026-06-29"

#contresults1 <- "annual_est_take5_thru2024_1e+05__2026-07-07"
contresults2 <- "annual_est_take5.1.1_thru2024_10000_tst_2026-07-07"
contresults3 <- "annual_est_take5.1.1.a_thru2024_30000_tst_2026-07-08"
contresults4 <- "annual_est_take5.1.1.a2_thru2024_1e+05_2026-07-07"
contresults5 <- "annual_est_take5.1.1.a_simpB4pH_thru2024_40000_tst_2026-07-08"
contresults6 <- "annual_est_take5.1.1.c_fixH_thru2024_20000_tst_2026-07-08"

Historical <- readRDS(paste0(".\\output\\bayes_posts\\",histresults,".rds"))

#Contemporary_a1 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults1,".rds"))
Contemporary_a2 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults2,".rds"))
Contemporary_a3 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults3,".rds"))
Contemporary_a4 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults4,".rds"))
Contemporary_a5 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults5,".rds"))
Contemporary_a6 <- postH #readRDS(paste0(".\\output\\bayes_posts\\",contresults6,".rds"))

modlist <- list(Historical=Historical,
                #Contemporary_a1 = Contemporary_a1,
                take5.1.1 = Contemporary_a2,
                take5.1.1.a = Contemporary_a3,
                take5.1.1.a2 = Contemporary_a4,
                take5.1.1.a_simpB4pH = Contemporary_a5,
                take5.1.1.c_fixH = Contemporary_a6
                )

#-------------------------------------------------------------------------------
# Compare all harvests

results <- modelcomp(modlist,start_yr = 1977,Y=48)
#str(results)
#results$rf_plotdat

#names(wes_palettes)

pal <- c("black",wes_palette("Darjeeling1"))

results$rf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "guided",
         #model != "Contemporary_a1",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
#  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$rf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  geom_line() +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$brf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  geom_line() +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$ye_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$dsr_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$sl_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

results$brf_plotdat %>% filter(model == "Contemporary_a2",
                      year == "2020",
                      area == "CI")


pHpel_mod %>% filter(is.na(area))
unique(pHpel_mod$area)

results$pHpel_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year") + 
  ylim(0,1)

results$pHye_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")+ 
  ylim(0,1)

results$pHye_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "charter",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")+ 
  ylim(0,1)

pHye_mod %>% filter(model == "Contemporary_a2",
                       year == "2020",
                       area == "CI")


results$pHnpny_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")+ 
  ylim(0,1)

results$pHdsr_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
  year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area+user, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")+ 
  ylim(0,1)

results$pHslope_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
  year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area + user, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")+ 
  ylim(0,1)

p_pelagic
p_yellow
p_black
p_dsr
p_slope

results$p_pelagic %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = p_pelagic, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion pelagic", x = "Year") +
  ylim(0,1)

results$p_black %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = p_black, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion black", x = "Year")+
  ylim(0,1)

results$p_yellow %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "charter",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = p_yellow, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  #  facet_wrap(~user) + 
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion yelloweye", x = "Year")+
  ylim(0,1)

results$p_dsr %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = p_dsr, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion dsr", x = "Year")+
  ylim(0,1)

results$p_slope %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "charter",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = p_slope, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion slope", x = "Year")+
  ylim(0,1)

results$pG %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
#  filter(user == "private") %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = pG, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = pG_lo95, ymax = pG_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion guided", x = "Year")+
  ylim(0,1)

results$bias_ests %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(data == "H",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = bc, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = bc_lo95, ymax = bc_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

results$bias_ests %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(data == "R",
         year > 2010) %>%
  mutate(model = fct_relevel(model,"Historical",
                             unique(results$rf_plotdat$model[results$rf_plotdat$model != "Historical"]))) %>%
  ggplot(aes(x = year, y = bc, color = model, fill = model, shape = model)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line() + 
#  geom_ribbon(aes(x=year,ymin = bc_lo95, ymax = bc_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")



