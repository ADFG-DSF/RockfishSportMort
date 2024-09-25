################################################################################
## Howard et al. 2020 method for estimating Black Rockfish harvests and releases
##
## Author: Phil Joy
## Last updated: Oct. 2024
##
## This code estimates BRF harvests and releases using the Howard et al methods
##
## NEED TO PROCESS THIS FIRST:
## SWHW data processed in swhs_processing.R
## logbook data processed in lb_processing.R
##
## Species apportionment data from portside sampling
##    Southcentral: Clay McKean (clay.mckean)
##    Southeast: Chris Hinds (chris.hinds@alaska.gov) and Diana Tersteeg (diana.tersteeg@alaska.gov)
##
## Need port break down for PWS and NGC from SWHS 
##    IPHC_YEAR_guipri_all_sentDAT.xlsx
##
################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)

YEAR <- 2022

# Read in the processed general rf data processed thus far: 
new_H <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_harv_",YEAR,".csv"))
new_R <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_rel_",YEAR,".csv"))

#get SE port sampling data:
SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_Region1_forR_2023.FINAL.xlsx"), 
                     sheet = "Sheet1",
                     range = paste0("A1:AZ1000"), 
                     na = "NA")
SE_port <- SE_port[rowSums(is.na(SE_port)) != ncol(SE_port), ]

#Read in SC port sampling data: 
SC_port_gui <- read.csv("data/raw_dat/Species_comp_SC/Spcomp_guided.csv")
SC_port_priv <- read.csv("data/raw_dat/Species_comp_SC/Spcomp_unguided.csv")

#For Southcentral we need to weight the samples for PWS and NG respective to the landings:
# Port level data comes with the IPHC reports from Jake:
SWHS_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SC\\IPHC_2022_guipri_all_sent09282023.xlsx"), 
                       sheet = "Halibut Area Harvest",
                       range = paste0("A5:X41"), 
                       na = "NA") %>% clean_names()

#get running tally of this:
#port_priv <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SC\\Spcomp_for_PWS_weighting.xlsx"), 
#                     sheet = "R2 unguide wt multiport",
#                     range = paste0("A3:E28"), 
#                     na = "NA")
#getting rid of this spreadsheat:
port_priv <- read.csv(paste0("data/raw_dat/",YEAR-1,"/NGPWS_priv_harv_byport.csv"))

unique(SWHS_port$bottom_area)
new_priv <- c(YEAR,
                   as.numeric(SWHS_port %>% filter(bottom_area == "2 - Seward", type == "0-PUB") %>% select(rfpri)),
                   as.numeric(SWHS_port %>% filter(bottom_area == "2 - Whittier + W PWS", type == "0-PUB") %>% select(rfpri)),
                   as.numeric(SWHS_port %>% filter(bottom_area == "2 - Valdez + E PWS", type == "0-PUB") %>% select(rfpri)),
                   as.numeric(SWHS_port %>% filter(bottom_area == "3 - LCI", type == "0-PUB") %>% select(rfpri))) 
print(rbind(port_priv,new_priv), n = 30) #actually bind these when you do a new year
#getting rid of this spreadsheat:
write.csv(port_priv,paste0("data/raw_dat/",YEAR,"/NGPWS_priv_harv_byport.csv"))

# species comp for new year for those pain in the ass ports:
unique(SC_port_priv$PORT)
colnames(SC_port_priv)

left_join(SC_port_priv %>% filter(PORT == "Seward" & CFMU == "NG" & YEAR > 1997) %>%
        group_by(YEAR) %>% 
        summarize(S_Pel = sum(Pel),
                  S_NP = sum(NP),
                  S_BRF = sum(BRF),
                  S_YE = sum(YE)),
      SC_port_priv %>% filter(PORT == "Homer" & CFMU == "NG" & YEAR > 1997) %>%
        group_by(YEAR) %>% 
        summarize(H_Pel = sum(Pel),
                  H_NP = sum(NP),
                  H_BRF = sum(BRF),
                  H_YE = sum(YE)),
      by = "YEAR") %>% replace(is.na(.),0) %>%
  left_join(port_priv, by = "YEAR") %>%
  mutate(pharv_sew = SEWARD / (SEWARD + HOMER),
         pharv_hom = HOMER / (SEWARD + HOMER),
         psamp_sew = (S_Pel + S_NP) / (S_Pel + S_NP + H_Pel + H_NP),
         psamp_hom = (H_Pel + H_NP) / (S_Pel + S_NP + H_Pel + H_NP),
         w_sew = pharv_sew / psamp_sew,
         w_hom = pharv_hom / psamp_hom,
         w_Pel_sew = S_Pel * w_sew,
         w_NP_sew = S_NP * w_sew,
         w_BRF_sew = S_BRF * w_sew,
         w_YE_sew = S_YE * w_sew,
         w_Pel_hom = H_Pel * w_hom,
         w_NP_hom = H_NP * w_hom,
         w_BRF_hom = H_BRF * w_hom,
         w_YE_hom = H_YE * w_hom,
         p_Pel = (w_Pel_sew + w_Pel_hom) / (w_Pel_sew + w_NP_sew + w_Pel_hom + w_NP_hom),
         p_NP = (w_NP_sew + w_NP_hom) / (w_Pel_sew + w_NP_sew + w_Pel_hom + w_NP_hom),
         p_YE = (w_YE_sew + w_YE_hom) / (w_Pel_sew + w_NP_sew + w_Pel_hom + w_NP_hom),
         p_BRF = (w_BRF_sew + w_BRF_hom) / (w_Pel_sew + w_NP_sew + w_Pel_hom + w_NP_hom),
         p_YEinNonpel = (w_YE_sew + w_YE_hom) / (w_NP_sew + w_NP_hom),
         p_BRFinNonpel = (w_BRF_sew + w_BRF_hom) / (w_Pel_sew + w_Pel_hom)) -> NG_priv_wts
View(NG_priv_wts)

#---
left_join(SC_port_priv %>% filter(PORT == "Seward" & CFMU == "PWSI" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(S_Pel = sum(Pel),
                      S_NP = sum(NP),
                      S_BRF = sum(BRF),
                      S_YE = sum(YE)),
          SC_port_priv %>% filter(PORT == "Whittier" & CFMU == "PWSI" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(W_Pel = sum(Pel),
                      W_NP = sum(NP),
                      W_BRF = sum(BRF),
                      W_YE = sum(YE)),
          by = "YEAR") %>% 
  left_join(SC_port_priv %>% filter(PORT == "Valdez" & CFMU == "PWSI" & YEAR > 1997) %>%
              group_by(YEAR) %>% 
              summarize(VC_Pel = sum(Pel),
                        VC_NP = sum(NP),
                        VC_BRF = sum(BRF),
                        VC_YE = sum(YE)),
            by = "YEAR") %>% 
  replace(is.na(.),0) %>%
  left_join(port_priv, by = "YEAR") %>%
  mutate(VALDEZ = `VALDEZ/CORD`) %>% select(-`VALDEZ/CORD`) %>%
  mutate(pharv_sew = SEWARD / (SEWARD + WHITTIER + VALDEZ),
         pharv_whi = WHITTIER / (SEWARD + WHITTIER + VALDEZ),
         pharv_vc = VALDEZ / (SEWARD + WHITTIER + VALDEZ),
         psamp_sew = (S_Pel + S_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_whi = (W_Pel + W_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_vc = (VC_Pel + VC_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         w_sew = pharv_sew / psamp_sew,
         w_whi = pharv_whi / psamp_whi,
         w_vc = pharv_vc / psamp_vc,
         w_Pel_sew = S_Pel * w_sew,
         w_NP_sew = S_NP * w_sew,
         w_BRF_sew = S_BRF * w_sew,
         w_YE_sew = S_YE * w_sew,
         w_Pel_whi = W_Pel * w_whi,
         w_NP_whi = W_NP * w_whi,
         w_BRF_whi = W_BRF * w_whi,
         w_YE_whi = W_YE * w_whi,
         w_Pel_vc = VC_Pel * w_vc,
         w_NP_vc = VC_NP * w_vc,
         w_BRF_vc = VC_BRF * w_vc,
         w_YE_vc = VC_YE * w_vc,
         p_Pel = (w_Pel_sew + w_Pel_whi + w_Pel_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_NP = (w_NP_sew + w_NP_whi + w_NP_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_YE = (w_YE_sew + w_YE_whi + w_YE_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_BRF = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_YEinNonpel = (w_YE_sew + w_YE_whi + w_YE_vc) / (w_NP_sew + w_NP_whi + w_NP_vc),
         p_BRFinNonpel = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_Pel_whi + w_Pel_vc)) -> PWSI_priv_wts
View(PWSI_priv_wts)

#---
left_join(SC_port_priv %>% filter(PORT == "Seward" & CFMU == "PWSO" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(S_Pel = sum(Pel),
                      S_NP = sum(NP),
                      S_BRF = sum(BRF),
                      S_YE = sum(YE)),
          SC_port_priv %>% filter(PORT == "Whittier" & CFMU == "PWSO" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(W_Pel = sum(Pel),
                      W_NP = sum(NP),
                      W_BRF = sum(BRF),
                      W_YE = sum(YE)),
          by = "YEAR") %>% 
  left_join(SC_port_priv %>% filter(PORT == "Valdez" & CFMU == "PWSO" & YEAR > 1997) %>%
              group_by(YEAR) %>% 
              summarize(VC_Pel = sum(Pel),
                        VC_NP = sum(NP),
                        VC_BRF = sum(BRF),
                        VC_YE = sum(YE)),
            by = "YEAR") %>% 
  replace(is.na(.),0) %>%
  left_join(port_priv, by = "YEAR") %>%
  mutate(VALDEZ = `VALDEZ/CORD`) %>% select(-`VALDEZ/CORD`) %>%
  mutate(pharv_sew = SEWARD / (SEWARD + WHITTIER + VALDEZ),
         pharv_whi = WHITTIER / (SEWARD + WHITTIER + VALDEZ),
         pharv_vc = VALDEZ / (SEWARD + WHITTIER + VALDEZ),
         psamp_sew = (S_Pel + S_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_whi = (W_Pel + W_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_vc = (VC_Pel + VC_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         w_sew = pharv_sew / psamp_sew,
         w_whi = pharv_whi / psamp_whi,
         w_vc = pharv_vc / psamp_vc,
         across(everything(), ~ ifelse(is.infinite(.), 0, .))) %>%
  replace(is.na(.),0) %>%
  mutate(w_Pel_sew = S_Pel * w_sew,
         w_NP_sew = S_NP * w_sew,
         w_BRF_sew = S_BRF * w_sew,
         w_YE_sew = S_YE * w_sew,
         w_Pel_whi = W_Pel * w_whi,
         w_NP_whi = W_NP * w_whi,
         w_BRF_whi = W_BRF * w_whi,
         w_YE_whi = W_YE * w_whi,
         w_Pel_vc = VC_Pel * w_vc,
         w_NP_vc = VC_NP * w_vc,
         w_BRF_vc = VC_BRF * w_vc,
         w_YE_vc = VC_YE * w_vc,
         p_Pel = (w_Pel_sew + w_Pel_whi + w_Pel_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_NP = (w_NP_sew + w_NP_whi + w_NP_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_YE = (w_YE_sew + w_YE_whi + w_YE_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_BRF = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_NP_sew + w_Pel_whi + w_NP_whi + w_Pel_vc + w_NP_vc),
         p_YEinNonpel = (w_YE_sew + w_YE_whi + w_YE_vc) / (w_NP_sew + w_NP_whi + w_NP_vc),
         p_BRFinNonpel = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_Pel_whi + w_Pel_vc)) -> PWSO_priv_wts
View(PWSO_priv_wts)

#-------------------------------------------------------------------------------
# Guided weighted stuff: 
# output this stuff when we did the logbook run through

# we want: NG by Seward & Homer
#          PWSI and PWSO by Seward, Whittier and Valdez/Cordova

gui_wts <- read.csv(paste0("data/raw_dat/",YEAR,"/for_NGPWS_wts.csv")) 

gui_wts %>%
  mutate(Year = YEAR) %>% select(port_site, RptArea, total_rfharv) %>%
  pivot_wider(names_from = port_site,
              values_from = total_rfharv) %>%
  mutate(VALDEZCORDOVA = VALDEZ + CORDOVA)

# track this guided harvest data ... 

port_gui <- read.csv(paste0("data/raw_dat/",YEAR,"/NGPWS_gui_harv_byport.csv"))

port_gui %>% filter(Year == 2022)

# Another error! Ignoring cordova in valdez cordova grouping... 































