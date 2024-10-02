################################################################################
## Calculation of species apportionments for Southcentral region, including
## weighting of NG and PWS ports according to catches
##
## Author: Phil Joy
## Last updated: Oct. 2024
##
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
##    IPHC_YEAR_guipri_all_sentDAT.xlsx from Jake Bozzini
##
################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)

YEAR <- 2022

# Read in the processed general rf data processed thus far: 
new_H <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_harv_",YEAR,".csv"))
new_R <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_rel_",YEAR,".csv"))

#get SE port sampling data:
SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_Region1_forR_2023.FINAL.xlsx"), 
                     sheet = "Sheet1",
                     range = paste0("A1:AZ1000"), 
                     na = "NA")
SE_port <- SE_port[rowSums(is.na(SE_port)) != ncol(SE_port), ]

#Read in SC port sampling data: 
SC_port_gui <- read.csv("data/raw_dat/Species_comp_SC/Spcomp_guided_093024.csv")
SC_port_priv <- read.csv("data/raw_dat/Species_comp_SC/Spcomp_unguided_093024.csv")

#For Southcentral we need to weight the samples for PWS and NG respective to the landings:
# Port level data comes with the IPHC reports from Jake:
SWHS_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SC\\IPHC_2022_guipri_all_sent09282023.xlsx"), 
#SWHS_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SC\\IPHC_2023_guipri_all_sent20240924.xlsx"), 
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
print(rbind(port_priv,new_priv)) #actually bind these when you do a new year

#2022 only: port_priv <- unique(port_priv)


port_priv <- rbind(port_priv,new_priv)
#getting rid of this spreadsheat:
write.csv(port_priv,paste0("data/raw_dat/",YEAR,"/NGPWS_priv_harv_byport.csv"),row.names = F)

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
  mutate(VALDEZ = VALDEZ.CORD) %>% select(-VALDEZ.CORD) %>%
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
  mutate(VALDEZ = VALDEZ.CORD) %>% select(-VALDEZ.CORD) %>%
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

gui_wts <- read.csv(paste0("data/raw_dat/",YEAR-1,"/for_NGPWS_wts.csv")) 

gui_wts %>%
  mutate(Year = YEAR) %>% select(port_site, RptArea, total_rfharv) %>%
  pivot_wider(names_from = port_site,
              values_from = total_rfharv) %>%
  replace(is.na(.),0) %>%
  mutate(VALDEZ.CORDOVA = VALDEZ + CORDOVA,
         Year = YEAR) %>% 
  select(Year,CFMU = RptArea,Seward = SEWARD,Homer = HOMER,
         Whittier = WHITTIER,Valdez.Cordova= VALDEZ.CORDOVA) -> new_P
  

# track this guided harvest data ... 
port_gui <- read.csv(paste0("data/raw_dat/",YEAR-1,"/NGPWS_gui_harv_byport.csv"))
port_gui <- port_gui[,-1]

rbind(port_gui %>% mutate(orig = "orig"),
      new_P %>% mutate(orig = "update")) %>% arrange(Year,CFMU)

# Another error! Ignoring cordova in valdez cordova grouping... 
port_gui <- rbind(port_gui %>% mutate(orig = "orig") %>% filter(Year < YEAR),
                  new_P %>% mutate(orig = "update")) %>% arrange(Year,CFMU)

write.csv(port_gui,paste0("data/raw_dat/",YEAR,"/NGPWS_gui_harv_byport.csv"))

# NG Calcs ......
unique(SC_port_gui$PORT)
colnames(SC_port_gui)

left_join(SC_port_gui %>% filter(PORT == "Seward" & CFMU == "NG" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(S_Pel = sum(Pel),
                      S_NP = sum(NP),
                      S_BRF = sum(BRF),
                      S_YE = sum(YE)),
          ##FLAG!! Clay's '98-00 don't match spreadsheet. Suspect the spreadsheet is wrong
          ## since we're just updating values and not recalculating old estimates we'll move along
          SC_port_gui %>% filter(PORT == "Homer" & CFMU == "NG" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(H_Pel = sum(Pel),
                      H_NP = sum(NP),
                      H_BRF = sum(BRF),
                      H_YE = sum(YE)),
          by = "YEAR") %>%
  left_join(port_gui %>% mutate(YEAR = Year) %>% 
              select(-c(Year,orig)) %>% filter(CFMU=="NG"), by = "YEAR") %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  #replace(is.na(.),0) %>%
  mutate(pharv_sew = Seward / (Seward + Homer),
         pharv_hom = Homer / (Seward + Homer),
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
         p_BRFinNonpel = (w_BRF_sew + w_BRF_hom) / (w_Pel_sew + w_Pel_hom))  -> NG_gui_wts
View(NG_gui_wts)

#PWSI calcs .... 
left_join(SC_port_gui %>% filter(PORT == "Seward" & CFMU == "PWSI" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(S_Pel = sum(Pel),
                      S_NP = sum(NP),
                      S_BRF = sum(BRF),
                      S_YE = sum(YE)),
          SC_port_gui %>% filter(PORT == "Whittier" & CFMU == "PWSI" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(W_Pel = sum(Pel),
                      W_NP = sum(NP),
                      W_BRF = sum(BRF),
                      W_YE = sum(YE)),
          by = "YEAR") %>% 
  left_join(SC_port_gui %>% filter(PORT == "Valdez" & CFMU == "PWSI" & YEAR > 1997) %>%
              group_by(YEAR) %>% 
              summarize(VC_Pel = sum(Pel),
                        VC_NP = sum(NP),
                        VC_BRF = sum(BRF),
                        VC_YE = sum(YE)),
            by = "YEAR") %>% 
  left_join(port_gui %>% mutate(YEAR = Year) %>% 
              select(-c(Year,orig)) %>% filter(CFMU=="PWSI"), by = "YEAR") %>%
  #replace(is.na(.),0) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  #left_join(port_priv, by = "YEAR") %>%
  #mutate(Valdez = VALDEZ.CORD) %>% select(-VALDEZ.CORD) %>%
  mutate(pharv_sew = Seward / (Seward + Whittier + Valdez.Cordova),
         pharv_whi = Whittier / (Seward + Whittier + Valdez.Cordova),
         pharv_vc = Valdez.Cordova / (Seward + Whittier + Valdez.Cordova),
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
         p_BRFinNonpel = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_Pel_whi + w_Pel_vc)) -> PWSI_gui_wts
View(PWSI_gui_wts)

#PWSO---
left_join(SC_port_gui %>% filter(PORT == "Seward" & CFMU == "PWSO" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(S_Pel = sum(Pel),
                      S_NP = sum(NP),
                      S_BRF = sum(BRF),
                      S_YE = sum(YE)),
          SC_port_gui %>% filter(PORT == "Whittier" & CFMU == "PWSO" & YEAR > 1997) %>%
            group_by(YEAR) %>% 
            summarize(W_Pel = sum(Pel),
                      W_NP = sum(NP),
                      W_BRF = sum(BRF),
                      W_YE = sum(YE)),
          by = "YEAR") %>% 
  left_join(SC_port_gui %>% filter(PORT == "Valdez" & CFMU == "PWSO" & YEAR > 1997) %>%
              group_by(YEAR) %>% 
              summarize(VC_Pel = sum(Pel),
                        VC_NP = sum(NP),
                        VC_BRF = sum(BRF),
                        VC_YE = sum(YE)),
            by = "YEAR") %>% 
  left_join(port_gui %>% mutate(YEAR = Year) %>% 
              select(-c(Year,orig)) %>% filter(CFMU=="PWSO"), by = "YEAR") %>%
  #replace(is.na(.),0) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  #left_join(port_priv, by = "YEAR") %>%
  #mutate(VALDEZ = VALDEZ.CORD) %>% select(-VALDEZ.CORD) %>%
  mutate(pharv_sew = Seward / (Seward + Whittier + Valdez.Cordova),
         pharv_whi = Whittier / (Seward + Whittier + Valdez.Cordova),
         pharv_vc = Valdez.Cordova / (Seward + Whittier + Valdez.Cordova),
         psamp_sew = (S_Pel + S_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_whi = (W_Pel + W_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         psamp_vc = (VC_Pel + VC_NP) / (S_Pel + S_NP + W_Pel + W_NP + VC_Pel + VC_NP),
         w_sew = pharv_sew / psamp_sew,
         w_whi = pharv_whi / psamp_whi,
         w_vc = pharv_vc / psamp_vc,
         across(everything(), ~ ifelse(is.infinite(.), 0, .))) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  #replace(is.na(.),0) %>%
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
         p_BRFinNonpel = (w_BRF_sew + w_BRF_whi + w_BRF_vc) / (w_Pel_sew + w_Pel_whi + w_Pel_vc)) -> PWSO_gui_wts
View(PWSO_gui_wts)

#Bundle up the weighted estimates:
wted_ests <- rbind(NG_priv_wts %>% mutate(User = "private",
                                          RptArea = "NG",
                                          wt_Pel = S_Pel + H_Pel,
                                          wt_NP = S_NP + H_NP,
                                          wt_BRF = S_BRF + H_BRF,
                                          wt_YE = S_YE + H_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE),
                   PWSI_priv_wts %>% mutate(User = "private",
                                            RptArea = "PWSI",
                                            wt_Pel = S_Pel + W_Pel + VC_Pel,
                                            wt_NP = S_NP + W_NP + VC_NP,
                                            wt_BRF = S_BRF + W_BRF + VC_BRF,
                                            wt_YE = S_YE + W_YE + VC_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE),
                   PWSO_priv_wts %>% mutate(User = "private",
                                            RptArea = "PWSO",
                                            wt_Pel = S_Pel + W_Pel + VC_Pel,
                                            wt_NP = S_NP + W_NP + VC_NP,
                                            wt_BRF = S_BRF + W_BRF + VC_BRF,
                                            wt_YE = S_YE + W_YE + VC_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE),
                   NG_gui_wts %>% mutate(User = "charter",
                                         RptArea = "NG",
                                         wt_Pel = S_Pel + H_Pel,
                                         wt_NP = S_NP + H_NP,
                                         wt_BRF = S_BRF + H_BRF,
                                         wt_YE = S_YE + H_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE),
                   PWSI_gui_wts %>% mutate(User = "charter",
                                           RptArea = "PWSI",
                                           wt_Pel = S_Pel + W_Pel + VC_Pel,
                                           wt_NP = S_NP + W_NP + VC_NP,
                                           wt_BRF = S_BRF + W_BRF + VC_BRF,
                                           wt_YE = S_YE + W_YE + VC_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE),
                   PWSO_gui_wts %>% mutate(User = "charter",
                                           RptArea = "PWSO",
                                           wt_Pel = S_Pel + W_Pel + VC_Pel,
                                           wt_NP = S_NP + W_NP + VC_NP,
                                           wt_BRF = S_BRF + W_BRF + VC_BRF,
                                           wt_YE = S_YE + W_YE + VC_YE) %>%
                     select(Year = YEAR, User, RptArea,  
                            p_Pel, p_NP, p_YE, p_BRF, p_YEinNonpel, p_BRFinNonpel,
                            wt_Pel, wt_NP, wt_BRF, wt_YE)
)

#-------------------------------------------------------------------------------
# Now deal with the Kodiak areas: 
#unique(SC_port_priv$CFMU)

#SC_port_priv %>% filter(CFMU %in% c("WESTSIDE","AFOGNAK")) %>%
#  group_by(YEAR,CFMU,USER) %>%
#  summarize(Pel = sum(Pel),
#            NP = sum(NP),
#            BRF = sum(BRF),
#            YE = sum(YE))
#-------------------------------------------------------------------------------
# Put together species and assemblage data: 
SE_port
SC_port_gui
SC_port_priv

#Portion calculation already done for SE (Go Chris!)
#Need to do it for SC
library(boot)
bootstrap_variance <- function(data, R = 1000) {
  # Function to compute the mean for bootstrapping
  mean_function <- function(data, indices) {
    return(mean(data[indices], na.rm = TRUE))
  }
  
  # Perform bootstrapping
  bootstrap_results <- boot(data = data, statistic = mean_function, R = R)
  
  # Return the variance of the bootstrap estimates
  return(var(bootstrap_results$t))
}

unique(SC_port_priv$CFMU)

SC_port_priv %>% 
  mutate(RptArea = CFMU) %>%
  group_by(RptArea,YEAR) %>%
  summarize(Pel_n = sum(Pel),
            NP_n = sum(NP),
            BRF_n = sum(BRF),
            YE_n = sum(YE),
            TotalRF_n = Pel_n + NP_n) %>%
  mutate(User = "private",
         NotYE_Nonpel_n = NP_n - YE_n) %>%
  select(Year = YEAR, User , TotalRF_n,YE_n,BRF_n,Pel_n,NP_n, NotYE_Nonpel_n) %>%
  rbind(SC_port_gui %>% 
          mutate(RptArea = CFMU) %>%
          group_by(RptArea,YEAR) %>%
          summarize(Pel_n = sum(Pel),
                    NP_n = sum(NP),
                    BRF_n = sum(BRF),
                    YE_n = sum(YE),
                    TotalRF_n = Pel_n + NP_n) %>%
          mutate(User = "charter",
                 NotYE_Nonpel_n = NP_n - YE_n) %>%
          select(Year = YEAR, User , TotalRF_n,YE_n,BRF_n,Pel_n,NP_n, NotYE_Nonpel_n)) %>%
  #rename(CFMU = RptArea) %>%
  relocate(c(Year,User,RptArea), .before = everything())-> SC_step1
 
View(SC_step1)
## FLAG!!! Clay's number are not matching up with historical stuff for CI. Looks like
## Clay's code is under counting things!! 
## Code for other areas in the weighting apportionment above matched up though, so 
## maybe Clay is accidentally excluding something?
# Other things I'm noting: 
# Afognak priv and gui only has one year? 
# Eastside priv & gui is good
# NG priv is good
# NG charter fucked up 2021 and 2022, but matches the weighted samples so thats that

# Northeast priv and gui is missing almost entirely!
# Both PWS priv and gui are good

# Clay is out until the 30th so will code this to move it along, but looking at his 
# code I'm guessing that there are missing assignments that are not in there.
# Coding this for now but will need to rerun and check once we get the port sampling fixed

# Have him check versus raw samples tab in species_comp_Region2_forPWS_weighting

SC_step1 %>% group_by(RptArea) %>%
  mutate(pYE = YE_n / TotalRF_n,
         var_pYE = (pYE*(1-pYE))/(TotalRF_n-1),
         pYE_avgRptArea = mean(pYE[TotalRF_n > 50 &
                                     Year < 2020],na.rm=T),
         var_pYE_avgRptArea = ifelse(is.na(pYE_avgRptArea),NA,
                                     bootstrap_variance(pYE[TotalRF_n > 50 &
                                                              Year < 2020])), #) -> dev
         
         pBRF = BRF_n / TotalRF_n,
         var_pBRF = (pBRF*(1-pBRF))/(TotalRF_n-1),
         pBRF_avgRptArea = mean(pBRF[TotalRF_n > 50 &
                                      Year < 2020],na.rm=T),
         var_pBRF_avgRptArea = ifelse(is.na(pBRF_avgRptArea),NA,
                                      bootstrap_variance(pBRF[TotalRF_n > 50 &
                                                               Year < 2020])),
         
         pPel = Pel_n / TotalRF_n,
         var_pPel = (pPel*(1-pPel))/(TotalRF_n-1),
         pPel_avgRptArea = mean(pPel[TotalRF_n > 50 &
                                       Year < 2020],na.rm=T),
         var_pPel_avgRptArea = ifelse(is.na(pPel_avgRptArea),NA,
                                      bootstrap_variance(pPel[TotalRF_n > 50 &
                                                                Year < 2020])),
         
         pNonpel = NP_n / TotalRF_n,
         var_pNonpel = (pNonpel*(1-pNonpel))/(TotalRF_n-1),
         pNonpel_avgRptArea = mean(pNonpel[TotalRF_n > 50 &
                                          Year < 2020],na.rm=T),
         var_pNonpel_avgRptArea = ifelse(is.na(pNonpel_avgRptArea),NA,
                                         bootstrap_variance(pNonpel[TotalRF_n > 50 &
                                                                   Year < 2020])),
         
         pYEinNonpel = YE_n / NP_n,
         var_pYEinNonpel = (pYEinNonpel*(1-pYEinNonpel))/(NP_n-1),
         pYEinNonpel_avgRptArea = mean(pYEinNonpel[NP_n > 50 &
                                                 Year < 2020],na.rm=T),
         var_pYEinNonpel_avgRptArea = ifelse(is.na(pYEinNonpel_avgRptArea),NA,
                                             bootstrap_variance(pYEinNonpel[NP_n > 50 &
                                                                          Year < 2020])),
         
         pBRFinPel = BRF_n / Pel_n,
         var_pBRFinPel = (pBRFinPel*(1-pBRFinPel))/(Pel_n-1),
         pBRFinPel_avgRptArea = mean(pBRFinPel[Pel_n > 50 &
                                                   Year < 2020],na.rm=T),
         var_pBRFinPel_avgRptArea = ifelse(is.na(pBRFinPel_avgRptArea),NA,
                                           bootstrap_variance(pBRFinPel[Pel_n > 50 &
                                                                            Year < 2020]))) -> SC_port_raw

SC_port_raw
SE_port
#Step 2 is dealing with the weighted area proportions: 
# Here we'll substitute the proportional data from the weighted analysis above for
# NG, PWSI and PWSO
colnames(left_join(SC_port_raw,wted_ests,by = c("Year","RptArea","User")))

left_join(SC_port_raw,wted_ests,by = c("Year","RptArea","User")) %>%
  mutate(pYE = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                      p_YE,pYE),
         var_pYE = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                          (pYE*(1-pYE))/(TotalRF_n-1),var_pYE),

         pBRF = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                       p_BRF,pBRF),
         var_pBRF = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                           (pBRF*(1-pBRF))/(TotalRF_n-1),var_pBRF),
         
         pPel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                       p_Pel,pPel),
         var_pPel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                           (pPel*(1-pPel))/(TotalRF_n-1),var_pPel),
         
         pNonpel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                          p_NP,pNonpel),
         var_pNonpel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                              (pNonpel*(1-pNonpel))/(TotalRF_n-1),var_pNonpel),
         
         pYEinNonpel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                              p_YEinNonpel,pYEinNonpel),
         var_pYEinNonpel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                                  (pYEinNonpel*(1-pYEinNonpel))/(NP_n-1),var_pYEinNonpel),
         
         pBRFinPel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                            p_BRFinNonpel,pBRFinPel),
         var_pBRFinPel = ifelse(RptArea %in% c("NG","PWSI","PWSO"),
                                (pBRFinPel*(1-pBRFinPel))/(Pel_n-1),var_pBRFinPel),
         #need to update counts as well for Reimer model
         YE_n = ifelse(RptArea %in% c("NG","PWSI","PWSO"),wt_YE,YE_n),
         BRF_n = ifelse(RptArea %in% c("NG","PWSI","PWSO"),wt_BRF,BRF_n),
         Pel_n = ifelse(RptArea %in% c("NG","PWSI","PWSO"),wt_Pel,Pel_n),
         NP_n = ifelse(RptArea %in% c("NG","PWSI","PWSO"),wt_NP,NP_n),
         NotYE_Nonpel_n = ifelse(RptArea %in% c("NG","PWSI","PWSO"),(wt_NP - wt_YE),NotYE_Nonpel_n)) -> SC_port_fin

View(SC_port_fin)
# looks good,

# Here we would append this if necessary to the older data. 
# This should just be mass producible, but instructions were clear NOT to update
# old values, although documentation is scant on why.

# This is where we picked up from the spreadsheet copypaste extravaganza:
#lastyr <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SC\\Spcomp_for_PWS_weighting.xlsx"), 
#                    sheet = "species_comp_Region2_forR",
#                    range = paste0("A1:AG1000"), 
#                    na = "NA")
#lastyr <- lastyr[rowSums(is.na(lastyr)) != ncol(lastyr),]

lastyr <- read.csv(paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR-1,".csv"))

tosave <- SC_port_fin[,-c(34:43)]
names(tosave) <-names(lastyr)

#checking calcs
View(rbind(lastyr,tosave) %>% arrange(User, Rpt_Area, Year) %>% filter(Year > 2018) %>% data.frame())

Species_comp_Region2

final <- rbind(lastyr %>% filter(Year < YEAR),
               tosave %>% filter(Year == YEAR)) %>% arrange(User, Rpt_Area, Year) %>% data.frame()
View(final)

write.csv(final,paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR,".csv"),row.names=F)














