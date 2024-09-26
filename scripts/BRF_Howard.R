################################################################################
# BLACK ROCKFISH HARVEST AND RELEASE CALCULATIONS
#
# Author: Phil Joy
# Last updated: October 2024
#
# This code replaces the BRF harvest and BRF release tabs in the excel files
# harvest estimates excel version_thruYEAR.xlsx and release estimates excel version_thruYEAR.xlsx
# files that used to get the cut and paste routine. 
# The code will pull those old files to update them but future updating will
# occur through the R files written here.
#
# Before running this it is necessary to incorporate the SWHS and logbook data and 
# get the port sampling species apportionment data from Chris Hinds (SE) and 
# Clay Mckean (SC)
#
# Precursor codes to run:
# swhw.processing.R
# lb_processing.R
# SC_apportionment_calcs.R
#
#-------------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)

YEAR <- 2022

# Read in the processed general rf data processed thus far: 
new_H <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_harv_",YEAR,".csv"))
new_R <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_rel_",YEAR,".csv"))
#temp patch
new_H <- new_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))
new_R <- new_R %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))

#read in logbook harvest and release estimate
LB_H <- read.csv(paste0("data/raw_dat/logbook_harvest_thru",YEAR,".csv"))
LB_R <- read.csv(paste0("data/raw_dat/logbook_release_thru",YEAR,".csv"))
LB_H<-LB_H[,-1] #get rid of this when the code is rerun clean
LB_R<-LB_R[,-1] #get rid of this when the code is rerun clean

#temp patch
LB_H <- LB_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))


#get SE port sampling data:
SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_Region1_forR_2023.FINAL.xlsx"), 
                     sheet = "Sheet1",
                     range = paste0("A1:AZ1000"), 
                     na = "NA")
SE_port <- SE_port[rowSums(is.na(SE_port)) != ncol(SE_port), ]

#get SC port sampling data:
SC_port <- read.csv(paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR,"_INCOMPLETE.csv"))
SC_port <- SC_port[,-1]
#combine for species comp estimates
colnames(SE_port); ncol(SE_port)
colnames(SC_port); ncol(SC_port)

setdiff(SC_port,SE_port[,-c(34:52)])

ncol(SE_port) - ncol(SC_port) -> dif

names(SC_port) <- names(SE_port)

new_columns <- setNames(as.list(rep(NA, dif)), paste0("new", 1:dif))

SC_port <- SC_port %>%
  mutate(!!!new_columns)
ncol(SE_port) - ncol(SC_port)

names(SC_port) <- names(SE_port)

spec_apor <- rbind(SE_port,SC_port) %>% 
  rename(RptArea = Rpt_Area) %>%
  mutate(User = as.factor(ifelse(User == "Charter","charter",
                       ifelse(User == "Private","private",User))),
         RptArea = as.factor(RptArea)) %>% 
  mutate_if(is.character, ~as.numeric(.))

str(spec_apor)

#get the last BRF run down: 
BRF_lastH <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\harvest estimates excel version_thru",YEAR,".xlsx"), 
                     sheet = "BRF harvest",
                     range = paste0("A2:Z1000"), 
                     na = "NA")
BRF_lastH <- BRF_lastH[rowSums(is.na(BRF_lastH)) != ncol(BRF_lastH), ]

BRF_lastR <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\release estimates excel version_thru",YEAR,".xlsx"), 
                       sheet = "BRF release",
                       range = paste0("A2:Z1000"), 
                       na = "NA")
BRF_lastR <- BRF_lastR[rowSums(is.na(BRF_lastR)) != ncol(BRF_lastR), ]

#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
colnames(spec_apor)

BRF_gui <- new_H %>%
  select(Region, year, RptArea,Log_rfharv) %>%
  left_join(LB_H %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,Gui_pelharv = pelagic_harv),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year)) %>%
              select(year,RptArea,
                     gui_pBRFinPel = pBRFinPel,
                     gui_var_pBRFinPel = var_pBRFinPel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pBRFinPel = as.numeric(gui_pBRFinPel),
         gui_var_pBRFinPel = as.numeric(gui_var_pBRFinPel),
         GuiBRF = Gui_pelharv * gui_pBRFinPel,
         var_GuiBRF = (Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiBRF = sqrt(var_GuiBRF),
         GuiBRF_UPRLWR95 = 1.96 * sqrt_GuiBRF)

BRF_pri <- new_H %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfharv,var_PRIV_rfharv) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pBRF = ifelse(TotalRF_n > 50,pBRF,pBRF_avgRptArea),
                     priv_var_pBRF = ifelse(TotalRF_n > 50,var_pBRF,var_pBRF_avgRptArea)) %>%
              select(year,RptArea,
                     priv_pBRF,
                     priv_var_pBRF),
            by = c("year", "RptArea")) %>%
  mutate(PRIV_BRF = PRIV_rfharv * priv_pBRF,
         var_PrivBRF =(PRIV_rfharv^2) * priv_var_pBRF + (priv_pBRF^2) * var_PRIV_rfharv - (priv_var_pBRF*var_PRIV_rfharv),
         sqrt_PrivBRF = sqrt(var_PrivBRF),
         PrivBRF_UPRLWR95 = 1.96 * sqrt_PrivBRF)

break_col <- as.data.frame(matrix(nrow=nrow(BRF_gui),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(BRF_gui),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

BRF_harvest <- cbind(BRF_gui,break_col,BRF_pri %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalBRFharv = GuiBRF + PRIV_BRF,
         var_totalBRFharv = var_GuiBRF + var_PrivBRF,
         sqrt_totalBRF = sqrt(var_totalBRFharv),
         TotalBRF_UPRLWR95 = 1.96 * sqrt_totalBRF)

# Add it onto the running sheet:
colnames(BRF_lastH) <- colnames(BRF_harvest)
BRF_lastH <- BRF_lastH %>% data.frame() %>% 
  mutate(RptArea = as.factor(RptArea),
         Region = as.factor(Region)) %>% 
  mutate_if(is.character, ~as.numeric(.))
BRF_lastH <- BRF_lastH[,-26]

BRF_lastH %>% filter(year == 2022 & Region == "SE")

BRF_harvest %>% filter(year == 2022 & Region == "SE")

updated_BRF_H <- rbind(BRF_lastH,BRF_harvest) %>% arrange(Region,RptArea,year)

updated_BRF_H %>% filter(year == 2022 & Region == "SE")

# *** FLAG for 9/26: Need to fix EWYKT to new data.. only thing missing! almost fucking the! 
#-------------------------------------------------------------------------------
# SCRAP
unique(BRF_harvest$RptArea)

view(spec_apor %>% filter(Year == 2022))

spec_apor %>% filter(User == "private" & Year == 2022) %>%
  mutate(year = as.integer(Year),
         priv_pBRF = ifelse(TotalRF_n > 50,pBRF,pBRF_avgRptArea),
         priv_var_pBRF = ifelse(TotalRF_n > 50,var_pBRF,var_pBRF_avgRptArea)) %>%
  select(year,RptArea,TotalRF_n,pBRF,var_pBRF,priv_pBRF,priv_var_pBRF) #%>% filter(year == 2022)

spec_apor %>% filter(Year == 2022 & User == "private")


str(spec_apor)
LB_H %>% filter(year == YEAR) %>%
  select(Region,year,RptArea,Gui_pelharv = pelagic_harv)

unique(spec_apor$User)

spec_apor %>% filter(User == "charter") %>%
  rename(year = Year) %>%
  select(year,RptArea,
         g_pBRFinPel = pBRFinPel,
         g_var_pBRFinPel = pBRFinPel)%>% filter(year == 2022)

View(spec_apor)

#%>%
  mutate(GuiBRF = Gui_pelharv * g_pBRFinPel,
         var_GuiBRF = (Gui_pelharv^2) * g_var_pBRFinPel,
         sqrt_GuiBRF = sqrt(var_GuiBRF),
         GuiBRF_UPRLWR95 = 1.96 * sqrt_GuiBRF)





























