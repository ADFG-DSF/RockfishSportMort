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
# FLAG FLAG FLAG FLAG!!! Variance for var_PrivSPECIES in spreadsheets is WRONG!!!!
# Spreadsheets fixed 9-26-24. Minimal difference in results, but all previous
# estimates of harvest and releases underestimated the variance
# 
# Note2: In the end all of the variance is incorrect and these calculations should
# have been done in log space.
#-------------------------------------------------------------------------------
library(xlsx)
library(writexl)
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)
library(openxlsx)
library(dplyr)

library(stringr)

YEAR <- 2024

# Read in the processed general rf data processed thus far: 
new_H <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_harv_",YEAR,".csv"))
new_R <- read.csv(paste0("data/raw_dat/",YEAR,"/SWHS_LB_rel_",YEAR,".csv"))
#temp patch
#new_H <- new_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))
#new_R <- new_R %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))

#read in logbook harvest and release estimate
LB_H <- read.csv(paste0("data/raw_dat/logbook_harvest_thru",YEAR,".csv"))
LB_R <- read.csv(paste0("data/raw_dat/logbook_release_thru",YEAR,".csv"))

#temp patch
LB_H %>% filter(RptArea == "EWYKT" & Region == "SC")
LB_H <- LB_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))

LB_H %>% filter(year == YEAR)
#get SE port sampling data:
#SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_Region1_forR_2023.FINAL.xlsx"), 
SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024.xlsx"), 
                     #sheet = "Sheet1", 2023
                     sheet = "MHS num Fish", #2024; different format
                     range = paste0("A1:AZ1000"), 
                     na = "NA")
SE_port <- SE_port[rowSums(is.na(SE_port)) != ncol(SE_port), ]

#get SC port sampling data:
SC_port <- read.csv(paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR,".csv"))
SC_port %>% filter(Year == YEAR)

#combine for species comp estimates
colnames(SE_port); ncol(SE_port)
colnames(SC_port); ncol(SC_port)

# need to add extra columns to SC to facilitate combining the two regions for analysis
# in 2024 SE changed some of their column names. Fuckinghell.
SE_port <- SE_port %>%
  rename_with(~ str_replace_all(.x, "ave", "avg"))

colnames(SE_port); ncol(SE_port)
colnames(SC_port); ncol(SC_port)

setdiff(colnames(SE_port),colnames(SC_port))
setdiff(colnames(SC_port),colnames(SE_port))
#OK, one of SC names is off. Motherf#$%r
SC_port %>% rename(var_pBRFinPel = varBRFinPel) -> SC_port

setdiff(colnames(SE_port),colnames(SC_port))
setdiff(colnames(SC_port),colnames(SE_port))

se_columns <- setdiff(colnames(SE_port),colnames(SC_port))

dif <- ncol(SE_port)-ncol(SC_port)
#new_columns <- setNames(as.list(rep(NA, dif)), paste0("new", 1:dif))
new_columns <- setNames(as.list(rep(NA, dif)), se_columns)

SC_port <- SC_port %>%
  mutate(!!!new_columns)
ncol(SE_port) - ncol(SC_port)

setdiff(colnames(SE_port),colnames(SC_port))
setdiff(colnames(SC_port),colnames(SE_port))

spec_apor <- rbind(SE_port,SC_port) %>% 
  rename(RptArea = Rpt_Area) %>%
  mutate(User = as.factor(ifelse(User == "Charter","charter",
                       ifelse(User == "Private","private",User))),
         RptArea = as.factor(RptArea)) %>% 
  #mutate(RptArea = as.factor(ifelse(RptArea %in% c("WESTSIDE"),"WKMA",RptArea))) %>%
  mutate_if(is.character, ~as.numeric(.)) 


# Before we get going we need to deal with the Kodiak decision tree
# Eastside gui_pBRFinPel = average or raw depending on sample size
# Eastside Priv_pBRF = Northeast pBRF
# Afognak gui_pBRFinPel = Northeast pBRFinPel
# Afognak priv_pBRF = Northeast pBRF
# WKMA gui_pBRFinPel = Afognak gui_pBRFinPel
# WKMA Priv_pBRF = Afognak privBRF
# SKMA gui_pBRFinPel = Easside pBRFinPel
# SKMA priv_pBRF = Eastside priv_pBRF

spec_apor <- spec_apor %>%
  mutate(RptArea = case_when(
    RptArea == "WESTSIDE" ~ "WKMA",
    TRUE ~ as.character(RptArea)  # Keep other values as characters
  )) %>%
  mutate(RptArea = factor(RptArea))

spec_apor %>% data.frame()

left_join(spec_apor,
          spec_apor %>% filter(Year == 2019) %>% 
            select(RptArea,User,
                   use_pBRF_aRA = pBRF_avgRptArea,
                   use_var_pBRF_aRA = var_pBRF_avgRptArea,
                   use_pBRFinPel_aRA = pBRFinPel_avgRptArea,
                   use_var_pBRFinPel_aRA = var_pBRFinPel_avgRptArea),
          by = c("RptArea","User")) -> spec_apor

View(spec_apor %>% filter(Year == YEAR) %>% data.frame())

# look at port sample sizes for Kodiak in new year:
Kod <- spec_apor %>% filter(Year == YEAR,
                            RptArea %in% c("EASTSIDE","AFOGNAK","WKMA","NORTHEAST","SKMA")) %>% 
  droplevels() %>%
  select(User,RptArea,TotalRF_n,Pelagic_n)

Kod #Do substitutions for sample sizes < 50

spec_apor <- spec_apor %>% filter(Year == YEAR) %>%
  mutate(use_pBRF_aRA = ifelse(RptArea %in% c("EASTSIDE","AFOGNAK","WKMA"),
                               pull(spec_apor[spec_apor$RptArea == "NORTHEAST" & 
                                                spec_apor$Year == YEAR &
                                                spec_apor$User == "private", "pBRF"]),
                               ifelse(RptArea == "SKMA",
                                      pull(spec_apor[spec_apor$RptArea == "EASTSIDE" & 
                                                       spec_apor$Year == YEAR &
                                                       spec_apor$User == "private", "pBRF"]),use_pBRF_aRA)),
         use_var_pBRF_aRA = ifelse(RptArea %in% c("EASTSIDE","AFOGNAK","WKMA"),
                               pull(spec_apor[spec_apor$RptArea == "NORTHEAST" & 
                                                spec_apor$Year == YEAR &
                                                spec_apor$User == "private", "var_pBRF"]),
                               ifelse(RptArea == "SKMA",
                                      pull(spec_apor[spec_apor$RptArea == "EASTSIDE" & 
                                                       spec_apor$Year == YEAR &
                                                       spec_apor$User == "private", "var_pBRF"]),use_var_pBRF_aRA)),
         use_pBRFinPel_aRA = ifelse(RptArea %in% c("EASTSIDE","AFOGNAK","WKMA"),
                               pull(spec_apor[spec_apor$RptArea == "NORTHEAST" & 
                                                spec_apor$Year == YEAR &
                                                spec_apor$User == "charter", "pBRFinPel"]),
                               ifelse(RptArea == "SKMA",
                                      pull(spec_apor[spec_apor$RptArea == "EASTSIDE" & 
                                                       spec_apor$Year == YEAR &
                                                       spec_apor$User == "charter", "pBRFinPel"]),use_pBRFinPel_aRA)),
         use_var_pBRFinPel_aRA = ifelse(RptArea %in% c("EASTSIDE","AFOGNAK","WKMA"),
                                    pull(spec_apor[spec_apor$RptArea == "NORTHEAST" & 
                                                     spec_apor$Year == YEAR &
                                                     spec_apor$User == "charter", "var_pBRFinPel"]),
                                    ifelse(RptArea == "SKMA",
                                           pull(spec_apor[spec_apor$RptArea == "EASTSIDE", "var_pBRFinPel"]),use_var_pBRFinPel_aRA))) 

#Add in missing Kodiak areas:
with(spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE")), 
     table(User, RptArea))

spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),Year == YEAR) %>%
  select(User,RptArea,TotalRF_n,Pelagic_n,pBRF,var_pBRF,pBRFinPel,var_pBRFinPel,
         use_pBRF_aRA,use_var_pBRF_aRA,use_pBRFinPel_aRA,use_var_pBRFinPel_aRA)

spec_apor <- spec_apor %>% 
  add_row(Year = YEAR, RptArea = "SKMA", User = "charter",
          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
                                          spec_apor$User == "charter", "use_pBRF_aRA"]),
          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE"&
                                              spec_apor$User == "charter", "use_var_pBRF_aRA"]),
          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE"&
                                               spec_apor$User == "charter", "use_pBRFinPel_aRA"]),
          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE"&
                                                   spec_apor$User == "charter", "use_var_pBRFinPel_aRA"])) %>%
#  add_row(Year = YEAR, RptArea = "SKMA", User = "private",
#          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
#          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
#                                          spec_apor$User == "private", "use_pBRF_aRA"]),
#          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
#                                              spec_apor$User == "private", "use_var_pBRF_aRA"]),
#          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
#                                               spec_apor$User == "private", "use_pBRFinPel_aRA"]),
#          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
#                                                   spec_apor$User == "private", "use_var_pBRFinPel_aRA"])) %>%
#  add_row(Year = YEAR, RptArea = "WKMA", User = "private",
#          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
#          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                          spec_apor$User == "private", "pBRF"]),
#          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                              spec_apor$User == "private", "var_pBRF"]),
#          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                               spec_apor$User == "private", "pBRFinPel"]),
#          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                                   spec_apor$User == "private", "var_pBRFinPel"])) %>%
#  add_row(Year = YEAR, RptArea = "EASTSIDE", User = "private",
#          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
#          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                          spec_apor$User == "private", "pBRF"]),
#          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                              spec_apor$User == "private", "var_pBRF"]),
#          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                               spec_apor$User == "private", "pBRFinPel"]),
#          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                                   spec_apor$User == "private", "var_pBRFinPel"])) %>%
#  add_row(Year = YEAR, RptArea = "AFOGNAK", User = "private",
#          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
#          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                          spec_apor$User == "private", "pBRF"]),
#          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                              spec_apor$User == "private", "var_pBRF"]),
#          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                               spec_apor$User == "private", "pBRFinPel"]),
#          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "NORTHEAST" &
#                                                   spec_apor$User == "private", "var_pBRFinPel"])) %>%
  add_row(Year = YEAR, RptArea = "SKMA", User = "private",
          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0,
          use_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
                                          spec_apor$User == "charter", "use_pBRF_aRA"]),
          use_var_pBRF_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
                                              spec_apor$User == "charter", "use_var_pBRF_aRA"]),
          use_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
                                               spec_apor$User == "charter", "use_pBRFinPel_aRA"]),
          use_var_pBRFinPel_aRA = pull(spec_apor[spec_apor$RptArea == "EASTSIDE" &
                                                   spec_apor$User == "charter", "use_var_pBRFinPel_aRA"]))

#Check:
with(spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE")), 
     table(User, RptArea))

spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),Year == YEAR) %>%
  select(User,RptArea,TotalRF_n,Pelagic_n,pBRF,var_pBRF,pBRFinPel,var_pBRFinPel,
         use_pBRF_aRA,use_var_pBRF_aRA,use_pBRFinPel_aRA,use_var_pBRFinPel_aRA)
#-------------------------------------------------------------------------------
#get the last BRF run down: 
# 2024 coding starting with 2022 data using the old spreadsheets to compare and convert
#BRF_lastH <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR-1,"\\harvest estimates excel version_thru",YEAR,".xlsx"), 
#                     sheet = "BRF harvest",
#                     range = paste0("A2:Z1000"), 
#                     na = "NA")
#BRF_lastH <- BRF_lastH[rowSums(is.na(BRF_lastH)) != ncol(BRF_lastH), ]

#BRF_lastR <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR-1,"\\release estimates excel version_thru",YEAR,".xlsx"), 
#                       sheet = "BRF release",
#                       range = paste0("A2:Z1000"), 
#                       na = "NA")
#BRF_lastR <- BRF_lastR[rowSums(is.na(BRF_lastR)) != ncol(BRF_lastR), ]

# With 2023 and beyond you will pull and update the csv files created in this workflow:
BRF_lastH <- read.csv(paste0("output/BRF_harv_Howard_thru",YEAR-1,".csv"))
BRF_lastR <- read.csv(paste0("output/BRF_rel_Howard_thru",YEAR-1,".csv"))

#*** Bug in code with releases; RptArea got changed to numbers instead of names
#Temp fix here:
#BRF_lastR <- read_xlsx(paste0(".\\output\\release_estimates_Howard_thru",YEAR-1,".xlsx"), 
#                       sheet = "BRF release",
#                       range = paste0("A1:Y1000"), 
#                       na = "NA")
#BRF_lastR <- BRF_lastR[rowSums(is.na(BRF_lastR)) != ncol(BRF_lastR), ]
#YE sheet good so will use it to relabel RptArea
#YE_lastR <- read_xlsx(paste0(".\\output\\release_estimates_Howard_thru",YEAR-1,".xlsx"), 
#                       sheet = "YE release",
#                       range = paste0("A1:AB1000"), 
#                       na = "NA")
#YE_lastR <- YE_lastR[rowSums(is.na(YE_lastR)) != ncol(YE_lastR), ]

#match <- YE_lastR %>% select(Region,year,RptArea,Log_rfrel)

#BRF_lastR %>% mutate(RptArea_num = RptArea) %>% select(-RptArea) %>%
#  left_join(match, by = c("Region","year","Log_rfrel")) %>%
#  relocate(RptArea, .after = year) %>%
#  relocate(RptArea_num, .after = RptArea) -> try
#with(try,table(RptArea,RptArea_num))
#View(try)

#BRF_lastR <- try %>% select(-RptArea_num)
#---HARVESTS--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
spec_apor %>% filter(User == "private" & RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE")) %>% data.frame()

BRF_guiH <- new_H %>%
  select(Region, year, RptArea,Log_rfharv) %>%
  left_join(LB_H %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,Gui_pelharv = pelagic_harv),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pBRFinPel = ifelse(Pelagic_n < 50, use_pBRFinPel_aRA,pBRFinPel),
                     gui_var_pBRFinPel = ifelse(Pelagic_n < 50, use_var_pBRFinPel_aRA, var_pBRFinPel)) %>%
              select(year,RptArea,
                     gui_pBRFinPel,
                     gui_var_pBRFinPel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pBRFinPel = as.numeric(gui_pBRFinPel),
         gui_var_pBRFinPel = as.numeric(gui_var_pBRFinPel),
         GuiBRF = Gui_pelharv * gui_pBRFinPel,
         var_GuiBRF = (Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiBRF = sqrt(var_GuiBRF),
         GuiBRF_UPRLWR95 = 1.96 * sqrt_GuiBRF)

BRF_priH <- new_H %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfharv,var_PRIV_rfharv) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pBRF = ifelse(TotalRF_n > 50,pBRF,use_pBRF_aRA),
                     priv_var_pBRF = ifelse(TotalRF_n > 50,var_pBRF,use_var_pBRF_aRA)) %>%
              select(year,RptArea,
                     priv_pBRF,
                     priv_var_pBRF),
            by = c("year", "RptArea")) %>%
  mutate(PRIV_BRF = PRIV_rfharv * priv_pBRF,
         var_PrivBRF =(PRIV_rfharv^2) * priv_var_pBRF + (priv_pBRF^2) * var_PRIV_rfharv + (priv_var_pBRF*var_PRIV_rfharv),
         sqrt_PrivBRF = sqrt(var_PrivBRF),
         PrivBRF_UPRLWR95 = 1.96 * sqrt_PrivBRF)

break_col <- as.data.frame(matrix(nrow=nrow(BRF_guiH),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(BRF_guiH),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

BRF_harvest <- cbind(BRF_guiH,break_col,BRF_priH %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalBRFharv = GuiBRF + PRIV_BRF,
         var_totalBRFharv = var_GuiBRF + var_PrivBRF,
         sqrt_totalBRF = sqrt(var_totalBRFharv),
         TotalBRF_UPRLWR95 = 1.96 * sqrt_totalBRF)

# Add it onto the running sheet:
#BRF_lastH <- BRF_lastH[,-1] #get rid of this when doing 2024; should be saved without rownames now
# colnames(BRF_lastH) <- colnames(BRF_harvest) #these should already be the same for 2023 and beyond
#BRF_lastH <- BRF_lastH %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(BRF_lastH); ncol(BRF_harvest)
#BRF_lastH <- BRF_lastH[,-26]

updated_BRF_H <- rbind(BRF_lastH,BRF_harvest) %>% arrange(Region,RptArea,year)

updated_BRF_H %>% filter(year >= 2022 ) 
#checks out! just save one 2022 row
#updated_BRF_H <- rbind(BRF_lastH %>% filter(year < YEAR),
#                       BRF_harvest) %>% 
#  mutate(RptArea = case_when(
#    RptArea == "NORTHEAS" ~ "NORTHEAST",
#    TRUE ~ as.character(RptArea))) %>% 
#  arrange(Region,RptArea,year)

unique(updated_BRF_H$RptArea)

write.csv(updated_BRF_H, paste0("output/BRF_harv_Howard_thru",YEAR,".csv"),row.names = F)

unique(updated_BRF_H$RptArea)

# START EXCEL WORKBOOK OF RESULTS FOR MORTALITY AND BIOMASS ESTIMATION:
harv_est_xlsx <- createWorkbook()
addWorksheet(harv_est_xlsx, "BRF harvest")
writeData(harv_est_xlsx, "BRF harvest", updated_BRF_H)
saveWorkbook(harv_est_xlsx, paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#---RELEASES--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
colnames(spec_apor)

BRF_guiR <- new_R %>%
  select(Region, year, RptArea,Log_rfrel) %>%
  left_join(LB_R %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,Gui_pelrel = pelagic_rel),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pBRFinPel = ifelse(Pelagic_n < 50, use_pBRFinPel_aRA,pBRFinPel),
                     gui_var_pBRFinPel = ifelse(Pelagic_n < 50, use_var_pBRFinPel_aRA, var_pBRFinPel)) %>%
              select(year,RptArea,
                     gui_pBRFinPel,
                     gui_var_pBRFinPel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pBRFinPel = as.numeric(gui_pBRFinPel),
         gui_var_pBRFinPel = as.numeric(gui_var_pBRFinPel),
         GuiBRF = Gui_pelrel * gui_pBRFinPel,
         var_GuiBRF = (Gui_pelrel^2) * gui_var_pBRFinPel,
         sqrt_GuiBRF = sqrt(var_GuiBRF),
         GuiBRF_UPRLWR95 = 1.96 * sqrt_GuiBRF)

BRF_priR <- new_R %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfrel,var_PRIV_rfrel) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pBRF = ifelse(TotalRF_n > 50,pBRF,use_pBRF_aRA),
                     priv_var_pBRF = ifelse(TotalRF_n > 50,var_pBRF,use_var_pBRF_aRA)) %>%
              select(year,RptArea,
                     priv_pBRF,
                     priv_var_pBRF),
            by = c("year", "RptArea")) %>%
  mutate(PRIV_BRF = PRIV_rfrel * priv_pBRF,
         var_PrivBRF =(PRIV_rfrel^2) * priv_var_pBRF + (priv_pBRF^2) * var_PRIV_rfrel + (priv_var_pBRF*var_PRIV_rfrel),
         sqrt_PrivBRF = sqrt(var_PrivBRF),
         PrivBRF_UPRLWR95 = 1.96 * sqrt_PrivBRF)

BRF_release <- cbind(BRF_guiR,break_col,BRF_priR %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalBRFrel = GuiBRF + PRIV_BRF,
         var_totalBRFrel = var_GuiBRF + var_PrivBRF,
         sqrt_totalBRF = sqrt(var_totalBRFrel),
         TotalBRF_UPRLWR95 = 1.96 * sqrt_totalBRF)

# Add it onto the running sheet:
BRF_lastR <- BRF_lastR %>% select(-X)
#colnames(BRF_lastR) <- colnames(BRF_release)
#BRF_lastR <- BRF_lastR %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(BRF_lastR); ncol(BRF_release)
#BRF_lastR <- BRF_lastR[,-26]

updated_BRF_R <- rbind(BRF_lastR,BRF_release) %>% arrange(Region,RptArea,year)

updated_BRF_R %>% filter(year >= 2023)
# CSEO values diff between new R and old excel. Foud a copy-paste error in excel version:

#updated_BRF_R <- rbind(BRF_lastR %>% filter(year < YEAR),
#                       BRF_release) %>% 
#  mutate(RptArea = case_when(
#    RptArea == "NORTHEAS" ~ "NORTHEAST",
#    TRUE ~ as.character(RptArea))) %>%
#  arrange(Region,RptArea,year)

#unique(updated_BRF_R$RptArea)

write.csv(updated_BRF_R,paste0("output/BRF_rel_Howard_thru",YEAR,".csv"), row.names = F)

rel_est_xlsx <- createWorkbook()
sheet = addWorksheet(rel_est_xlsx, "BRF release")
writeData(rel_est_xlsx, "BRF release", updated_BRF_R)
saveWorkbook(rel_est_xlsx, paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#-------------------------------------------------------------------------------
# Summary and plots
# Harvest and release by year and user and CFMU / RptArea
str(updated_BRF_H)

unique(updated_BRF_H$RptArea)

updated_BRF_H %>% select(Region,RptArea,year,
                         Guided = GuiBRF, SE_Gui = sqrt_GuiBRF ,
                         Private = PRIV_BRF, SE_Priv = sqrt_PrivBRF,
                         Total = TotalBRFharv,SE_Tot = sqrt_totalBRF)->BRF_harv_table

Kodiak_H <- BRF_harv_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_H <- BRF_harv_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_H <- BRF_harv_table %>% filter(Region == "SE")

updated_BRF_R %>% select(Region,RptArea,year,
                         Guided = GuiBRF,SE_Gui = sqrt_GuiBRF ,
                         Private = PRIV_BRF,SE_Priv = sqrt_PrivBRF,
                         Total =TotalBRFrel,SE_Tot = sqrt_totalBRF)->BRF_rel_table

Kodiak_R <- BRF_rel_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_R <- BRF_rel_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_R <- BRF_rel_table %>% filter(Region == "SE")


BRF_harv_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Harvest"
  ) %>%
  left_join(BRF_harv_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
              pivot_longer(
                cols = c(SE_Gui,SE_Priv,SE_Tot),
                names_to = "User",
                values_to = "SE"
              ) %>% 
              mutate(User = ifelse(User == "SE_Gui","Guided",
                                   ifelse(User == "SE_Priv","Private","Total"))),
            by=c("year","Region","RptArea","User")) -> Hplot_dat

BRF_rel_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Releases"
  ) %>%
  left_join(BRF_rel_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
              pivot_longer(
                cols = c(SE_Gui,SE_Priv,SE_Tot),
                names_to = "User",
                values_to = "SE"
              ) %>% 
              mutate(User = ifelse(User == "SE_Gui","Guided",
                                   ifelse(User == "SE_Priv","Private","Total"))),
            by=c("year","Region","RptArea","User")) %>%
  mutate(hi95 = Releases + 1.96*SE,
         lo95 = ifelse(Releases - 1.96*SE < 1,
                       1,Releases - 1.96*SE))-> Rplot_dat

library(wesanderson)
names(wes_palettes)
cols <- wes_palette("AsteroidCity1")

ggplot(Hplot_dat %>% filter(Region == "SE")) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,ymin=Harvest-1.96*SE,ymax=Harvest+1.96*SE,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Harvest,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Harvest (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/SE_BRF_harv.png", width = 6, height = 4)

#Hplot_dat %>% filter(RptArea == "NORTHEAST")

ggplot(Hplot_dat %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                           "WKMA","SKMA"))) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,ymin=Harvest-1.96*SE,ymax=Harvest+1.96*SE,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Harvest,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Harvest (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/KOD_BRF_harv.png", width = 6, height = 4)

ggplot(Hplot_dat %>% filter(RptArea %in% c("CI","NG","PWSI",
                                           "PWSO"))) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,ymin=Harvest-1.96*SE,ymax=Harvest+1.96*SE,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Harvest,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Harvest (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/SC_BRF_harv.png", width = 6, height = 4)

ggplot(Rplot_dat %>% filter(Region == "SE")) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,
                  ymin=hi95,
                  ymax=lo95,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Releases,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Releases (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/SE_BRF_rel.png", width = 6, height = 4)

ggplot(Rplot_dat %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                           "WKMA","SKMA"))) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,ymin=lo95,ymax=hi95,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Releases,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Releases (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/KOD_BRF_rel.png", width = 6, height = 4)

ggplot(Rplot_dat %>% filter(RptArea %in% c("CI","NG","PWSI",
                                           "PWSO"))) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  geom_ribbon(aes(year,ymin=lo95,ymax=hi95,
                  fill = User),col=NA, alpha = 0.2) +
  geom_line(aes(year,Releases,col=User)) +
  facet_wrap(~RptArea) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Releases (numbers of fish)") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("figures/SC_BRF_rel.png", width = 6, height = 4)

Kodiak_rep <- createWorkbook()
addWorksheet(Kodiak_rep, "BRF harvest")
addWorksheet(Kodiak_rep, "BRF release")
writeData(Kodiak_rep, "BRF harvest", Kodiak_H)
writeData(Kodiak_rep, "BRF release", Kodiak_R)
insertImage(Kodiak_rep, "BRF harvest", "figures/KOD_BRF_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(Kodiak_rep, "BRF release", "figures/KOD_BRF_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(Kodiak_rep, paste0("output/reports/Kodiak_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SC_rep <- createWorkbook()
addWorksheet(SC_rep, "BRF harvest")
addWorksheet(SC_rep, "BRF release")
writeData(SC_rep, "BRF harvest", SC_H)
writeData(SC_rep, "BRF release", SC_R)
insertImage(SC_rep, "BRF harvest", "figures/SC_BRF_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SC_rep, "BRF release", "figures/SC_BRF_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(SC_rep, paste0("output/reports/SC_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SE_rep <- createWorkbook()
addWorksheet(SE_rep, "BRF harvest")
addWorksheet(SE_rep, "BRF release")
writeData(SE_rep, "BRF harvest", SE_H)
writeData(SE_rep, "BRF release", SE_R)
insertImage(SE_rep, "BRF harvest", "figures/SE_BRF_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SE_rep, "BRF release", "figures/SE_BRF_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(SE_rep, paste0("output/reports/SE_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)
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





























