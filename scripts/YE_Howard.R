################################################################################
# YELLOWEYE ROCKFISH HARVEST AND RELEASE CALCULATIONS
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
library(xlsx)
library(writexl)
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)
library(openxlsx)

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
#LB_H<-LB_H[,-1] #get rid of this when the code is rerun clean
#LB_R<-LB_R[,-1] #get rid of this when the code is rerun clean

#temp patch
LB_H %>% filter(RptArea == "EWYKT" & Region == "SC")
LB_H <- LB_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))


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
#SC_port <- SC_port[,-1]
#combine for species comp estimates
colnames(SE_port); ncol(SE_port)
colnames(SC_port); ncol(SC_port)

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
# need to add extra columns to SC to facilitate cobining the two regions for analysis
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

str(spec_apor)
unique(spec_apor$RptArea)


spec_apor <- spec_apor %>%
  mutate(RptArea = case_when(
    RptArea == "WESTSIDE" ~ "WKMA",
    TRUE ~ as.character(RptArea)  # Keep other values as characters
  )) %>%
  mutate(RptArea = factor(RptArea))


#-------------------------------------------------------------------------------
#get the last BRF run down: 
# 2024 coding starting with 2022 data using the old spreadsheets to compare and convert
#YE_lastH <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\harvest estimates excel version_thru",YEAR,".xlsx"), 
#                     sheet = "YE harvest",
#                     range = paste0("A2:AC1000"), 
#                     na = "NA")
#YE_lastH <- YE_lastH[rowSums(is.na(YE_lastH)) != ncol(YE_lastH), ]

#YE_lastR <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\release estimates excel version_thru",YEAR,".xlsx"), 
#                       sheet = "YE release",
#                       range = paste0("A2:AC1000"), 
#                       na = "NA")
#YE_lastR <- YE_lastR[rowSums(is.na(YE_lastR)) != ncol(YE_lastR), ]

#colnames(YE_lastH) <- colnames(YE_harvest)

# With 2023 and beyond you will pull and update the csv files created in this workflow:
YE_lastH <- read.csv(paste0("output/YE_harv_Howard_thru",YEAR-1,".csv")) %>% select(-X)
YE_lastR <- read.csv(paste0("output/YE_rel_Howard_thru",YEAR-1,".csv")) %>% select(-X)

#-------------------------------------------------------------------------------
# Before we get going we need to deal with the Kodiak decision tree
# Eastside gui_pBRFinPel = average or raw depending on sample size
# Eastside Priv_pBRF = Northeast pBRF
# Afognak gui_pBRFinPel = Northeast pBRFinPel
# Afognak priv_pBRF = Northeast pBRF
# WKMA gui_pBRFinPel = Afognak gui_pBRFinPel
# WKMA Priv_pBRF = Afognak privBRF
# SKMA gui_pBRFinPel = Easside pBRFinPel
# SKMA priv_pBRF = Eastside priv_pBRF

# Method for getting gui_pYE for Kodiak calculations: 
# 1) get the YE_lastH 
# 2) Only use data before 2020
# 3) calculate yearly Gui_Yeh / Gui_rfharv
# 4) use the average and variance for column R and S.
# 5) Repeat and rinse with the release data

colnames(YE_lastH)

#YE_lastH %>% filter(...3 %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),
#                    ...2 > 2005 ) %>%
YE_lastH %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),
                    year > 2005 ) %>%
  group_by(RptArea) %>%
  summarize(gui_pYE = mean(Gui_Yeh / Log_rfharv),
            var_gui_pYE = var(Gui_Yeh / Log_rfharv)) %>%
  #summarize(gui_pYE = mean(`Gui_Yeharv (GYi)` / Gui_rfharv),
  #          var_gui_pYE = var(`Gui_Yeharv (GYi)` / Gui_rfharv)) %>%
  mutate(Year = YEAR)-> KOD_H_pYE
colnames(KOD_H_pYE) <- c("RptArea","gui_pYE_harv","var_gui_pYE_harv","Year")

YE_lastR %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),
                    year > 2005) %>%
  group_by(RptArea) %>%
  summarize(gui_pYE = mean(Gui_Yer / Log_rfrel),
            var_gui_pYE = var(Gui_Yer / Log_rfrel)) %>%
  mutate(Year = YEAR) -> KOD_R_pYE
colnames(KOD_R_pYE) <- c("RptArea","gui_pYE_rel","var_gui_pYE_rel","Year")

KOD_YE_crap <- left_join(KOD_H_pYE,KOD_R_pYE, by = c("Year","RptArea"))

#Need to add in rows for missing Kodiak areas:
with(spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),
                          Year == YEAR), 
     table(User, RptArea))
#Only SKMA missing in 2023

spec_apor <- spec_apor %>% 
  #add_row(Year = YEAR, RptArea = "AFOGNAK", User = "private",
  #      TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0) %>%
  #add_row(Year = YEAR, RptArea = "EASTSIDE", User = "private",
  #        TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0) %>%
  #add_row(Year = YEAR, RptArea = "WKMA", User = "private",
  #        TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0) %>%
  add_row(Year = YEAR, RptArea = "SKMA", User = "private",
          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0) %>%
  #add_row(Year = YEAR, RptArea = "WKMA", User = "charter",
  #        TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0) %>%
  add_row(Year = YEAR, RptArea = "SKMA", User = "charter",
          TotalRF_n = 0, YE_n = 0, Black_n = 0, Pelagic_n = 0, Nonpel_n = 0, NotYE_Nonpel_n = 0)

with(spec_apor %>% filter(RptArea %in% c("NORTHEAST","AFOGNAK","WKMA","SKMA","EASTSIDE"),
                          Year == YEAR), 
     table(User, RptArea))

spec_apor <- left_join(spec_apor,KOD_YE_crap, by = c("Year","RptArea"))
unique(spec_apor$Year)
spec_apor %>% filter (!is.na("gui_pYE_harv"))
unique(spec_apor$gui_pYE_harv)
#---HARVESTS--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
YE_guiH <- new_H %>%
  select(Region, year, RptArea,Log_rfharv) %>%
  left_join(LB_H %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,
                     Gui_nonpelharv = nonpel_harv,
                     Gui_Yeh = ye_harv),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year)) %>%
              select(year,RptArea,
                     gui_pYEinNonpel = pYEinNonpel,
                     gui_var_pYEinNonpel = var_pYEinNonpel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pYEinNonpel = NA, #as.numeric(gui_pBRFinPel), #calculation not used; left in here to track pre24 spreadsheets
         gui_var_pYEinNonpel = NA, #as.numeric(gui_var_pBRFinPel),
         GuiYE = Gui_Yeh,
         var_GuiYE = 0, #(Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiYE = sqrt(var_GuiYE),
         GuiYE_UPRLWR95 = 1.96 * sqrt_GuiYE)

YE_priH <- new_H %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfharv,var_PRIV_rfharv) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pYE = ifelse(TotalRF_n > 50,pYE,pYE_avgRptArea),
                     priv_var_pYE = ifelse(TotalRF_n > 50,var_pYE,var_pYE_avgRptArea),
                     gui_pYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                                       gui_pYE_harv,NA),
                     var_gui_pYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                                           var_gui_pYE_harv,NA)) %>%
              select(year,RptArea,
                     priv_pYE,
                     priv_var_pYE,
                     gui_pYE, var_gui_pYE),
            by = c("year", "RptArea")) %>%
  mutate(Priv_YE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                           PRIV_rfharv * gui_pYE,
                           PRIV_rfharv * priv_pYE),
         var_PrivYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                              (PRIV_rfharv^2) * var_gui_pYE + (gui_pYE^2) * var_PRIV_rfharv + (var_gui_pYE*var_PRIV_rfharv),
                              (PRIV_rfharv^2) * priv_var_pYE + (priv_pYE^2) * var_PRIV_rfharv + (priv_var_pYE*var_PRIV_rfharv)),
         sqrt_PrivYE = sqrt(var_PrivYE),
         PrivYE_UPRLWR95 = 1.96 * sqrt_PrivYE)

break_col <- as.data.frame(matrix(nrow=nrow(YE_priH),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(YE_priH),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

nrow(unique(YE_guiH)); nrow(YE_priH)

YE_harvest <- cbind(unique(YE_guiH),break_col,YE_priH %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalYEharv = GuiYE + Priv_YE,
         var_totalYEharv = var_GuiYE + var_PrivYE,
         sqrt_totalYE = sqrt(var_totalYEharv),
         TotalYE_UPRLWR95 = 1.96 * sqrt_totalYE)
 
# Add it onto the running sheet:
#colnames(YE_lastH) <- colnames(YE_harvest)
#YE_lastH <- YE_lastH %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(YE_lastH); ncol(YE_harvest)
#YE_lastH <- YE_lastH[,-29]

updated_YE_H <- rbind(YE_lastH,YE_harvest) %>% arrange(Region,RptArea,year)

updated_YE_H %>% filter(year == YEAR) 
#checks out! just save one 2022 row
#updated_YE_H <- rbind(YE_lastH %>% filter(year < YEAR),
#                      YE_harvest) %>% 
#  mutate(RptArea = case_when(
#    RptArea == "NORTHEAS" ~ "NORTHEAST",
#    TRUE ~ as.character(RptArea))) %>% arrange(Region,RptArea,year)

write.csv(updated_YE_H, paste0("output/YE_harv_Howard_thru",YEAR,".csv"), row.names = F)

# For EXCEL recording, the BRF analysis is where you create the workbook: 
harv_est_xlsx <- loadWorkbook(paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(harv_est_xlsx, "YE harvest")
writeData(harv_est_xlsx, "YE harvest", updated_YE_H)
saveWorkbook(harv_est_xlsx, paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)

#---RELEASES--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
colnames(spec_apor)

YE_guiR <- new_R %>%
  select(Region, year, RptArea,Log_rfrel) %>%
  left_join(LB_R %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,
                     Gui_nonpelrel = nonpel_rel,
                     Gui_Yer = ye_rel),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year)) %>%
              select(year,RptArea,
                     gui_pYEinNonpel = pYEinNonpel,
                     gui_var_pYEinNonpel = var_pYEinNonpel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pYEinNonpel = NA, #as.numeric(gui_pBRFinPel), #calculation not used; left in here to track pre24 spreadsheets
         gui_var_pYEinNonpel = NA, #as.numeric(gui_var_pBRFinPel),
         GuiYE = Gui_Yer,
         var_GuiYE = 0, #(Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiYE = sqrt(var_GuiYE),
         GuiYE_UPRLWR95 = 1.96 * sqrt_GuiYE)

YE_guiR <- unique(YE_guiR)

# For releases we want to apply to avg reporting area average from pre-2020 when
# retention regulations went into place and port sampling is no longer indicative
# of what has been released
left_join(spec_apor,
          spec_apor %>% filter(Year == 2019) %>% 
            select(RptArea,User,
                   use_pYE_aRA = pYE_avgRptArea,
                   use_var_pYE_aRA = var_pYE_avgRptArea),
          by = c("RptArea","User"))  -> spec_apor

colnames(spec_apor)

YE_priR <- new_R %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfrel,var_PRIV_rfrel) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pYE = ifelse(TotalRF_n < 50,
                                       use_pYE_aRA, 
                                       ifelse(RptArea %in% c("CI","PWSI","PWSO","NG"),
                                              pYE,use_pYE_aRA)),
                     priv_var_pYE = ifelse(TotalRF_n < 50,
                                       use_var_pYE_aRA, 
                                       ifelse(RptArea %in% c("CI","PWSI","PWSO","NG"),
                                              var_pYE,use_var_pYE_aRA)),
                     #priv_pYE = use_pYE_aRA, #ifelse(year < 2020, pYE_avgRptArea,
                                       #ifelse(TotalRF_n > 50,pYE,pYE_avgRptArea)),
                     #priv_var_pYE = use_var_pYE_aRA, #ifelse(year < 2020, pYE_avgRptArea,
                                           #ifelse(TotalRF_n > 50,var_pYE,var_pYE_avgRptArea)),
                     gui_pYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                                       gui_pYE_rel,NA),
                     var_gui_pYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                                           var_gui_pYE_rel,NA)) %>%
              select(year,RptArea,
                     priv_pYE,
                     priv_var_pYE,
                     gui_pYE,var_gui_pYE),
            by = c("year", "RptArea")) %>%
  mutate(#Priv_YE = PRIV_rfrel * priv_pYE,
         #var_PrivYE =(PRIV_rfrel^2) * priv_var_pYE + (priv_pYE^2) * var_PRIV_rfrel + (priv_var_pYE*var_PRIV_rfrel),
         Priv_YE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                           PRIV_rfrel * gui_pYE,
                           PRIV_rfrel * priv_pYE),
         var_PrivYE = ifelse (RptArea %in% c("AFOGNAK","WKMA","SKMA","EASTSIDE"),
                              (PRIV_rfrel^2) * var_gui_pYE + (gui_pYE^2) * var_PRIV_rfrel + (var_gui_pYE*var_PRIV_rfrel),
                              (PRIV_rfrel^2) * priv_var_pYE + (priv_pYE^2) * var_PRIV_rfrel + (priv_var_pYE*var_PRIV_rfrel)),
         sqrt_PrivYE = ifelse(is.na(var_PrivYE) | var_PrivYE < 0,0,sqrt(var_PrivYE)) ,
         PrivYE_UPRLWR95 = 1.96 * sqrt_PrivYE
         )

YE_release <- cbind(YE_guiR,break_col,YE_priR %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalYErel = GuiYE + Priv_YE,
         var_totalYErel = var_GuiYE + var_PrivYE,
         sqrt_totalYE = sqrt(var_totalYErel),
         TotalYE_UPRLWR95 = 1.96 * sqrt_totalYE)

# Add it onto the running sheet:
head(YE_lastR %>% data.frame())
head(YE_release %>% data.frame())

#colnames(YE_lastR) <- colnames(YE_release)
#YE_lastR <- YE_lastR %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(YE_lastR); ncol(YE_release)
#YE_lastR <- YE_lastR[,-29]

updated_YE_R <- rbind(YE_lastR,YE_release) %>% arrange(Region,RptArea,year)

updated_YE_R %>% filter(year == YEAR)
# CSEO values diff between new R and old excel. Foud a copy-paste error in excel version:

#updated_YE_R <- rbind(YE_lastR %>% filter(year < YEAR),
#                       YE_release) %>% 
#  mutate(RptArea = case_when(
#    RptArea == "NORTHEAS" ~ "NORTHEAST",
#    TRUE ~ as.character(RptArea)))%>% arrange(Region,RptArea,year)
write.csv(updated_YE_R,paste0("output/YE_rel_Howard_thru",YEAR,".csv"), row.names = F)

# For EXCEL recording, the BRF analysis is where you create the workbook: 
rel_est_xlsx <- loadWorkbook(paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(rel_est_xlsx, "YE release")
writeData(rel_est_xlsx, "YE release", updated_YE_R)
saveWorkbook(rel_est_xlsx, paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#-------------------------------------------------------------------------------
# Summary and plots
# Harvest and release by year and user and CFMU / RptArea
str(updated_YE_H)

updated_YE_H %>% select(Region,RptArea,year,
                         Guided = GuiYE,SE_Gui = sqrt_GuiYE ,
                         Private = Priv_YE,SE_Priv = sqrt_PrivYE,
                         Total = TotalYEharv,SE_Tot = sqrt_totalYE)->YE_harv_table

Kodiak_H <- YE_harv_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_H <- YE_harv_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_H <- YE_harv_table %>% filter(Region == "SE")

updated_YE_R %>% select(Region,RptArea,year,
                         Guided = GuiYE,SE_Gui = sqrt_GuiYE ,
                         Private = Priv_YE,SE_Priv = sqrt_PrivYE,
                         Total =TotalYErel,SE_Tot = sqrt_totalYE)->YE_rel_table

Kodiak_R <- YE_rel_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_R <- YE_rel_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_R <- YE_rel_table %>% filter(Region == "SE")

colnames(YE_harv_table)
YE_harv_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Harvest"
  ) %>%
  left_join(YE_harv_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
              pivot_longer(
                cols = c(SE_Gui,SE_Priv,SE_Tot),
                names_to = "User",
                values_to = "SE"
              ) %>% 
              mutate(User = ifelse(User == "SE_Gui","Guided",
                                   ifelse(User == "SE_Priv","Private","Total"))),
            by=c("year","Region","RptArea","User")) -> Hplot_dat

YE_rel_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Releases"
  ) %>%
  left_join(YE_rel_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
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

ggsave("figures/SE_YE_harv.png", width = 6, height = 4)

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

ggsave("figures/KOD_YE_harv.png", width = 6, height = 4)

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

ggsave("figures/SC_YE_harv.png", width = 6, height = 4)

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

ggsave("figures/SE_YE_rel.png", width = 6, height = 4)

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

ggsave("figures/KOD_YE_rel.png", width = 6, height = 4)

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

ggsave("figures/SC_YE_rel.png", width = 6, height = 4)

Kodiak_rep <- loadWorkbook(paste0("output/reports/Kodiak_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(Kodiak_rep, "YE harvest")
addWorksheet(Kodiak_rep, "YE release")
writeData(Kodiak_rep, "YE harvest", Kodiak_H)
writeData(Kodiak_rep, "YE release", Kodiak_R)
insertImage(Kodiak_rep, "YE harvest", "figures/KOD_YE_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(Kodiak_rep, "YE release", "figures/KOD_YE_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(Kodiak_rep, paste0("output/reports/Kodiak_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SC_rep <- loadWorkbook(paste0("output/reports/SC_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(SC_rep, "YE harvest")
addWorksheet(SC_rep, "YE release")
writeData(SC_rep, "YE harvest", SC_H)
writeData(SC_rep, "YE release", SC_R)
insertImage(SC_rep, "YE harvest", "figures/SC_YE_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SC_rep, "YE release", "figures/SC_YE_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(SC_rep, paste0("output/reports/SC_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SE_rep <- loadWorkbook(paste0("output/reports/SE_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(SE_rep, "YE harvest")
addWorksheet(SE_rep, "YE release")
writeData(SE_rep, "YE harvest", SE_H)
writeData(SE_rep, "YE release", SE_R)
insertImage(SE_rep, "YE harvest", "figures/SE_YE_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SE_rep, "YE release", "figures/SE_YE_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(SE_rep, paste0("output/reports/SE_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)
#NOTE FLAG GODDAMN:
# Harvest and catches should really be in log space. Fucking hell... 

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





























