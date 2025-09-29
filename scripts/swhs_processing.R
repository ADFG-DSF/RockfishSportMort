################################################################################
## This code imports SWHS data into working data sheets for rockfish harvest, 
## release and mortality estimation
##
## Author: Phil Joy
## Last updated: 10/25
##
## Data sources:
## Statewide Harvest Survey SWHS (Nick Smith at nick.smith@alaska.gov)
## Typically available in late Septemner

# Specific code request available in excel workbook with the data titled:
# rf_byMgmtUnit_DATEPULLED.xls
################################################################################

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)

YEAR <- 2024

#2022
SWHS_file_name <- "rf_byMgmtUnit_20240305.xlsx"
#2023
SWHS_file_name <- "rf_byMgmtUnit_sent20240925.xlsx"
#2024
SWHS_file_name <- "rf_byMgmtUnit_sent20250916.xlsx"

# KODIAK: Between 2017 and 2021 Kodiak harvests were not handled properly. The last
# FS I confused the sample size requirements for SWHS responses (12+) and those 
# required for port sampling (50+). As such, they were improperly borrowing Kodiak
# harvest estimates from adjacent areas when the number of respondents was between
# 12 and 50 when they should have used actual estimates. This has been corrected
# for 2022 and forward but remains an issue for past estimates. 
  
# Read in data file used for analysis and to be updated:
# First year coding this and doing a redundant run on 2022. Typically you will
# read this in from last year's folder and then save it into this year's folder:
gui_cat <- #guided catch
  read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
            sheet = "gui_cat",
            range = paste0("A4:R",YEAR-2011+5), 
            na = "NA") %>% rename_all(~ gsub("\"", "", .)); gui_cat


gui_cat_se <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                        sheet = "guicat_se",
                        range = paste0("A4:Q",YEAR-2011+5), 
                        na = "NA") %>% rename_all(~ gsub("\"", "", .)); gui_cat_se

gui_har <- #guided harvest
  read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
            sheet = "gui_har",
            range = paste0("A4:R",YEAR-2011+5), 
            na = "NA") %>% rename_all(~ gsub("\"", "", .)); gui_har

gui_har_se <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                        sheet = "guihar_se",
                        range = paste0("A4:Q",YEAR-2011+5), 
                        na = "NA") %>% rename_all(~ gsub("\"", "", .)); gui_har_se

pri_cat <- #prvate catch
  read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
            sheet = "pri_cat",
            range = paste0("A4:R",YEAR-2011+5), 
            na = "NA") %>% rename_all(~ gsub("\"", "", .)); pri_cat

pri_cat_se <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                        sheet = "pricat_se",
                        range = paste0("A4:Q",YEAR-2011+5), 
                        na = "NA") %>% rename_all(~ gsub("\"", "", .)); pri_cat_se

pri_har <- #private catch
  read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
            sheet = "pri_har",
            range = paste0("A4:R",YEAR-2011+5), 
            na = "NA") %>% rename_all(~ gsub("\"", "", .)); pri_har

pri_har_se <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                        sheet = "prihar_se",
                        range = paste0("A4:Q",YEAR-2011+5), 
                        na = "NA") %>% rename_all(~ gsub("\"", "", .)); pri_har_se

#-------------------------------------------------------------------------------
# Responses: Minimum sample size is 12:
gui_resp <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                        sheet = "gui_resp",
                        range = paste0("A4:C1000"), 
                        na = "NA") %>% rename_all(~ gsub("\"", "", .)) %>%
  mutate(year = YEAR) %>% select(-YEAR); gui_resp
gui_resp <- gui_resp[rowSums(is.na(gui_resp)) != ncol(gui_resp), ]

pri_resp <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\",SWHS_file_name), 
                      sheet = "pri_resp",
                      range = paste0("A4:C1000"), 
                      na = "NA") %>% rename_all(~ gsub("\"", "", .)) %>%
  mutate(year = YEAR) %>% select(-YEAR)
pri_resp <- pri_resp[rowSums(is.na(pri_resp)) != ncol(pri_resp), ]

gui_resp %>% filter(respused < 12 & year > (YEAR-3))

pri_resp %>% filter(respused < 12 & year > (YEAR-3))

#-------------------------------------------------------------------------------
# Calculate RELEASES = catch - harvest

gui_rel <- gui_cat - gui_har
gui_rel <- gui_rel %>% mutate(YEAR = gui_cat$YEAR)

gui_rel_se <- sqrt((gui_cat_se^2)+(gui_har_se^2))
gui_rel_se <- gui_rel_se %>% mutate(year = gui_cat$YEAR)

pri_rel <- pri_cat - pri_har
pri_rel <- pri_rel %>% mutate(YEAR = pri_cat$YEAR)

pri_rel_se <- sqrt((pri_cat_se^2)+(pri_har_se^2))
pri_rel_se <- pri_rel_se %>% mutate(year = pri_cat$YEAR)

#-------------------------------------------------------------------------------
# Update working data sheets and save as csv files to be updated with logbook
# and port sampling data. Once complete the csv files can be compiled back into
# an excel worksheet since that is the format that has been used. 

# SWHS HARVESTS updated from last year: 
#rock_harv <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR-1,"\\harvest estimates excel version_thru",YEAR-1,".xlsx"), 
#                       sheet = "rockfish harvests",
#                       range = paste0("A1:R1000"), 
#                       na = "NA")
#rock_harv <- rock_harv[rowSums(is.na(rock_harv)) != ncol(rock_harv), ]

#colnames(rock_harv)

new_gharv <- gui_har[nrow(gui_har),] %>% mutate(var = "guiSWHS_rfharv") %>% select(-Total) # (G-hatSi)

new_gharv_se <- gui_har_se[nrow(gui_har),] %>% mutate(YEAR = as.factor(year),
                                                      var = "var_guiSWHS_rfharv") %>% # (var G-hatSi)
  select(-year) %>% relocate(YEAR, .before = everything())
new_gharv_var <- new_gharv_se %>% mutate(across(where(is.numeric), ~ .^2))

new_pharv <- pri_har[nrow(pri_har),] %>% mutate(var = "privSWHS_rfharv") %>% select(-Total) # (G-hatSi)

new_pharv_se <- pri_har_se[nrow(pri_har),] %>% mutate(YEAR = as.factor(year),
                                                      var = "var_privSWHS_rfharv") %>% # (var G-hatSi)
  select(-year) %>% relocate(YEAR, .before = everything())
new_pharv_var <- new_pharv_se %>% mutate(across(where(is.numeric), ~ .^2))

new_H <- rbind(new_gharv,
                 new_gharv_var,
                 new_pharv,
                 new_pharv_var)


new_H <- t(new_H %>% relocate(var, .before = everything())) %>% data.frame() 
colnames(new_H) = new_H[1, ] # the first row will be the header
new_H = new_H[-1, ] 
new_H %>% slice(-1) %>% 
  rownames_to_column(var = "RptArea") %>%
  mutate_at(vars(2:5),as.numeric)  %>%
  mutate(Region = ifelse(RptArea %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO"),
                         "SE","SC"),
         Log_rfharv = NA, #(Gi)
         year = YEAR,
         SWHS_gprop = guiSWHS_rfharv / (guiSWHS_rfharv + privSWHS_rfharv), #(p-hatgi)
         var_SWHS_gprop = ((((guiSWHS_rfharv)^2*var_privSWHS_rfharv)+
                              ((privSWHS_rfharv)^2*var_guiSWHS_rfharv))/
                             (guiSWHS_rfharv+privSWHS_rfharv)^4)
         ) %>%
  relocate(c(Region,year,RptArea,Log_rfharv), .before = everything()) -> new_H; new_H

#no harvsts in SOKO2SAP in 2024

# SWHS RELEASES
#rock_rel <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR-1,"\\release estimates excel version_thru",YEAR-1,".xlsx"), 
#                       sheet = "rockfish release",
#                       range = paste0("A1:R1000"), 
#                       na = "NA")
#rock_rel <- rock_rel[rowSums(is.na(rock_rel)) != ncol(rock_rel), ]

#colnames(rock_rel)

new_grel <- gui_rel[nrow(gui_rel),] %>% mutate(var = "guiSWHS_rfrel") %>% select(-Total) # (G-hatSi)

new_grel_se <- gui_rel_se[nrow(gui_rel),] %>% mutate(YEAR = as.factor(year),
                                                      var = "var_guiSWHS_rfrel") %>% # (var G-hatSi)
  select(-year) %>% relocate(YEAR, .before = everything())
new_grel_var <- new_grel_se %>% mutate(across(where(is.numeric), ~ .^2))

new_prel <- pri_rel[nrow(pri_rel),] %>% mutate(var = "privSWHS_rfrel") %>% select(-Total) # (G-hatSi)

new_prel_se <- pri_rel_se[nrow(pri_rel),] %>% mutate(YEAR = as.factor(year),
                                                      var = "var_privSWHS_rfrel") %>% # (var G-hatSi)
  select(-year) %>% relocate(YEAR, .before = everything())
new_prel_var <- new_prel_se %>% mutate(across(where(is.numeric), ~ .^2))

new_R <- rbind(new_grel,
               new_grel_var,
               new_prel,
               new_prel_var)


new_R <- t(new_R %>% relocate(var, .before = everything())) %>% data.frame() 
colnames(new_R) = new_R[1, ] # the first row will be the header
new_R = new_R[-1, ] 
new_R %>% slice(-1) %>% 
  rownames_to_column(var = "RptArea") %>%
  mutate_at(vars(2:5),as.numeric)  %>%
  mutate(Region = ifelse(RptArea %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO"),
                         "SE","SC"),
         Log_rfrel = NA, #(Gi)
         year = YEAR,
         SWHS_gprop = guiSWHS_rfrel / (guiSWHS_rfrel + privSWHS_rfrel), #(p-hatgi)
         var_SWHS_gprop = ((((guiSWHS_rfrel)^2*var_privSWHS_rfrel)+
                              ((privSWHS_rfrel)^2*var_guiSWHS_rfrel))/
                             (guiSWHS_rfrel+privSWHS_rfrel)^4)
  ) %>%
  relocate(c(Region,year,RptArea,Log_rfrel), .before = everything()) -> new_R; new_R

# KODIAK: Because of inadequate sample sizes we need to make substitutions for harvest
# and release numbers:
gui_resp %>% filter(respused < 12 & year == YEAR) #Only need values for SKMA:
gui_resp %>% filter(year == YEAR)

pri_resp %>% filter(respused < 12 & year == YEAR) #2023: SKMA and EASTSIDE
pri_resp %>% filter(year == YEAR) 

# Noted: large inconsistency in what sample sizes were deemed appropriate!!! WTF
# Howard et al says 12 is minimal, but SW was censoring samples sizes under 50!
# They must have confused minimal sample sizes for port sampling data versus
#
# SWHS responses. We'll switch back to the 12 cutoff:
#
# Real inconsistency too in which areas were used for substitutions
# i.e., SKMA is all over the place; order of preference seems to be WKMA, Eastside, Afognak

# KODIAK substitutions:
# Eastside  = Northeast pBRF
# Afognak  = Northeast 
# WKMA = Afognak 
# SKMA = Eastside in Saras stuff, but WKMA in Howard et al. Eastside makes more sense on the mnap in Figure 4 

new_H[new_H$RptArea == "WKMA", "guiSWHS_rfharv"][[1]]
new_H[new_H$RptArea == "WKMA", "priSWHS_rfharv"][[1]]

new_H %>% 
  add_row(Region = "SC", year = YEAR, RptArea = "SKMA",
          guiSWHS_rfharv = new_H[new_H$RptArea == "WKMA", "guiSWHS_rfharv"],
          var_guiSWHS_rfharv = new_H[new_H$RptArea == "WKMA", "var_guiSWHS_rfharv"],
          privSWHS_rfharv = new_H[new_H$RptArea == "WKMA", "privSWHS_rfharv"],
          var_privSWHS_rfharv = new_H[new_H$RptArea == "WKMA", "var_privSWHS_rfharv"],
          SWHS_gprop = guiSWHS_rfharv / (guiSWHS_rfharv + privSWHS_rfharv), #(p-hatgi)
          var_SWHS_gprop = ((((guiSWHS_rfharv)^2*var_privSWHS_rfharv)+
                               ((privSWHS_rfharv)^2*var_guiSWHS_rfharv))/
                              (guiSWHS_rfharv+privSWHS_rfharv)^4)) %>%
  mutate(privSWHS_rfharv = ifelse(RptArea %in% c("EASTSIDE"), #2022c("EASTSIDE","WKMA"),
                                  new_H[new_H$RptArea == "NORTHEAST", "privSWHS_rfharv"],
                                  privSWHS_rfharv),
         var_privSWHS_rfharv = ifelse(RptArea %in% c("EASTSIDE"), #2022c("EASTSIDE","WKMA"),
                                      new_H[new_H$RptArea == "NORTHEAST", "var_privSWHS_rfharv"],
                                      var_privSWHS_rfharv)) -> new_H
  
new_R %>% 
  add_row(Region = "SC", year = YEAR, RptArea = "SKMA",
          guiSWHS_rfrel = new_R[new_R$RptArea == "WKMA", "guiSWHS_rfrel"],
          var_guiSWHS_rfrel = new_R[new_R$RptArea == "WKMA", "var_guiSWHS_rfrel"],
          privSWHS_rfrel = new_R[new_R$RptArea == "WKMA", "privSWHS_rfrel"],
          var_privSWHS_rfrel = new_R[new_R$RptArea == "WKMA", "var_privSWHS_rfrel"],
          SWHS_gprop = guiSWHS_rfrel / (guiSWHS_rfrel + privSWHS_rfrel), #(p-hatgi)
          var_SWHS_gprop = ((((guiSWHS_rfrel)^2*var_privSWHS_rfrel)+
                               ((privSWHS_rfrel)^2*var_guiSWHS_rfrel))/
                              (guiSWHS_rfrel+privSWHS_rfrel)^4)) %>%
  mutate(privSWHS_rfrel = ifelse(RptArea %in% c("EASTSIDE"), #2022c("EASTSIDE","WKMA"),
                                  new_R[new_R$RptArea == "NORTHEAST", "privSWHS_rfrel"],
                                  privSWHS_rfrel),
         var_privSWHS_rfrel = ifelse(RptArea %in% c("EASTSIDE"), #2022c("EASTSIDE","WKMA"),
                                      new_R[new_R$RptArea == "NORTHEAST", "var_privSWHS_rfrel"],
                                      var_privSWHS_rfrel)) -> new_R

#-------------------------------------------------------------------------------

write.csv(new_H,paste0("data/raw_dat/",YEAR,"/SWHS_harv_",YEAR,".csv"), row.names = F)
write.csv(new_R,paste0("data/raw_dat/",YEAR,"/SWHS_rel_",YEAR,".csv"), row.names = F)

#OK... now go deal with the logbook stuff in lb_processing.R







