################################################################################
# SLOPE ROCKFISH HARVEST AND RELEASE CALCULATIONS
#
# Author: Phil Joy
# Last updated: October 2024
#
# This is currently done for SE only, but I left in the SC data for if it becomes
# necessary
#
# This code replaces the Slope harvest and Slope release tabs in the excel files
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

YEAR <- 2023

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
SC_port <- read.csv(paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR,".csv"))
SC_port <- SC_port[,-1]
#combine for species comp estimates
colnames(SE_port); ncol(SE_port)
colnames(SC_port); ncol(SC_port)

# need to add extra columns to SC to facilitate cobining the two regions for analysis
dif <- ncol(SE_port)-ncol(SC_port)
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

#-------------------------------------------------------------------------------
#get the last BRF run down: 
# 2024 coding starting with 2022 data using the old spreadsheets to compare and convert
#SLO_lastH <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\harvest estimates excel version_thru",YEAR,".xlsx"), 
#                     sheet = "Slope harvest",
#                     range = paste0("A2:Y1000"), 
#                     na = "NA")
#SLO_lastH <- SLO_lastH[rowSums(is.na(SLO_lastH)) != ncol(SLO_lastH), ]

#SLO_lastR <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\release estimates excel version_thru",YEAR,".xlsx"), 
#                       sheet = "Slope release",
#                       range = paste0("A2:Y1000"), 
#                       na = "NA")
#SLO_lastR <- SLO_lastR[rowSums(is.na(SLO_lastR)) != ncol(SLO_lastR), ]

# With 2023 and beyond you will pull and update the csv files created in this workflow:
SLO_lastH <- read.csv(paste0("output/SLO_harv_Howard_thru",YEAR-1,".csv"))
SLO_lastR <- read.csv(paste0("output/SLO_rel_Howard_thru",YEAR-1,".csv"))
SLO_lastH<-SLO_lastH[,-1] #get rid of this when the code is rerun clean
SLO_lastR<-SLO_lastR[,-1] #get rid of this when the code is rerun clean
#---HARVESTS--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:

# Need rpt area averages to use because of sample size issues and changes in regulation
# in 2019 making it impossible to assess the proportion of fish being released.
#look:
spec_apor %>% filter(Year > 2017,
                     RptArea %in% c("CSEO","EWYKT",#"EYKT","IBS",
                                    "NSEI","NSEO","SSEI","SSEO")) %>% 
  select(Year,RptArea,TotalRF_n,YE_n,Black_n,Pelagic_n,Nonpel_n,NotYE_Nonpel_n,
         pSlopeinNonP,pSlopeinNonP_avgRptArea,pSlope,pSlope_avgRptArea) %>%data.frame()

#save values:
left_join(spec_apor,
          spec_apor %>% filter(Year == 2019) %>% 
            select(RptArea,User,
                   use_pSlope_aRA = pSlope_avgRptArea,
                   use_var_pSlope_aRA = var_pSlope_avgRptArea,
                   use_pSlopeinNP_aRA = pSlopeinNonP_avgRptArea,
                   use_var_pSlopeinNP_aRA = var_pSlopeinNonP_avgRptArea),
          by = c("RptArea","User"))  -> spec_apor

SLO_guiH <- new_H %>% filter(Region == "SE") %>%
  select(Region, year, RptArea,Log_rfharv) %>%
  left_join(LB_H %>% filter(year == YEAR & Region == "SE") %>%
              select(Region,year,RptArea,Gui_Nonpelharv = nonpel_harv),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter" &
                                   RptArea %in% c("CSEO","EWYKT","EYKT","IBS",
                                                  "NSEI","NSEO","SSEI","SSEO")) %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pSlopeinNonpel = ifelse(Nonpel_n > 50,pSlopeinNonP,use_pSlopeinNP_aRA),#pSlopeinNonP,
                     gui_var_pSlopeinNonpel = ifelse(Nonpel_n > 50,var_pSlopeinNonP,use_var_pSlopeinNP_aRA)) %>%
              select(year,RptArea,
                     gui_pSlopeinNonpel,
                     gui_var_pSlopeinNonpel),
            by = c("year", "RptArea")) %>%
  mutate(gui_pSlopeinNonpel = as.numeric(gui_pSlopeinNonpel),
         gui_var_pSlopeinNonpel = as.numeric(gui_var_pSlopeinNonpel),
         GuiSlope = Gui_Nonpelharv * gui_pSlopeinNonpel,
         var_GuiSlope = (Gui_Nonpelharv^2) * gui_var_pSlopeinNonpel,
         sqrt_GuiSlope = sqrt(var_GuiSlope),
         GuiSlope_UPRLWR95 = 1.96 * sqrt_GuiSlope)

SLO_priH <- new_H %>% filter(Region == "SE") %>%
  select(Region, year, RptArea,PRIV_rfharv,var_PRIV_rfharv) %>%
  left_join(spec_apor %>% filter(User == "private" &
                                   RptArea %in% c("CSEO","EWYKT","EYKT","IBS",
                                                  "NSEI","NSEO","SSEI","SSEO")) %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pSlope = ifelse(TotalRF_n > 50,pSlope,pSlope_avgRptArea),
                     priv_var_pSlope = ifelse(TotalRF_n > 50,var_pSlope,var_pSlope_avgRptArea)) %>%
              select(year,RptArea,
                     priv_pSlope,
                     priv_var_pSlope),
            by = c("year", "RptArea")) %>%
  mutate(Priv_Slope = PRIV_rfharv * priv_pSlope,
         var_PrivSlope =(PRIV_rfharv^2) * priv_var_pSlope + (priv_pSlope^2) * var_PRIV_rfharv + (priv_var_pSlope*var_PRIV_rfharv),
         sqrt_PrivSlope = sqrt(var_PrivSlope),
         PrivSlope_UPRLWR95 = 1.96 * sqrt_PrivSlope)

break_col <- as.data.frame(matrix(nrow=nrow(SLO_guiH),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(SLO_guiH),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

SLO_harvest <- cbind(SLO_guiH,break_col,SLO_priH %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalSlopeharv = GuiSlope + Priv_Slope,
         var_totalSlopeharv = var_GuiSlope + var_PrivSlope,
         sqrt_totalSlope = sqrt(var_totalSlopeharv),
         TotalSlope_UPRLWR95 = 1.96 * sqrt_totalSlope)

# Add it onto the running sheet:
colnames(SLO_lastH) ; colnames(SLO_harvest)
#SLO_lastH <- SLO_lastH %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(SLO_lastH); ncol(SLO_harvest)
#SLO_lastH <- SLO_lastH[,-26]

updated_SLO_H <- rbind(SLO_lastH,SLO_harvest) %>% arrange(Region,RptArea,year)

updated_SLO_H %>% filter(year == 2022 & Region == "SE") 
#checks out! just save one 2022 row
#updated_SLO_H <- rbind(SLO_lastH %>% filter(year < YEAR),
#                       SLO_harvest) %>% arrange(Region,RptArea,year)

write.csv(updated_SLO_H, paste0("output/SLO_harv_Howard_thru",YEAR,".csv"),row.names = F)

# For EXCEL recording, the BRF analysis is where you create the workbook: 
harv_est_xlsx <- loadWorkbook(paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(harv_est_xlsx, "Slope harvest")
writeData(harv_est_xlsx, "Slope harvest", updated_SLO_H)
saveWorkbook(harv_est_xlsx, paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#---RELEASES--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:

# Note that Sara and Brianna used long term pre-2020 averages because of regulation changes
# Not much thought in this...

SLO_guiR <- new_R %>% filter(Region == "SE") %>%
  select(Region, year, RptArea,Log_rfrel) %>%
  left_join(LB_R %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,Gui_Nonpelrel = nonpel_rel),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter" &
                                   RptArea %in% c("CSEO","EWYKT","EYKT","IBS",
                                                  "NSEI","NSEO","SSEI","SSEO")) %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pSlopeinNonpel = use_pSlopeinNP_aRA, #ifelse(Nonpel_n < 50,use_pSlopeinNP_aRA, pSlopeinNonP),#pSlopeinNonP,
                     gui_var_pSlopeinNonpel = use_var_pSlopeinNP_aRA) %>% #ifelse(Nonpel_n < 50,use_var_pSlopeinNP_aRA, var_pSlopeinNonP)) %>%
              select(year,RptArea,
                     gui_pSlopeinNonpel,#pSlopeinNonP,
                     gui_var_pSlopeinNonpel), # var_pSlopeinNonP),
            by = c("year", "RptArea")) %>%
  mutate(gui_pSlopeinNonpel = as.numeric(gui_pSlopeinNonpel),
         gui_var_pSlopeinNonpel = as.numeric(gui_var_pSlopeinNonpel),
         GuiSlope = Gui_Nonpelrel * gui_pSlopeinNonpel,
         var_GuiSlope = (Gui_Nonpelrel^2) * gui_var_pSlopeinNonpel,
         sqrt_GuiSlope = sqrt(var_GuiSlope),
         GuiSlope_UPRLWR95 = 1.96 * sqrt_GuiSlope)

SLO_priR <- new_R %>% filter(Region == "SE") %>%#colnames(new_H)
  select(Region, year, RptArea,PRIV_rfrel,var_PRIV_rfrel) %>%
  left_join(spec_apor %>% filter(User == "private" &
                                   RptArea %in% c("CSEO","EWYKT","EYKT","IBS",
                                                  "NSEI","NSEO","SSEI","SSEO")) %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pSlope = use_pSlope_aRA,
                     priv_var_pSlope = use_var_pSlope_aRA) %>%
              select(year,RptArea,
                     priv_pSlope,
                     priv_var_pSlope),
            by = c("year", "RptArea")) %>%
  mutate(Priv_Slope = PRIV_rfrel * priv_pSlope,
         var_PrivSlope =(PRIV_rfrel^2) * priv_var_pSlope + (priv_pSlope^2) * var_PRIV_rfrel + (priv_var_pSlope*var_PRIV_rfrel),
         sqrt_PrivSlope = sqrt(var_PrivSlope),
         PrivSlope_UPRLWR95 = 1.96 * sqrt_PrivSlope)

SLO_release <- cbind(SLO_guiR,break_col,SLO_priR %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalSloperel = GuiSlope + Priv_Slope,
         var_totalSloperel = var_GuiSlope + var_PrivSlope,
         sqrt_totalSlope = sqrt(var_totalSloperel),
         TotalSlope_UPRLWR95 = 1.96 * sqrt_totalSlope)

# Add it onto the running sheet:
colnames(SLO_lastR) ; colnames(SLO_release)
#SLO_lastR <- SLO_lastR %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(SLO_lastR); ncol(SLO_release)
#SLO_lastR <- SLO_lastR[,-26]

updated_SLO_R <- rbind(SLO_lastR,SLO_release) %>% arrange(Region,RptArea,year)

updated_SLO_R %>% filter(year == 2022 & Region == "SE") %>% 
  mutate(checkTot = GuiSlope + Priv_Slope)

updated_SLO_R %>% filter(RptArea == "EWYKT" & year == 2022)
# CSEO values diff between new R and old excel. Foud a copy-paste error in excel version:

#updated_SLO_R <- rbind(SLO_lastR %>% filter(year < YEAR),
#                       SLO_release) %>% arrange(Region,RptArea,year)
write.csv(updated_SLO_R,paste0("output/SLO_rel_Howard_thru",YEAR,".csv"), row.names = F)

# For EXCEL recording, the BRF analysis is where you create the workbook: 
rel_est_xlsx <- loadWorkbook(paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(rel_est_xlsx, "Slope release")
writeData(rel_est_xlsx, "Slope release", updated_SLO_R)
saveWorkbook(rel_est_xlsx, paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#-------------------------------------------------------------------------------
# Summary and plots
# Harvest and release by year and user and CFMU / RptArea
str(updated_SLO_H)

updated_SLO_H %>% select(Region,RptArea,year,
                         Guided = GuiSlope, SE_Gui = sqrt_GuiSlope ,
                         Private = Priv_Slope, SE_Priv = sqrt_PrivSlope,
                         Total = TotalSlopeharv,SE_Tot = sqrt_totalSlope)->SLO_harv_table

#Kodiak_H <- SLO_harv_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
#                                                     "WKMA","SKMA")) 
#SC_H <- SLO_harv_table %>% filter(Region == "SC",
#                                  RptArea %in% c("CI","NG","PWSI",
#                                                 "PWSO")) 

SE_H <- SLO_harv_table %>% filter(Region == "SE")

updated_SLO_R %>% select(Region,RptArea,year,
                         Guided = GuiSlope,SE_Gui = sqrt_GuiSlope ,
                         Private = Priv_Slope,SE_Priv = sqrt_PrivSlope,
                         Total =TotalSloperel,SE_Tot = sqrt_totalSlope)->SLO_rel_table

#Kodiak_R <- SLO_rel_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
#                                                     "WKMA","SKMA")) 
#SC_R <- SLO_rel_table %>% filter(Region == "SC",
#                                  RptArea %in% c("CI","NG","PWSI",
#                                                 "PWSO")) 

SE_R <- SLO_rel_table %>% filter(Region == "SE")


colnames(SLO_harv_table)
SLO_harv_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Harvest"
  ) %>%
  left_join(SLO_harv_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
              pivot_longer(
                cols = c(SE_Gui,SE_Priv,SE_Tot),
                names_to = "User",
                values_to = "SE"
              ) %>% 
              mutate(User = ifelse(User == "SE_Gui","Guided",
                                   ifelse(User == "SE_Priv","Private","Total"))),
            by=c("year","Region","RptArea","User")) -> Hplot_dat

SLO_rel_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Releases"
  ) %>%
  left_join(SLO_rel_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
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

ggsave("figures/SE_SLO_harv.png", width = 6, height = 4)


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

ggsave("figures/SE_SLO_rel.png", width = 6, height = 4)

#-- Save in excel report file
SE_rep <- loadWorkbook(paste0("output/reports/SE_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(SE_rep, "Slope harvest")
addWorksheet(SE_rep, "Slope release")
writeData(SE_rep, "Slope harvest", SE_H)
writeData(SE_rep, "Slope release", SE_R)
insertImage(SE_rep, "Slope harvest", "figures/SE_SLO_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SE_rep, "Slope release", "figures/SE_SLO_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
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





























