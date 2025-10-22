################################################################################
# PELAGIC ROCKFISH HARVEST AND RELEASE CALCULATIONS
#
# Author: Phil Joy
# Last updated: October 2024
#
# This code calculates Pelagic numbers that were not done prior to this
# 
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
new_H <- new_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))
new_R <- new_R %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))

# First time with PELAGICS so need all harvest data:
all_H <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\harvest estimates excel version_thru",YEAR,".xlsx"), 
                   sheet = "rockfish harvests",
                   range = paste0("A1:R1000"), 
                   na = "NA")
all_H <- all_H[rowSums(is.na(all_H)) != ncol(all_H), ]

all_R <- read_xlsx(paste0(".\\data\\raw_dat\\",YEAR,"\\release estimates excel version_thru",YEAR,".xlsx"), 
                   sheet = "rockfish release",
                   range = paste0("A1:Y1000"), 
                   na = "NA")
all_R <- all_R[rowSums(is.na(all_R)) != ncol(all_R), ]

names(all_H) <- names(new_H)
names(all_R) <- names(new_R)

head(all_H %>% data.frame(), n = 5); head(new_H %>% data.frame(), n = 5)
head(all_R %>% data.frame(), n = 5); head(new_R %>% data.frame(), n = 5)

#read in logbook harvest and release estimate
LB_H <- read.csv(paste0("data/raw_dat/logbook_harvest_thru",YEAR,".csv"))
LB_R <- read.csv(paste0("data/raw_dat/logbook_release_thru",YEAR,".csv"))


#temp patch
LB_H <- LB_H %>% mutate(Region = ifelse(RptArea == "EWYKT","SE",Region))


#get SE port sampling data:
#SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_Region1_forR_2023.FINAL.xlsx"), 
# SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024.xlsx"), 
#SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024_RUN_08-Oct-2025.xlsx"), 
SE_port <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024_RUN_22-Oct-2025.xlsx"), 
                     #sheet = "Sheet1", 2023
                     sheet = "MHS num Fish", #2024; different format
                     range = paste0("A1:DX1000"), # paste0("A1:BX1000"), 
                     na = "NA")
SE_port <- SE_port[rowSums(is.na(SE_port)) != ncol(SE_port), ]

#get SC port sampling data:
SC_port <- read.csv(paste0("data/raw_dat/Species_comp_SC/Species_comp_Region2_thru",YEAR,".csv"))

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



str(spec_apor)

#-------------------------------------------------------------------------------
# No previous Pelagic calculations, so will need to call up '22 ests when doing '23 ests in Oct '24
# 2024 coding starting with 2022 data using the old spreadsheets to compare and convert
PEL_lastH <- read.csv(paste0("output/PEL_harv_Howard_thru",YEAR-1,".csv")) %>% select(-X)
PEL_lastR <- read.csv(paste0("output/PEL_rel_Howard_thru",YEAR-1,".csv")) %>% select(-X)

#---HARVESTS--------------------------------------------------------------------
#Calculate this year's estimates:
# Need rpt area averages to use because of sample size issues and changes in regulation
# in 2019 making it impossible to assess the proportion of fish being released.
#look:
spec_apor %>% filter(Year > 2017) %>% #,
                     #RptArea %in% c("CSEO","EWYKT",#"EYKT","IBS",
                     #                "NSEI","NSEO","SSEI","SSEO")) %>% 
  select(Year,RptArea,TotalRF_n,YE_n,Black_n,Pelagic_n,Nonpel_n,NotYE_Nonpel_n,
         pPel,pPel_avgRptArea) %>%data.frame()

#save values:
left_join(spec_apor,
          spec_apor %>% filter(Year == 2019) %>% 
            select(RptArea,User,
                   use_pPel_aRA = pPel_avgRptArea,
                   use_var_pPel_aRA = var_pPel_avgRptArea),
                   #use_pSlopeinNP_aRA = pSlopeinNonP_avgRptArea,
                   #use_var_pSlopeinNP_aRA = var_pSlopeinNonP_avgRptArea),
          by = c("RptArea","User"))  -> spec_apor

#Pel_guiH <- all_H %>%
Pel_guiH <- new_H %>%
  select(Region, year, RptArea,Log_rfharv) %>%
  left_join(LB_H %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,
                     Gui_pelharv = pelagic_harv),
                     #Gui_Pelh = ye_harv),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pPel = ifelse(year < 2006,use_pPel_aRA,NA),
                     gui_var_pPel = ifelse(year < 2006,use_var_pPel_aRA,NA)) %>%
              select(year,RptArea,
                     gui_pPel,
                     gui_var_pPel),
            by = c("year", "RptArea")) %>%
  mutate(#gui_pPel = NA, #as.numeric(gui_pBRFinPel), #calculation not used; left in here to track pre24 spreadsheets
         #gui_var_pPel = NA, #as.numeric(gui_var_pBRFinPel),
         GuiPel = Gui_pelharv,
         var_GuiPel = ifelse(year < 2006,
                             (Gui_pelharv^2) * gui_var_pPel,0), #(Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiPel = sqrt(var_GuiPel),
         GuiPel_UPRLWR95 = 1.96 * sqrt_GuiPel)

Pel_priH <- new_H %>%
#Pel_priH <- all_H %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfharv,var_PRIV_rfharv) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pPel = ifelse(TotalRF_n > 50,pPel,use_pPel_aRA),
                     priv_var_pPel = ifelse(TotalRF_n > 50,var_pPel,use_var_pPel_aRA)) %>% #,
                     #gui_pPel = NA,
                     #var_gui_pPel = NA) %>%
              select(year,RptArea,
                     priv_pPel,
                     priv_var_pPel),
            by = c("year", "RptArea")) %>%
  mutate(Priv_Pel = PRIV_rfharv * priv_pPel,
         var_PrivPel = (PRIV_rfharv^2) * priv_var_pPel + (priv_pPel^2) * var_PRIV_rfharv + (priv_var_pPel*var_PRIV_rfharv),
         sqrt_PrivPel = sqrt(var_PrivPel),
         PrivPel_UPRLWR95 = 1.96 * sqrt_PrivPel)

break_col <- as.data.frame(matrix(nrow=nrow(Pel_guiH),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(Pel_guiH),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

Pel_harvest <- cbind(Pel_guiH,break_col,Pel_priH %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalPelharv = GuiPel + Priv_Pel,
         var_totalPelharv = var_GuiPel + var_PrivPel,
         sqrt_totalPel = sqrt(var_totalPelharv),
         TotalPel_UPRLWR95 = 1.96 * sqrt_totalPel)
 
Pel_harvest %>% filter(Region == "SE")
# Add it onto the running sheet:
#colnames(YE_lastH) <- colnames(YE_harvest)
#YE_lastH <- YE_lastH %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(PEL_lastH); ncol(Pel_harvest)
#YE_lastH <- YE_lastH[,-29]

updated_Pel_H <- rbind(PEL_lastH,Pel_harvest) %>% arrange(Region,RptArea,year)

#updated_YE_H %>% filter(year == 2022 & Region == "SE") 
#checks out! just save one 2022 row
#updated_Pel_H <- Pel_harvest #rbind(YE_lastH %>% filter(year < YEAR),
                      #YE_harvest) %>% arrange(Region,RptArea,year)

write.csv(updated_Pel_H, paste0("output/PEL_harv_Howard_thru",YEAR,".csv"))

# For EXCEL recording, the BRF analysis is where you create the workbook: 
harv_est_xlsx <- loadWorkbook(paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(harv_est_xlsx, "PEL harvest")
writeData(harv_est_xlsx, "PEL harvest", updated_Pel_H)
saveWorkbook(harv_est_xlsx, paste0("output/harvest_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)
#---RELEASES--------------------------------------------------------------------
#Calculate this year's estimates:
# To stay consistent we'll populate the spreadsheet with all the redundancies:
colnames(spec_apor)

Pel_guiR <- new_R %>%
  select(Region, year, RptArea,Log_rfrel) %>%
  left_join(LB_R %>% filter(year == YEAR) %>%
              select(Region,year,RptArea,
                     Gui_pelrel = pelagic_rel),
            by = c("year","RptArea","Region")) %>%
  left_join(spec_apor %>% filter(User == "charter") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     gui_pPel = ifelse(year < 2006,use_pPel_aRA,NA),
                     gui_var_pPel = ifelse(year < 2006,use_var_pPel_aRA,NA)) %>%
              select(year,RptArea,
                     gui_pPel,
                     gui_var_pPel),
            by = c("year", "RptArea")) %>%
  mutate(GuiPel = Gui_pelrel,
         var_GuiPel = ifelse(year < 2006,
                             (Gui_pelrel^2) * gui_var_pPel,0), #(Gui_pelharv^2) * gui_var_pBRFinPel,
         sqrt_GuiPel = sqrt(var_GuiPel),
         GuiPel_UPRLWR95 = 1.96 * sqrt_GuiPel)

Pel_priR <- new_R %>% #colnames(new_H)
  select(Region, year, RptArea,PRIV_rfrel,var_PRIV_rfrel) %>%
  left_join(spec_apor %>% filter(User == "private") %>%
              rename(year = Year) %>%
              mutate(year = as.integer(year),
                     priv_pPel = ifelse(TotalRF_n > 50,pPel,use_pPel_aRA),
                     priv_var_pPel = ifelse(TotalRF_n > 50,var_pPel,use_var_pPel_aRA)) %>%
              select(year,RptArea,
                     priv_pPel,
                     priv_var_pPel),
            by = c("year", "RptArea")) %>%
  mutate(Priv_Pel = PRIV_rfrel * priv_pPel,
         var_PrivPel =(PRIV_rfrel^2) * priv_var_pPel + (priv_pPel^2) * var_PRIV_rfrel + (priv_var_pPel*var_PRIV_rfrel),
         sqrt_PrivPel = ifelse(is.na(var_PrivPel) | var_PrivPel < 0,0,sqrt(var_PrivPel)) ,
         PrivPel_UPRLWR95 = 1.96 * sqrt_PrivPel
         )

print(Pel_priR %>% filter(Region == "SE"), n = 30)

break_col <- as.data.frame(matrix(nrow=nrow(Pel_guiR),ncol = 1)) # to keep spreadsheet consistent
break_col2 <- as.data.frame(matrix(nrow=nrow(Pel_guiR),ncol = 1)) # to keep spreadsheet consistent
colnames(break_col) <- "blank"
colnames(break_col2) <- "blank2"

Pel_release <- cbind(Pel_guiR,break_col,Pel_priR %>% select(-c(Region,year,RptArea)),break_col2) %>%
  mutate(TotalPelrel = GuiPel + Priv_Pel,
         var_totalPelrel = var_GuiPel + var_PrivPel,
         sqrt_totalPel = sqrt(var_totalPelrel),
         TotalPel_UPRLWR95 = 1.96 * sqrt_totalPel)

Pel_release %>% filter(Region == "SE")

# Add it onto the running sheet:
#head(YE_lastR %>% data.frame())
#head(YE_release %>% data.frame())

#colnames(YE_lastR) <- colnames(YE_release)
#YE_lastR <- YE_lastR %>% data.frame() %>% 
#  mutate(RptArea = as.factor(RptArea),
#         Region = as.factor(Region)) %>% 
#  mutate_if(is.character, ~as.numeric(.))
ncol(PEL_lastR); ncol(Pel_release)
#YE_lastR <- YE_lastR[,-29]

updated_Pel_R <- rbind(PEL_lastR,Pel_release) %>% arrange(Region,RptArea,year)

#updated_YE_R %>% filter(year == 2022 & Region == "SE")
# CSEO values diff between new R and old excel. Foud a copy-paste error in excel version:

#updated_Pel_R <- Pel_release #rbind(YE_lastR %>% filter(year < YEAR),
                       #YE_release) %>% arrange(Region,RptArea,year)
write.csv(updated_Pel_R,paste0("output/Pel_rel_Howard_thru",YEAR,".csv"))

# For EXCEL recording, the BRF analysis is where you create the workbook: 
rel_est_xlsx <- loadWorkbook(paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"))
addWorksheet(rel_est_xlsx, "PEL release")
writeData(rel_est_xlsx, "PEL release", updated_Pel_R)
saveWorkbook(rel_est_xlsx, paste0("output/release_estimates_Howard_thru",YEAR,".xlsx"),overwrite=T)

#-------------------------------------------------------------------------------
# Summary and plots
# Harvest and release by year and user and CFMU / RptArea
str(updated_YE_H)

updated_Pel_H %>% select(Region,RptArea,year,
                         Guided = GuiPel,SE_Gui = sqrt_GuiPel ,
                         Private = Priv_Pel,SE_Priv = sqrt_PrivPel,
                         Total = TotalPelharv,SE_Tot = sqrt_totalPel)->Pel_harv_table

Kodiak_H <- Pel_harv_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_H <- Pel_harv_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_H <- Pel_harv_table %>% filter(Region == "SE")


updated_Pel_R %>% select(Region,RptArea,year,
                         Guided = GuiPel,SE_Gui = sqrt_GuiPel ,
                         Private = Priv_Pel,SE_Priv = sqrt_PrivPel,
                         Total =TotalPelrel,SE_Tot = sqrt_totalPel)->Pel_rel_table

Kodiak_R <- Pel_rel_table %>% filter(RptArea %in% c("AFOGNAK","EASTSIDE","NORTHEAST",
                                                     "WKMA","SKMA")) 
SC_R <- Pel_rel_table %>% filter(Region == "SC",
                                  RptArea %in% c("CI","NG","PWSI",
                                                 "PWSO")) 

SE_R <- Pel_rel_table %>% filter(Region == "SE")


colnames(Pel_harv_table)
Pel_harv_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Harvest"
  ) %>%
  left_join(Pel_harv_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
              pivot_longer(
                cols = c(SE_Gui,SE_Priv,SE_Tot),
                names_to = "User",
                values_to = "SE"
              ) %>% 
              mutate(User = ifelse(User == "SE_Gui","Guided",
                                   ifelse(User == "SE_Priv","Private","Total"))),
            by=c("year","Region","RptArea","User")) -> Hplot_dat

Pel_rel_table %>% select(year,Region,RptArea,Guided,Private,Total) %>%
  pivot_longer(
    cols = c(Guided, Private, Total),
    names_to = "User",
    values_to = "Releases"
  ) %>%
  left_join(Pel_rel_table %>% select(year,Region,RptArea,SE_Gui,SE_Priv,SE_Tot) %>%
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

ggsave("figures/SE_PEL_harv.png", width = 6, height = 4)

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

ggsave("figures/KOD_PEL_harv.png", width = 6, height = 4)

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

ggsave("figures/SC_PEL_harv.png", width = 6, height = 4)

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

ggsave("figures/SE_PEL_rel.png", width = 6, height = 4)

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

ggsave("figures/SC_PEL_rel.png", width = 6, height = 4)

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

ggsave("figures/SC_PEL_rel.png", width = 6, height = 4)

#Kodiak_rep <- loadWorkbook(paste0("output/reports/Kodiak_RF_HowMth_thru",YEAR,".xlsx"))
#addWorksheet(Kodiak_rep, "PEL harvest")
#addWorksheet(Kodiak_rep, "PEL release")
#writeData(Kodiak_rep, "PEL harvest", Kodiak_H)
#writeData(Kodiak_rep, "PEL release", Kodiak_R)
#insertImage(Kodiak_rep, "PEL harvest", "figures/KOD_PEL_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
#insertImage(Kodiak_rep, "PEL release", "figures/KOD_PEL_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
#saveWorkbook(Kodiak_rep, paste0("output/reports/Kodiak_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SC_rep <- loadWorkbook(paste0("output/reports/SC_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(SC_rep, "PEL harvest")
addWorksheet(SC_rep, "PEL release")
writeData(SC_rep, "PEL harvest", SC_H)
writeData(SC_rep, "PEL release", SC_R)
insertImage(SC_rep, "PEL harvest", "figures/SC_PEL_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SC_rep, "PEL release", "figures/SC_PEL_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
saveWorkbook(SC_rep, paste0("output/reports/SC_RF_HowMth_thru",YEAR,".xlsx"),overwrite=T)

SE_rep <- loadWorkbook(paste0("output/reports/SE_RF_HowMth_thru",YEAR,".xlsx"))
addWorksheet(SE_rep, "PEL harvest")
addWorksheet(SE_rep, "PEL release")
writeData(SE_rep, "PEL harvest", SE_H)
writeData(SE_rep, "PEL release", SE_R)
insertImage(SE_rep, "PEL harvest", "figures/SE_PEL_harv.png", width = 10, height = 7, startRow = 1, startCol = 12)
insertImage(SE_rep, "PEL release", "figures/SE_PEL_rel.png", width = 10, height = 7, startRow = 1, startCol = 12)
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





























