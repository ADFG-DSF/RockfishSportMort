################################################################################
# Data preparation for the Bayesian sport fish harvest estimation model
#
# Author: Phil Joy & Adam Reimer
# 
# Last updated: Nov 2024
#
# This script will take the raw and semi-processed rockfish data and prepare
# it for use in the model
#
################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)

REP_YR <- 2023

#look up table for region
lut <- 
  data.frame(region = c(rep("Kodiak", 6), rep("Central", 4), rep("Southeast", 6)),
             area = c("afognak", "WKMA", "SOKO2SAP", "northeast", "eastside", "BSAI", 
                      "CI", "NG", "PWSI", "PWSO", 
                      "EWYKT", "NSEO", "NSEI", "CSEO", "SSEO", "SSEI"),
             stringsAsFactors = FALSE) %>%
  arrange(region, area)

#-------------------------------------------------------------------------------
# Logbook data ----
#-------------------------------------------------------------------------------
H_ayg0 <- #logbook harvest by area, user = guided, year
read.csv(paste0("data/raw_dat/logbook_harvest_thru",REP_YR,".csv")) %>% 
  select(-c(Region,not_ye_nonpel_harv))
  
colnames(H_ayg0)
colnames(H_ayg0) <- c("year", "area", "H", "Hp", "Hnp", "Hye")

table(H_ayg0$Hye[H_ayg0$year <= 2005], useNA = "always") #these should all be NA
H_ayg0$Hye[H_ayg0$year <= 2005] <- NA

table(H_ayg0$area)
H_ayg0 %>%
  group_by(area) %>%
  summarise(H = sum(H)) %>%
  print(n = 100)

H_ayg0 %>% filter(is.na(area))

H_ayg0 %>% filter(year > 2019 & area %in% c("BSAI","ALEUTIAN","BERING",
                                            "EWYKT","IBS","EKYT",
                                            "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                                            "WKMA","WESTSIDE","MAINLAND"))
#Note BSAI = aleutian + bering
1144+20
#Note EWYKT = IBS + EYKT
14019+59280
#Note SOKO2PEN / SOKO2SAP= southeast + southwest + sakpen + chignik
11603+140+372+915
#Note WKMA = westside + mainland
39414+290

# Identify where logbook data amalgamations need to be done. They were not done
# for 2 of the 4 amalgamated areas in 2022 and 2023 so keep an eye on this: 
H_ayg0 %>% mutate(AMALG = ifelse(area %in% c("ALEUTIAN","BERING"),"BSAI",
                                 ifelse(area %in% c("IBS","EYKT"),"EWYKT",
                                        ifelse(area %in% c("SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK"),"SOKO2SAP",
                                               ifelse(area %in% c("WESTSIDE","MAINLAND"),"WKMA",NA))))) %>%
  filter(!is.na(AMALG)) %>%
  group_by(year,AMALG) %>%
  summarise(H = sum(H, na.rm = T),
         Hp = sum(Hp, na.rm = T),
         Hnp = sum(Hnp, na.rm = T),
         Hye = sum(Hye, na.rm = T)) -> a_check

H_ayg0 %>% filter(area %in% c("BSAI","ALEUTIAN","BERING",
                                            "EWYKT","IBS","EYKT",
                                            "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                                            "WKMA","WESTSIDE","MAINLAND")) %>%
  bind_rows(a_check %>% mutate(area = AMALG) %>% select(-AMALG)) %>%
  arrange(year,area) -> amalg
 
with(amalg %>%
       filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
     table(area,year)) # These should all be 2's if the amalgamated areas are already done
                      # If they are 1's then they aren't yet in the data set and
                      # need to be added in.

#!!! BSAI and SOKO2SAP missing in 2022 and 2023 so need to add them back in
H_ayg0 %>% bind_rows(amalg %>% filter(year %in% c(2022,2023) & area %in% c("BSAI","SOKO2SAP"))) %>%
  arrange(area, year) -> H_ayg0

#double check... 
with(H_ayg0 %>%
       filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
     table(area,year))    

H_ayg <-
  H_ayg0 %>%
  filter(!(area %in% c("ALEUTIAN", "BERING", "IBS", "EYKT", "SOUTHEAS", "SOUTHWES", #get rid of areas contained in amalgamated areas
                       "SAKPEN", "CHIGNIK", "SKMA", "WESTSIDE", "MAINLAND",
                       "SOUTHEAST","SOUTHWEST"))) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         area = ifelse(area == "northeas", "northeast", area)) %>%
  left_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
  arrange(region, area, year)

table(H_ayg$region, H_ayg$area)

H_ayg %>% filter(is.na(area))

H_ayg %>%
  ggplot(aes(x = year, y = H, color = area)) +
    geom_line() +
    facet_grid(region ~ .)

H_ayg %>% filter(region == "Kodiak")

saveRDS(H_ayg, ".\\data\\bayes_dat\\H_ayg.rds")

# -Logbook release data: -------------------------------------------------------
R_ayg0 <- #logbook harvest by area, user = guided, year
  read.csv(paste0("data/raw_dat/logbook_release_thru",REP_YR,".csv")) %>% 
  select(-c(Region,not_ye_nonpel_rel))

colnames(R_ayg0)
colnames(R_ayg0) <- c("year", "area", "R", "Rp", "Rnp", "Rye")

table(R_ayg0$Rye[R_ayg0$year <= 2005], useNA = "always") #these should all be NA
R_ayg0$Rye[R_ayg0$year <= 2005] <- NA

table(R_ayg0$area)
R_ayg0 %>%
  group_by(area) %>%
  summarise(R = sum(R)) %>%
  print(n = 100)

R_ayg0 %>% filter(is.na(area))

R_ayg0 %>% filter(year > 2019 & area %in% c("BSAI","ALEUTIAN","BERING",
                                            "EWYKT","IBS","EKYT",
                                            "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                                            "WKMA","WESTSIDE","MAINLAND"))

# Identify where logbook data amalgamations need to be done. They were not done
# for 2 of the 4 amalgamated areas in 2022 and 2023 so keep an eye on this: 
R_ayg0 %>% mutate(AMALG = ifelse(area %in% c("ALEUTIAN","BERING"),"BSAI",
                                 ifelse(area %in% c("IBS","EYKT"),"EWYKT",
                                        ifelse(area %in% c("SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK"),"SOKO2SAP",
                                               ifelse(area %in% c("WESTSIDE","MAINLAND"),"WKMA",NA))))) %>%
  filter(!is.na(AMALG)) %>%
  group_by(year,AMALG) %>%
  summarise(R = sum(R, na.rm = T),
            Rp = sum(Rp, na.rm = T),
            Rnp = sum(Rnp, na.rm = T),
            Rye = sum(Rye, na.rm = T)) -> a_check2

R_ayg0 %>% filter(area %in% c("BSAI","ALEUTIAN","BERING",
                              "EWYKT","IBS","EYKT",
                              "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                              "WKMA","WESTSIDE","MAINLAND")) %>%
  bind_rows(a_check2 %>% mutate(area = AMALG) %>% select(-AMALG)) %>%
  arrange(year,area) -> amalg2; amalg2

with(amalg2 %>%
       filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
     table(area,year)) # These should all be 2's if the amalgamated areas are already done
                       # If they are 1's then they aren't yet in the data set and
                       # need to be added in. 

#!!! BSAI and SOKO2SAP missing in all years so need to add them back in
R_ayg0 %>% bind_rows(amalg2 %>% filter(area %in% c("BSAI","SOKO2SAP"))) %>%
  arrange(area, year) -> R_ayg0

#double check... 
with(R_ayg0 %>%
       filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
     table(area,year))    

R_ayg <-
  R_ayg0 %>%
  filter(!(area %in% c("ALEUTIAN", "BERING", "IBS", "EYKT", "SOUTHEAS", "SOUTHWES", #get rid of areas contained in amalgamated areas
                       "SAKPEN", "CHIGNIK", "SKMA", "WESTSIDE", "MAINLAND",
                       "SOUTHEAST","SOUTHWEST"))) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         area = ifelse(area == "northeas", "northeast", area)) %>%
  left_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
  arrange(region, area, year)

table(R_ayg$region, R_ayg$area)

R_ayg %>% filter(is.na(area))

R_ayg %>%
  ggplot(aes(x = year, y = R, color = area)) +
  geom_line() +
  facet_grid(region ~ .)

R_ayg %>% filter(region == "Kodiak")

saveRDS(R_ayg, ".\\data\\bayes_dat\\R_ayg.rds")

#-------------------------------------------------------------------------------
# SWHS Data
#-------------------------------------------------------------------------------
# PRE-1996 DATA:
#------------------------
#need to retain unknowns to apportion out in model...
lut_pre96 <- lut %>%
  add_row(region = "Unknown", area = "UNKNOWN")

Hhat_ay77to95 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\SWHS_1977_1995_rf_estimates_sent20240813.xlsx"), 
                           sheet = "harvest",
                           range = "A4:Q23") %>%
  mutate_at(c("NORTHEAST","IBS","AFOGNAK","SSEO","BERING_AND_ALEUTIAN","NSEO",
              "WESTSIDE","EASTSIDE"),as.numeric) %>%
  rename("BSAI" = "BERING_AND_ALEUTIAN") %>%
  rowwise() %>%
  mutate(EWYKT = IBS, #No EKYT in pre-96
         WKMA = WESTSIDE,  #no mainland 
         PWSO = NA, # no PWSO in pre-96
         SOKO2SAP = NA) %>% # no southeast, no southwest, no sakpen, no chignik in pre-96) %>% 
  pivot_longer(!year, names_to = "area", values_to = "H")  %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area),area)) %>%
#need to retain unknowns to apportion out in model...
  right_join(lut_pre96, by = "area") %>%
  mutate(area = factor(area, lut_pre96$area, ordered = TRUE)) %>% 
  arrange(region, area, year)

table(Hhat_ay77to95$region, Hhat_ay77to95$area)

with(Hhat_ay77to95, table(area, year))

Chat_ay77to95 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\SWHS_1977_1995_rf_estimates_sent20240813.xlsx"), 
            sheet = "catch",
            range = "A4:Q10") %>%
  mutate_at(c("NORTHEAST","IBS","AFOGNAK","SSEO","BERING_AND_ALEUTIAN","NSEO",
              "WESTSIDE","EASTSIDE"),as.numeric) %>%
  rename("BSAI" = "BERING_AND_ALEUTIAN") %>%
  rowwise() %>%
  mutate(EWYKT = IBS, #No EKYT in pre-96
         WKMA = WESTSIDE,  #no mainland 
         PWSO = NA, # no PWSO in pre-96
         SOKO2SAP = NA) %>% # no southeast, no southwest, no sakpen, no chignik in pre-96) %>% 
  pivot_longer(!year, names_to = "area", values_to = "C")  %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area),area)) %>%
  #need to retain unknowns to apportion out in model...
  right_join(lut_pre96, by = "area") %>%
  mutate(area = factor(area, lut_pre96$area, ordered = TRUE)) %>% 
  arrange(region, area, year)

table(Chat_ay77to95$region, Chat_ay77to95$area)

with(Chat_ay77to95, table(area, year))

#------------------------
# CONTEMPORARY SWHS DATA but NO USER GROUPS:
#------------------------
# what is the name of this year's data file?
swhs_dat <- "rf_byMgmtUnit_sent20240925.xlsx"

# Hhat_ay data ----
Hhat_ay0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "RF_har",
            range = "A4:Q19") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)

table(Hhat_ay0$region, Hhat_ay0$area)
with(Hhat_ay0, table(area, year))

seHhat_ay <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "Har_se",
            range = "A4:Q19") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
  arrange(region, area, year)
table(seHhat_ay$region, seHhat_ay$area)

Hhat_ay <- left_join(Hhat_ay0, seHhat_ay, by = c("year", "area", "region"))

#bind with older data. Apply maximum observed cv per region to past
Hhat_ay <- rbind(Hhat_ay77to95 %>% mutate(seH = NA),
                 Hhat_ay) %>%
  mutate(cv = seH / H) %>%
  group_by(area) %>% 
  mutate(seH = ifelse(is.na(seH),max(cv,na.rm=T) * H,seH),
         seH = ifelse(is.infinite(seH),0.75 * H, seH)) %>% 
  ungroup() %>% 
  mutate(cv = seH / H) %>% arrange(region,area,year) #%>% select(-cv)

Hhat_ay %>%
  select(year, area, region, H, seH) %>%
  ggplot(aes(year, H)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")

Hhat_ay %>%
  select(year, area, region, H, seH, cv) %>%
  ggplot(aes(year, cv)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")

Hhat_ay %>% filter(area == 'SSEO') %>% print(n = 50)

#save it
saveRDS(Hhat_ay, ".\\data\\bayes_dat\\Hhat_ay.rds")

Hhat_ay %>% filter (area == "UNKNOWN")
# Chat_ay data ----
Chat_ay0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "RF_cat",
            range = "A4:Q19") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "C") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Chat_ay0$region, Chat_ay0$area)

seChat_ay <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "Cat_se",
            range = "A4:Q19") %>%
  pivot_longer(!year, names_to = "area", values_to = "seC") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ay$region, seChat_ay$area)

Chat_ay <- left_join(Chat_ay0, seChat_ay, by = c("year", "area", "region"))

#bind with older data. Assume that CV on older data is 0.2
Chat_ay <- rbind(Chat_ay77to95 %>% mutate(seC = NA),
                 Chat_ay) %>% 
  mutate(cv = seC / C) %>%
  group_by(area) %>% 
  mutate(seC = ifelse(is.na(seC),max(cv,na.rm=T) * C, seC),
         seC = ifelse(is.infinite(seC),0.75 * C, seC)) %>% 
  ungroup() %>% 
  mutate(cv = seC / C) %>%
  arrange(region,area,year)
#save it
Chat_ay %>% filter (area == "UNKNOWN")
saveRDS(Chat_ay, ".\\data\\bayes_dat\\Chat_ay.rds")

#------------------------
# CONTEMPORARY SWHS DATA WITH USER GROUPS:
#------------------------
# Hhat_ayu data ----
Hhat_ayg0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "gui_har",
            range = paste0("A4:Q",REP_YR - 2006)) %>% 
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)

table(Hhat_ayg0$region, Hhat_ayg0$area)
with(Hhat_ayg0, table(area, year))

seHhat_ayg <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "guihar_se",
            range = paste0("A4:Q",REP_YR - 2006)) %>% #CHANGE THIS TO GET ALL DATA IN NEW YEAR!!!! 
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seHhat_ayg$region, seHhat_ayg$area)

Hhat_ayg <- left_join(Hhat_ayg0, seHhat_ayg, by = c("year", "area", "region", "user"))

Hhat_ayp0 <- #p for private
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "pri_har",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Hhat_ayp0$region, Hhat_ayp0$area)

seHhat_ayp <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "prihar_se",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seHhat_ayp$region, seHhat_ayp$area)

Hhat_ayp <- left_join(Hhat_ayp0, seHhat_ayp, by = c("year", "area", "region", "user"))
Hhat_ayu <- rbind(Hhat_ayg, Hhat_ayp)
saveRDS(Hhat_ayu, ".\\data\\bayes_dat\\Hhat_ayu.rds")


# Chat_ayu data ----
Chat_ayg0 <- #p for private
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "gui_cat",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "C") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Chat_ayg0$region, Chat_ayg0$area)

seChat_ayg <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "guicat_se",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  pivot_longer(!year, names_to = "area", values_to = "seC") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ayg$region, seChat_ayg$area)

Chat_ayg <- left_join(Chat_ayg0, seChat_ayg, by = c("year", "area", "region", "user"))

Chat_ayp0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "pri_cat",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "C") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Chat_ayp0$region, Chat_ayp0$area)

seChat_ayp <- 
  read_xlsx(paste0(".\\data\\raw_dat\\",REP_YR,"\\",swhs_dat), 
            sheet = "pricat_se",
            range = paste0("A4:Q",REP_YR - 2006)) %>%
  pivot_longer(!year, names_to = "area", values_to = "seC") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ayp$region, seChat_ayp$area)

Chat_ayp <- left_join(Chat_ayp0, seChat_ayp, by = c("year", "area", "region", "user"))
Chat_ayu <- rbind(Chat_ayg, Chat_ayp)
saveRDS(Chat_ayu, ".\\data\\bayes_dat\\Chat_ayu.rds")


#-------------------------------------------------------------------------------
# Species comp / port sampling data ----
#-------------------------------------------------------------------------------

sppcompR1_0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_Region1_forR_",REP_YR,".FINAL.xlsx"), 
            range = "A1:I1000") %>%
  rename_all(.funs = tolower) %>%
  mutate(user = tolower(user)) %>%
  rename(area = rpt_area) %>% 
  filter_all(any_vars(!is.na(.))) %>%
  mutate_at(c("totalrf_n","ye_n","black_n","pelagic_n","nonpel_n","notye_nonpel_n"),as.numeric)

unique(sppcompR1_0$user)

table(sppcompR1_0$area)

str(sppcompR1_0)
#Note EKYKT = IBS + EKYT ; checking , should be all TRUE
sppcompR1_0$totalrf_n[sppcompR1_0$area == "EWYKT"] == sppcompR1_0$totalrf_n[sppcompR1_0$area == "EYKT"] + sppcompR1_0$totalrf_n[sppcompR1_0$area == "IBS"]

sppcompR1 <- 
  sppcompR1_0 %>% 
  right_join(lut[lut$region == "Southeast",], by = "area") %>% #drops IBS and EYKT
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(sppcompR1$area)

sppcompR2_0 <- read.csv(paste0(".\\data\\raw_dat\\species_comp_SC\\species_comp_Region2_thru",REP_YR,".csv")) %>%
#  read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SC\\species_comp_Region2_thru",REP_YR,".csv"), 
#            range = "A1:I433") %>%
  rename_all(.funs = tolower) %>%
  rename(area = rpt_area) %>%
  select(area,year,user,totalrf_n,ye_n,black_n,pelagic_n,nonpel_n,notye_nonpel_n)

unique(sppcompR2_0$user)

table(sppcompR2_0$area)

with(sppcompR2_0, table(area,user))

sppcompR2_0 %>% filter(area == "EASTSIDE")
#Note no samples from SOKO2SAP (= southeast + southwest + sakpen + chignik)
#Note no samples from BSAI (= aleutian + bering)
#Note only westside from WKMA (= westside + mainland)
sppcompR2 <- 
  sppcompR2_0%>%
  rbind(data.frame(area = rep(c("SOKO2SAP", "BSAI", "WKMA"), each = 2 * length(unique(sppcompR2_0$year))),
                   year = rep(unique(sppcompR2_0$year), times = 2 * 3),
                   user = rep(rep(c("charter", "private"), each = length(unique(sppcompR2_0$year))), times = 3),
                   totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA)) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut[lut$region != "Southeast",], by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(sppcompR2$region, sppcompR2$area)

S_ayu <- 
  rbind(sppcompR1, sppcompR2) %>%
  mutate_at(vars(ye_n:notye_nonpel_n), .funs = function(x){x = ifelse(.$totalrf_n == 0, NA, x)}) %>%
  arrange(user, area, year)
table(S_ayu$region, S_ayu$area)
saveRDS(S_ayu, ".\\data\\bayes_dat\\S_ayu.rds")
