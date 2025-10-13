################################################################################
# Data preparation for the Bayesian sport fish harvest estimation model
#
# Author: Phil Joy & Adam Reimer
# 
# Last updated: Oct 2025
#
# This script will take the raw and semi-processed rockfish data and prepare
# it for use in the model
#
################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)

REP_YR <- 2024

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
  select(-c(Region))
  #select(-c(Region,not_ye_nonpel_harv))
  
colnames(H_ayg0)
colnames(H_ayg0) <- c("year", "area", "H", "Hp", "Hnp", "Hye", "Ho")

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
         Hye = sum(Hye, na.rm = T),
         Ho = sum(Ho, na.rm=T)) -> a_check

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

#!!! BSAI and SOKO2SAP missing in 2022 and 2023 and 2024 so need to add them back in
H_ayg0 %>% bind_rows(amalg %>% filter(year %in% c(2022,2023,2024) & area %in% c("BSAI","SOKO2SAP"))) %>%
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

saveRDS(H_ayg, ".\\data\\bayes_dat\\H_ayg.rds")

# -Logbook release data: -------------------------------------------------------
R_ayg0 <- #logbook harvest by area, user = guided, year
  read.csv(paste0("data/raw_dat/logbook_release_thru",REP_YR,".csv")) %>% 
  #select(-c(Region,not_ye_nonpel_rel))
  select(-c(Region))

colnames(R_ayg0)
colnames(R_ayg0) <- c("year", "area", "R", "Rp", "Rnp", "Rye","Ro")

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

saveRDS(R_ayg, ".\\data\\bayes_dat\\R_ayg.rds")

#-------------------------------------------------------------------------------
# SWHS Data
#-------------------------------------------------------------------------------
# PRE-1996 DATA:
# 
#------------------------
#need to retain unknowns to apportion out in model...
lut_pre96 <- lut %>%
  add_row(region = "Southeast", area = "UNKNOWN_R1") %>%
  add_row(region = "Unknown_R2", area = "UNKNOWN_R2") %>%
  add_row(region = "Central", area = "PWSO_I")

Hhat_ay77to95 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\SWHS_1977_1995_rf_estimates_sent20241107.xlsx"), 
                           sheet = "harvest",
                           range = "A4:T23") %>%
  mutate_at(c("UNKNOWN_R1","UNKNOWN_R2","EWYKT","BSAI","SOKO2SAP","NSEO","AFOGNAK",
              "SSEO","PWSO_I","WKMA","EASTSIDE"),as.numeric) %>%
  rowwise() %>%
  mutate(PWSO = NA) %>% # no southeast, no southwest, no sakpen, no chignik in pre-96) %>% 
  pivot_longer(!year, names_to = "area", values_to = "H")  %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area),area)) %>%
#need to retain unknowns to apportion out in model...
  right_join(lut_pre96, by = "area") %>%
  mutate(area = factor(area, lut_pre96$area, ordered = TRUE)) %>% 
  arrange(region, area, year)

table(Hhat_ay77to95$region, Hhat_ay77to95$area)

with(Hhat_ay77to95, table(area, year))

Chat_ay77to95 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\SWHS_1977_1995_rf_estimates_sent20241107.xlsx"), 
            sheet = "catch",
            range = "A4:R10") %>%
  mutate_at(c("WKMA","EASTSIDE"),as.numeric) %>%
  rowwise() %>%
  mutate(PWSO = NA) %>% # no southeast, no southwest, no sakpen, no chignik in pre-96) %>% 
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
#swhs_dat <- "rf_byMgmtUnit_sent20240925.xlsx" #2023
swhs_dat <- "rf_byMgmtUnit_sent20250916.xlsx" #2024

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

#-------------------------------------------------------------------------------
#Get contemporary Chat 
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# Apportion pre1996 unknown area data based on 1996 data
# Note: Ideally this would be done in the estimation model to propogate the
# uncertainty, but while developing the base model we'll do it outside the model
# here. -pj

#-------------------------------------------------------------------------------

Hhat96 <- Hhat_ay %>% filter(year == 1996)

H_Appors <- Hhat96 %>% 
  mutate(region = ifelse(region %in% c("Kodiak","Central"),"Unknown_R2",region)) %>% #include Kodiak in Region 2 unknowns
  group_by(region) %>%
  mutate(H_tot = sum(H),
         p = H / H_tot) %>% 
  select(-c(year,H,seH,H_tot)) %>%
  full_join(Hhat_ay77to95 %>% filter(area %in% c("UNKNOWN_R1","UNKNOWN_R2")) %>%
              rename(areax = area) %>%
              rename(H_unk = H),
            by = "region") %>% 
  #mutate(H = p * H_unk) %>% 
  full_join(Hhat_ay77to95 %>% filter(area %in% c("PWSO_I") & !is.na(H)) %>%
              rename(areax = area) %>%
              rename(H_unk = H) %>%
              mutate(PWS_IO_ratio = Hhat96$H[Hhat96$area == "PWSI"] / 
                       (Hhat96$H[Hhat96$area == "PWSO"] + Hhat96$H[Hhat96$area == "PWSI"]),
                     PWSI = H_unk * PWS_IO_ratio,
                     PWSO = H_unk * (1 - PWS_IO_ratio)) %>%
              select(-c(areax,H_unk,region,PWS_IO_ratio)) %>%
              pivot_longer(cols = c("PWSI","PWSO"),
                           names_to = "area",
                           values_to = "H_pwsio"),
            by = c("area","year")) %>% 
  rowwise() %>%
  mutate(H = sum(p * H_unk , H_pwsio, na.rm = T)) %>% 
  select(year,area,region,H) %>%
  mutate(region = ifelse(area %in% c("CI","NG","PWSI","PWSO"),"Central",
                         ifelse(area %in% c("afognak","eastside","northeast","BSAI","SOKO2SAP","WKMA"),"Kodiak",region))) %>%
  filter(H > 0) #%>%
  

rbind(Hhat_ay77to95 %>% filter(area != "UNKNOWN_R1" & area != "UNKNOWN_R2" & 
                                 area != "PWSO_I"),
      H_Appors) %>%
  group_by(year,area,region) %>%
  summarise(H = sum(H, na.rm = T)) -> Hhat_ay77to95
  
unique(Hhat_ay77to95$area)
#
#bind with older data. Apply maximum observed cv per region to past
#
Hhat_ay <- rbind(Hhat_ay77to95 %>% mutate(seH = NA) %>% data.frame(),
                 Hhat_ay) %>%
  mutate(cv = seH / H) %>%
  group_by(area) %>% 
  mutate(seH = ifelse(is.na(seH),max(cv,na.rm=T) * H,seH),
         seH = ifelse(is.infinite(seH),0.75 * H, seH)) %>% 
  ungroup() %>% 
  mutate(cv = seH / H,
         H = round(H,0)) %>% arrange(region,area,year) #%>% select(-cv)

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

#
# Apportion pre1996 Cahtches:
#
Chat96 <- Chat_ay %>% filter(year == 1996)

C_Appors <- Chat_ay77to95 %>% filter(area %in% c("PWSO_I") & !is.na(C)) %>%
              rename(areax = area) %>%
              rename(C_unk = C) %>%
              mutate(PWS_IO_ratio = Chat96$C[Chat96$area == "PWSI"] / 
                       (Chat96$C[Chat96$area == "PWSO"] + Chat96$C[Chat96$area == "PWSI"]),
                     PWSI = C_unk * PWS_IO_ratio,
                     PWSO = C_unk * (1 - PWS_IO_ratio)) %>%
              select(-c(areax,C_unk,region,PWS_IO_ratio)) %>%
              pivot_longer(cols = c("PWSI","PWSO"),
                           names_to = "area",
                           values_to = "C") %>%
  mutate(region = "Central")#,
            #by = c("area","year")) %>% filter(area %in% c("PWSI"))
rbind(Chat_ay77to95 %>% filter(area != "UNKNOWN_R1" & area != "UNKNOWN_R2" & 
                                 area != "PWSO_I"),
      C_Appors) %>%
  group_by(year,area,region) %>%
  summarise(C = sum(C, na.rm = T)) ->  Chat_ay77to95 

Chat_ay77to95 %>% print(n = 100)

unique(Chat_ay77to95$area)

#bind with older data. Assume that CV on older data is 0.2
Chat_ay <- rbind(Chat_ay77to95 %>% mutate(seC = NA) %>% data.frame(),
                 Chat_ay) %>% 
  mutate(cv = seC / C) %>%
  group_by(area) %>% 
  mutate(seC = ifelse(is.na(seC),max(cv,na.rm=T) * C, seC),
         seC = ifelse(is.infinite(seC),0.75 * C, seC)) %>% 
  ungroup() %>% 
  mutate(cv = seC / C,
         C = round(C, 0)) %>%
  arrange(region,area,year)
#save it
Chat_ay %>% filter (area == "UNKNOWN")

Chat_ay %>%
  select(year, area, region, C, seC) %>%
  ggplot(aes(year, C)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")

Chat_ay %>%
  select(year, area, region, C, seC, cv) %>%
  ggplot(aes(year, cv)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")

saveRDS(Chat_ay, ".\\data\\bayes_dat\\Chat_ay.rds")

unique(Chat_ay$year)
#-------------------------------------------------------------------------------
# CONTEMPORARY SWHS DATA WITH USER GROUPS:
#-------------------------------------------------------------------------------
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
# Southeast: When entering data in 2025 for the 2024 estimates we found a large
# discrepency in the SE data between the two years. Going forward it will be 
# necessary to check their data until their system is worked out to be trustworthy.
S_ayu_ly <- readRDS(".//data//bayes_dat//S_ayu.rds") #last year's data
table(S_ayu_ly$region, S_ayu_ly$area)
table(S_ayu_ly$year, S_ayu_ly$area)

#2023 data set
S_ayu_ly2 <-cbind(read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_Region1_forR_",REP_YR-1,".FINAL.xlsx"), #2023
              range = c("A1:I1000")),
    read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_Region1_forR_",REP_YR-1,".FINAL.xlsx"), 
              range = c("AH1:AH1000")),
    read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_Region1_forR_",REP_YR-1,".FINAL.xlsx"), 
              range = c("AQ1:AQ1000")) ) %>%
  rename_all(.funs = tolower) %>%
  mutate(user = tolower(user)) %>%
  rename(area = rpt_area) %>% 
  filter_all(any_vars(!is.na(.)))

#2024 data set
sppcompR1_0 <- 
  read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_MHS_Region1_forR_",REP_YR,"_RUN_08-Oct-2025.xlsx"), 
            range = c("A1:N1000"),
            sheet = "MHS num Fish")  %>%
  rename_all(.funs = tolower) %>%
  mutate(user = tolower(user)) %>%
  rename(area = rpt_area) %>% 
  filter_all(any_vars(!is.na(.))) %>%
#  select(-c("totalrf_n_rel","totalrf_n_res","totalrf_n_nonres")) %>%
  select(-c("totalrf_n_rel")) %>%
  mutate_at(c("totalrf_n","ye_n","black_n","pelagic_n","nonpel_n",
              "notye_nonpel_n","dsr_n","slope_n",
              "pelnbrf_n","dsrnye_n"),as.numeric)

# compare the data:
colnames(sppcompR1_0)
table(sppcompR1_0$year, sppcompR1_0$area, sppcompR1_0$user)

table(S_ayu_ly$year, S_ayu_ly$area, S_ayu_ly$user)

SE_ly <- S_ayu_ly %>% filter(region == "Southeast")


#As of 9/29/25 the new SE data is fucked up. For now I may just patch on the new
# As of 10/9/2025 the SE crew has thing fixed and this next bracketed section can 
# be skipped. Leaving it in for now for next year to check data stays consistent
{
  # 2024 data to the old data through 2023.
  as <- unique(sppcompR1_0$area)
  us <- unique(sppcompR1_0$user)
  
  for (a in as){ #a <-as[1]
    for (u in us){ #u <- us[1]
      eg24 <- sppcompR1_0 %>% filter(area == a & user == u) %>%
        select(-c(slope_lg_n, slope_sm_n,dsrnye_n,pelnbrf_n,notye_nonpel_n)) %>% data.frame()
      eg23 <- S_ayu_ly2 %>% filter(area == a & user == u) %>%
        mutate(across(4:11, as.numeric)) %>% select(-notye_nonpel_n)
      #str(eg24); str(eg23)
      setdiff(eg23,eg24) -> abs_fr_24_x
      setdiff(eg24,eg23) -> new_in_24_x
      
      #abs_fr_24 <- abs_fr_24 %>%
      #   filter(!if_all(4:11, ~ .x == 0 | is.na(.x)))
      
      if (a == as[1] & u == us[1]){
        abs_fr_24 <- abs_fr_24_x
        new_in_24 <- new_in_24_x
      } else {
        abs_fr_24 <- rbind(abs_fr_24_x,abs_fr_24) %>%
          arrange(area,user,year)
        new_in_24 <- rbind(new_in_24_x,new_in_24)
      }
    }
  }
  
  rbind(abs_fr_24 %>% mutate(dat_yr = 2023, cat = "'23 data absent from '24 data",
                             data_source = "Species_comp_Region1_forR_2023.FINAL"),
        new_in_24 %>% mutate(dat_yr = 2024, cat = "new in '24, absent/different in '23 data",
                             data_source = "Species_comp_MHS_Region1_forR_2024_RUN_30-Sep-2025")) %>%
    arrange(area,user,year) %>%
    filter(year != 2024) -> datcomp
  
  #save the discrepencies to verify with Region 1 staff
  write.csv(datcomp,"data/raw_dat/Species_comp_SE/Region1_data_discrepencies.csv", row.names = FALSE)
}

str(sppcompR1_0)
#Note EKYKT = IBS + EKYT ; checking , should be all TRUE
# Not recorded in new SE data in 2025 (for 2024 data calcs)
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
  select(area,year,user,totalrf_n,ye_n,black_n,pelagic_n,nonpel_n,notye_nonpel_n) %>%
  mutate(dsr_n = NA, slope_n = NA)

unique(sppcompR2_0$user)

table(sppcompR2_0$area)

with(sppcompR2_0, table(area,user))

# Noted in 2024 that there were some Reg2 based charters fishing in the EWYKT:
sppcompR2_0 %>% filter(area %in% c("EYKT","IBS"))
# remove them from Region 2 and add to region 1:
sppcompR2_0 %>% filter(area %in% c("EYKT","IBS")) %>%
  summarize(year = "2024", user = "charter", area = "EWYKT", region = "Southeast",
            totalrf_n = sum(totalrf_n),
            ye_n = sum(ye_n),
            black_n = sum(black_n),
            pelagic_n = sum(pelagic_n),
            nonpel_n = sum(nonpel_n),
            notye_nonpel_n =  sum(notye_nonpel_n),
            dsr_n = 0,slope_n = 0,
            pelnbrf_n = pelagic_n - black_n,
            #slope_sm_n = 0, slope_lg_n = 0,
            dsrnye_n = 0) -> R1fish_in_R2ports

sppcompR1 %>% filter(area == "EWYKT" & year == REP_YR & user == "charter") -> R1_EWYKT

coldifs <- setdiff(colnames(sppcompR1),colnames(R1fish_in_R2ports)); coldifs

rbind(R1_EWYKT,
      R1fish_in_R2ports) %>%
  summarize(year = as.numeric("2024"), user = "charter", area = "EWYKT", region = "Southeast",
            totalrf_n = sum(totalrf_n),
            ye_n = sum(ye_n),
            black_n = sum(black_n),
            pelagic_n = sum(pelagic_n),
            nonpel_n = sum(nonpel_n),
            notye_nonpel_n =  sum(notye_nonpel_n),
            dsr_n = sum(dsr_n),
            slope_n = sum(dsr_n),
            pelnbrf_n = sum(pelnbrf_n),
            #slope_sm_n = sum(slope_sm_n), 
            #slope_lg_n = sum(slope_lg_n),
            dsrnye_n = sum(dsrnye_n)) -> patch

sppcompR1 %>%
  rows_update(patch, by = c("year","user","area")) -> try
sppcompR1 %>% filter(area == "EWYKT" & year == REP_YR & user == "charter")
try %>% filter(area == "EWYKT" & year == REP_YR & user == "charter")

sppcompR1 <- try

sppcompR2_0 %>% filter(area == "EASTSIDE")
#Note no samples from SOKO2SAP (= southeast + southwest + sakpen + chignik)
#Note no samples from BSAI (= aleutian + bering)
#Note only westside from WKMA (= westside + mainland)
sppcompR2 <- 
  sppcompR2_0%>%
  rbind(data.frame(area = rep(c("SOKO2SAP", "BSAI", "WKMA"), each = 2 * length(unique(sppcompR2_0$year))),
                   year = rep(unique(sppcompR2_0$year), times = 2 * 3),
                   user = rep(rep(c("charter", "private"), each = length(unique(sppcompR2_0$year))), times = 3),
                   totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA,
                   dsr_n = NA, slope_n = NA)) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut[lut$region != "Southeast",], by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(sppcompR2$region, sppcompR2$area)

S_ayu <- 
  rbind(sppcompR1 %>%
          select(-c(pelnbrf_n,dsr_n)) %>%
          rename(dsr_n = dsrnye_n), 
#          select(-c(slope_lg_n,slope_sm_n,pelnbrf_n,dsrnye_n)), 
        sppcompR2) %>%
  mutate_at(vars(ye_n:notye_nonpel_n), .funs = function(x){x = ifelse(.$totalrf_n == 0, NA, x)}) %>%
  mutate(year = as.character(year)) %>%
  arrange(user, area, year)

table(S_ayu$region, S_ayu$area)

saveRDS(data.frame(S_ayu), ".\\data\\bayes_dat\\S_ayu.rds")


#-------------------------------------------------------------------------------
# Kodiak hydroacoustic supplemental data  ----
#-------------------------------------------------------------------------------
#Note no samples from SOKO2SAP (= southeast + southwest + sakpen + chignik)
#Note no samples from BSAI (= aleutian + bering)
#Note only westside from WKMA (= westside + mainland)

kha <- read.csv("data/raw_dat/kodiak_stereocom_dat.csv") %>%
  clean_names() %>%
  mutate(district = tolower(district),
         area = ifelse(district %in% c("southeast","southwest","chignik"),"SOKO2SAP",
                       ifelse(district %in% c("westside","mainland"),"WKMA",
                              ifelse(district == "shumagins","BSAI",district))),
         across(where(is.character), ~ ifelse(grepl("^[0-9,]+$", .), as.numeric(gsub(",", "", .)), .)),
         percent_brf = percent_brf/100,
         rf_var = se_rf_abund ^ 2,
         brf_var = se_brf_abund ^ 2)

kha %>% group_by(year,area) %>%
  summarise(rf_tot = sum(rf_abund),
            brf_tot = sum(brf_abund),
            n_stations = sum(n_stations),
            rf_var = sum(rf_var),
            brf_var = sum(brf_var),
            rf_cv = sqrt(rf_var) / rf_tot,
            brf_cv = sqrt(brf_var) / brf_tot,
            cov = ifelse(is.na(cov(rf_abund, brf_abund, use = "complete.obs")),
                         0, cov(rf_abund, brf_abund, use = "complete.obs")),
            #cov = cov(rf_abund,brf_abund),
            prop_brf = brf_tot / rf_tot,
            prop_var = ((1/rf_tot)^2) * brf_var +
              ((brf_tot/(rf_tot^2))^2) * rf_var, #- 2 * (brf_tot / (rf_tot^3)) * cov
            prop_var2 = pmax(
              (1 / rf_tot^2) * brf_var +
                (brf_tot^2 / rf_tot^4) * rf_var -
                2 * (brf_tot / rf_tot^3) * cov,
              0
            ),
            prop_se = sqrt(prop_var),
            prop_cv = prop_se / prop_brf) -> kha

print(kha,n=70)

saveRDS(kha, ".\\data\\bayes_dat\\kha.rds")

#-------------------------------------------------------------------------------
# Release mortality and Biomass data
#-------------------------------------------------------------------------------
# Note that the 2 regions approach things a bit differently.
# Southcentral factors in long term estimates of depth-at-release data to adjust
# their mortality esimtates while southeast applies a standard rate base on year
# when dwr mechanisms were mandated. 
# Secondly, southeast divides their slope numbers into large and small slope rf.
ly_wt <- readRDS(".//data//bayes_dat//wt_rm_dat.rds") %>%
  mutate(assemblage = factor(assemblage, 
                             levels = c("black","yelloweye","pelnbrf","dsrlessye","slope"))) %>%
  arrange(assemblage, user,region, area, year) 

#Running 2023 numbers in 2024
# Southcentral data:
#sc_wt_rm <- read.csv("data/raw_dat/Species_comp_SC/rf_mort_sc.csv") %>% clean_names() %>%
#  mutate(#cfmu = tolower(cfmu),
#         user = tolower(user),
#         assemblage = tolower(assemblage))

#sc_wt_rm %>% group_by(year, cfmu, assemblage, user) %>%
#  summarize(wt_kg = weighted.mean(avg_wt_kg,p_rel,na.rm=T),
#            wt_cv = ifelse(is.na(weighted.mean(se_wt_kg) / wt_kg),1,
#                           weighted.mean(se_wt_kg) / wt_kg),
#            r_mort = weighted.mean(mort_rate, p_rel, na.rm = T)) -> sc_wt_rm

# 2024 numbers in 2025:
sc_wt <- read.csv("data/raw_dat/Species_comp_SC/rf_mean_wt_SC.csv") %>% clean_names() %>%
  mutate(assemblage = ifelse(sp == 142,"black",
                             ifelse(sp == 145,"yelloweye","other")),
         wt_lbs = mean_wt * 2.20462262,
         wt_cv = sd_wt * 2.20462262 / wt_lbs,
         user = tolower(user)) %>%
  rename(area = cfmu) %>%
  right_join(lut %>% filter(region %in% c("Central","Kodiak")) %>%
               mutate(area = toupper(area)),
             by = "area") %>%
  filter(boats >= 4 & n_fish > 5) %>%
  select(-c(sp,mean_wt,sd_wt))

head(sc_wt)
unique(sc_wt$area)

sc_rm <- read.csv("data/raw_dat/Species_comp_SC/rf_mort_sc24.csv") %>% clean_names() %>%
  rename(area = cfmu) %>%
  select(year,assemblage,user,area,rel_cat,p_rel,pcat_surface,pcat_drm,mort_rate) %>%
  mutate(assemblage = tolower(assemblage),
         user = tolower(user)) %>% 
  group_by(year, area, assemblage, user) %>%
  summarize(r_mort = weighted.mean(mort_rate, p_rel, na.rm = T))

head(sc_rm); unique(sc_rm$area)

sc_rm %>% filter(is.na(r_mort))

ggplot(sc_rm, aes(x= year, y = r_mort, col = area, type = user)) +
  geom_line() +
  facet_wrap(~assemblage)

sc_rm %>% group_by(user,assemblage,area) %>% 
  summarize(mrate = r_mort[which.min(year)]) -> sc_mrates_oldest

sc_rm %>% group_by(user,assemblage,area) %>% 
  summarize(mrate_dwr = r_mort[which.max(year)]) -> sc_mrates_dwr

expand.grid(year = seq(1977,(max(sc_rm$year)),1),
            area = unique(sc_rm$area),
            assemblage = unique(sc_rm$assemblage),
            user = unique(sc_rm$user)) %>%
  left_join(sc_mrates_oldest, by = c("user","assemblage","area")) %>%
  left_join(sc_mrates_dwr, by = c("user","assemblage","area")) %>%
  full_join(sc_rm, by = c("user","assemblage","area","year")) %>%
  mutate(r_mort = ifelse(is.na(r_mort) & year < 2013,mrate,
                         ifelse(is.na(r_mort) & year > 2012,mrate_dwr,r_mort)),
         area = toupper(area)) %>%
  mutate(r_mort = ifelse(is.na(r_mort) & year < 2013,mrate,
                         ifelse(is.na(r_mort) & year > 2012,mrate_dwr,r_mort)),
         area = toupper(area)) %>%
  select(year,area,assemblage,user,r_mort) %>%
  right_join(lut %>% filter(region %in% c("Central","Kodiak")) %>%
               mutate(area = toupper(area)),
             by = "area") -> r2_rm

r2_rm <- rbind(r2_rm %>%
                 mutate(area = ifelse(area %in% c("AFOGNAK","EASTSIDE"),
                                      tolower(area),area)),
               r2_rm %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "BSAI"),
               r2_rm %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "SOKO2SAP"),
               r2_rm %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "WKMA"),
               r2_rm %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "afognak"),
               r2_rm %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "eastside")) -> r2_rm

sc_wt <- rbind(sc_wt %>%
                 mutate(area = ifelse(area %in% c("AFOGNAK","EASTSIDE"),
                                      tolower(area),area)),
               sc_wt %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "BSAI"),
               sc_wt %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "SOKO2SAP"),
               sc_wt %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "WKMA"),
               sc_wt %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "afognak"),
               sc_wt %>% filter(area == "NORTHEAST") %>%
                 mutate(area = "eastside")) %>%
  mutate(wt_lbs = ifelse(area %in% c("BSAI","SOKO2SAP","WKMA","afognak","eastside"),
                         NA,wt_lbs),
         wt_cv = ifelse(area %in% c("BSAI","SOKO2SAP","WKMA","afognak","eastside"),
                        1,wt_cv))

full_join(sc_wt,r2_rm,by = c("year","area","assemblage","user","region")) %>%
  mutate(wt_cv = ifelse(is.na(wt_cv),1,wt_cv)) -> r2_wt_rm

r2_wt_rm %>% filter(is.na(r_mort) & year <= REP_YR) -> mismort; mismort

ggplot(r2_wt_rm, aes(x= year, y = r_mort, col = area)) +
  geom_line() + geom_point() +
  facet_wrap(~assemblage + user)

r2_wt_rm %>% filter(is.na(r_mort) & year <= REP_YR) -> damnit; damnit

r2_wt_rm %>% filter(user == damnit$user, area == damnit$area,year == damnit$year, assemblage == damnit$assemblage)

r2_wt_rm %>% filter(area == "BSAI" & year == 2017)

#Southeast data
se_wt <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024_RUN_08-Oct-2025.xlsx"), 
#se_wt <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_.xlsx"), 
                       sheet = "Wt matrix",
                       range = paste0("A1:G2000")) %>% clean_names() %>%
  mutate(cfmu = rpt_area, #tolower(rpt_area),
         user = tolower(user),
         assemblage = tolower(assemblage)) %>%
  select(-rpt_area) %>%
  filter(rowSums(is.na(.)) < ncol(.))

unique(se_wt$cfmu)
unique(se_wt$assemblage)

se_rm <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_2024_RUN_08-Oct-2025.xlsx"), 
#se_rm <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_.xlsx"), 
                   sheet = "Mortality Rates",
                   range = paste0("A1:E2000")) %>% clean_names() %>%
  mutate(year = as.numeric(year),
         cfmu = rpt_area, #tolower(rpt_area),
         user = tolower(user),
         assemblage = tolower(assemblage)) %>%
  select(-rpt_area) %>%
  filter(rowSums(is.na(.)) < ncol(.))

unique(se_rm$cfmu)
unique(se_rm$assemblage)

#se_rm %>% mutate(assemblage = ifelse(assemblage %in% c("slope_lg","slope_sm"),
#                                     "slope",assemblage)) %>%
#  group_by(year,user,assemblage) %>%
#  summarize(mrate = mean(mrate, na.rm=T)) ->mrates

#se_slopes <- read_xlsx(paste0(".\\data\\raw_dat\\Species_comp_SE\\Species_comp_MHS_Region1_forR_.xlsx"), 
#                   sheet = "MHS num Fish",
#                   range = paste0("A1:R300")) %>% clean_names() %>%
#  mutate(year = as.numeric(year),
#         cfmu = rpt_area, #tolower(rpt_area),
#         user = tolower(user)) %>%
#  select(year,cfmu,user,slope_lg_n,slope_sm_n) %>%
#  pivot_longer(cols = c(slope_lg_n,slope_sm_n),
#               values_to = "slope_n",
#               names_to = "slope_size") %>%
#  mutate(assemblage = str_remove(slope_size,"_n")) %>%
#  select(-slope_size) %>%
#  filter(rowSums(is.na(.)) < ncol(.))

#print(se_slopes %>% filter(year > 2010) %>%
#        arrange(year,cfmu,user),n = 50)

mrates <- se_rm %>% group_by(year,user,assemblage) %>%
  reframe(mrate = mrate)

#with(sc_wt_rm, table(year,cfmu, assemblage, user))
full_join(se_rm,se_wt,by = c("year","user","assemblage","cfmu")) %>%
  #full_join(se_slopes,by = c("year","cfmu","user","assemblage")) %>%
  #mutate(slope_cat = ifelse(assemblage %in% c("slope_lg","slope_sm"),
  #                          assemblage,NA)) %>%
  mutate(#assemblage = ifelse(assemblage %in% c("slope_lg","slope_sm"),
        #                     "slope",assemblage),
         mean_wt_lbs = ifelse(num_wts < 5, NA, mean_wt_lbs),
         std_error_m_wt = ifelse(num_wts < 5, NA, std_error_m_wt),
         wt_cv = std_error_m_wt / mean_wt_lbs) %>%
  left_join(mrates %>% mutate(mrate2 = mrate) %>% select(-mrate),
            by = c("year","user","assemblage")) %>%
  mutate(mrate = ifelse(is.na(mrate),mrate2,mrate)) %>%
  group_by(assemblage,year,user,cfmu) %>%
  #mutate(wt_lbs = ifelse(assemblage != "slope",mean_wt_lbs,
  #                        weighted.mean(mean_wt_lbs,slope_n,na.rm=T)),
  #       wt_cv = ifelse(assemblage != "slope",wt_cv,
  #                       weighted.mean(wt_cv,slope_n,na.rm=T))) %>%
  mutate(wt_lbs = mean_wt_lbs,
         wt_cv = wt_cv) %>%
  #mutate(wt_lbs = ifelse(is.nan(wt_lbs),# & mean_wt_lbs > 0,
  #                       weighted.mean(mean_wt_lbs,num_wts,na.rm=T),
  #                       wt_lbs),
  #       wt_cv = ifelse(is.nan(wt_cv),# & mean_wt_lbs > 0,
  #                       weighted.mean(std_error_m_wt / mean_wt_lbs,num_wts,na.rm=T),
  #                      wt_cv)) %>%
  ungroup() %>% #filter(!(is.na(wt_lbs) & slope_n == 0)) -> gack #%>% 
  select(year,cfmu,assemblage,user,wt_lbs,wt_cv,mrate) %>%
  unique() %>% filter(!is.na(user))-> se_wt_rm

print(se_wt_rm %>% filter(assemblage == "slope" & cfmu == "EWYKT") %>%
        arrange(user,year),n=80) 
with(se_wt_rm %>% filter(assemblage == "slope" & cfmu == "EWYKT"),
     table(year,user))

#fill in release mortalities back to 1977

se_wt_rm %>% group_by(user,assemblage,cfmu) %>% 
  summarize(mrate_old = mrate[which.min(year)]) -> se_mrates_oldest

se_wt_rm %>% group_by(user,assemblage,cfmu) %>% 
  summarize(mrate_dwr = mrate[which.max(year)]) -> se_mrates_dwr

expand.grid(year = seq(1977,max(se_wt_rm$year,na.rm=T),1),
            cfmu = unique(se_wt_rm$cfmu),
            assemblage = unique(se_wt_rm$assemblage),
            user = unique(se_wt_rm$user)) %>%
  left_join(se_mrates_oldest, by = c("user","assemblage","cfmu")) %>%
  left_join(se_mrates_dwr, by = c("user","assemblage","cfmu")) %>%
  full_join(se_wt_rm, by = c("user","assemblage","cfmu","year")) %>%
  mutate(r_mort = ifelse(is.na(mrate) & year < 2013,mrate_old,
                         ifelse(is.na(mrate) & year > 2012,mrate_dwr,mrate)),
         wt_cv = ifelse(is.na(wt_cv),1,wt_cv)) %>%
  select(year,cfmu,assemblage,user,wt_lbs ,wt_cv,r_mort) -> r1_wt_rm

ggplot(r1_wt_rm, aes(x= year, y = r_mort, col = cfmu)) +
  geom_line() + geom_point() +
  facet_wrap(~assemblage + user)

rbind(r1_wt_rm %>% rename(area = cfmu),
      r2_wt_rm %>% select(colnames(r1_wt_rm %>% rename(area = cfmu)))) %>%
  mutate(area = ifelse(area != "NORTHEAST",area,"northeast"),
         wt_cv = ifelse(wt_cv == 0,1,wt_cv)) %>% 
  #select(-cfmu) %>%
  left_join(lut,by = "area") %>%
  arrange(assemblage, region, area, year)-> wt_rm_dat

pal <- c("darkorange","darkgrey")

wt_rm_dat %>% filter(!is.na(user)) %>%
  mutate(Species = ifelse(assemblage %in% c("dsrlessye","yelloweye","slope"),"Demersal shelf (yelloweye) and slope",
                             ifelse(assemblage %in% c("pelnbrf","black"),"Pelagics (black)",assemblage)),
         #area = factor(area, unique(H_ayg$area), ordered = TRUE)
         area = factor(area, unique(lut$area), ordered = TRUE)) %>%
  mutate(Species = str_wrap(Species, width = 21),
         User = user) %>%
  ggplot(aes(x = year, y = r_mort, col = Species, shape = User)) +
  facet_wrap(~area) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  geom_line(aes(linetype = User),size = 0.8) + #geom_point() +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Release Mortality", x = "Year") +
  guides(linetype = guide_legend(nrow = 2))

ggsave("./figures/rel_mort.png", width = 10, height = 8) 

wt_rm_dat %>% filter(wt_cv == 0)

with(wt_rm_dat, table(region,area))

with(wt_rm_dat, table(region,year))

saveRDS(wt_rm_dat %>% filter(year <= REP_YR), ".\\data\\bayes_dat\\wt_rm_dat.rds")

##------------------------------------------------------------------------------
# Get mean weights by species across all years for priors:
wt_rm_dat %>% filter(!is.na(wt_lbs)) %>%
  group_by(assemblage) %>%
  summarise(mean_wt = mean(wt_lbs),
            med_wt = median(wt_lbs),
            var_wt = var(wt_lbs),
            sd_wt = sd(wt_lbs),
            cv_wt = sd_wt/mean_wt,
            tau_wt = 1/(sd_wt*sd_wt)) -> wt_priors; wt_priors

# Just looking at it here. This will be calculated and loaded as data in bayes_data_param_load.R

################################################################################
# Interview Data
# SE has interview data on the proportion released for both user groups so lets
# check it out and use it if we can!
################################################################################
z<-1.96 #for CI calculations and graphics

se_int <-  
  read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_MHS_Region1_forR_",REP_YR,"_RUN_08-Oct-2025.xlsx"), 
            range = c("A1:N1000"),
            sheet = "MHS num Fish")  %>%
  rename_all(.funs = tolower) %>%
  mutate(user = tolower(user)) %>%
  rename(area = rpt_area) %>% 
  #filter_all(any_vars(!is.na(.))) %>%
  #select(-c("totalrf_n_rel","totalrf_n_res","totalrf_n_nonres")) %>%
  select(-c("totalrf_n_rel")) %>%
  mutate_at(c("totalrf_n","ye_n","black_n","pelagic_n","nonpel_n",
              "notye_nonpel_n","dsr_n","slope_n",
              "pelnbrf_n","dsrnye_n"),as.numeric) %>%
  cbind(read_xlsx(paste0(".\\data\\raw_dat\\species_comp_SE\\Species_comp_MHS_Region1_forR_",REP_YR,"_RUN_08-Oct-2025.xlsx"), 
                  range = c("AI1:AQ1000"),
                  sheet = "MHS num Fish")  %>%
          rename_all(.funs = tolower) %>%
          mutate_at(c("ye_n_rel","black_n_rel","pelagic_n_rel","nonpel_n_rel",
                      "notye_nonpel_n_rel","dsr_n_rel","slope_n_rel",
                      "pelnbrf_n_rel","dsrnye_n_rel"),as.numeric)) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(totalrf_n_rel = ye_n_rel+pelagic_n_rel+notye_nonpel_n_rel) %>%
  mutate(pH_ye = ye_n / (ye_n + ye_n_rel),
         pH_pel = pelagic_n / (pelagic_n + pelagic_n_rel),
         pH_black = black_n / (black_n + black_n_rel),
         pH_other = notye_nonpel_n / (notye_nonpel_n + notye_nonpel_n_rel),
         pH_dsr = dsrnye_n / (dsrnye_n + dsrnye_n_rel),
         pH_slope = slope_n / (slope_n + slope_n_rel)) #,

#check slope and dsr = other
se_int %>% mutate(slopedsr_n = dsrnye_n + slope_n,
                  slopedsr_n_rel = dsrnye_n_rel + slope_n_rel) %>%
  ggplot(aes(x = slopedsr_n, y = notye_nonpel_n)) + geom_point()

pel_ch <- se_int%>% select(year,area,user,pelagic_n_rel)
ye_ch <- se_int%>% select(year,area,user,ye_n_rel)
black_ch <- se_int%>% select(year,area,user,black_n_rel)
dsr_ch <- se_int%>% select(year,area,user,dsr_n_rel)
slope_ch <- se_int%>% select(year,area,user,slope_n_rel)

with(pel_ch,table(area,year,user)) %>% data.frame() %>% filter(Freq == 0) %>% mutate(Freq = "missing") %>% rename(Pelagic_rels = Freq)

full_join(with(pel_ch,table(area,year,user)) %>% data.frame() %>% filter(Freq == 0) %>% mutate(Freq = "missing") %>% rename(Pelagic_rels = Freq),
          with(ye_ch,table(area,year,user)) %>% data.frame() %>% filter(Freq == 0) %>% mutate(Freq = "missing") %>% rename(ye_rels = Freq),
          by = c("area","year","user"))

sc_int <- read.csv("data/raw_dat/Species_comp_SC/sc_rf_release.csv") %>%
  clean_names() %>% mutate(user = tolower(user)) %>%
  rename(area = cfmu,
         ye_n = yekept,
         ye_n_rel = yerel,
         pelagic_n = pelkept,
         pelagic_n_rel = pelrel,
         notye_nonpel_n = npkept,
         notye_nonpel_n_rel = nprel) 

sc_int %>% filter(area %in% c("WESTSIDE","MAINLAND")) %>% 
  group_by(year,user,area) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(area = "WKMA") -> wkma

sc_int %>% filter(area %in% c("SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK")) %>% 
  group_by(year,user,area) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(area = "SOKO2SAP")->soko2sap

sc_int %>% filter(area %in% c("ALEUTIAN","BERING")) %>% 
  group_by(year,user,area) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(area = "BSAI")->bsai

sc_int <- sc_int %>% 
  filter(!area %in% c("WESTSIDE","MAINLAND","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK")) %>%
  rbind(wkma,soko2sap,bsai) %>%
  mutate(dsr_n = NA,
         slope_n = NA,
         black_n = NA,
         dsr_n_rel = NA,
         slope_n_rel = NA,
         black_n_rel = NA,
         dsrnye_n = NA,
         dsrnye_n_rel = NA,
         pH_ye = ye_n / (ye_n + ye_n_rel),
         pH_pel = pelagic_n / (pelagic_n + pelagic_n_rel),
         pH_black = NA, #black_n / (black_n + black_n_rel),
         pH_other = ifelse(year > 2010,
                           notye_nonpel_n / (notye_nonpel_n + notye_nonpel_n_rel),
                           (notye_nonpel_n - ye_n) / (notye_nonpel_n + notye_nonpel_n_rel - ye_n - ye_n_rel)), #npkept
         pH_dsr = NA,
         pH_slope = NA) %>%
  select(-c(rfkept,rfrel))

head(se_int); head(sc_int)

int <- rbind(sc_int, se_int %>% select(colnames(sc_int))) %>%
  mutate(area = factor(area, toupper(lut$area), ordered = TRUE),
         lo95pH_ye = (pH_ye + z^2/(2*(ye_n + ye_n_rel)) - 
                        z * sqrt((pH_ye*(1 - pH_ye) + z^2/(4*(ye_n + ye_n_rel))) / (ye_n + ye_n_rel))) /
           (1 + z^2/(ye_n + ye_n_rel)),
         hi95pH_ye = (pH_ye + z^2/(2*(ye_n + ye_n_rel)) + 
                        z * sqrt((pH_ye*(1 - pH_ye) + z^2/(4*(ye_n + ye_n_rel))) / (ye_n + ye_n_rel))) /
           (1 + z^2/(ye_n + ye_n_rel)),
         lo95pH_pel = (pH_pel + z^2/(2*(pelagic_n + pelagic_n_rel)) - 
                         z * sqrt((pH_pel*(1 - pH_pel) + z^2/(4*(pelagic_n + pelagic_n_rel))) / (pelagic_n + pelagic_n_rel))) /
           (1 + z^2/(pelagic_n + pelagic_n_rel)),
         hi95pH_pel = (pH_pel + z^2/(2*(pelagic_n + pelagic_n_rel)) + 
                         z * sqrt((pH_pel*(1 - pH_pel) + z^2/(4*(pelagic_n + pelagic_n_rel))) / (pelagic_n + pelagic_n_rel))) /
           (1 + z^2/(pelagic_n + pelagic_n_rel)),
         lo95pH_black = (pH_black + z^2/(2*(black_n + black_n_rel)) - 
                           z * sqrt((pH_black*(1 - pH_black) + z^2/(4*(black_n + black_n_rel))) / (black_n + black_n_rel))) /
           (1 + z^2/(black_n + black_n_rel)),
         hi95pH_black = (pH_black + z^2/(2*(black_n + black_n_rel)) + 
                           z * sqrt((pH_black*(1 - pH_black) + z^2/(4*(black_n + black_n_rel))) / (black_n + black_n_rel))) /
           (1 + z^2/(black_n + black_n_rel)),
         lo95pH_other = (pH_other + z^2/(2*(notye_nonpel_n + notye_nonpel_n_rel)) - 
                           z * sqrt((pH_other*(1 - pH_other) + z^2/(4*(notye_nonpel_n + notye_nonpel_n_rel))) / (notye_nonpel_n + notye_nonpel_n_rel))) /
           (1 + z^2/(notye_nonpel_n + notye_nonpel_n_rel)),
         #lo95pH_other = pmax(0,pH_other - 1.96 * sqrt(pH_other*(1-pH_other)/(notye_nonpel_n + notye_nonpel_n_rel))),
         hi95pH_other = (pH_other + z^2/(2*(notye_nonpel_n + notye_nonpel_n_rel)) + 
                           z * sqrt((pH_other*(1 - pH_other) + z^2/(4*(notye_nonpel_n + notye_nonpel_n_rel))) / (notye_nonpel_n + notye_nonpel_n_rel))) /
           (1 + z^2/(notye_nonpel_n + notye_nonpel_n_rel)),
         lo95pH_dsr = (pH_dsr + z^2/(2*(dsrnye_n + dsrnye_n_rel)) - 
                         z * sqrt((pH_dsr*(1 - pH_dsr) + z^2/(4*(dsrnye_n + dsrnye_n_rel))) / (dsrnye_n + dsrnye_n_rel))) /
           (1 + z^2/(dsrnye_n + dsrnye_n_rel)),
         #lo95pH_dsr = pmax(0,pH_dsr - 1.96 * sqrt(pH_dsr*(1-pH_dsr)/(dsrnye_n + dsrnye_n_rel))),
         hi95pH_dsr = (pH_dsr + z^2/(2*(dsrnye_n + dsrnye_n_rel)) + 
                         z * sqrt((pH_dsr*(1 - pH_dsr) + z^2/(4*(dsrnye_n + dsrnye_n_rel))) / (dsrnye_n + dsrnye_n_rel))) /
           (1 + z^2/(dsrnye_n + dsrnye_n_rel)),
         lo95pH_slope = (pH_slope + z^2/(2*(slope_n + slope_n_rel)) - 
                           z * sqrt((pH_slope*(1 - pH_slope) + z^2/(4*(slope_n + slope_n_rel))) / (slope_n + slope_n_rel))) /
           (1 + z^2/(slope_n + slope_n_rel)),
         hi95pH_slope = (pH_slope + z^2/(2*(slope_n + slope_n_rel)) + 
                           z * sqrt((pH_slope*(1 - pH_slope) + z^2/(4*(slope_n + slope_n_rel))) / (slope_n + slope_n_rel))) /
           (1 + z^2/(slope_n + slope_n_rel))) 
  
str(int)

int %>% filter(area == "CI" & year < 2010)

ggplot(int, 
       aes(x = year, y = pH_pel, col = user)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_pel, ymin = lo95pH_pel)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("Pelagic") +
  ylab("Proportion harvested") + ylim(0,1)

ggplot(int, 
       aes(x = year, y = pH_black, col = user)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_black, ymin = lo95pH_black)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("Black") +
  ylab("Proportion harvested") + ylim(0,1)

ggplot(int, 
       aes(x = year, y = pH_ye, col = user)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_ye, ymin = lo95pH_ye)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("Yelloweye") +
  ylab("Proportion harvested")+ ylim(0,1)

ggplot(int,  
       aes(x = year, y = pH_dsr, col = user)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_dsr, ymin = lo95pH_dsr)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("DSR (non-yelloweye)") +
  ylab("Proportion harvested")+ ylim(0,1)

ggplot(int,  
       aes(x = year, y = pH_slope, col = user)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_slope, ymin = lo95pH_slope)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("Slope") +
  ylab("Proportion harvested")+ ylim(0,1)

max((se_int$var),na.rm = T)

#I don't trust the variance of 0
# Compare to logbook data: 
H_ayg <- readRDS(".//data//bayes_dat//H_ayg.rds") %>% 
  mutate(H_lb = ifelse(H == 0, 1, H),
         area = toupper(factor(area, lut$area, ordered = TRUE))) 

# Logbook releases by area, year for guided trips
R_ayg <- readRDS(".//data//bayes_dat//R_ayg.rds") %>% 
  mutate(R_lb = ifelse(R == 0, 1, R),
         Rye = ifelse(year < 2006, NA,Rye),
         area = toupper(factor(area, lut$area, ordered = TRUE))) 

head(H_ayg); head(R_ayg)
head(se_int)

unique(H_ayg$area);unique(R_ayg$area) 

with(H_ayg, table(year,area))

full_join(H_ayg,R_ayg,by = c("year","area","region")) %>%
  mutate(p_h = H / (R+H),
         p_h_p = Hp / (Rp+Hp),
         p_h_np = Hnp / (Rnp + Hnp),
         p_h_ye = Hye / (Rye + Hye),
         p_h_o = Ho / (Ro + Ho)) %>%
  select(year,area,p_h,p_h_p,p_h_np,p_h_ye,p_h_o) %>%
  pivot_longer(cols = c(p_h,p_h_p,p_h_np,p_h_ye,p_h_o),
               names_to = "assemblage",
               values_to = "p_h") %>%
  mutate(assemblage = ifelse(assemblage == "p_h","all",
                             ifelse(assemblage == "p_h_p", "Pel",
                                    ifelse(assemblage == "p_h_np", "NonPel",
                                           ifelse(assemblage == "p_h_ye", "Yelloweye",
                                                  ifelse(assemblage == "p_h_o","Other",NA))))),
         user = "charter",
         proportion = 1-p_h,
         var = 0,
         lo95 = NA,
         hi95 = NA,
         lo95_h = NA, hi95_h = NA,
         source = "lb") %>%
  mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
         User = user) -> lb_hprob

baseTXT <- 17 # base text size
legTXT <- 12 #legend text size
axTXT <- 16 # axis tic label text size
axTiTXT <- 17 # axis title text size
barwidth <- 1 #bar width on bar graphs

ggplot(int %>% mutate(year = as.integer(year),
                      User = user), #%>% filter(assemblage == "Black"), 
       aes(x = year, y = pH_pel, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_pel, ymin = lo95pH_pel)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Pel") %>%
              mutate(pH_pel = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  #scale_linetype_manual(values = c("Model" = 1), name = "Source") +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("Pelagic") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_pelagics.png")  

unique(se_int$area) -> se_area

ggplot(int %>% mutate(year = as.integer(year),
                      User = user) %>% filter(area %in% se_area), 
       aes(x = year, y = pH_black, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_black, ymin = lo95pH_black)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Pel",
                                       area %in% se_area) %>%
              mutate(pH_black = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  #scale_linetype_manual(values = c("Model" = 1), name = "Source") +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("Black and Logbook pelagics") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_black-pelagics.png")

ggplot(int %>% mutate(year = as.integer(year),
                      User = user), #%>% filter(assemblage == "Black"), 
       aes(x = year, y = pH_ye, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_ye, ymin = lo95pH_ye)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Yelloweye") %>%
              mutate(pH_ye = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  #scale_linetype_manual(values = c("Model" = 1), name = "Source") +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("Yelloweye") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_yelloweye.png")

ggplot(int %>% mutate(year = as.integer(year),
                      User = user), #%>% filter(assemblage == "Black"), 
       aes(x = year, y = pH_other, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_other, ymin = lo95pH_other)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Other") %>%
              mutate(pH_other = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  #scale_linetype_manual(values = c("Model" = 1), name = "Source") +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("Other (non-pelagic, non-yelloweye)") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_other.png")

ggplot(int %>% mutate(year = as.integer(year),
                      User = user) %>% filter(area %in% se_area), 
       aes(x = year, y = pH_dsr, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_dsr, ymin = lo95pH_dsr)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Other",
                                       area %in% se_area) %>%
              mutate(pH_dsr = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("DSR and logbook others") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_dsr-other.png")

ggplot(int %>% mutate(year = as.integer(year),
                      User = user) %>% filter(area %in% se_area), 
       aes(x = year, y = pH_slope, shape = User, fill = User, color = "Interviews")) +
  geom_point(size = 2) + geom_line(linetype = 2, size = 0.25, alpha = 0.0) +
  geom_errorbar(aes(ymax = hi95pH_slope, ymin = lo95pH_slope)) +
  geom_line(data = lb_hprob %>% filter(assemblage == "Other",
                                       area %in% se_area) %>%
              mutate(pH_slope = p_h), aes(color = "Logbook"),
            linetype = 1, size = 1.25, #col = "darkgrey", 
            alpha = 0.5) +
  facet_wrap(~area) +
  scale_shape_manual(values = c(21, 24)) + 
  theme_bw(base_size = baseTXT)+
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         legend.position = "bottom",
         legend.title = element_text(size = axTiTXT),  # Adjust legend title size
         legend.text = element_text(size = axTXT)) +
  ggtitle("Slope and logbook others") +
  scale_colour_grey(start = 0.5, end = 0) + scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(values = c("Interviews" = "black", "Logbook" = "red")) +
  ylab("Proportion harvested") + xlab("Year") +
  labs(colour = "Source", User = "User group")

ggsave("figures/int_vs_lb_propH_slope-other.png")

#-- Prep and save for Bayes model
int %>% filter(year > 1994) %>%
  select(year,user,area,
         pelagic_n,pelagic_n_rel,
         ye_n,ye_n_rel,
         #black_n,black_n_rel,
         other_n = notye_nonpel_n,other_n_rel = notye_nonpel_n_rel,
         dsr_n,dsr_n_rel,
         slope_n,slope_n_rel) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK","EASTSIDE","NORTHEAST"),
                       tolower(area),toupper(area))) %>%
  mutate(area = factor(area, lut$area, ordered = TRUE),
         pelagic_c = pelagic_n + pelagic_n_rel,
         ye_c = ye_n + ye_n_rel,
         other_c = other_n + other_n_rel,
         dsr_c = dsr_n + dsr_n_rel,
         slope_c = slope_n + slope_n_rel) %>%
  right_join(lut,by = "area") -> int_for_mod

int_for_mod %>% mutate(area = factor(area, lut$area, ordered = TRUE)) -> int_for_mod

str(int_for_mod)
unique(int_for_mod$area)
with(int, table(area,year))
colnames(int)

saveRDS(int_for_mod, ".\\data\\bayes_dat\\Int_ayu.rds")
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#----
#Scrapola:
int %>% filter(area == "CSEO") %>%
  select(year,user,pelagic_n,pelagic_n_rel,pH_pel)





ggplot(int %>% mutate(year = as.integer(year)),  
       aes(x = year, y = pH_dsr, col = user, shape = user)) +
  geom_line(data = lb_hprob %>% filter(#area %in% se_areas &
                                         assemblage == "Other") %>%
              mutate(pH_dsr = p_h),
            linetype = 1, size = 1.25, col = "lightgrey") +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_dsr, ymin = lo95pH_dsr)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("DSR & LB other") +
  scale_colour_grey(start = 0.6, end = 0) + scale_fill_grey(start = 0.6, end = 0) +
  ylab("Proportion harvested") + xlab("Year")

ggplot(int %>% mutate(year = as.integer(year)),  
       aes(x = year, y = pH_slope, col = user, shape = user)) +
  geom_line(data = lb_hprob %>% filter(#area %in% se_areas &
                                         assemblage == "Other") %>%
              mutate(pH_slope = p_h),
            linetype = 1, size = 1.25, col = "lightgrey") +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymax = hi95pH_slope, ymin = lo95pH_slope)) +
  facet_wrap(~area) +
  theme_bw() + ggtitle("Slope & LB other") +
  scale_colour_grey(start = 0.6, end = 0) + scale_fill_grey(start = 0.6, end = 0) +
  ylab("Proportion harvested") + xlab("Year")


with(int,table(year,area,user))
int %>% filter(year>2019 & user == "private")




S_ayu_ly <- readRDS(".//data//bayes_dat//S_ayu.rds")
str(S_ayu_ly)

head(S_ayu_ly); head(int_for_mod)
lut


























ggplot(int %>% mutate(year = as.integer(year)),
       aes(x = year, y = pH_pel, shape = user, fill = user)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = hi95pH_pel, ymin = lo95pH_pel)) +
  
  # add model line with pseudo 'user' aesthetic so it gets a legend entry
  geom_line(
    data = lb_hprob %>%
      filter(assemblage == "Pel") %>%
      mutate(pH_pel = p_h, user = "Model"),
    aes(x = year, y = pH_pel, linetype = user, color = user),
    size = 1.25, alpha = 0.5
  ) +
  
  facet_wrap(~area) +
  scale_shape_manual(
    values = c("charter" = 21, "private" = 24, "Model" = NA),
    breaks = c("charter", "private", "Model")   # legend order
  ) +
  scale_fill_grey(start = 0.6, end = 0.2) +
  scale_color_manual(
    values = c("charter" = "black", "private" = "black", "Model" = "darkgrey"),
    breaks = c("charter", "private", "Model")
  ) +
  scale_linetype_manual(
    values = c("charter" = 0, "private" = 0, "Model" = 1),
    breaks = c("charter", "private", "Model")
  ) +
  
  guides(
    shape = guide_legend(
      override.aes = list(
        linetype = c(0, 0, 1),
        shape = c(21, 24, NA),
        fill = c("grey40", "grey20", NA)
      )
    ),
    fill = "none",
    color = "none",
    linetype = "none"
  ) +
  
  theme_bw(base_size = baseTXT) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = axTiTXT),
    legend.text = element_text(size = axTXT)
  ) +
  ggtitle("Pelagic") +
  ylab("Proportion harvested") +
  xlab("Year")




