library(readxl)
library(tidyverse)
library(ggplot2)

# Logbook data ----
H_ayg0 <- #logbook harvest by area, user = guided, year
  read_xlsx(".\\rawdata\\logbook_harvest.xlsx", 
            sheet = "logbook_harvest",
            range = "B1:G595", 
            na = "NA")

colnames(H_ayg0)
colnames(H_ayg0) <- c("year", "area", "H", "Hp", "Hnp", "Hye")

table(H_ayg0$Hye[H_ayg0$year <= 2005], useNA = "always") #these should all be NA
H_ayg0$Hye[H_ayg0$year <= 2005] <- NA

table(H_ayg0$area)
H_ayg0 %>%
  group_by(area) %>%
  summarise(H = sum(H)) %>%
  print(n = 100)
#Note BSAI = aleutian + bering
1144+20
#Note EKYKT = IBS + EKYT
14019+59280
#Note SOKO2PEN = southeast + southwest + sakpen + chignik
11603+140+372+915
#Note WKMA = westside + mainland
39414+290

#look up table for region
lut <- 
  data.frame(region = c(rep("Kodiak", 6), rep("Central", 4), rep("Southeast", 6)),
             area = c("afognak", "WKMA", "SOKO2SAP", "northeast", "eastside", "BSAI", 
                      "CI", "NG", "PWSI", "PWSO", 
                      "EWYKT", "NSEO", "NSEI", "CSEO", "SSEO", "SSEI"),
             stringsAsFactors = FALSE) %>%
  arrange(region, area)

H_ayg <-
  H_ayg0 %>%
  filter(!(area %in% c("ALEUTIAN", "BERING", "IBS", "EYKT", "SOUTHEAS", "SOUTHWES", "SAKPEN", "CHIGNIK", "SKMA", "WESTSIDE", "MAINLAND"))) %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAS"), tolower(area), area),
         area = ifelse(area == "northeas", "northeast", area)) %>%
  left_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
  arrange(region, area, year)
table(H_ayg$region, H_ayg$area)


H_ayg %>%
  ggplot(aes(x = year, y = H, color = area)) +
    geom_line() +
    facet_grid(region ~ .)
saveRDS(H_ayg, ".\\data\\H_ayg.rds")




# Hhat_ay data ----
Hhat_ay0 <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "RF_har",
            range = "A4:Q19") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Hhat_ay0$region, Hhat_ay0$area)

seHhat_ay <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "Har_se",
            range = "A4:Q19") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
  arrange(region, area, year)
table(seHhat_ay$region, seHhat_ay$area)

Hhat_ay <- left_join(Hhat_ay0, seHhat_ay, by = c("year", "area", "region"))
saveRDS(Hhat_ay, ".\\data\\Hhat_ay.rds")




# Chat_ay data ----
Chat_ay0 <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
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
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "Cat_se",
            range = "A4:Q19") %>%
  pivot_longer(!year, names_to = "area", values_to = "seC") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area)) %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ay$region, seChat_ay$area)

Chat_ay <- left_join(Chat_ay0, seChat_ay, by = c("year", "area", "region"))
saveRDS(Chat_ay, ".\\data\\Chat_ay.rds")




# Hhat_ayu data ----
Hhat_ayg0 <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "gui_har",
            range = "A4:Q13") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Hhat_ayg0$region, Hhat_ayg0$area)

seHhat_ayg <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "guihar_se",
            range = "A4:Q13") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seHhat_ayg$region, seHhat_ayg$area)

Hhat_ayg <- left_join(Hhat_ayg0, seHhat_ayg, by = c("year", "area", "region", "user"))

Hhat_ayp0 <- #p for private
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "pri_har",
            range = "A4:Q13") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Hhat_ayp0$region, Hhat_ayp0$area)

seHhat_ayp <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "prihar_se",
            range = "A4:Q13") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seHhat_ayp$region, seHhat_ayp$area)

Hhat_ayp <- left_join(Hhat_ayp0, seHhat_ayp, by = c("year", "area", "region", "user"))
Hhat_ayu <- rbind(Hhat_ayg, Hhat_ayp)
saveRDS(Hhat_ayu, ".\\data\\Hhat_ayu.rds")





# Chat_ayu data ----
Chat_ayg0 <- #p for private
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "gui_cat",
            range = "A4:Q13") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Chat_ayg0$region, Chat_ayg0$area)

seChat_ayg <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "guicat_se",
            range = "A4:Q13") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "guided") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ayg$region, seChat_ayg$area)

Chat_ayg <- left_join(Chat_ayg0, seChat_ayg, by = c("year", "area", "region", "user"))

Chat_ayp0 <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "pri_cat",
            range = "A4:Q13") %>%
  rename(year = YEAR) %>%
  pivot_longer(!year, names_to = "area", values_to = "H") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(Chat_ayp0$region, Chat_ayp0$area)

seChat_ayp <- 
  read_xlsx(".\\rawdata\\SWHS_rf_byMgmtUnit_20210108.xlsx", 
            sheet = "pricat_se",
            range = "A4:Q13") %>%
  pivot_longer(!year, names_to = "area", values_to = "seH") %>%
  mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
         user = "private") %>%
  right_join(lut, by = "area") %>%
  mutate(area = factor(area, lut$area, ordered = TRUE)) %>% 
  arrange(region, area, year)
table(seChat_ayp$region, seChat_ayp$area)

Chat_ayp <- left_join(Chat_ayp0, seChat_ayp, by = c("year", "area", "region", "user"))
Chat_ayu <- rbind(Chat_ayg, Chat_ayp)
saveRDS(Chat_ayu, ".\\data\\Chat_ayu.rds")

