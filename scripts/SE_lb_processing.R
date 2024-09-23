################################################################################
## Code to import raw sport fish catch and harvest data
## Original author: Adam Reimer
## Current author: Phil Joy philip.joy@alaska.gov
## Last updated: October 2024
#
# This code will import and prep data for analysis
##
## Data sources:
## Statewide Harvest Survey SWHS (Nick Smith at nick.smith@alaska.gov)
## Charter Logbook Data (Ben Jevons ben.jevons@alaska.gov)
## Species apportionment data from portside sampling
##    Southcentral: Clay McKean (clay.mckean)
##    Southeast: Chris Hinds (chris.hinds@alaska.gov) and Diana Tersteeg (diana.tersteeg@alaska.gov)
################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)

# year of new data
YEAR <- 2022

# Logbook data -----------------------------------------------------------------

# PJ todo #1: need to code from raw logbook data that used to be SAS based and create
## a file that matches "logbook_harvest.xlsx"

raw_log23 <- read.csv(paste0("data/raw_dat/",2023,"LogbookData.8.2.2024.csv")) %>% #date is the date the data was pulled by logbook folks
  clean_names()
str(raw_log)

raw_log22 <- read_sas("data/raw_dat/statewide_2022_100923.sas7bdat") %>% clean_names() %>%
  mutate(logsdate = logdate_new)

colnames(raw_log23)
colnames(raw_log22)

raw_log <- raw_log22

log <- raw_log %>% 
  mutate(
    date = as.Date(logdate, format = "%m/%d/%Y"),
    year = year(date),  # Extract year from logdate_new
    #rfharv = p_rock_kept + y_rock_kept + o_rock_kept,  # Sum of rock kept variables
    rfharv = rowSums(across(c(p_rock_kept,y_rock_kept,o_rock_kept)), na.rm=T),  # Sum of rock kept variables
    #rfrel = p_rock_rel + y_rock_rel + o_rock_rel,  # Sum of rock released variables
    rfrel = rowSums(across(c(p_rock_rel,y_rock_rel,o_rock_rel)), na.rm=T),
    rfcatch = rfharv + rfrel,  # Total catch
    sfstat = case_when(  # Assign stat area based on conditions
      !is.na(prime_bott) ~ prime_bott,
      is.na(prime_bott) & !is.na(prime_salm) ~ prime_salm,
      prime_bott == 0 & !is.na(prime_salm) ~ prime_salm
    ),
    sfstat = ifelse(sfstat == 113456, 113450, sfstat)  # Correct logbook error
  )

# Assign NMFS areas based on sfstat values
log <- log %>%
  mutate(
    RptArea = case_when(
      !is.na(sfstat) & (floor(sfstat / 1000) %in% c(101,102,103,105,106,107,108) | sfstat %in% c(315401,315431,325401,325431)) ~ "SSEI",
      !is.na(sfstat) & (floor(sfstat / 1000) %in% c(104,335) | sfstat %in% c(345401,345430,345500,345537,355401,355430,355500,355530,365330,365400,365430,365500,365530)) ~ "SSEO",
      !is.na(sfstat) & floor(sfstat / 1000) %in% c(109,110,111,112,114,115) ~ "NSEI",
      !is.na(sfstat) & (floor(sfstat / 1000) %in% c(116,182,375,385,395) | sfstat %in% c(181100,181601,181602,181603)) ~ "EYKT",
      !is.na(sfstat) & (floor(sfstat / 1000) %in% c(183,185,186,189,191,192,405,415,425,435) | sfstat %in% c(181400,181500,181604,181605)) ~ "IBS",
      !is.na(sfstat) & sfstat %in% c(345607,355601,355631,365600,365630,365701,154003,154002) ~ "CSEO",
      !is.na(sfstat) & sfstat %in% c(365731,365732,365801,365802) ~ "NSEO",
      !is.na(sfstat) & floor(sfstat / 1000) == 113 ~ case_when(
        sfstat >= 113510 & sfstat <= 113590 ~ "NSEI",  # NSEI-Peril Strait
        sfstat >= 113110 & sfstat <= 113450 ~ "CSEO",
        sfstat >= 113611 & sfstat <= 113660 ~ "CSEO",
        sfstat >= 113710 & sfstat <= 113970 ~ "NSEO"
      ),
      TRUE ~ "ZZZZ"  # Default case
    )
  )

head(log)


harvest_summary <- log %>%
  group_by(RptArea) %>%
  summarise(weighted_rfharv = sum(rfharv, na.rm = TRUE)) %>%
  mutate(weighted_rfharv = replace_na(weighted_rfharv, 0)) %>%
  filter(RptArea != "ZZZZ") %>%
  mutate(percent =  100 * weighted_rfharv / sum(weighted_rfharv))# To handle missing values

print(harvest_summary)

# Find missing NMFS assignments where rfharv > 0 and RptArea is empty
MissingNMFS <- log %>%
  filter(rfharv > 0 & (is.na(RptArea) | RptArea == ""))

# Print relevant columns of MissingNMFS
MissingNMFS %>%
  select(date, port_site, sfstat, rfharv) %>%
  print()

# Identify port sites of missing harvest data (rfharv) with a weighted frequency table
port_site_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_rfharv = sum(rfharv, na.rm = TRUE)) %>%
  mutate(weighted_rfharv = replace_na(weighted_rfharv, 0))  # To handle missing values

print(port_site_summary_H)

# 2006 onward identify port sites of missing harvest data - prockkept
prockkept_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_prockkept = sum(p_rock_kept, na.rm = TRUE)) %>%
  mutate(weighted_prockkept = replace_na(weighted_prockkept, 0))  # Handle missing values

print(prockkept_summary)

# 2006 onward identify port sites of missing harvest data - yrockkept
yrockkept_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_yrockkept = sum(y_rock_kept, na.rm = TRUE)) %>%
  mutate(weighted_yrockkept = replace_na(weighted_yrockkept, 0))  # Handle missing values

print(yrockkept_summary)

# 2006 onward identify port sites of missing harvest data - orockkept
orockkept_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_orockkept = sum(o_rock_kept, na.rm = TRUE)) %>%
  mutate(weighted_orockkept = replace_na(weighted_orockkept, 0))  # Handle missing values

print(orockkept_summary)

# First block: Summarizing harvest data by port_site and RptArea
# Sort by year, port_site, and RptArea
log_sorted_1 <- log %>%
  arrange(year, port_site, RptArea)

str(log_sorted_1)
# Summarize the data (equivalent to proc means with sum)
H_sum_1 <- log_sorted_1 %>%
  group_by(port_site, RptArea) %>%
  summarise(
    total_rfharv = sum(rfharv, na.rm = TRUE),
    total_prockkept = sum(p_rock_kept, na.rm = TRUE),
    total_yrockkept = sum(y_rock_kept, na.rm = TRUE),
    total_orockkept = sum(o_rock_kept, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfharv, total_prockkept, total_yrockkept, total_orockkept), ~round(., 0)))  %>%
  filter(RptArea %in% c("NSEO","CSEO","SSEO","EYKT","IBS","NSEI","SSEI"))# maxdec = 0 equivalent

# Print the summary for the first block
print(H_sum_1)

# Second block: Summarizing harvest data by port_SWHS and CF Management Unit
# Sort by year, port_site, port_SWHS, and RptArea
log_sorted_2 <- log %>%
  arrange(year, port_site, port_swhs, RptArea)

# Summarize the data (equivalent to proc means with sum)
H_sum_2 <- log_sorted_2 %>%
  group_by(year, RptArea) %>%
  summarise(N_obs = n(),
            total_rfharv = sum(rfharv, na.rm = TRUE),
            total_prockkept = sum(p_rock_kept, na.rm = TRUE),
            total_yrockkept = sum(y_rock_kept, na.rm = TRUE),
            total_orockkept = sum(o_rock_kept, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfharv, total_prockkept, total_yrockkept, total_orockkept), ~round(., 0)))  # maxdec = 0 equivalent

# Print the summary for the second block
print(H_sum_2)  

# Titles in R: you can add titles within output print statements if needed
cat('Southeast Region charter KNOWN rf harvest by port_SWHS and CF Management Unit caught (logbook).')

# Diagnostic - Check CF Mgmt Area assignments (RELEASE)
# Weighted frequency table for RptArea (like SAS proc freq with a weight option)
release_summary <- log %>%
  group_by(RptArea) %>%
  summarise(weighted_rfrel = sum(rfrel, na.rm = TRUE)) %>%
  mutate(weighted_rfrel = replace_na(weighted_rfrel, 0))  # To handle missing values

print(release_summary)

# Find missing NMFS assignments where rfrel > 0 and RptArea is empty
MissingNMFS <- log %>%
  filter(rfrel > 0 & (is.na(RptArea) | RptArea == ""))

# Print relevant columns of MissingNMFS
MissingNMFS %>%
  select(date, port_site, sfstat, rfrel) %>%
  print()

# Identify port sites of missing release data (rfrel) with a weighted frequency table
port_site_rfrel_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_rfrel = sum(rfrel, na.rm = TRUE)) %>%
  mutate(weighted_rfrel = replace_na(weighted_rfrel, 0))  # Handle missing values

print(port_site_rfrel_summary)

# 2006 onward identify port sites of missing release data - prockrel
port_site_prockrel_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_prockrel = sum(p_rock_rel, na.rm = TRUE)) %>%
  mutate(weighted_prockrel = replace_na(weighted_prockrel, 0))  # Handle missing values

print(port_site_prockrel_summary)

# 2006 onward identify port sites of missing release data - yrockrel
port_site_yrockrel_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_yrockrel = sum(y_rock_rel, na.rm = TRUE)) %>%
  mutate(weighted_yrockrel = replace_na(weighted_yrockrel, 0))  # Handle missing values

print(port_site_yrockrel_summary)

# 2006 onward identify port sites of missing release data - orockrel
port_site_orockrel_summary <- MissingNMFS %>%
  group_by(port_site) %>%
  summarise(weighted_orockrel = sum(o_rock_rel, na.rm = TRUE)) %>%
  mutate(weighted_orockrel = replace_na(weighted_orockrel, 0))  # Handle missing values

print(port_site_orockrel_summary)

# First block: Find typical harvest areas for ports where data is available (rfrel, prockrel, yrockrel, orockrel)
# Sort by year, port_site, and RptArea
log_sorted_1 <- log %>%
  arrange(year, port_site, RptArea)

# Summarize the data by port_site and RptArea
R_sum_1 <- log_sorted_1 %>%
  group_by(port_site, RptArea) %>%
  summarise(
    total_rfrel = sum(rfrel, na.rm = TRUE),
    total_prockrel = sum(p_rock_rel, na.rm = TRUE),
    total_yrockrel = sum(y_rock_rel, na.rm = TRUE),
    total_orockrel = sum(o_rock_rel, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfrel, total_prockrel, total_yrockrel, total_orockrel), ~round(., 0))) %>%
  filter(RptArea %in% c("NSEO","CSEO","SSEO","EYKT","IBS","NSEI","SSEI"))

# Print the summary for the first block
print(R_sum_1)

# Second block: RF release by SWHS and CF Management Unit
# Sort by year, port_site, port_SWHS, and RptArea
log_sorted_2 <- log %>%
  arrange(year, port_site, port_swhs, RptArea)

# Summarize the data by year and RptArea
R_sum_2 <- log_sorted_2 %>%
  group_by(year, RptArea) %>%
  summarise(
    N_obs = n(),
    total_rfrel = sum(rfrel, na.rm = TRUE),
    total_prockrel = sum(p_rock_rel, na.rm = TRUE),
    total_yrockrel = sum(y_rock_rel, na.rm = TRUE),
    total_orockrel = sum(o_rock_rel, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfrel, total_prockrel, total_yrockrel, total_orockrel), ~round(., 0)))

# Print the summary for the second block
print(R_sum_2)

# Title equivalent for the first block (optional)
cat('Southeast Region charter KNOWN rf release by port_SWHS and CF Management Unit caught (logbook).\n')

# Title equivalent for the second block (optional)
cat('Southeast Region charter KNOWN rf release by port_SWHS and CF Management Unit caught (logbook).\n')

# Fill in holes of missing RptArea based on port_site; this can be replaced with what I have started for groundfish harvest reporting to NMFS
log <- log %>%
  mutate(RptArea = case_when(
    port_site == 'YAKUTAT' & RptArea == "" ~ 'IBS',
    port_site == 'ICY BAY LODGE' & RptArea == "" ~ 'IBS',
    port_site == 'KETCHIKAN' & RptArea == "" ~ 'SSEI',
    port_site == 'SITKA' & RptArea == "" ~ 'CSEO',
    port_site == 'SALMON FALLS' & RptArea == "" ~ 'SSEI',
    port_site == 'AUKE BAY' & RptArea == "" ~ 'NSEI',
    port_site == 'KNUDSON COVE' & RptArea == "" ~ 'SSEI',
    port_site == 'KLAWOCK' & RptArea == "" ~ 'SSEO',
    port_site == 'CRAIG' & RptArea == "" ~ 'SSEO',
    port_site == 'S KAIGANI BAY' & RptArea == "" ~ 'SSEI',
    port_site == 'SPORTSMAN COVE' & RptArea == "" ~ 'SSEI',
    port_site == 'PYBUS POINT LODGE' & RptArea == "" ~ 'NSEI',
    port_site == 'PYBUS POINT' & RptArea == "" ~ 'NSEI',
    port_site == 'ELFIN COVE' & RptArea == "" ~ 'NSEO',
    port_site == 'KILLISNOO' & RptArea == "" ~ 'NSEI',
    port_site == 'PETERSBURG' & RptArea == "" ~ 'NSEI',
    port_site == 'COFFMAN COVE' & RptArea == "" ~ 'SSEI',
    port_site == 'THORNE BAY' & RptArea == "" ~ 'SSEI',
    port_site == 'JUNEAU' & RptArea == "" ~ 'NSEI',
    port_site == 'EL CAPITAN LODGE' & RptArea == "" ~ 'SSEI',
    port_site == 'FALSE ISLAND' & RptArea == "" ~ 'NSEI',
    port_site == 'WATERFALL' & RptArea == "" ~ 'SSEO',
    port_site == 'PELICAN' & RptArea == "" ~ 'NSEO',
    port_site == 'GUSTAVUS' & RptArea == "" ~ 'NSEO',
    port_site == 'ANGOON' & RptArea == "" ~ 'NSEI',
    port_site == 'SEALING COVE' & RptArea == "" ~ 'CSEO',
    port_site == 'WHALE PASS (POW - SE)' & RptArea == "" ~ 'NSEI',
    port_site == 'WHALE PASS (POW - SE' & RptArea == "" ~ 'NSEI',
    port_site == 'CRESCENT HARBOR' & RptArea == "" ~ 'CSEO',
    port_site == 'DOVE ISLAND LODGE' & RptArea == "" ~ 'CSEO',
    port_site == 'ORR ISLAND' & RptArea == "" ~ 'SSEI',
    port_site == 'SALMON LANDING' & RptArea == "" ~ 'SSEI',
    port_site == 'WRANGELL' & RptArea == "" ~ 'SSEI',
    port_site == 'YES BAY' & RptArea == "" ~ 'SSEI',
    port_site == 'CANNERY COVE' & RptArea == "" ~ 'NSEI',
    port_site == 'CLOVER PASS' & RptArea == "" ~ 'SSEI',
    port_site == 'CLOVER BAY' & RptArea == "" ~ 'SSEI',
    port_site == 'SKAGWAY' & RptArea == "" ~ 'NSEI',
    port_site == 'SPRUCE MILL NEW FLT' & RptArea == "" ~ 'SSEI',
    port_site == 'PORT ALEXANDER' & RptArea == "" ~ 'CSEO',
    port_site == 'PORT ALTHORP' & RptArea == "" ~ 'NSEI',
    port_site == 'GLACIER BAY' & RptArea == "" ~ 'NSEI',
    port_site == 'HOONAH' & RptArea == "" ~ 'NSEO',
    port_site == 'NAUKATI' & RptArea == "" ~ 'SSEI',
    port_site == 'NICHOLS BAY' & RptArea == "" ~ 'SSEI',
    port_site == 'PORT PROTECTION' & RptArea == "" ~ 'SSEI',
    port_site == 'SILVER KING LODGE' & RptArea == "" ~ 'SSEI',
    port_site == 'SURESTRIKE' & RptArea == "" ~ 'SSEO',
    port_site == 'THOMAS BASIN' & RptArea == "" ~ 'SSEI',
    port_site == 'WHALERS COVE' & RptArea == "" ~ 'NSEI',
    port_site == 'WARM SPRINGS BAY' & RptArea == "" ~ 'NSEI',
    port_site == 'BOARDWALK' & RptArea == "" ~ 'SSEI',
    port_site == 'KAKE' & RptArea == "" ~ 'NSEI',
    port_site == 'SALTERY COVE' & RptArea == "" ~ 'SSEI',
    port_site == 'SHELTER ISLAND' & RptArea == "" ~ 'NSEI',
    port_site == 'CEDARS LODGE' & RptArea == "" ~ 'SSEI',
    port_site == 'BARANOF' & RptArea == "" ~ 'NSEI',
    port_site == 'BAR HARBOR' & RptArea == "" ~ 'SSEI',
    port_site == 'ROCKY PASS RESORT' & RptArea == "" ~ 'NSEI',
    port_site == 'WHALE BAY' & RptArea == "" ~ 'CSEO',
    port_site == 'CORONATION ISLAND' & RptArea == "" ~ 'SSEO',
    port_site == 'FUNTER BAY' & RptArea == "" ~ 'NSEI',
    port_site == 'HANSEN FLOAT' & RptArea == "" ~ 'SSEI',
    port_site == 'TENAKEE' & RptArea == "" ~ 'NSEI',
    port_site == 'COSMOS COVE' & RptArea == "" ~ 'NSEI',
    port_site == 'BARTLETT COVE' & RptArea == "" ~ 'NSEI',
    port_site == 'MORNE ISLAND' & RptArea == "" ~ 'CSEO',
    TRUE ~ RptArea # Leave other RptArea values as they are
  ))

# Filter log for 'H' in port_swhs and create Hdata
Hdata <- log %>%
  filter(port_swhs == 'H')

# Group by RptArea and sfstat, and calculate the total harvest (rfharv)
bystat <- Hdata %>%
  group_by(RptArea, sfstat) %>%
  summarise(FREQ = n(),
            harv = sum(rfharv, na.rm = TRUE)) %>%
  arrange(RptArea, sfstat)

# Print the summarised data
print(bystat)

# Overall Region 1 rockfish harvest by CF Management Unit (HARVEST)
summary_harvest <- log %>%
  group_by(year, RptArea) %>%
  summarise(total_rfharv = sum(rfharv, na.rm = TRUE)) %>%
  arrange(year, RptArea)

# Print the summary of rockfish harvest
print(summary_harvest)

# Overall Region 1 rockfish harvest by CF Management Unit (CATCH)
summary_catch <- log %>%
  group_by(year, RptArea) %>%
  summarise(total_rfcatch = sum(rfcatch, na.rm = TRUE)) %>%
  arrange(year, RptArea)

# Print the summary of rockfish catch
print(summary_catch)

# RF harv by SWHS where port of landing is located and CF Management Unit where the fish was caught
summary_swhs_rfharv <- log %>%
  group_by(year, port_site, port_swhs, RptArea) %>%
  summarise(
    total_rfharv = sum(rfharv, na.rm = TRUE),
    total_yrockkept = sum(y_rock_kept, na.rm = TRUE),
    total_prockkept = sum(p_rock_kept, na.rm = TRUE)
  ) %>%
  arrange(year, port_site, port_swhs, RptArea) 

# Print the summary
print(summary_swhs_rfharv)

# Generate a list of Prince of Wales ports of landing where SWHS = 'B'
port_sites_B <- log %>%
  #filter(SWHS == 'B') %>%
  filter(port_swhs == 'B') %>%
  count(port_site) %>%
  arrange(port_site)

# Print the list of port_sites
print(port_sites_B)

# Now identify west side port sites
west_side_ports <- c('BEAR VALLEY LODGE', 'CRAIG', 'CALDER BAY', 'DALL ISLAND', 'EDNA BAY', 
                     'EL CAPITAN LODGE', 'HYDABURG', 'KLAWOCK', 'LOG CABIN RESORT', 'NAUKATI', 
                     'NICHOLS BAY', 'ORR ISLAND', 'POINT BAKER', 'PORT ST NICHOLAS', 'SARKAR COVE', 
                     'SEA OTTER SOUND', 'SHELTER COVE LODGE', 'STEAMBOAT BAY', 'SURESTRIKE', 
                     'WADLEIGH ISLAND', 'WATERFALL', 'WINTER HARBOR')

# Filter for only west side port sites
Bwest <- log %>%
  filter(port_site %in% west_side_ports)

# Summarise the data for west side charter logbook harvest
summary_Bwest <- Bwest %>%
  group_by(year, port_swhs, RptArea) %>%
  summarise(N_obs = n(),
            total_rfharv = sum(rfharv, na.rm = TRUE)) %>%
  arrange(year, port_swhs, RptArea)

# Print the summary of harvest on the west side
print(summary_Bwest)

#------------------------------------------------------------------------------
# update historical file: 

