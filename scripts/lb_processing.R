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

write.csv(raw_log22 %>% data.frame(),"data/raw_dat/statewide_2022_100923.csv")

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
    prime_bott = case_when(
      prime_bott == 495952 ~ 495932,
      prime_bott == 475939 ~ 475932,
      TRUE ~ prime_bott
    ),
    sfstat = case_when(  # Assign stat area based on conditions
      !is.na(prime_bott) ~ prime_bott,
      is.na(prime_bott) & !is.na(prime_salm) ~ prime_salm,
      prime_bott == 0 & !is.na(prime_salm) ~ prime_salm
    ),
    sfstat = ifelse(sfstat == 113456, 113450, sfstat)  # Correct logbook error
  )

write.csv(log %>% data.frame(),"data/raw_dat/statewide_2022_100923_log.csv")

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
      
      !is.na(sfstat) & floor(sfstat / 1000) == 26 ~ "MAINLAND", #*{Mainland salmon stat areas, Area Q/R};
      !is.na(sfstat) & floor(sfstat %/% 1000) %in% c(27, 576) ~ "CHIGNIK", # *{Chignik salmon stat areas, Area R};
      !is.na(sfstat) & floor(sfstat %/% 1000) == 28 ~ "SAKPEN", # *{South Alaska Peninsula Eastern District salmon stat areas, Area R};
      !is.na(sfstat) & floor(sfstat %/% 1000) %in% c(30, 664, 674, 684, 694) ~ "ALEUTIAN", #*{South Alaska Peninsula Western District salmon stat areas, Area R};
      !is.na(sfstat) & floor(sfstat %/% 1000) == 31 ~ "BERING", #Any harvest in Bering Sea, salmon stat areas
      !is.na(sfstat) & floor(sfstat %/% 1000) %in% c(445, 446, 455, 465) ~ "PWSO", #PWS Outside, Area J
      !is.na(sfstat) & floor(sfstat %/% 1000) %in% c(496, 105) ~ "NG", #North Gulf Coast, Area J, Stat area in 2016 listed as 105932 but likely 505932
      !is.na(sfstat) & floor(sfstat %/% 1000) %in% c(221, 223, 516, 526) ~ "CI", #Cook Inlet, Area P
      !is.na(sfstat) & floor(sfstat / 1000) == 25 ~ case_when(
        sfstat >= 25110 & sfstat <= 25235 ~ "AFOGNAK", #Afognak salmon stat areas, Area Q
        sfstat >= 25311 & sfstat <= 25640 ~ "WESTSIDE", #Westside salmon stat areas, Area Q
        sfstat >= 25710 & sfstat <= 25770 ~ "SOUTHWEST", #Southwest salmon stat areas, Area Q
        sfstat >= 25810 & sfstat <= 25830 ~ "EASTSIDE", #Eastside salmon stat areas, Area Q
        sfstat >= 25840 & sfstat <= 25870 ~ "SOUTHEAST", #Southeast salmon stat areas, Area Q
        sfstat >= 25880 & sfstat <= 25890 ~ "SOUTHWEST", #Southwest salmon stat areas, Area Q
        sfstat >= 25910 & sfstat <= 25940 ~ "NORTHEAST", # Northeast salmon stat areas, Area Q
        sfstat >= 25941 & sfstat <= 25948 ~ "EASTSIDE" #Eastside salmon stat areas, Area Q
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 222 ~ case_when(
        sfstat >= 222000 & sfstat <= 222050 ~ "CI", #ACook inlet, ara P
        sfstat == 222060 ~ "NG" #Area P
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 456 ~ case_when(
        sfstat >= 456001 & sfstat <= 456004 ~ "PWSO", #Area J
        sfstat %in% c(456031, 456032) ~ "PWSI" #Area J
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 466 ~ case_when(
        sfstat %in% c(466001, 466002, 466004, 466005) ~ "PWSO", #Area J
        sfstat %in% c(466003, 466031, 466032, 466033, 466100) ~ "PWSI" #Area J
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 475 ~ case_when(
        sfstat >= 475500 & sfstat <= 475932 | sfstat %in% (475934) ~ "PWSO", #Area J
        sfstat %in% c(475933) ~ "PWSI" #Area J
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 476 ~ case_when(
        sfstat %in% c(476001, 476002) ~ "PWSO", #Area J
        sfstat >= 476003 & sfstat <= 476102 ~ "PWSI" #Area J
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 485 ~ case_when(
        sfstat >= 485430 & sfstat <= 485831 ~ "PWSO", #Area J
        sfstat %in% c(485901, 485931, 485935) ~ "PWSO",
        sfstat %in% c(485832, 485902, 485933, 485934) ~ "NG",
        sfstat %in% c(485932) ~ "PWSI" #Area J
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 486 ~ case_when(
        sfstat %in% c(486002) ~ "PWSO", #Area J
        sfstat >= 486003 & sfstat <= 486100 ~ "PWSI", #Area J
        sfstat %in% c(486001) ~ "PWSI"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 495 ~ case_when(
        sfstat >= 495400 & sfstat <= 495700 ~ "EASTSIDE", #Eastside, Area Q
        sfstat >= 495901 & sfstat <= 495939 | sfstat %in% c(495831) ~ "NG", #NGC, area J
        sfstat %in% c(495730) ~ "NORTHEAST", #Northeast, Area Q
        sfstat %in% c(495800, 495832) ~ "AFOGNAK" #Afognak salmon stat areas, Area Q
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 505 ~ case_when(
        sfstat >= 505330 & sfstat <= 505934 | sfstat %in% c(505831) ~ "NG", #Eastside, Area Q
        sfstat %in% c(505730) ~ "NORTHEAST", #Northeast, Area Q
        sfstat %in% c(505800, 505832) ~ "AFOGNAK" #Afognak salmon stat areas, Area Q
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 515 ~ case_when(
        sfstat >= 515300 & sfstat <= 515700 ~ "EASTSIDE",
        sfstat >= 515901 & sfstat <= 515906 | sfstat %in% c(515831, 515832) ~ "NG", #Eastside, Area Q
        sfstat %in% c(515730) ~ "NORTHEAST", #Northeast, Area Q
        sfstat %in% c(515801, 515802, 515833) ~ "AFOGNAK", #Afognak salmon stat areas, Area Q
        sfstat >= 515907 & sfstat <= 515939 ~ "CI", #Cook Inlet, Area P
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 525 ~ case_when(
        sfstat >= 525230 & sfstat <= 525703  ~ "EASTSIDE", #Eastside, Area Q
        sfstat %in% c(525831, 525835, 525836, 525837) ~ "NG", #NNGC in Area J
        sfstat %in% c(525731, 525732, 525733) ~ "NORTHEAST", #Afognak salmon stat areas, Area Q
        sfstat >= 525801 & sfstat <= 525807 | sfstat %in% c(525832, 525833, 525834) ~ "AFOGNAK", 
        sfstat >= 525901 & sfstat <= 525932  ~ "CI"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 535 ~ case_when(
        sfstat >= 535230 & sfstat <= 535634 | sfstat %in% c(535703, 535704, 535705)  ~ "SOUTHEAST", #SOUTHEAST, Area Q
        sfstat %in% c(5535635, 535702) ~ "SOUTHWEST", #NNGC in Area J
        sfstat %in% c(535701, 535731, 535732, 535733, 535734) ~ "WESTSIDE", #
        sfstat %in% c(535706, 535707) ~ "EASTSIDE", #
        sfstat %in% c(535801, 535832) ~ "MAINLAND", 
        sfstat %in% c(535802, 535803, 535831) ~ "AFOGNAK", 
        sfstat %in% c(535833, 535834) ~ "NG", 
        sfstat >= 535901 & sfstat <= 535933  ~ "CI"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 545 ~ case_when(
        sfstat >= 545200 & sfstat <= 545633 | sfstat %in% c(545704)  ~ "SOUTHWEST", #SOUTHEAST, Area Q
        sfstat %in% c(545701, 545702, 545703, 545732, 545733, 545734) ~ "WESTSIDE", #NNGC in Area J
        sfstat %in% c(545731, 545801, 545802, 545803) ~ "MAINLAND", #
        sfstat %in% c(545804) ~ "AFOGNAK", #
        sfstat %in% c(545900) ~ "CI"
      ),
      
      !is.na(sfstat) & floor(sfstat / 1000) == 555 ~ case_when(
        sfstat >= 555200 & sfstat <= 555630 ~ "SOUTHWEST", #SOUTHEAST, Area Q
        sfstat %in% c(555701) ~ "WESTSIDE", #NNGC in Area J
        sfstat %in% c(555702, 555731, 555732, 555733) ~ "MAINLAND"
      ),
    
      !is.na(sfstat) & floor(sfstat / 1000) == 565 ~ case_when(
        sfstat %in% c(565131, 565201, 565231, 565301, 565331, 565401) ~ "SAKPEN", #South Alaska Peninsula Eastern District, Area R
        sfstat %in% c(565132, 565202, 565232, 565302, 565333, 565403, 565432, 565502, 565534, 565604, 565635) ~ "SOUTHWEST",
        sfstat %in% c(565332, 565402, 565431, 565501, 565531, 565533, 565602, 565603, 565631, 565632, 565633, 565634, 565701, 565703) ~ "CHIGNIK", #Chignik, Area R
        sfstat %in% c(565702, 565704) ~ "WESTSIDE"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 575 ~ case_when(
        sfstat >= 575131 & sfstat <= 575401 | sfstat %in% c(575404, 575431) ~ "SAKPEN", 
        sfstat >= 575731 & sfstat <= 575830  ~ "BERING",  #Any harvest in Bering Sea
        sfstat >= 575432 & sfstat <= 575635 | sfstat %in% c(575402, 575403) ~ "CHIGNIK"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 585 ~ case_when(
        sfstat >= 585100 & sfstat <= 585431 | sfstat %in% c(585501) ~ "SAKPEN", 
        sfstat >= 585631 & sfstat <= 585830  ~ "BERING",  #Any harvest in Bering Sea
        sfstat %in% c(585432, 585502, 585531, 585532, 585601, 585602, 585603) ~ "CHIGNIK"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 595 ~ case_when(
        sfstat >= 595100 & sfstat <= 595533  ~ "SAKPEN", 
        sfstat >= 595631 & sfstat <= 595833  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 605 ~ case_when(
        sfstat >= 605100 & sfstat <= 605533  ~ "SAKPEN", 
        sfstat >= 605534 & sfstat <= 605834  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 615 ~ case_when(
        sfstat >= 615030 & sfstat <= 615532  ~ "SAKPEN", 
        sfstat >= 615533 & sfstat <= 615834  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 625 ~ case_when(
        sfstat >= 625030 & sfstat <= 625502  ~ "SAKPEN", 
        sfstat >= 625503 & sfstat <= 625832  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 635 ~ case_when(
        sfstat >= 635030 & sfstat <= 635436 | sfstat %in% c(635502)  ~ "SAKPEN", 
        sfstat >= 635503 & sfstat <= 635800 | sfstat %in% c(635501)  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 645 ~ case_when(
        sfstat >= 645500 & sfstat <= 645401 | sfstat %in% c(645403, 645404, 645405, 645432)  ~ "SAKPEN", 
        sfstat >= 645406 & sfstat <= 645431 | sfstat %in% c(645402)  ~ "ALEUTIAN", # 
        sfstat >= 645433 & sfstat <= 645730   ~ "BERING",
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 655 ~ case_when(
        sfstat >= 655000 & sfstat <= 655413 ~ "ALEUTIAN", 
        sfstat >= 655430 & sfstat <= 655700  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 665 ~ case_when(
        sfstat >= 665000 & sfstat <= 665405 ~ "ALEUTIAN", 
        sfstat >= 665430 & sfstat <= 665600  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 675 ~ case_when(
        sfstat >= 675000 & sfstat <= 675400 ~ "ALEUTIAN", 
        sfstat >= 675430 & sfstat <= 675600  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 685 ~ case_when(
        sfstat >= 685000 & sfstat <= 685400 ~ "ALEUTIAN", 
        sfstat >= 685430 & sfstat <= 685600  ~ "BERING"
      ),
      !is.na(sfstat) & floor(sfstat / 1000) == 695 ~ case_when(
        sfstat >= 695000 & sfstat <= 695400 ~ "ALEUTIAN", 
        sfstat >= 695430 & sfstat <= 695600  ~ "BERING"
      ),
        #TRUE ~ NA_character_
      #TRUE ~ NA_character_
      TRUE ~ "ZZZZ"  # Default case
    ),
    region = ifelse(RptArea %in% c("CSEO","EYKT","IBS","NSEI","NSEO","SSEI","SSEO"),
                    "SE","SC")
  )
  
with(log, table(sfstat,RptArea)) -> RA_sfstat
write.csv(RA_sfstat,"data/RptArea_x_sfstat.csv")

head(log)
unique(log$RptArea)

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
port_site_summary_H <- MissingNMFS %>%
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
  group_by(port_site, RptArea, region) %>%
  summarise(N_obs = n(),
    total_rfharv = sum(rfharv, na.rm = TRUE),
    total_prockkept = sum(p_rock_kept, na.rm = TRUE),
    total_yrockkept = sum(y_rock_kept, na.rm = TRUE),
    total_orockkept = sum(o_rock_kept, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfharv, total_prockkept, total_yrockkept, total_orockkept), ~round(., 0))) %>%
  #filter(RptArea %in% c("NSEO","CSEO","SSEO","EYKT","IBS","NSEI","SSEI"))# maxdec = 0 equivalent
  filter(region %in% c("SC"))
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

# Breakdown by resident and non-resident
res_log <- log %>% filter(rfharv > 0) %>%
  group_by(RptArea, year) %>%
  summarise(N_obs = n(),
    rclient_sum = sum(r_client, na.rm = TRUE),
    nclient_sum = sum(n_client, na.rm = TRUE)
  )

# View the summary
print(res_log)
# Titles in R: you can add titles within output print statements if needed
#cat('Southeast Region charter KNOWN rf harvest by port_SWHS and CF Management Unit caught (logbook).')

# RELEASES #-----------------------------------------------------------------------
with(log, table(port_site,RptArea)) %>% data.frame() ->sites
write.csv(sites,"data/port_sites_andRptArea.csv")

# Diagnostic - Check CF Mgmt Area assignments (RELEASE)
freq_table <- log %>% #filter(region == "SC") %>%
  group_by(RptArea) %>%
  dplyr::summarise(Total = sum(rfrel, na.rm = TRUE)) %>%
  ungroup() %>% complete(RptArea, fill = list(Total = 0))# Show missing values too

# Display the frequency table
print(freq_table, n=24)

# FLAG!! Something is fucked up with release data. Code for harvests mirrors SAS output
# but release data does not. SE matches, but not SC. Did some digging but no luck. Maybe? something is off
# from BBs having to convert to SAS when LB program stopped producing SAS code?
# Will try reconstructing from raw data and see if we can find out where the problem
# is on Monday 9/23. Go into raw data file and filter out the stuff below. Will have
# to relabel stuff

log %>% #
  filter(port_site == "OLD HARBOR") %>% group_by(RptArea, sfstat) %>%
  summarize(n = n(),
            Total = sum(rfrel, na.rm = TRUE),
            total_prockrel = sum(p_rock_rel, na.rm = TRUE),
            total_yrockrel = sum(y_rock_rel, na.rm = TRUE),
            total_orockrel = sum(o_rock_rel, na.rm = TRUE))

## Figured it out!!! SAS code is ignoring releases when there were no rf kept.
## Not sure why that is, but I've verified through direct examination of data
## and hand calculations that R estimates are correct. I have no idea how far
## back this issue goes, perhaps when Brianna started pulling data directly from 
## the data base? But data is there... just a misapplied filter. 2022 samples will be updated. 

# BINGO.... this is it. AFOGNAK example. Releases only counted when rockfish are caught. 

log %>% 
  filter(sfstat %in% c(25110,25111,25112,25130,25181,25182,25183,25184,25185,25234,
                       515801,515833,525802,525803,525804,525805,525806,525807,525832,
                       525833,525834,535802,535803)) %>%
  filter(is.na(p_rock_kept), # This right here. 
         is.na(y_rock_kept),
         is.na(o_rock_kept)) %>%
  group_by(RptArea) %>%
  summarize(n = n(),
            Total = sum(rfrel, na.rm = TRUE),
            total_prockrel = sum(p_rock_rel, na.rm = TRUE),
            total_yrockrel = sum(y_rock_rel, na.rm = TRUE),
            total_orockrel = sum(o_rock_rel, na.rm = TRUE))

# 2. Create the 'MissingNMFS' dataset
MissingNMFS <- log %>%
  filter(rfrel > 0 & (is.na(RptArea) | RptArea == ""))

# 3. Print the 'MissingNMFS' dataset with selected variables
MissingNMFS %>%
  select(logdate_new, port_site, sfstat, rfrel) %>%
  print(n = Inf)


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
  group_by(port_site, RptArea, region) %>%
  summarise(N_obs = n(),
    total_rfrel = sum(rfrel, na.rm = TRUE),
    total_prockrel = sum(p_rock_rel, na.rm = TRUE),
    total_yrockrel = sum(y_rock_rel, na.rm = TRUE),
    total_orockrel = sum(o_rock_rel, na.rm = TRUE)
  ) %>%
  mutate(across(c(total_rfrel, total_prockrel, total_yrockrel, total_orockrel), ~round(., 0))) %>%
  filter(region %in% c("SC"))

# Print the summary for the first block
print(R_sum_1)

log_sorted_1 %>% filter(RptArea == "AFOGNAK" & port_site == "AFOGNAK") -> dbug

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
print(R_sum_2, n = 25)

db <- log %>% filter(RptArea == "CI") %>%
  arrange(year, port_site, RptArea) 

sum(db$p_rock_rel, na.rm = T)
View(db)
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
# Apportion SC unknown harvests (unassignd to reporting area)
# This code will need to be modified year to year depending on which areas and ports
# have unassigned fish... 

MissingNMFS %>%
  select(date, port_site, sfstat, rfharv) %>%
  print()

# Identify port sites of missing harvest data (rfharv) with a weighted frequency table
port_site_summary_H <- MissingNMFS %>%
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

unique(H_sum_1$port_site)

SC_appor <- H_sum_1 %>% 
  mutate(port_site = toupper(port_site)) %>%
  mutate(port_site = ifelse(port_site == "Kodiak","KODIAK",port_site),) %>%
  filter(port_site %in% c("ADAK","HOMER","Kodiak","VALDEZ","KODIAK"))

SC_appor %>%  group_by(port_site) %>% filter(!is.na(RptArea)) %>%
  mutate(pt_tot_rf = sum(total_rfharv, na.rm=T),
         pt_tot_prf = sum(total_prockkept, na.rm=T),
         pt_tot_yrf = sum(total_yrockkept, na.rm=T),
         pt_tot_orf = sum(total_orockkept, na.rm=T),
         prop_p = total_prockkept / pt_tot_prf,
         prop_y = total_yrockkept / pt_tot_yrf,
         prop_o = total_orockkept / pt_tot_orf) %>%
  ungroup() -> SC_appor

ZZs <- SC_appor %>% filter(RptArea == "ZZZZ")

miss_sa <- H_sum_1 %>% 
  mutate(port_site = toupper(port_site)) %>%
  mutate(port_site = ifelse(port_site == "Kodiak","KODIAK",port_site),) %>%
  filter(port_site %in% c("ADAK","HOMER","Kodiak","VALDEZ","KODIAK")) %>%
  filter(is.na(RptArea))

UnknownH <- rbind(ZZs[,1:8],miss_sa) %>%
  group_by(port_site) %>%
  summarize(unk_rfharv = sum(total_rfharv),
            unk_p_harv = sum(total_prockkept),
            unk_y_harv = sum(total_yrockkept),
            unk_o_harv = sum(total_orockkept)) 

SC_appor <- left_join(SC_appor,UnknownH,by = c("port_site")) %>%
  #mutate(tot_pel_h = total_prockkept + unk_p_harv * prop_p,
  #       tot_ye_h = total_yrockkept + unk_y_harv * prop_y,
  #       tot_oth_h = total_orockkept + unk_o_harv * prop_o,
  #       tot_rf_h = tot_pel_h + tot_ye_h + tot_oth_h) %>% 
  group_by(RptArea) %>%
  summarize(unk_p_harv = sum(unk_p_harv * prop_p, na.rm = T),
            unk_y_harv = sum(unk_y_harv * prop_y, na.rm = T),
            unk_o_harv = sum(unk_o_harv * prop_o, na.rm = T),
            tot_unk_h = unk_p_harv + unk_y_harv + unk_o_harv) %>%
  replace(is.na(.), 0)

# now add this back into final harvest estimate for the year: 
H_sum_F <- left_join(H_sum_2,SC_appor,by = "RptArea") %>% data.frame() %>%
  replace(is.na(.), 0) %>% 
  mutate(unk_p_harv,
         tot_rf_harv = round(total_rfharv + tot_unk_h, 0),
         tot_pel_harv = round(total_prockkept + unk_p_harv, 0),
         tot_ye_harv = round(total_yrockkept + unk_y_harv, 0),
         tot_o_harv = round(total_orockkept + unk_o_harv, 0))

#Get EWYKT
H_sum_F <- rbind(H_sum_F,
                 H_sum_F %>% filter(RptArea %in% c("EYKT","IBS")) %>%
        summarise(.,across(where(is.numeric),sum)) %>%
        mutate(year = YEAR,
               RptArea = "EWYKT"))

#-------------------------------------------------------------------------------
# Update logbook harvest and release data sheets.
lb_harv <- read.csv("data/raw_dat/logbook_harvest_byYear.csv")
lb_rel <- read.csv("data/raw_dat/logbook_release_byYear.csv")

lb_harv2 <- rbind(lb_harv %>% filter(year < 2022), #get rid of fucked up 2022 estimates
                 H_sum_F %>% mutate(Region = ifelse(RptArea %in% c("CSEO","NSEO","EYKT","IBS","NSEI",
                                                                   "NSEO","SSEI","SSEO"),"SE","SC")) %>%
                   mutate(nonpel_harv = tot_ye_harv + tot_o_harv) %>% 
                   select(Region, year, RptArea, rfharv = tot_rf_harv, pelagic_harv = tot_pel_harv,
                          nonpel_harv, ye_harv = tot_ye_harv, not_ye_nonpel_harv = tot_o_harv) %>%
                   filter(RptArea != "ZZZZ" & RptArea != 0) ) %>% 
  arrange(year,RptArea)

lb_harv2 %>% filter(year == 2022)

write.csv(lb_harv2, "data/raw_dat/logbook_harvest_byYear.csv")

#*Note... caught what appears to be a cutting and pasting error in PWSO in 2022

R_sum_2 <- rbind(R_sum_2,
                 R_sum_2 %>% filter(RptArea %in% c("EYKT","IBS")) %>%
                   summarise(.,across(where(is.numeric),sum)) %>%
                   mutate(year = YEAR,
                          RptArea = "EWYKT"))

lb_rel2 <- rbind(lb_rel,
                 R_sum_2 %>% mutate(Region = ifelse(RptArea %in% c("CSEO","NSEO","EYKT","IBS","NSEI",
                                                                   "NSEO","SSEI","SSEO"),"SE","SC"),
                                    nonpel_rel = total_yrockrel + total_orockrel) %>%
                   select(Region, year, RptArea, rfrel = total_rfrel, pelagic_rel = total_prockrel,
                          nonpel_rel, ye_rel = total_yrockrel, not_ye_nonpel_rel = total_orockrel) %>%
                   filter(RptArea != "ZZZZ" & RptArea != 0)) %>% 
  arrange(year,RptArea)

lb_rel2 %>% filter(year == 2022)

# Notes for tomorrow: clean up the truncated up RptArea names

write.csv(lb_rel2, "data/raw_dat/logbook_release_byYear.csv")






