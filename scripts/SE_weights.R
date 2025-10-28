################################################################################
# Southeast Weight Data exam and data preparation
#
# Current Author: Phil Joy (philip.joy@alaska.gov)
#
# Lats updated: November 2024
#
################################################################################

library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)

source(".\\scripts//bayes_data_param_load.R")

wts <- read.csv("data/raw_dat/Species_Comp_SE/SE_RF_2006_2025_MHS_AWL_IntvID.csv") %>%
  clean_names() 

str(wts)

wts %>%
  mutate(date = as.Date(date, format = "%d-%b-%y"),
         length = as.numeric(length),
         len_cm = as.numeric(len_cm),
         weight_kg = as.numeric(weight_kg)) -> wts

unique(wts$analysis_grp)

# Notes from Diana Tersteeg:
#Starting in 2017 with the database, we had a shiftid and interviewid variable which 
# identified individual shifts (a unique day worked by an individual technician at 
# a port and harbor) and individual interviewid (a unique boat interview during a shiftid).  

# To recreate this for earlier years, I concatenated the SHSITE for location, the 
# date, harbor, and sampler type (catch or creel).  For the Interview Id I concatenated 
# the new shiftID and Boat Number/interview number).  For 2006-2016 I was able to 
# recreate a unique Shift Id and for 2011-2016 I was able to create a unique 
# InterviewID (however there were 2 records in 2011 in Sitka, and 23 records in 
# 2011 in Ketchikan where there was not an interview number recorded – these were 
# all lumped into one interview for Sitka and two interviews for Ketchikan based 
# on day and harbor).

# 2017: interviewid
# 2011-2016 also interviewid
# 2006-2010: nada:

wts %>% filter(!is.na(weight_kg)) %>%
  group_by(year,analysis_grp,gf_area,user_group) %>%
  summarise(n_samps = n(),
            n_ints = n_distinct(interview_id)) -> samps


ggplot(samps)





























