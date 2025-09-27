################################################################################
# Checking out mortality and weight data from regions:
################################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(janitor)
###############################################################################
dat <- read.csv("data/raw_dat/Species_comp_SC/rf_mort_sc24.csv") %>%
  clean_names() %>% mutate(rel_cat = as.factor(rel_cat))

ggplot(dat, aes(x = year, y = mort_rate, col = assemblage, shape = rel_cat)) +
  geom_point() +
  facet_wrap(~cfmu + user)

ggplot(dat, aes(x = year, y = p_rel, col = assemblage, shape = rel_cat)) +
  geom_point() +
  facet_wrap(~cfmu + user)
