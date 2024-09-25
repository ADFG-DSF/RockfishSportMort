library(tidyverse)

library(haven)


get_data <- function(a) {
  
  
  print(a)
  
  files <- list.files(path=a,
                      pattern="*.sas7bdat", full.names=F, recursive=FALSE)
  
  
  for(i in seq(1, length(files))) {
    print(files[[i]])
    n <- gsub(".sas7bdat", "",files[[i]])
    assign(n, read_sas(paste0(a,files[[i]])), envir = .GlobalEnv)
    
  }
  
}

# This code will create a function that attributes a CFMU based on ADFG Stat area
area_split <- function(a) {
  a %>%
    #    filter(YEAR >= 1993) %>%
    mutate(CFMU = case_when(
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(26) ~ "MAINLAND",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(27, 576) ~ "CHIGNIK",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(28) ~ "SAKPEN",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(30, 664, 674, 684, 694) ~ "ALEUTIAN",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(31) ~ "BERING",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(445, 446, 455, 465) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(496, 105) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(221, 223, 516, 526) ~ "CI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25110 & STATAREA <= 25235) ~ "AFOGNAK",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25311 & STATAREA <= 25640) ~ "WESTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25710 & STATAREA <= 25770) ~ "SOUTHWEST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25810 & STATAREA <= 25830) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25840 & STATAREA <= 25870) ~ "SOUTHEAST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25880 & STATAREA <= 25890) ~ "SOUTHWEST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25910 & STATAREA <= 25940) ~ "NORTHEAST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 25 & (STATAREA >= 25941 & STATAREA <= 25948) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 222 & (STATAREA >= 222000 & STATAREA <= 222050) ~ "CI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 222 & STATAREA == 222060 ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 456 & (STATAREA >= 456001 & STATAREA <= 456004) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 456 & STATAREA %in% c(456031, 456032) ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 466 & STATAREA %in% c(466001, 466002, 466004, 466005) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 466 & STATAREA %in% c(466003, 466031, 466032, 466033, 466100) ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 475 & (STATAREA >= 475500 & STATAREA <= 475932 | STATAREA == 475934) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 475 & STATAREA == 475933 ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 476 & STATAREA %in% c(476001, 476002) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 476 & (STATAREA >= 476003 & STATAREA <= 476102) ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 485 & (STATAREA >= 485430 & STATAREA <= 485831 | STATAREA %in% c(485901, 485931, 485935)) ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 485 & STATAREA %in% c(485832, 485902, 485933, 485934) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 485 & STATAREA == 485932 ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 486 & STATAREA == 486002 ~ "PWSO",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 486 & (STATAREA == 486001 | (STATAREA >= 486003 & STATAREA <= 486100)) ~ "PWSI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 495 & (STATAREA >= 495400 & STATAREA <= 495700) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 495 & (STATAREA >= 495901 & STATAREA <= 495939 | STATAREA == 495831) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 495 & STATAREA == 495730 ~ "NORTHEAST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 495 & STATAREA %in% c(495800, 495832) ~ "AFOGNAK",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 505 & (STATAREA >= 505330 & STATAREA <= 505700) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 505 & (STATAREA >= 505901 & STATAREA <= 505934 | STATAREA == 505831) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 505 & STATAREA == 505730 ~ "NORTHEAST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 505 & STATAREA %in% c(505800, 505832) ~ "AFOGNAK",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 515 & (STATAREA >= 515300 & STATAREA <= 515700) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 515 & (STATAREA >= 515901 & STATAREA <= 515906 | STATAREA %in% c(515831, 515832)) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 515 & STATAREA == 515730 ~ "NORTHEAST",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 515 & STATAREA %in% c(515801, 515802, 515833) ~ "AFOGNAK",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 515 & (STATAREA >= 515907 & STATAREA <= 515939) ~ "CI",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 525 & (STATAREA >= 525230 & STATAREA <= 525703) ~ "EASTSIDE",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 525 & STATAREA %in% c(525831, 525835, 525836, 525837) ~ "NG",
      !is.na(STATAREA) & floor(STATAREA / 1000) == 525 & STATAREA %in% c(525800, 525803) ~ "PWSO",
      TRUE ~ "UNKNOWN"
    ))  %>% 
    filter(!is.na(CFMU),
           CFMU != 'UNKNOWN')
}


get_data("S:/RTS/fbiv_data/Documents/Data/SASdata/RF/") # Load in Rockfish Bio Data



rock2020$LENGTH <- rock2020$FORK_LENGTH # Starting in 2020, Fork length was collected

library(plyr)

# Combine Rockfish data
rfdat <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                  rock2004, rock2005, rock2006, rock2007, rock2008, 
                                  rock2009, rock2010, rock2011, rock2012, rock2013, 
                                  rock2014, rock2015, rock2016, rock2017, rock2018, 
                                  rock2019, rock2020, rock2021, rock2022)) 

detach(package:plyr)

# Use area_split to tag CFMU onto rockfish data
rf <- rfdat %>% area_split() %>% 
  mutate(
    USER = case_when(
      USER == 'SewMilC' ~ 'Charter',
      TRUE ~ USER
    ),
    is_brf = case_when( 
      SP == 142 ~ 1,
      TRUE ~ 0
    ),
    is_ye = case_when(
      SP == 145 ~ 1,
      TRUE ~ 0
    ),
    is_pel = case_when(
      ASSEMB == 'Pelagic' | SP == 154 ~ 1, # A few Dusky rockfish from 2013 did not get labelled as Pelagic Assemblage
      TRUE ~ 0
    ),
    is_np = case_when(
      ASSEMB %in% c('Demersal', 'Slope') ~ 1,
      TRUE ~ 0
    ),
    is_np_not_ye = case_when(
      is_np == 1 & SP != 145 ~ 1,
      TRUE ~ 0
    )
  )

rf_c <- rf %>% 
  filter(USER == 'Charter')

rf_p <- rf %>% 
  filter(USER == 'Private')
############################
# Guided
############################
Spcomp_c <- rf_c %>% 
  group_by(USER, YEAR, PORT, CFMU) %>% 
  reframe(
    Pel = sum(is_pel),
    NP = sum(is_np),
    BRF = sum(is_brf),
    YE = sum(is_ye)
  )

write.csv(Spcomp_c, 'O:/DSF/GOAB/Data_requests/Phil Joy/Spcomp_guided.csv',
          row.names = FALSE)


###########################
Spcomp_p <- rf_p %>% 
  group_by(USER, YEAR, PORT, CFMU) %>% 
  reframe(
    Pel = sum(is_pel),
    NP = sum(is_np),
    BRF = sum(is_brf),
    YE = sum(is_ye)
  )

write.csv(Spcomp_p, 'O:/DSF/GOAB/Data_requests/Phil Joy/Spcomp_unguided.csv',
          row.names = FALSE)



