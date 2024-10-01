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
    mutate(
      CFMU = case_when(
        !is.na(STATAREA) & (floor(STATAREA / 1000) %in% c(101,102,103,105,106,107,108) | STATAREA %in% c(315401,315431,325401,325431)) ~ "SSEI",
        !is.na(STATAREA) & (floor(STATAREA / 1000) %in% c(104,335) | STATAREA %in% c(345401,345430,345500,345537,355401,355430,355500,355530,365330,365400,365430,365500,365530)) ~ "SSEO",
        !is.na(STATAREA) & floor(STATAREA / 1000) %in% c(109,110,111,112,114,115) ~ "NSEI",
        !is.na(STATAREA) & (floor(STATAREA / 1000) %in% c(116,182,375,385,395) | STATAREA %in% c(181100,181601,181602,181603)) ~ "EYKT",
        !is.na(STATAREA) & (floor(STATAREA / 1000) %in% c(183,185,186,189,191,192,405,415,425,435) | STATAREA %in% c(181400,181500,181604,181605)) ~ "IBS",
        !is.na(STATAREA) & STATAREA %in% c(345607,355601,355631,365600,365630,365701,154003,154002) ~ "CSEO",
        !is.na(STATAREA) & STATAREA %in% c(365731,365732,365801,365802) ~ "NSEO",
        !is.na(STATAREA) & floor(STATAREA / 1000) == 113 ~ case_when(
          STATAREA >= 113510 & STATAREA <= 113590 ~ "NSEI",  # NSEI-Peril Strait
          STATAREA >= 113110 & STATAREA <= 113450 ~ "CSEO",
          STATAREA >= 113611 & STATAREA <= 113660 ~ "CSEO",
          STATAREA >= 113710 & STATAREA <= 113970 ~ "NSEO"
        ),
        
        !is.na(STATAREA) & floor(STATAREA / 1000) == 26 ~ "MAINLAND", # *{Mainland salmon stat areas, Area Q/R};
        !is.na(STATAREA) & floor(STATAREA %/% 1000) %in% c(27, 576) ~ "CHIGNIK", # *{Chignik salmon stat areas, Area R};
        !is.na(STATAREA) & floor(STATAREA %/% 1000) == 28 ~ "SAKPEN", # *{South Alaska Peninsula Eastern District salmon stat areas, Area R};
        !is.na(STATAREA) & floor(STATAREA %/% 1000) %in% c(30, 664, 674, 684, 694) ~ "ALEUTIAN", # *{South Alaska Peninsula Western District salmon stat areas, Area R};
        !is.na(STATAREA) & floor(STATAREA %/% 1000) == 31 ~ "BERING", #Any harvest in Bering Sea, salmon stat areas
        !is.na(STATAREA) & floor(STATAREA %/% 1000) %in% c(445, 446, 455, 465) ~ "PWSO", #PWS Outside, Area J
        !is.na(STATAREA) & floor(STATAREA %/% 1000) %in% c(496, 105) ~ "NG", #North Gulf Coast, Area J, Stat area in 2016 listed as 105932 but likely 505932
        !is.na(STATAREA) & floor(STATAREA %/% 1000) %in% c(221, 223, 516, 526) ~ "CI", #Cook Inlet, Area P
        !is.na(STATAREA) & floor(STATAREA / 1000) == 25 ~ case_when(
          STATAREA >= 25110 & STATAREA <= 25235 ~ "AFOGNAK", #Afognak salmon stat areas, Area Q
          STATAREA >= 25311 & STATAREA <= 25640 ~ "WESTSIDE", #Westside salmon stat areas, Area Q
          STATAREA >= 25710 & STATAREA <= 25770 ~ "SOUTHWEST", #Southwest salmon stat areas, Area Q
          STATAREA >= 25810 & STATAREA <= 25830 ~ "EASTSIDE", #Eastside salmon stat areas, Area Q
          STATAREA >= 25840 & STATAREA <= 25870 ~ "SOUTHEAST", #Southeast salmon stat areas, Area Q
          STATAREA >= 25880 & STATAREA <= 25890 ~ "SOUTHWEST", #Southwest salmon stat areas, Area Q
          STATAREA >= 25910 & STATAREA <= 25940 ~ "NORTHEAST", # Northeast salmon stat areas, Area Q
          STATAREA >= 25941 & STATAREA <= 25948 ~ "EASTSIDE" #Eastside salmon stat areas, Area Q
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 222 ~ case_when(
          STATAREA >= 222000 & STATAREA <= 222050 ~ "CI", #ACook inlet, ara P
          STATAREA == 222060 ~ "NG" #Area P
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 456 ~ case_when(
          STATAREA >= 456001 & STATAREA <= 456004 ~ "PWSO", #Area J
          STATAREA %in% c(456031, 456032) ~ "PWSI" #Area J
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 466 ~ case_when(
          STATAREA %in% c(466001, 466002, 466004, 466005) ~ "PWSO", #Area J
          STATAREA %in% c(466003, 466031, 466032, 466033, 466100) ~ "PWSI" #Area J
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 475 ~ case_when(
          STATAREA >= 475500 & STATAREA <= 475932 | STATAREA %in% (475934) ~ "PWSO", #Area J
          STATAREA %in% c(475933) ~ "PWSI" #Area J
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 476 ~ case_when(
          STATAREA %in% c(476001, 476002) ~ "PWSO", #Area J
          STATAREA >= 476003 & STATAREA <= 476102 ~ "PWSI" #Area J
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 485 ~ case_when(
          STATAREA >= 485430 & STATAREA <= 485831 ~ "PWSO", #Area J
          STATAREA %in% c(485901, 485931, 485935) ~ "PWSO",
          STATAREA %in% c(485832, 485902, 485933, 485934) ~ "NG",
          STATAREA %in% c(485932) ~ "PWSI" #Area J
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 486 ~ case_when(
          STATAREA %in% c(486002) ~ "PWSO", #Area J
          STATAREA >= 486003 & STATAREA <= 486100 ~ "PWSI", #Area J
          STATAREA %in% c(486001) ~ "PWSI"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 495 ~ case_when(
          STATAREA >= 495400 & STATAREA <= 495700 ~ "EASTSIDE", #Eastside, Area Q
          STATAREA >= 495901 & STATAREA <= 495939 | STATAREA %in% c(495831) ~ "NG", #NGC, area J
          STATAREA %in% c(495730) ~ "NORTHEAST", #Northeast, Area Q
          STATAREA %in% c(495800, 495832) ~ "AFOGNAK" #Afognak salmon stat areas, Area Q
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 505 ~ case_when(
          STATAREA >= 505330 & STATAREA <= 505934 | STATAREA %in% c(505831) ~ "NG", #Eastside, Area Q
          STATAREA %in% c(505730) ~ "NORTHEAST", #Northeast, Area Q
          STATAREA %in% c(505800, 505832) ~ "AFOGNAK" #Afognak salmon stat areas, Area Q
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 515 ~ case_when(
          STATAREA >= 515300 & STATAREA <= 515700 ~ "EASTSIDE",
          STATAREA >= 515901 & STATAREA <= 515906 | STATAREA %in% c(515831, 515832) ~ "NG", #Eastside, Area Q
          STATAREA %in% c(515730) ~ "NORTHEAST", #Northeast, Area Q
          STATAREA %in% c(515801, 515802, 515833) ~ "AFOGNAK", #Afognak salmon stat areas, Area Q
          STATAREA >= 515907 & STATAREA <= 515939 ~ "CI", #Cook Inlet, Area P
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 525 ~ case_when(
          STATAREA >= 525230 & STATAREA <= 525703  ~ "EASTSIDE", #Eastside, Area Q
          STATAREA %in% c(525831, 525835, 525836, 525837) ~ "NG", #NNGC in Area J
          STATAREA %in% c(525731, 525732, 525733) ~ "NORTHEAST", #Afognak salmon stat areas, Area Q
          STATAREA >= 525801 & STATAREA <= 525807 | STATAREA %in% c(525832, 525833, 525834) ~ "AFOGNAK", 
          STATAREA >= 525901 & STATAREA <= 525932  ~ "CI"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 535 ~ case_when(
          STATAREA >= 535230 & STATAREA <= 535634 | STATAREA %in% c(535703, 535704, 535705)  ~ "SOUTHEAST", #SOUTHEAST, Area Q
          STATAREA %in% c(5535635, 535702) ~ "SOUTHWEST", #NNGC in Area J
          STATAREA %in% c(535701, 535731, 535732, 535733, 535734) ~ "WESTSIDE", #
          STATAREA %in% c(535706, 535707) ~ "EASTSIDE", #
          STATAREA %in% c(535801, 535832) ~ "MAINLAND", 
          STATAREA %in% c(535802, 535803, 535831) ~ "AFOGNAK", 
          STATAREA %in% c(535833, 535834) ~ "NG", 
          STATAREA >= 535901 & STATAREA <= 535933  ~ "CI"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 545 ~ case_when(
          STATAREA >= 545200 & STATAREA <= 545633 | STATAREA %in% c(545704)  ~ "SOUTHWEST", #SOUTHEAST, Area Q
          STATAREA %in% c(545701, 545702, 545703, 545732, 545733, 545734) ~ "WESTSIDE", #NNGC in Area J
          STATAREA %in% c(545731, 545801, 545802, 545803) ~ "MAINLAND", #
          STATAREA %in% c(545804) ~ "AFOGNAK", #
          STATAREA %in% c(545900) ~ "CI"
        ),
        
        !is.na(STATAREA) & floor(STATAREA / 1000) == 555 ~ case_when(
          STATAREA >= 555200 & STATAREA <= 555630 ~ "SOUTHWEST", #SOUTHEAST, Area Q
          STATAREA %in% c(555701) ~ "WESTSIDE", #NNGC in Area J
          STATAREA %in% c(555702, 555731, 555732, 555733) ~ "MAINLAND"
        ),
        
        !is.na(STATAREA) & floor(STATAREA / 1000) == 565 ~ case_when(
          STATAREA %in% c(565131, 565201, 565231, 565301, 565331, 565401) ~ "SAKPEN", #South Alaska Peninsula Eastern District, Area R
          STATAREA %in% c(565132, 565202, 565232, 565302, 565333, 565403, 565432, 565502, 565534, 565604, 565635) ~ "SOUTHWEST",
          STATAREA %in% c(565332, 565402, 565431, 565501, 565531, 565533, 565602, 565603, 565631, 565632, 565633, 565634, 565701, 565703) ~ "CHIGNIK", #Chignik, Area R
          STATAREA %in% c(565702, 565704) ~ "WESTSIDE"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 575 ~ case_when(
          STATAREA >= 575131 & STATAREA <= 575401 | STATAREA %in% c(575404, 575431) ~ "SAKPEN", 
          STATAREA >= 575731 & STATAREA <= 575830  ~ "BERING",  #Any harvest in Bering Sea
          STATAREA >= 575432 & STATAREA <= 575635 | STATAREA %in% c(575402, 575403) ~ "CHIGNIK"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 585 ~ case_when(
          STATAREA >= 585100 & STATAREA <= 585431 | STATAREA %in% c(585501) ~ "SAKPEN", 
          STATAREA >= 585631 & STATAREA <= 585830  ~ "BERING",  #Any harvest in Bering Sea
          STATAREA %in% c(585432, 585502, 585531, 585532, 585601, 585602, 585603) ~ "CHIGNIK"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 595 ~ case_when(
          STATAREA >= 595100 & STATAREA <= 595533  ~ "SAKPEN", 
          STATAREA >= 595631 & STATAREA <= 595833  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 605 ~ case_when(
          STATAREA >= 605100 & STATAREA <= 605533  ~ "SAKPEN", 
          STATAREA >= 605534 & STATAREA <= 605834  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 615 ~ case_when(
          STATAREA >= 615030 & STATAREA <= 615532  ~ "SAKPEN", 
          STATAREA >= 615533 & STATAREA <= 615834  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 625 ~ case_when(
          STATAREA >= 625030 & STATAREA <= 625502  ~ "SAKPEN", 
          STATAREA >= 625503 & STATAREA <= 625832  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 635 ~ case_when(
          STATAREA >= 635030 & STATAREA <= 635436 | STATAREA %in% c(635502)  ~ "SAKPEN", 
          STATAREA >= 635503 & STATAREA <= 635800 | STATAREA %in% c(635501)  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 645 ~ case_when(
          STATAREA >= 645500 & STATAREA <= 645401 | STATAREA %in% c(645403, 645404, 645405, 645432)  ~ "SAKPEN", 
          STATAREA >= 645406 & STATAREA <= 645431 | STATAREA %in% c(645402)  ~ "ALEUTIAN", # 
          STATAREA >= 645433 & STATAREA <= 645730   ~ "BERING",
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 655 ~ case_when(
          STATAREA >= 655000 & STATAREA <= 655413 ~ "ALEUTIAN", 
          STATAREA >= 655430 & STATAREA <= 655700  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 665 ~ case_when(
          STATAREA >= 665000 & STATAREA <= 665405 ~ "ALEUTIAN", 
          STATAREA >= 665430 & STATAREA <= 665600  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 675 ~ case_when(
          STATAREA >= 675000 & STATAREA <= 675400 ~ "ALEUTIAN", 
          STATAREA >= 675430 & STATAREA <= 675600  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 685 ~ case_when(
          STATAREA >= 685000 & STATAREA <= 685400 ~ "ALEUTIAN", 
          STATAREA >= 685430 & STATAREA <= 685600  ~ "BERING"
        ),
        !is.na(STATAREA) & floor(STATAREA / 1000) == 695 ~ case_when(
          STATAREA >= 695000 & STATAREA <= 695400 ~ "ALEUTIAN", 
          STATAREA >= 695430 & STATAREA <= 695600  ~ "BERING"
        ),
        #TRUE ~ NA_character_
        #TRUE ~ NA_character_
        TRUE ~ "ZZZZ"  # Default case
      )
      )  %>% 
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

write.csv(Spcomp_c, 'O:/DSF/GOAB/Data_requests/Phil Joy/Spcomp_guided_093024.csv',
          row.names = FALSE)


################################33
# Ensure you have all combinations of USER, YEAR, PORT, CFMU
all_combinations <- expand_grid(
  USER = unique(rf_c$USER),
  YEAR = unique(rf_c$YEAR),
  PORT = unique(rf_c$PORT),
  CFMU = unique(rf_c$CFMU)
)

# Reframe your data
Spcomp_c <- rf_c %>% 
  group_by(USER, YEAR, PORT, CFMU) %>% 
  summarise(
    Pel = sum(is_pel, na.rm = TRUE),
    NP = sum(is_np, na.rm = TRUE),
    BRF = sum(is_brf, na.rm = TRUE),
    YE = sum(is_ye, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  right_join(all_combinations, by = c("USER", "YEAR", "PORT", "CFMU")) %>%
  replace_na(list(
    Pel = 0,
    NP = 0,
    BRF = 0,
    YE = 0
  ))


##################################3333
write.csv(Spcomp_c, 'O:/DSF/GOAB/Data_requests/Phil Joy/Spcomp_guided_093024.csv',
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

write.csv(Spcomp_p, 'O:/DSF/GOAB/Data_requests/Phil Joy/Spcomp_unguided_093024.csv',
          row.names = FALSE)



