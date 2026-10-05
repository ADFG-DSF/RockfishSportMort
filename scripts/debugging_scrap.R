source(".\\scripts//bayes_data_param_load.R")

list2env(readinData_contemporary2(spl_knts = 4,
                                  start_comp_yr = start_ests,
                                  start_kodcomp_yr = 1998, #don't change
                                  start_yr = start_ests,
                                  end_yr = end_yr,
                                  b4_start = 2011, #don't change
                                  SE06 = "exclude"), #SE06 = "exclude"
         .GlobalEnv)

jags_dat_mod <- jags_dat

jags_dat$Hlb_ayg
jags_dat$Rlb_ayg

pH <- jags_dat$Hlb_ayg / (jags_dat$Hlb_ayg + jags_dat$Rlb_ayg)

jags_dat$Hlbo_ayg
jags_dat$Rlbo_ayg

pHo <- jags_dat$Hlbo_ayg / (jags_dat$Hlbo_ayg + jags_dat$Rlbo_ayg)

jags_dat$Hlbp_ayg / (jags_dat$Hlbp_ayg + jags_dat$Rlbp_ayg)

jags_dat$Hlby_ayg / (jags_dat$Hlby_ayg + jags_dat$Rlby_ayg)

t((jags_dat$Hlb_ayg - jags_dat$Hlbp_ayg - jags_dat$Hlby_ayg)
  /(jags_dat$Rlb_ayg - jags_dat$Rlbp_ayg - jags_dat$Rlby_ayg + 
      jags_dat$Hlb_ayg - jags_dat$Hlbp_ayg - jags_dat$Hlby_ayg)) %>%
  data.frame()


  
t(jags_dat$Hlbo_ayg /(jags_dat$Rlbo_ayg + jags_dat$Hlbo_ayg)) %>% data.frame() 

H_ayg %>% filter(area == "SOKO2SAP")
R_ayg %>% filter(area == "SOKO2SAP")

H_ayg %>% left_join(R_ayg, by = c("year","area","region")) %>%
  mutate(pH = H / (H + R),
         pHp = Hp / (Hp + Rp),
         pHy = Hye / (Hye + Rye),
         pH = Ho / (Ho + Ro)) -> raw_pH

raw_pH %>% filter(area == "SOKO2SAP")

  H_ayg2 <- readRDS(paste0(".//data//bayes_dat//H_ayg","",".rds")) %>% 
    mutate(H_lb = ifelse(H == 0, 1, H))
  
  # Logbook releases by area, year for guided trips
  R_ayg2 <- readRDS(paste0(".//data//bayes_dat//R_ayg","",".rds")) %>% 
    mutate(R_lb = ifelse(R == 0, 1, R),
           Rye = ifelse(year < 2006, NA,Rye))
  
  H_ayg2 %>% left_join(R_ayg2, by = c("year","area","region")) %>%
    mutate(pH = H / (H + R),
           pHp = Hp / (Hp + Rp),
           pHy = Hye / (Hye + Rye),
           pH = Ho / (Ho + Ro)) -> raw_pH2
  
  raw_pH2 %>% filter(area == "SOKO2SAP",
                     year > 2019)
  
  
  raw_pH %>% filter(area == "BSAI")
  raw_pH2 %>% filter(area == "BSAI", year > 2019)
  
  raw_pH %>% filter(area == "CI")
  raw_pH2 %>% filter(area == "CI", year > 2019)
  
  H_ayg0 %>% filter(area == "SOKO2SAP")  
  
  a_check %>% filter(year >= 2020,
                     AMALG == "SOKO2SAP") %>% arrange(AMALG)
  
  
  H_ayg %>% filter(area == "SOKO2SAP")  
  
  
  ##############################################################################
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
                                              "WKMA","WESTSIDE","MAINLAND")) %>%
    arrange(area,year) -> check; check
  #Note BSAI = aleutian + bering
  #Note EWYKT = IBS + EYKT
  #Note SOKO2PEN / SOKO2SAP= southeast + southwest + sakpen + chignik
  #Note WKMA = westside + mainland
  with(check, table(area,year))
  
  # years & areas with 0 harvests
  with(H_ayg0, table(year,area)) %>% data.frame() %>%
    filter(Freq < 1) %>% arrange(area,year) -> missingLBdat; missingLBdat
  
  #years where there were no recorded harvests in raw areas
  LB0yrs <- missingLBdat %>% filter(!area %in% c("BSAI","EWYKT","SOKO2PEN","WKMA")) %>%
    mutate(year = as.integer(as.character(year)),
           area = as.character(area));LB0yrs
  str(LB0yrs); str(H_ayg0)
  
  #add in 0's for years and areas with no harvests
  #for(i in 1:nrow(LB0yrs)){
  #  H_ayg0 <- H_ayg0 %>%
  #    add_row(year = LB0yrs$year[i],
  #            area = LB0yrs$area[i],
  #            H = 0, Hp = 0, Hnp = 0, Hye = 0, Ho = 0)
  #}
  
  with(H_ayg0, table(year,area))
  
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
  
  a_check %>% filter(year >= 2020) %>% arrange(AMALG)
  H_ayg0 %>% filter(year >= 2020) %>% filter(area %in% c("BSAI","EWYKT",
                                                         "SOKO2SAP",
                                                         "WKMA"))
  #check how our amalgamations match up with precanned from LB program:
  a_check %>% filter(year >= 2020) %>% mutate(area = AMALG) %>%
    left_join(H_ayg0 %>% filter(year >= 2020) %>% filter(area %in% c("BSAI","EWYKT",
                                                                     "SOKO2SAP",
                                                                     "WKMA")),
              by = c("year","area")) %>% arrange(area) %>% print(n = 50)
  
  #can see that there is missing shit for SOKO2SAP and BsAI
  rbind(H_ayg0 %>% filter(!area %in% c("BSAI","SOKO2SAP") &
                            year > 2021),
        a_check %>% filter(year > 2021) %>% 
          mutate(area = AMALG) %>% 
          select(-AMALG)) %>%
    rbind(H_ayg0 %>% filter( year < 2022)) %>% unique() %>%
    arrange(area,year) -> Hayg0patch
  
  #check we didn't screw up:
  Hayg0patch %>% filter(year >= 2020,
                        area %in% c("BSAI","EWYKT",
                                    "SOKO2SAP",
                                    "WKMA")) %>% 
    left_join(H_ayg0 %>% filter(year >= 2020) %>% filter(area %in% c("BSAI","EWYKT",
                                                                     "SOKO2SAP",
                                                                     "WKMA")),
              by = c("year","area")) %>% arrange(area) 
  
  with(Hayg0patch, table(year,area))
  
  H_ayg <- Hayg0patch %>%
    filter(!(area %in% c("ALEUTIAN", "BERING", "IBS", "EYKT", "SOUTHEAS", "SOUTHWES", #get rid of areas contained in amalgamated areas
                         "SAKPEN", "CHIGNIK", "SKMA", "WESTSIDE", "MAINLAND",
                         "SOUTHEAST","SOUTHWEST"))) %>%
    mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
           area = ifelse(area == "northeas", "northeast", area)) %>%
    left_join(lut, by = "area") %>%
    mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
    arrange(region, area, year)
  
  table(H_ayg$region, H_ayg$area)
  with(H_ayg, table(year,area)) # any 0's in the new year? Yes in 2025 for BSAI
  
  t(with(H_ayg, table(year,area))) %>% data.frame() %>%
    filter(Freq == 0) -> zero_ch; zero_ch
  
  for(i in 1:nrow(zero_ch)){
    H_ayg %>% add_row(year = as.integer(as.character(zero_ch$year[i])),
                      area = ordered(zero_ch$area[i], levels = levels(H_ayg$area)),
                      H = 0, Hp = 0, Hnp = 0, Hye = 0, Ho = 0,
                      region = "Kodiak") -> H_ayg
  }
  
  with(H_ayg, table(year,area))
  
  saveRDS(H_ayg, paste0(".\\data\\bayes_dat\\H_ayg",REP_YR,".rds"))
  
  
  #-------------------------------------------------------------------
  
  # are there rows for each year? Yes for 2025 data examined on 10-1-26
  
  H_ayg0 %>% filter(area %in% c("BSAI","ALEUTIAN","BERING",
                                "EWYKT","IBS","EYKT",
                                "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                                "WKMA","WESTSIDE","MAINLAND")) %>%
    bind_rows(a_check %>% mutate(area = AMALG) %>% select(-AMALG)) %>%
    arrange(year,area) -> amalg
  
  amalg %>%
    filter(area %in% c("BSAI","SOKO2SAP"))
  
  with(amalg %>%
         filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
       table(area,year)) # These should all be 2's if the amalgamated areas are already done
  # If they are 1's then they aren't yet in the data set and
  # need to be added in.
  
  #!!! BSAI missing since 2022 so need to add them in here
  H_ayg0 %>% bind_rows(amalg %>% filter(year %in% seq(2022,REP_YR,1) & area %in% c("BSAI"))) %>%
    arrange(area, year) %>% unique() -> H_ayg0
  
  #double check... 
  with(H_ayg0 %>%
         filter(area %in% c("BSAI","EWYKT","SOKO2SAP","WKMA")),
       table(area,year))   
  
  with(H_ayg0,
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
  
  saveRDS(H_ayg, paste0(".\\data\\bayes_dat\\H_ayg",REP_YR,".rds"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
