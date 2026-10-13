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
  
  a_check %>% filter(year >= 2020) %>% arrange(AMALG) %>% print(n = 50)
  H_ayg0 %>% filter(year >= 2020) %>% filter(area %in% c("BSAI","EWYKT",
                                                         "SOKO2SAP",
                                                         "WKMA")) %>%
    arrange(area)
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
  table(H_ayg$region, H_ayg$area)
  
  saveRDS(H_ayg, paste0(".\\data\\bayes_dat\\H_ayg",REP_YR,".rds"))
  
  #-----------------------------------------------------------------------------
  
  
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
  
  R_ayg0 %>% filter(year > 2005 & area %in% c("BSAI","ALEUTIAN","BERING",
                                              "EWYKT","IBS","EKYT",
                                              "SOKO2PEN","SOKO2SAP","SOUTHEAST","SOUTHWEST","SAKPEN","CHIGNIK",
                                              "WKMA","WESTSIDE","MAINLAND")) %>%
    arrange(area,year) -> check2; check2
  
  
  with(check2, table(area,year))
  
  # years & areas with 0 harvests
  with(R_ayg0, table(year,area)) %>% data.frame() %>%
    filter(Freq < 1) %>% arrange(area,year) -> missingLBdat2; missingLBdat2
  
  #years where there were no recorded harvests in raw areas
  LB0yrs2 <- missingLBdat2 %>% filter(!area %in% c("BSAI","EWYKT","SOKO2PEN","WKMA")) %>%
    mutate(year = as.integer(as.character(year)),
           area = as.character(area));LB0yrs2
  str(LB0yrs2); str(R_ayg0)
  
  with(R_ayg0, table(year,area))
  
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
              Rye = sum(Rye, na.rm = T),
              Ro = sum(Ro, na.rm=T)) -> a_check2
  
  a_check2 %>% filter(year >= 2020) %>% arrange(AMALG) %>% print(n = 50)
  R_ayg0 %>% filter(year >= 2020) %>% filter(area %in% c("BSAI","EWYKT",
                                                         "SOKO2SAP",
                                                         "WKMA")) %>%
    arrange(area)
  #check how our amalgamations match up with precanned from LB program:
  a_check2 %>% filter(year >= 1996) %>% mutate(area = AMALG) %>%
    left_join(R_ayg0 %>% filter(year >= 1996) %>% filter(area %in% c("BSAI","EWYKT",
                                                                     "SOKO2SAP",
                                                                     "WKMA")),
              by = c("year","area")) %>% arrange(area) %>% print(n = 100)
  
  #can see that there is missing shit for SOKO2SAP and BsAI
  rbind(R_ayg0 %>% filter(!area %in% c("BSAI","SOKO2SAP") &
                            year > 1996),
        a_check2 %>% filter(year > 1996,
                            AMALG %in% c("BSAI","SOKO2SAP")) %>% 
          mutate(area = AMALG,
                 Rye = ifelse(year < 2006, NA, Rye),
                 Ro = ifelse(year < 2006, NA, Ro)) %>% 
          select(-AMALG)) %>%
    rbind(R_ayg0 %>% filter( year < 1997)) %>% unique() %>%
    arrange(area,year) -> Rayg0patch
  
  #check we didn't screw up:
  Rayg0patch %>% filter(year >= 1996,
                        area %in% c("BSAI","EWYKT",
                                    "SOKO2SAP",
                                    "WKMA")) %>% 
    left_join(R_ayg0 %>% filter(year >= 1996) %>% filter(area %in% c("BSAI","EWYKT",
                                                                     "SOKO2SAP",
                                                                     "WKMA")),
              by = c("year","area")) %>% 
    arrange(area) 
  
  with(Rayg0patch, table(year,area))
  
  Rayg0patch %>% filter(area %in% c("BSAI","EWYKT",
                                    "SOKO2SAP",
                                    "WKMA"))
  
  R_ayg <- Rayg0patch %>%
    filter(!(area %in% c("ALEUTIAN", "BERING", "IBS", "EYKT", "SOUTHEAS", "SOUTHWES", #get rid of areas contained in amalgamated areas
                         "SAKPEN", "CHIGNIK", "SKMA", "WESTSIDE", "MAINLAND",
                         "SOUTHEAST","SOUTHWEST"))) %>%
    mutate(area = ifelse(area %in% c("AFOGNAK", "EASTSIDE", "NORTHEAST"), tolower(area), area),
           area = ifelse(area == "northeas", "northeast", area)) %>%
    left_join(lut, by = "area") %>%
    mutate(area = factor(area, lut$area, ordered = TRUE)) %>%
    arrange(region, area, year)
  
  table(R_ayg$region, R_ayg$area)
  with(R_ayg, table(year,area)) # any 0's in the new year? Yes in 2025 for BSAI
  
  t(with(R_ayg, table(year,area))) %>% data.frame() %>%
    filter(Freq == 0) -> zero_ch2; zero_ch2
  
  for(i in 1:nrow(zero_ch2)){
    R_ayg %>% add_row(year = as.integer(as.character(zero_ch$year[i])),
                      area = ordered(zero_ch2$area[i], levels = levels(R_ayg$area)),
                      R = 0, Rp = 0, Rnp = 0, Rye = 0, Ro = 0,
                      region = "Kodiak") -> R_ayg
  }
  
  with(R_ayg, table(year,area))
  table(R_ayg$region, R_ayg$area)
  
  saveRDS(R_ayg, paste0(".\\data\\bayes_dat\\R_ayg",REP_YR,".rds"))
  
  #----------------------------------------------------------------------------
  bc_obs <- 
    (jags_dat$Hhat_ayg/jags_dat$Hlb_ayg)[,35:Y] %>%
    t() %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = unique(Hhat_ayu$year[Hhat_ayu$year <= end_yr]),
           source = "observed LB") %>%
    pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
    mutate(data = "H") %>% 
    rbind((jags_dat$Rhat_ayg/jags_dat$Rlb_ayg)[,35:Y] %>%
            #rbind((jags_dat$Chat_ayg/(jags_dat$Rlb_ayg + jags_dat$Hlb_ayg))[,35:Y] %>%
            t() %>%
            as.data.frame() %>%
            setNames(nm = unique(H_ayg$area)) %>%
            mutate(year = unique(Hhat_ayu$year[Hhat_ayu$year <= end_yr]),
                   source = "observed LB") %>%
            pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
            mutate(data = "R")) %>%
    mutate(bc_lo95 = NA,
           bc_hi95 = NA) 
  
  
  
  max(bc_obs$bc)
  
  bc_obs %>% filter(data == "H") -> Hbc_obs
  
  Hbc_obs %>% filter(bc == max(Hbc_obs$bc))
  Hbc_obs %>% filter(area == "BSAI")
  
  bc_obs %>% filter(year == 2024 & data == "H") %>% print(n = 50)
  
  mu_bc_H_h
  
  
  
  
  
  mu_bc_H_h2 <- data.frame(area = unique(H_ayg$area), 
                          mu_bc = apply(exp(postH_h$sims.list$mu_bc_H), 
                                        2, mean),
                          med_bc = apply(exp(postH_h$sims.list$mu_bc_H), 
                                         2, median))
  
  exp(postH_h$sims.list$mu_bc_H)[,5]
  
  mean(exp(postH_h$sims.list$mu_bc_H)[,5])
  
  rbind(bc_mod2, bc_obs) %>%
    filter(data == "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    ggplot(aes(x = year, y = bc, color = source)) +
    geom_ribbon(aes(ymin = bc_lo95, ymax = bc_hi95, fill = source), 
                alpha = 0.2, color = NA) +
    geom_point() +
    geom_line() +
    #coord_cartesian(ylim = c(0, 5)) +
    geom_hline(aes(yintercept = med_bc), data = mu_bc_H_h) +
    facet_wrap(. ~ area, scale = "free") + theme_bw(base_size = baseTXT)+
    theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
           legend.position = "bottom",
           plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
           legend.title = element_text(size = axTiTXT),  # Adjust legend title size
           legend.text = element_text(size = axTXT)) +
    scale_colour_grey(start = 0.6, end = 0) + scale_fill_grey(start = 0.6, end = 0) +
    labs(y = "Harvest Bias", x = "Year")
  
  ################################################################################
  Hs
  with(Hs, table(variable,year))
  with(Rs, table(variable,year))
  
  Hs %>% filter(Rhat > 1.1)
  Rs %>% filter(Rhat > 1.1)
  
  lut <- 
    data.frame(region = c(rep("Kodiak", 6), rep("Central", 4), rep("Southeast", 6)),
               area = c("afognak", "WKMA", "SOKO2SAP", "northeast", "eastside", "BSAI", 
                        "CI", "NG", "PWSI", "PWSO", 
                        "EWYKT", "NSEO", "NSEI", "CSEO", "SSEO", "SSEI"),
               stringsAsFactors = FALSE) %>%
    arrange(region, area) %>%
    mutate(area_n = seq(1,16,1),
           region_n = c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3))
  
  sp_lut <- data.frame(species_n = c(1,2,3,4,5),
                       species = c("Black",
                                   "Yelloweye",
                                   "non-YE, non-pelagic",
                                   "DSR","Slope"))
  
  Hs %>% filter(Rhat > 1.1) %>% 
    group_by(variable,area) %>%
    mutate(year = year + 1976) %>%
    summarise(n = n(),
              meanmean = round(mean(mean),2),
              meanmedian = round(mean(median),2),
              minRhat = min(Rhat),
              maxRhat = max(Rhat),
              meansd = round(mean(sd),2),
              meanskew = round(mean(skew),2),
              years =  paste(sort(unique(year)), collapse = ", ")) %>%
    tidyr::separate(variable, into = c("sp_code", "user_code"), sep = "_") %>%
    mutate(
      Species = recode(sp_code,
                       Ho = "non-YE, non-pelagic",
                       Hb = "black",
                       Hp = "pelagic",
                       Hy = "yelloweye",
                       Hs = "slope",
                       Hd = "dsr",
                       H  = "all"
      ),
      User = recode(user_code,
                    ay  = "total",
                    ayg = "guided",
                    ayu = "unguided"
      )
    ) %>% select(-c(sp_code,user_code)) %>%
    left_join(lut, by = "area") %>%
    relocate(c(User,Species), .before = n) %>%
    relocate(region, .before = area) %>%
    mutate(area = as.character(area),
           area = if_else(area == toupper(area),area,stringr::str_to_sentence(area))) %>%
    arrange(region,area,User,Species) -> Hs_unconv
  
  colnames(Hs_unconv) <- c("Region","Area","User","Species","n","Mean of the means",
                           "Mean of the medians", "Min. Rhat","Max Rhat", 
                           "Mean of the SDs", "Mean skew", "Years")
  
  Hs_unconv
  sum(Hs_unconv$n)
  nrow(Hs)
  nrow(Hs %>% filter(Rhat > 1.01))
  
  Rs %>% filter(Rhat > 1.1) %>% 
    group_by(variable,area) %>%
    mutate(year = year + 1976) %>%
    summarise(n = n(),
              meanmean = round(mean(mean),2),
              meanmedian = round(mean(median),2),
              minRhat = min(Rhat),
              maxRhat = max(Rhat),
              meansd = round(mean(sd),2),
              meanskew = round(mean(skew),2),
              years =  paste(sort(unique(year)), collapse = ", ")) %>%
    tidyr::separate(variable, into = c("sp_code", "user_code"), sep = "_") %>%
    mutate(
      Species = recode(sp_code,
                       Ro = "non-YE, non-pelagic",
                       Rb = "black",
                       Rp = "pelagic",
                       Ry = "yelloweye",
                       Rs = "slope",
                       Rd = "dsr",
                       R  = "all"
      ),
      User = recode(user_code,
                    ay  = "total",
                    ayg = "guided",
                    ayu = "unguided"
      )
    ) %>% select(-c(sp_code,user_code)) %>%
    left_join(lut, by = "area") %>%
    relocate(c(User,Species), .before = n) %>%
    relocate(region, .before = area) %>%
    mutate(area = as.character(area),
           area = if_else(area == toupper(area),area,stringr::str_to_sentence(area))) %>%
    arrange(region,area,User,Species) -> Rs_unconv
  
  colnames(Rs_unconv) <- c("Region","Area","User","Species","n","Mean of the means",
                           "Mean of the medians", "Min. Rhat","Max Rhat", 
                           "Mean of the SDs", "Mean skew", "Years")
  
  y
  sum(Rs_unconv$n)
  nrow(Rs)
  nrow(Rs %>% filter(Rhat > 1.01))
  
  filter(grepl("p_", parameter) &
           !grepl("Hp_",parameter) &
           !grepl("Rp_",parameter) &
           !grepl("Tp_",parameter) &
           !grepl("Bp_",parameter)) %>% 
    separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
    mutate(index = str_replace(index, "\\]", "")) %>%
    separate(index, into = c("area_n", "year","user"), sep = ",", fill = "right") %>%
    mutate(across(starts_with("index"), as.numeric)) %>% arrange(area_n,year) %>%
    mutate(year = as.numeric(year),
           area_n = as.character(area_n)) %>%
    full_join(area_codes,by = "area_n") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))  %>% 
    filter(!is.na(year) & year < 44)
  
  Ps_h <- as.data.frame(postH_h$summary) %>%
    mutate(parameter = rownames(postH_h$summary)) %>%
    select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
    mutate(mean = round(mean,3),
           median = round(median,3),
           lower = round(lower,3),
           upper = round(upper,3),
           Rhat = round(Rhat,3),
           skew = round((mean - median)/sd,3),
           sd = round(sd,3)) %>% 
    filter(grepl("p_", parameter) &
             !grepl("Hp_",parameter) &
             !grepl("Rp_",parameter) &
             !grepl("Tp_",parameter) &
             !grepl("Bp_",parameter)) %>%
    separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
    mutate(index = str_replace(index, "\\]", "")) %>%
    separate(index, into = c("area_n", "year","user"), sep = ",", fill = "right") %>%
    mutate(across(starts_with("index"), as.numeric)) %>% arrange(area_n,year) %>%
    mutate(year = as.numeric(year),
           area_n = as.character(area_n)) %>%
    full_join(area_codes,by = "area_n") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>% 
    filter(!is.na(year) & year < 44)
  
  Ps_c <- as.data.frame(postH_c$summary) %>%
    mutate(parameter = rownames(postH_c$summary)) %>%
    select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
    mutate(mean = round(mean,3),
           median = round(median,3),
           lower = round(lower,3),
           upper = round(upper,3),
           Rhat = round(Rhat,3),
           skew = round((mean - median)/sd,3),
           sd = round(sd,3)) %>% 
    filter(grepl("p_", parameter) &
             !grepl("Hp_",parameter) &
             !grepl("Rp_",parameter) &
             !grepl("Tp_",parameter) &
             !grepl("Bp_",parameter)) %>%
    separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
    mutate(index = str_replace(index, "\\]", "")) %>%
    separate(index, into = c("area_n", "year","user"), sep = ",", fill = "right") %>%
    mutate(across(starts_with("index"), as.numeric)) %>% arrange(area_n,year) %>%
    mutate(year = as.numeric(year),
           area_n = as.character(area_n)) %>%
    full_join(area_codes,by = "area_n") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>% 
    filter(!is.na(year) & year > 43)
  
  Ps <- rbind(Ps_h,Ps_c)
unique(Ps$variable)  
nrow(Ps)
nrow(Ps %>% filter(Rhat > 1.01))  
nrow(Ps %>% filter(Rhat > 1.1))  

region_codes <- data.frame(region_n = c(1,2,3),
                           region = c("Southcentral",
                                      "Kodiak",
                                      "Southeast"))

betas_h <- as.data.frame(postH_h$summary) %>%
  mutate(parameter = rownames(postH_h$summary)) %>%
  select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
  mutate(mean = round(mean,3),
         median = round(median,3),
         lower = round(lower,3),
         upper = round(upper,3),
         Rhat = round(Rhat,3),
         skew = round((mean - median)/sd,3),
         sd = round(sd,3)) %>% 
  filter(grepl("beta", parameter)) %>%
  separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
  mutate(index = str_replace(index, "\\]", "")) %>%
  separate(index, into = c("area_n", "species"), sep = ",", fill = "right") %>%
  mutate(across(starts_with("index"), as.numeric)) %>% 
#  arrange(area_n,'year/species') %>%
  mutate(#year = as.numeric('species'),
         area_n = as.character(area_n),
         region_n = case_when(
           str_detect(variable, "mu_|tau_") ~ area_n,
           TRUE ~ as.character(lut$region_n[match(
             as.numeric(area_n), lut$area_n
           )])
         ),
         area_n = if_else(
           str_detect(variable, "mu_|tau_"),
           NA_character_,
           area_n
         ),
         area = lut$area[match(as.numeric(area_n), lut$area_n)],
         region = ifelse(str_detect(variable, "_kod"),
                         "Kodiak",
                         lut$region[match(as.numeric(region_n), lut$region_n)]),
         timeperiod = "historical") %>%
  filter(!is.na(variable))

betas_c <- as.data.frame(postH_c$summary) %>%
  mutate(parameter = rownames(postH_c$summary)) %>%
  select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
  mutate(mean = round(mean,3),
         median = round(median,3),
         lower = round(lower,3),
         upper = round(upper,3),
         Rhat = round(Rhat,3),
         skew = round((mean - median)/sd,3),
         sd = round(sd,3)) %>% 
  filter(grepl("beta", parameter)) %>%
  separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
  mutate(index = str_replace(index, "\\]", "")) %>%
  separate(index, into = c("area_n", "species"), sep = ",", fill = "right") %>%
  mutate(across(starts_with("index"), as.numeric)) %>% 
  #  arrange(area_n,'year/species') %>%
  mutate(#year = as.numeric('species'),
    area_n = as.character(area_n),
    region_n = case_when(
      str_detect(variable, "mu_|tau_") ~ area_n,
      TRUE ~ as.character(lut$region_n[match(
        as.numeric(area_n), lut$area_n
      )])
    ),
    area_n = if_else(
      str_detect(variable, "mu_|tau_"),
      NA_character_,
      area_n
    ),
    area = lut$area[match(as.numeric(area_n), lut$area_n)],
    region = ifelse(str_detect(variable, "_kod"),
                    "Kodiak",
                    lut$region[match(as.numeric(region_n), lut$region_n)]),
    timeperiod = "contemporary") %>%
  filter(!is.na(variable))

betas <- rbind(betas_h,betas_c) %>%
  mutate(species_n = species,
         species = sp_lut$species[match(as.numeric(species_n), sp_lut$species_n)])

nrow(betas)
nrow(betas %>% filter(Rhat > 1.01))
nrow(betas %>% filter(Rhat > 1.1)) 

betas %>% filter(Rhat > 1.1) %>%
  select(Region = region,
         Area = area,
         Species = species,
         Mean = mean, Median = median, 'Lower CI' = lower, 'Upper CI' = upper,
         Rhat, SD = sd, Skew = skew, Model = timeperiod
  ) -> betas_unconv
  
true_params <- c("mu_lambda_H","sigma_lambda_H","sigma_H",#"beta_H","beta0_H",
                 "lambda_H",
                 #betas,
                 #"re"
                 "sd_prigui_pre",
                 "mu_bc_H","sd_bc_H","logbc_H",
                 "mu_bc_R","sd_bc_R","logbc_R",
                 "b1_pG","b2_pG","pG")
  
oparms_h <- as.data.frame(postH_h$summary) %>%
  mutate(parameter = rownames(postH_h$summary)) %>%
  select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
  mutate(mean = round(mean,3),
         median = round(median,3),
         lower = round(lower,3),
         upper = round(upper,3),
         Rhat = round(Rhat,3),
         skew = round((mean - median)/sd,3),
         sd = round(sd,3)) %>%  
  
filter(str_detect(parameter, paste(true_params, collapse = "|"))) %>%
  separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
  mutate(index = str_replace(index, "\\]", "")) %>%
  separate(index, into = c("area_n", "year/species"), sep = ",", fill = "right") %>%
  mutate(across(starts_with("index"), as.numeric)) %>% arrange(area_n) %>%
  mutate(year = NA,
         area_n = as.character(area_n)) %>%
  full_join(area_codes,by = "area_n") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         timeperiod = "historical") %>% 
  filter(!is.na(variable))

oparms_c <- as.data.frame(postH_c$summary) %>%
  mutate(parameter = rownames(postH_c$summary)) %>%
  select(parameter, mean, median = `50%`, lower = `2.5%`,upper = `97.5%`,Rhat,sd) %>%
  mutate(mean = round(mean,3),
         median = round(median,3),
         lower = round(lower,3),
         upper = round(upper,3),
         Rhat = round(Rhat,3),
         skew = round((mean - median)/sd,3),
         sd = round(sd,3)) %>%  
  
  filter(str_detect(parameter, paste(true_params, collapse = "|"))) %>%
  separate(parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
  mutate(index = str_replace(index, "\\]", "")) %>%
  separate(index, into = c("area_n", "year/species"), sep = ",", fill = "right") %>%
  mutate(across(starts_with("index"), as.numeric)) %>% arrange(area_n) %>%
  mutate(year = NA,
         area_n = as.character(area_n)) %>%
  full_join(area_codes,by = "area_n") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         timeperiod = "contemporary") %>% 
  filter(!is.na(variable))

oparms <- rbind(oparms_h, oparms_c)

nrow(oparms)
nrow(oparms %>% filter(Rhat > 1.01))
oparms %>% filter(Rhat > 1.01)
  
  
rbind(oparms_h, oparms_c) %>%
  mutate(species_n = species,
         species = sp_lut$species[match(as.numeric(species_n), sp_lut$species_n)])
  
round(100*(nrow(betas %>% filter(Rhat > 1.1))+nrow(oparms %>% filter(Rhat > 1.1)))/(nrow(betas)+nrow(oparms)),2)
  
ceiling(max(Hs %>% filter(Rhat > 1.1) %>% select(median)))
round(100*nrow(Hs %>% filter(median <= 10 & Rhat > 1.1))/nrow(Hs %>% filter(Rhat > 1.1)),0)
