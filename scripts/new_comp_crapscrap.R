S_ayu0 <- readRDS(".//data//bayes_dat//S_ayu.rds") 

S_ayuX <- readRDS(".//data//bayes_dat//S_ayu_thru23.rds") 

head(S_ayu0); head(S_ayuX)

str(S_ayu0);str(S_ayuX)


S_ayu <- 
  S_ayu0 %>% mutate(year = as.integer(year)) %>%
  bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                       year = rep(rep(1996:1997, each = 2), times = 6),
                       user = rep(c("charter", "private"), times = 12),
                       totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA,
                       dsr_n = NA, slope_n = NA)) %>%
  filter(year >= 1996) %>%
  arrange(user, area, year) 

X_ayu <- 
  S_ayuX %>% mutate(year = as.integer(year)) %>%
  bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                       year = rep(rep(1996:1997, each = 2), times = 6),
                       user = rep(c("charter", "private"), times = 12),
                       totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA,
                       dsr_n = NA, slope_n = NA)) %>%
  filter(year >= 1996) %>%
  arrange(user, area, year) 

head(S_ayu); head(X_ayu)

comp <- S_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
  mutate(area_n = as.numeric(area), 
         user_n = ifelse(user == "charter", 0, 1), 
         year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
         #region_n = ifelse()
         source = 1) %>% 
  select(year, year_n, area_n, user_n, source, N = totalrf_n, 
         pelagic = pelagic_n, black = black_n, yellow = ye_n, 
         other = notye_nonpel_n, dsr = dsr_n, slope = slope_n,
         region,area) %>%
  filter(N != 0) %>%
  mutate(yellow = ifelse(N - pelagic == 0, NA, yellow)) #,

compX <- X_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
  mutate(area_n = as.numeric(area), 
         user_n = ifelse(user == "charter", 0, 1), 
         year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
         #region_n = ifelse()
         source = 1) %>% 
  select(year, year_n, area_n, user_n, source, N = totalrf_n, 
         pelagic = pelagic_n, black = black_n, yellow = ye_n, 
         other = notye_nonpel_n, dsr = dsr_n, slope = slope_n,
         region,area) %>%
  filter(N != 0) %>%
  mutate(yellow = ifelse(N - pelagic == 0, NA, yellow)) #,

head(comp); head(compX)

setdiff(comp,compX)

rbind(comp %>% mutate(data = "new"),
      compX %>% mutate(data = "old")) -> comparing

ggplot(comparing,aes(y = yellow, x = year, col = data, type = as.factor(user_n))) +
  geom_line() + facet_wrap(~area)

comp_area = comp$area_n
comp_year = comp$year_n
comp_user = comp$user_n
comp_N = comp$N #Nayu
comp_pelagic = comp$pelagic
comp_black = comp$black
comp_yellow = comp$yellow
comp_other = comp$other
comp_dsr = comp$dsr
comp_slope = comp$slope
comp_rslope = comp$slope
N = dim(comp)[1]

SEn1 = min(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 0,])))
SEn2 = max(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 0,])))
SEn3 = min(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 1,])))
SEn4 = max(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 1,])))

comp_areaX = compX$area_n
comp_yearX = compX$year_n
comp_userX = compX$user_n
comp_NX = compX$N #Nayu
comp_pelagicX = compX$pelagic
comp_blackX = compX$black
comp_yellowX = compX$yellow
comp_otherX = compX$other
comp_dsrX = compX$dsr
comp_slopeX = compX$slope
comp_rslopeX = compX$slope
NX = dim(compX)[1]

SEn1X = min(as.numeric(row.names(compX[compX$region == "Southeast" & compX$user_n == 0,])))
SEn2X = max(as.numeric(row.names(compX[compX$region == "Southeast" & compX$user_n == 0,])))
SEn3X = min(as.numeric(row.names(compX[compX$region == "Southeast" & compX$user_n == 1,])))
SEn4X = max(as.numeric(row.names(compX[compX$region == "Southeast" & compX$user_n == 1,])))








