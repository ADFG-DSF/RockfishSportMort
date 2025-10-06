S_ayu0 <- readRDS(".//data//bayes_dat//S_ayu.rds") 
S_ayu <- 
  S_ayu0 %>% mutate(year = as.integer(year)) %>%
  bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                       year = rep(rep(1996:1997, each = 2), times = 6),
                       user = rep(c("charter", "private"), times = 12),
                       totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA,
                       dsr_n = NA, slope_n = NA)) %>%
  filter(year >= 1996) %>%
  arrange(user, area, year) 

#Interview data on kept and released 
I_ayu0 <- readRDS(".//data//bayes_dat//Int_ayu.rds") %>% arrange(area,user,year) %>%
  filter(!is.na(user))

setdiff(expand_grid(year = unique(I_ayu0$year),
                    area = unique(I_ayu0$area),
                    user = unique(I_ayu0$user)),
        I_ayu0 %>% select(year,area,user)) -> misdat
na_df <- as.data.frame(matrix(NA, nrow = nrow(misdat), 
                              ncol = length(colnames(I_ayu0)[4:18])))
colnames(na_df) <- colnames(I_ayu0)[4:18]

I_ayu <- 
  I_ayu0 %>% mutate(year = as.integer(year)) %>%
  bind_rows(bind_cols(misdat, na_df) %>%
              mutate(year = as.integer(year)) %>%
              right_join(I_ayu0 %>% select(area,region) %>% unique(),
                         by = "area")) %>%
  # filter(year >= 1996) %>%
  arrange(user, area, year) 


with(S_ayu0, table(year,area,user))
with(S_ayu, table(year,area,user))

with(I_ayu0, table(year,area,user))
with(I_ayu, table(year,area,user))

I_ayu0

ys <- unique(I_ayu0$year)

expand_grid(year = unique(I_ayu0$year),
            area = unique(I_ayu0$area),
            user = unique(I_ayu0$user)) -> fullmat

I_ayu0 %>% select(year,area,user) -> datmat

setdiff(expand_grid(year = unique(I_ayu0$year),
                    area = unique(I_ayu0$area),
                    user = unique(I_ayu0$user)),
        I_ayu0 %>% select(year,area,user)) -> misdat

nrow(fullmat)
nrow(datmat)
nrow(misdat)
nrow(datmat)+nrow(misdat)

misdat %>%
  mutate(across(.cols = all_of(colnames(I_ayu0)[3:18]), ~ NA, .names = "{.col}"))

colnames(I_ayu0)[4:18]

new_cols <- colnames(I_ayu0)[4:18]
na_df <- as.data.frame(matrix(NA, nrow = nrow(misdat), ncol = length(new_cols)))
colnames(na_df) <- new_cols

# bind to your original data frame
misdat <- bind_cols(misdat, na_df) %>%
  right_join(I_ayu0 %>% select(area,region) %>% unique(),
             by = "area")

I_ayu0 %>% select(area,region) %>% unique()

rbind(misdat,I_ayu0) -> try

setdiff(expand_grid(year = unique(I_ayu0$year),
                    area = unique(I_ayu0$area),
                    user = unique(I_ayu0$user)),
        I_ayu0 %>% select(year,area,user)) -> misdat
na_df <- as.data.frame(matrix(NA, nrow = nrow(misdat), 
                              ncol = length(colnames(I_ayu0)[4:18])))
colnames(na_df) <- colnames(I_ayu0)[4:18]


bind_cols(misdat, na_df) %>%
  right_join(I_ayu0 %>% select(area,region) %>% unique(),
             by = "area")

###############################################################################
start_yr <- 1977; end_yr <- 2023

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

int <- I_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
  mutate(area_n = as.numeric(as.factor(area)), 
         user_n = ifelse(user == "charter", 0, 1), 
         year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
         #region_n = ifelse()
         source = 1) %>%
  select(year, year_n, area_n, user_n, source, # N = totalrf_n, 
         inth_pel = pelagic_n, #black = black_n, 
         inth_yellow = ye_n, 
         inth_other = other_n, inth_dsr = dsr_n, inth_slope = slope_n,
         intc_pel = pelagic_c, #black = black_n, 
         intc_yellow = ye_c, 
         intc_other = other_c, 
         intc_dsr = dsr_c, 
         intc_slope = slope_c,
         region,area) %>%
  filter(!is.na(inth_pel))

I_ayu %>% filter(is.na(pelagic_n_rel) & !is.na(pelagic_n))
I_ayu0 %>% filter(is.na(pelagic_n_rel) & !is.na(pelagic_n))
se_int %>% filter(is.na(pelagic_n_rel) & !is.na(pelagic_n)) -> missing_se_rel

missing_se_rel %>% select(year,area) %>% unique() -> se_missing_release_dat

write.csv(se_missing_release_dat,"data/raw_dat/Species_comp_SE/SE_missing_release_data_Q.csv")

with(se_int,table(pelagic_n_rel,year,area))

int %>% filter(is.na(area_n)) 

nrow(int);nrow(I_ayu); nrow(int %>% filter(is.na(area_n)))
I_ayu$area
as.numeric(I_ayu$area)
as.numeric(I_ayu$area)

str(S_ayu$area)
str(I_ayu$area)
str(I_ayu)
View(int)
View(int %>% filter(inth_pel == 0))
