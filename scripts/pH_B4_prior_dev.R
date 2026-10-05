###########################################################
# This script is for developing priors for the B4 term describing the 
# relationship between private and guided pH (proportion harvested)
#
##############################################################

library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)

end_yr <- 2025


# Load the data 
H_ayg <- readRDS(paste0(".//data//bayes_dat//H_ayg",end_yr,".rds")) %>% 
  mutate(H_lb = ifelse(H == 0, 1, H))

# Logbook releases by area, year for guided trips
R_ayg <- readRDS(paste0(".//data//bayes_dat//R_ayg",end_yr,".rds")) %>% 
  mutate(R_lb = ifelse(R == 0, 1, R),
         Rye = ifelse(year < 2006, NA,Rye))

# SWHS harvests by area, year and user 
Hhat_ayu <- 
  readRDS(paste0(".//data//bayes_dat//Hhat_ayu_thru",end_yr,".rds"))  %>% 
  mutate(Hhat = ifelse(H == 0, 1, H), 
         seH = ifelse(seH == 0, 1, seH)) %>%
  arrange(area, user, year)

Chat_ayu <- 
  readRDS(paste0(".//data//bayes_dat//Chat_ayu_thru",end_yr,".rds"))  %>% 
  mutate(Chat = ifelse(C == 0, 1, C), 
         seC = ifelse(seC == 0, 1, seC)) %>%
  arrange(area, user, year)

Rhat_ayu <- Hhat_ayu %>%
  left_join(Chat_ayu, by = c("year","area","user","region")) %>%
  mutate(R = C - H,
         seR = sqrt(seH^2 + seC^2)) %>%
  mutate(Rhat = ifelse(R <= 0, 1, R), 
         seR = ifelse(seR == 0, 1, seR)) %>%
  arrange(area, user, year)

#-----------------------------------------------------------------------
# First step get the correlation between harvests and releases
Rhat_ayu %>% group_by(area,user) %>%
  summarize(cor = cor(Rhat, Hhat, use = "complete.obs"), 
            cov = cov(Rhat, Hhat, use = "complete.obs"), 
            .groups = "drop") %>%
  mutate(cov = ifelse(cov < 0,0,cov))-> R_H_cov

print(R_H_cov, n = 50)

#get priors from pri:gui release ratio
pri_rel_pr <- Rhat_ayu %>%
  select(year,area,user,Rhat,Hhat,seR,seH) %>% #not interested in bias
  full_join(R_H_cov, by = c("area","user")) %>%
  mutate(pH = Hhat / (Rhat + Hhat),
         var_pH = (Rhat^2 * seH^2 + Hhat^2 * seR^2 - 2 * Rhat * Hhat * cor * seH * seR) / 
           (Rhat + Hhat)^4,
         se_pH = sqrt(var_pH),
         cv_pH = se_pH / pH) %>%
  select(-c(Rhat,Hhat,seR,seH,cor,cov)) %>%
  pivot_wider(names_from = user,
              values_from = c(pH,se_pH,var_pH,cv_pH)) #%>%

#need correlation between pH_guided and pH_private
pri_rel_pr %>% group_by(area) %>%
  summarise(cor = cor(pH_private, pH_guided, use = "complete.obs"),
            .groups = "drop") -> pH_cor

pri_rel_pr %>% 
  full_join(pH_cor, by = "area") %>%
  mutate(prigui_ratio = pH_private/pH_guided,
         ratio_var = (se_pH_private^2 / pH_guided^2) +
           (pH_private^2 * se_pH_guided^2 / pH_guided^4) -
           (2 * pH_private * cor * se_pH_private * se_pH_guided / pH_guided^3),
         ratio_se = sqrt(ratio_var),
         ratio_cv = ratio_se / prigui_ratio,
         se_log = sqrt(log(1 + ratio_cv^2)),
         log_ratio = log(prigui_ratio),
         lower = exp(log_ratio - 1.96 * se_log),
         upper = exp(log_ratio + 1.96 * se_log),
         lower2 = exp(log_ratio -  se_log),
         upper2 = exp(log_ratio +  se_log)) -> pri_rel_pr

meds <- pri_rel_pr %>% group_by(area) %>%
  summarise(med_ratio = median(prigui_ratio),
            mean_ratio = mean(prigui_ratio),
            var_med_ratio = sum(ratio_var)/(n()^2),
            se_med_ratio = sqrt(var_med_ratio),
            cv_med_ratio = se_med_ratio / mean_ratio,
            max_cv = max(ratio_cv))

ggplot(pri_rel_pr,aes(x=year,y=prigui_ratio)) + 
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.25) +
  geom_ribbon(aes(ymin = lower2,
                  ymax = upper2),
              alpha = 0.25) +
  geom_line() + 
  geom_hline(aes(yintercept = 1), col = "blue", linetype = 2) +
  geom_hline(data = meds,aes(yintercept = mean_ratio), col ="black", linetype = 3) +
#  ylim(0,10) +
  facet_wrap(~area, scale = "free") +
#  facet_wrap(~area) +
  theme_bw() +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous(breaks=seq(2012,2025,2)) +
  labs(y = "Proportion harvested ratio (private:guided anglers)", x = "Year") 

ggsave("figures/bayes_model/pH_prigui_ratio.png")

pri_rel_pr %>% data.frame()

expand_grid(year = seq(1977,2010,1),
            area = unique(pri_rel_pr$area)) %>%
  full_join(meds, by = "area") %>%
  select(year,area,mean_ratio,max_cv) %>%
  rename(prigui_ratio = mean_ratio,
         ratio_cv = max_cv) %>% 
    arrange(area,year) %>%
  rowwise() %>%
  mutate(lower = (prigui_ratio  - 1.96 * ratio_cv * prigui_ratio ),
         upper = (prigui_ratio  + 1.96 * ratio_cv * prigui_ratio ),
         lower2 = (prigui_ratio  - ratio_cv * prigui_ratio ),
         upper2 = (prigui_ratio  + ratio_cv * prigui_ratio )) %>%
  rbind(pri_rel_pr %>% 
          select(year,area,prigui_ratio,ratio_cv,lower,upper,lower2,upper2)) -> prigui_priors #%>%
#  arrange(area,year) -> prigui_priors
View(prigui_priors)

ggplot(prigui_priors,aes(x=year,y=prigui_ratio)) + 
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.25) +
  geom_ribbon(aes(ymin = lower2,
                  ymax = upper2),
              alpha = 0.25) +
  geom_line() + 
  geom_hline(aes(yintercept = 1), col = "blue", linetype = 2) +
  geom_hline(data = meds,aes(yintercept = mean_ratio), col ="black", linetype = 3) +
  #  ylim(0,10) +
  facet_wrap(~area, scale = "free") +
  #  facet_wrap(~area) +
  theme_bw() +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous(breaks=seq(1978,2024,2)) +
  labs(y = "Proportion harvested ratio (private:guided anglers)", x = "Year") 

# Before saving, compare to last year to make sure nothing went sideways:

pri_rel_pr_old <- readRDS(".//data//bayes_dat//pri_rel_pr.rds")

pri_rel_pr_old %>% # filter(area %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")) %>%
  mutate(source = "last_year" ) %>% #select(-region) %>% 
  rbind(pri_rel_pr %>% mutate(source = "this_year")) -> check

ggplot(check, 
         aes(x = year, y = pH_guided, 
             colour = source, linetype = source, shape = source)) +
    facet_wrap(~area, scale = "free") +
    geom_line() + geom_point() 


saveRDS(pri_rel_pr, paste0(".\\data\\bayes_dat\\pri_rel_pr_thru",end_yr,".rds"))
#saveRDS(prigui_priors, ".\\data\\bayes_dat\\pri_rel_pr.rds")








