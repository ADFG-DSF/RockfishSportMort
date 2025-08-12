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

# Load the data 
H_ayg <- readRDS(".//data//bayes_dat//H_ayg.rds") %>% 
  mutate(H_lb = ifelse(H == 0, 1, H))

# Logbook releases by area, year for guided trips
R_ayg <- readRDS(".//data//bayes_dat//R_ayg.rds") %>% 
  mutate(R_lb = ifelse(R == 0, 1, R),
         Rye = ifelse(year < 2006, NA,Rye))

# SWHS harvests by area, year and user 
Hhat_ayu <- 
  readRDS(".//data//bayes_dat//Hhat_ayu.rds")  %>% 
  mutate(Hhat = ifelse(H == 0, 1, H), 
         seH = ifelse(seH == 0, 1, seH)) %>%
  arrange(area, user, year)

Chat_ayu <- 
  readRDS(".//data//bayes_dat//Chat_ayu.rds")  %>% 
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
         log_mean = log(prigui_ratio),
         lower = exp(log_mean - 1.96 * se_log),
         upper = exp(log_mean + 1.96 * se_log)) -> pri_rel_pr

ggplot(pri_rel_pr,aes(x=year,y=prigui_ratio)) + 
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.25) +
  geom_line() + 
  geom_hline(aes(yintercept = 1), col = "blue", linetype = 2) +
#  ylim(0,10) +
  facet_wrap(~area, scale = "free") +
#  facet_wrap(~area) +
  theme_bw() +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous(breaks=seq(2012,2024,2)) +
  labs(y = "Prop harvested by private:guided ratio", x = "Year") 

ggsave("figures/bayes_model/pH_prigui_ratio.png")


saveRDS(pri_rel_pr, ".\\data\\bayes_dat\\pri_rel_pr.rds")















