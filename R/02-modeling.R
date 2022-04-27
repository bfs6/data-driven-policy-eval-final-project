rm(list = ls())

####Read in Packages####
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)
library(zoo)


####Read in Data####
missouri_data <- fread("data/missouri_data_clean.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)


####Feature Engineering####
##Creating Voter Turnout Values for Previous Presidential Elections
missouri_data_clean <- 
  missouri_data %>% 
  mutate(g20161108_pct_voted_all = g20161108/total_reg,
         g20121106_pct_voted_all = g20121106/total_reg) %>% 
  
##7-Day Moving Averages for COVID
  pivot_longer(cols = c(starts_with("cases"), starts_with("deaths")),
               names_to = c("covid_type", "dates"),
               names_pattern = "(.*)_(.*)",
               values_to = "covid_counts") %>% 
  pivot_wider(names_from = "covid_type",
              values_from = "covid_counts") %>% 
  group_by(basename) %>%
  arrange(county_name, basename, dates) %>%
  mutate(cases_rollmean_7 = rollmeanr(cases, k = 7, fill = NA),
         deaths_rollmean_7 = rollmeanr(deaths, k = 7, fill = NA)) %>% 
  ungroup() %>% 
  mutate(cases_rollmean_7 = cases_rollmean_7/(popestimate2020/100),
         deaths_rollmean_7 = deaths_rollmean_7/(popestimate2020/100)) %>% 
  filter(dates >= "2020-11-01") %>% 
  select(-c(cases, deaths)) %>%
  pivot_wider(names_from = dates,
              names_glue = "{.value}_{dates}",
              values_from = c(cases_rollmean_7, deaths_rollmean_7)) %>%

##Convert Counts of Registered Voters from Each Party to Percentages
  mutate_at(vars(starts_with("party")), ~.x/total_reg) %>%

##Convert Counts of Registered Voters from Different Racial/Ethnic Groups to Percentages
  mutate_at(vars(starts_with("eth1")), ~.x/total_reg)


####Build Models####
##Unconditional Means Model
unconditional_means_model <- glmer(g20201103_pct_voted_all ~ (1|county_name), 
                                   weights = total_reg, data = missouri_data_clean, family = binomial)
summary(unconditional_means_model)

##Just Mask Mandate and Random Effects
fit1 <- glmer(g20201103_pct_voted_all ~ 
                full_county_mask_mandate_pre_election + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit1)

##Adding Primary Election Turnout
fit2 <- glmer(g20201103_pct_voted_all ~ pp20200310_pct_voted_all + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit2)

##Adding Other Presidential Elections
fit3 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit3)

fit4 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit4)

anova34 <- anova(fit4, fit3)
anova34

##Adding Mask Mandate Data
fit5 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + full_county_mask_mandate_pre_election + 
                (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit5)

fit6 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit6)

fit7 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + full_county_mask_mandate_pre_election + 
                county_seat_mask_mandate_pre_election + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit7)

anova67 <- anova(fit6, fit7)
anova67
  ##Not sure County Seat Mask mandate is necessarily useful

##Adding COVID Data
fit8 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + g20121106_pct_voted_all + 
                county_seat_mask_mandate_pre_election + `cases_rollmean_7_2020-11-03` + 
                `deaths_rollmean_7_2020-11-03` + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit8)

anova68 <- anova(fit6, fit8)
anova68

fit9 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                `cases_rollmean_7_2020-11-03` + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit9)

anova89 <- anova(fit8, fit9)
anova89

##Add Party Demographic Data
fit10 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                `cases_rollmean_7_2020-11-03` + party_rep + party_dem + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit10)

anova910 <- anova(fit9, fit10)
anova910

##Add Racial/Ethnic Background
fit11 <- glmer(g20201103_pct_voted_all ~ 
                pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                `cases_rollmean_7_2020-11-03` + party_rep + party_dem + eth1_eur + 
                eth1_hisp + eth1_aa + eth1_esa + eth1_oth + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
print(summary(fit11), correlation = TRUE)

anova1011 <- anova(fit10, fit11)
anova1011

##Model w/ Just Mask Mandate and Random Effect
fit12 <- glmer(g20201103_pct_voted_all ~ county_seat_mask_mandate_pre_election + (1|county_name), 
               weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit12)


