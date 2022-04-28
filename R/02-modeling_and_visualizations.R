rm(list = ls())

####Read in Packages####
library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)
library(zoo)
library(usmap)
library(rgdal)
library(mapproj)
library(maps)


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
  ##Not sure Full County Mask mandate is necessarily useful.

  ##Full County Mask Mandate is entirely reflected in county seat mask mandate 
  ##(if a county has a mask mandate, obviously the county seat will also have that mask mandate).
  ##For this reason, we see that county mask mandate is a more effective and useful variable to include in the models.

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
                `cases_rollmean_7_2020-11-03` + party_rep + party_dem + eth1_unk + 
                eth1_hisp + eth1_aa + eth1_esa + eth1_oth + eth1_eur + (1|county_name), 
              weights = total_reg, data = missouri_data_clean, family = binomial)
print(summary(fit11), correlation = TRUE)

anova1011 <- anova(fit10, fit11)
anova1011

##Model w/ Just Mask Mandate and Random Effect
fit12 <- glmer(g20201103_pct_voted_all ~ county_seat_mask_mandate_pre_election + (1|county_name), 
               weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit12)


####Visualizations####
##Understanding Outcome Variable
outcome_var_viz <- 
  missouri_data_clean %>% 
  ggplot(aes(x = g20201103_pct_voted_all)) + 
  geom_histogram(color = "green") + 
  labs(title = "Voter Turnout in 2020 Presidential Election in Missouri by Voting Precinct") + 
  xlab("Voter Turnout") + 
  ylab("Count")
outcome_var_viz
ggsave("plots/outcome_var_viz.pdf", outcome_var_viz, width = 11, height = 6)

##Read County Shapefiles
missouri_county_shp <- readOGR("data-raw/mo_cnty_2020_bound/mo_cnty_2020_bound.shp")
missouri_county_df <- 
  missouri_data_clean %>% 
  group_by(county_name, fips, popestimate2020,
           `cases_rollmean_7_2020-11-03`, `deaths_rollmean_7_2020-11-03`,
           county_seat_mask_mandate_pre_election, full_county_mask_mandate_pre_election) %>% 
  dplyr::summarize(pop100 = sum(pop100, na.rm = TRUE),
                   g20201103_voted_all = sum(g20201103_voted_all, na.rm = TRUE),
                   total_reg = sum(total_reg, na.rm = TRUE)) %>% 
  mutate(voter_turnout = g20201103_voted_all/total_reg, 
         total_reg_perc = total_reg/popestimate2020) %>% 
  left_join(missouri_county_shp %>% 
              fortify(region = "GEOID20") %>% 
              rename("fips" = "id") %>% 
              mutate(fips = as.numeric(fips)),
            by = "fips") %>% 
  rename("cases_rollmean_7" = "cases_rollmean_7_2020-11-03",
         "deaths_rollmean_7" = "deaths_rollmean_7_2020-11-03") %>% 
  ungroup()

##Read VTD Shapefiles
missouri_vtd_shp <- readOGR("data-raw/mo_vtd_2020_bound/mo_vtd_2020_bound.shp")
missouri_vtd_df <- 
  missouri_data_clean %>% 
  select(geocode, g20201103_pct_voted_all, total_reg, pop100, popestimate2020, county_name,
         `cases_rollmean_7_2020-11-03`, `deaths_rollmean_7_2020-11-03`,
         county_seat_mask_mandate_pre_election, full_county_mask_mandate_pre_election) %>%
  left_join(missouri_vtd_shp %>% 
              fortify(region = "GEOID20") %>% 
              rename("geocode" = "id"),
            by = "geocode") %>% 
  mutate(total_reg_perc = total_reg/pop100) %>% 
  rename("cases_rollmean_7" = "cases_rollmean_7_2020-11-03",
         "deaths_rollmean_7" = "deaths_rollmean_7_2020-11-03") %>% 
  ungroup() %>% 
  mutate(total_reg_perc = ifelse(total_reg > pop100, 0, total_reg_perc))

##Mapping Voter Turnout to County
county_voter_turnout_plot <-  
  ggplot(missouri_county_df, aes(x = long, y = lat, group = group, fill = voter_turnout)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "blue", name = "Voter Turnout", label = scales::percent) +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri 2020 Voter Turnout by Voting District")
county_voter_turnout_plot
ggsave("plots/county/county_voter_turnout_plot.pdf", county_voter_turnout_plot, width = 11, height = 6)

##Mapping Registered Voters by Percent of Total Population to County
county_reg_vote_perc_plot <-  
  ggplot(missouri_county_df, aes(x = long, y = lat, group = group, fill = total_reg_perc)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "blue", name = "Percent of Registered Voters", label = scales::percent) +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri Percent of Registered Voters for 2020 Presidential Election by County")
county_reg_vote_perc_plot
ggsave("plots/county/county_reg_vote_perc_plot.pdf", county_reg_vote_perc_plot, width = 11, height = 6)

##Mapping COVID Cases to County
county_covid_cases_plot <-  
  ggplot(missouri_county_df, aes(x = long, y = lat, group = group, fill = cases_rollmean_7)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "red", name = "COVID Cases per 100 People") +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri 7-Day Rolling Average of COVID Cases per 100 People by County (November 3, 2020)")
county_covid_cases_plot
ggsave("plots/county/county_covid_cases_plot.pdf", county_covid_cases_plot, width = 11, height = 6)

##Mapping COVID Deaths to County
county_covid_deaths_plot <-  
  ggplot(missouri_county_df, aes(x = long, y = lat, group = group, fill = deaths_rollmean_7)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "red", name = "COVID Deaths per 100 People") +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri 7-Day Rolling Average of COVID Deaths per 100 People by County (November 3, 2020)")
county_covid_deaths_plot
ggsave("plots/county/county_covid_deaths_plot.pdf", county_covid_deaths_plot, width = 11, height = 6)

##Mapping Mask Mandates to County
county_mask_mandate_plot <- 
  missouri_county_df %>% 
  mutate(mask_mandate = county_seat_mask_mandate_pre_election + full_county_mask_mandate_pre_election,
         mask_mandate = case_when(mask_mandate == 0 ~ "No Mask Mandate",
                                  mask_mandate == 1 ~ "Mask Mandate in County Seat",
                                  mask_mandate == 2 ~ "Full County Mask Mandate")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = mask_mandate)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_discrete(name = "Mask Mandate") + 
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri Mask Mandates by County")
county_mask_mandate_plot
ggsave("plots/county/county_mask_mandate_plot.pdf", county_mask_mandate_plot, width = 11, height = 6)

##Mapping Voter Turnout to Voting Precincts
vtd_voter_turnout_plot <-  
  ggplot(missouri_vtd_df, aes(x = long, y = lat, group = group, fill = g20201103_pct_voted_all)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "blue", name = "Voter Turnout", label = scales::percent) +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri Voter Turnout for 2020 Presidential Election by Voting District")
vtd_voter_turnout_plot
ggsave("plots/vtd/vtd_voter_turnout_plot.pdf", vtd_voter_turnout_plot, width = 11, height = 6)

##Mapping Registered Voters by Percent of Total Population to Voting Precincts
vtd_reg_vote_perc_plot <-  
  ggplot(missouri_vtd_df, aes(x = long, y = lat, group = group, fill = total_reg_perc)) + 
  geom_polygon(color = "gray", size = 0.05) + 
  scale_fill_gradient(low = "white", high = "blue", name = "Percent of Registered Voters", label = scales::percent) +
  coord_map() + 
  theme_void() + 
  labs(title = "Missouri Percent of Registered Voters for 2020 Presidential Election by Voting District")
vtd_reg_vote_perc_plot
ggsave("plots/vtd/vtd_reg_vote_perc_plot.pdf", vtd_reg_vote_perc_plot, width = 11, height = 6)

























