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
library(stargazer)
library(sjPlot)


####Read in Data####
missouri_data <- fread("data/missouri_data_clean.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)


####Feature Engineering####
##Creating Voter Turnout Values for Previous Presidential Elections
missouri_data_clean <- 
  missouri_data %>% 
  mutate(g20161108_pct_voted_all = g20161108/total_reg,
         g20121106_pct_voted_all = g20121106/total_reg) %>% 
  
##Regularize 7-Day Moving Averages for COVID
  mutate_at(vars(c(starts_with("cases_rollmean_7"), starts_with("deaths_rollmean_7"))), 
            ~.x/(popestimate2020/100)) %>% 

##Convert Counts of Registered Voters from Each Party to Percentages
  mutate_at(vars(starts_with("party")), ~.x/total_reg) %>%

##Convert Counts of Registered Voters from Different Racial/Ethnic Groups to Percentages
  mutate_at(vars(starts_with("eth1")), ~.x/total_reg) %>% 
  mutate(party_oth = party_oth + party_npp + party_unk) %>%
  
##Add Age Predictors
  mutate(age_18_24 = age_18_19 + age_20_24,
         age_25_34 = age_25_29 + age_30_34,
         age_35_54 = age_35_44 + age_45_54, 
         age_55_64 = age_55_64,
         age_65_plus = age_65_74 + age_75_84,
         total_reg_age = age_18_24 + age_25_34 + age_35_54 + age_55_64 + age_65_plus) %>% 
  mutate_at(vars(starts_with("age")), ~.x/total_reg_age) %>% 
  select(-total_reg_age)
  
  

missouri_data_clean_long_eth <- 
  missouri_data_clean %>% 
  pivot_longer(cols = c(starts_with("g20201103_pct_voted_"), starts_with("g20201103_reg_")),
               names_to = "ethnicity",
               values_to = "g20201103_pct_and_reg_eth") %>% 
  mutate(ethnicity = 
           ethnicity %>% 
           str_replace_all("g20201103_", "") %>% 
           str_replace_all("pct_voted", "pctvtd")) %>% 
  separate(ethnicity, into = c("vote_type", "ethnicity"), sep = "_") %>% 
  mutate(vote_type = 
           vote_type %>% 
           str_replace_all("pctvtd", "g20201103_pct_voted_all_eth") %>% 
           str_replace_all("reg", "g20201103_reg_all_eth")) %>% 
  pivot_wider(names_from = "vote_type", 
              values_from = "g20201103_pct_and_reg_eth") %>%
  filter(ethnicity != "all") %>%
  mutate(ethnicity = case_when(ethnicity == "eur" ~ "White",
                               ethnicity == "hisp" ~ "Hispanic",
                               ethnicity == "aa" ~ "Black/African American",
                               ethnicity == "esa" ~ "East and South Asian",
                               ethnicity == "oth" ~ "Other", 
                               ethnicity == "unk" ~ "Unknown", 
                               TRUE ~ "NA")) %>% 
  mutate(ethnicity = na_if(ethnicity, "NA")) %>% 
  mutate_if(is_numeric, ~replace_na(., 0))


####Build Models####
##Unconditional Means Model
unconditional_means_model <- glmer(g20201103_pct_voted_all ~ (1|county_name), 
                                   weights = total_reg, data = missouri_data_clean, family = binomial)
summary(unconditional_means_model)

##Unconditional Means Model for ethnicity Data
umm_ethnicity <- glmer(g20201103_pct_voted_all_eth ~ (1|county_name/basename) + (1|ethnicity), 
                       weights = g20201103_reg_all_eth, data = missouri_data_clean_long_eth, family = binomial)
summary(umm_ethnicity)

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

##Add Vote Type
fit12 <- glmer(g20201103_pct_voted_all ~ 
                 pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                 g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                 `cases_rollmean_7_2020-11-03` + party_dem + party_oth + party_rep +
                 eth1_unk + eth1_hisp + eth1_aa + eth1_esa + eth1_oth + eth1_eur + (1|county_name), 
               weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit12)
anova1112 <- anova(fit11, fit12)
anova1112

##Add Age
fit12_2 <- glmer(g20201103_pct_voted_all ~ 
                   pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                   g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                   `cases_rollmean_7_2020-11-03` + party_dem + party_oth + party_rep +
                   eth1_unk + eth1_hisp + eth1_aa + eth1_esa + eth1_oth + eth1_eur + 
                   age_25_34 + age_35_54 + age_55_64 + age_65_plus + age_18_24 + (1|county_name), 
                 weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit12_2)
anova12 <- anova(fit12, fit12_2)
anova12

##Model w/ Just Mask Mandate and Random Effect
fit13 <- glmer(g20201103_pct_voted_all ~ county_seat_mask_mandate_pre_election + (1|county_name), 
               weights = total_reg, data = missouri_data_clean, family = binomial)
summary(fit13)

##Change Data and Add Ethnicity Random Effect
fit14 <- glmer(g20201103_pct_voted_all_eth ~ 
                 pp20200310_pct_voted_all + g20161108_pct_voted_all + 
                 g20121106_pct_voted_all + county_seat_mask_mandate_pre_election + 
                 `cases_rollmean_7_2020-11-03` + party_dem + party_oth + party_rep + 
                 absentee_perc  + (1|county_name/basename) + (1|ethnicity), 
               weights = g20201103_reg_all_eth, data = missouri_data_clean_long_eth, family = binomial)
summary(fit14)



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
  ungroup() %>%
  mutate(voter_turnout = g20201103_voted_all/total_reg, 
         total_reg_perc = total_reg/popestimate2020) %>% 
  left_join(missouri_county_shp %>% 
              fortify(region = "GEOID20") %>% 
              rename("fips" = "id") %>% 
              mutate(fips = as.numeric(fips)),
            by = "fips") %>% 
  rename("cases_rollmean_7" = "cases_rollmean_7_2020-11-03",
         "deaths_rollmean_7" = "deaths_rollmean_7_2020-11-03")

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


####Descriptive Statistics####
##Race Composition of Missouri
race_comp_mo <- 
  missouri_data_clean %>% 
  select(county_name, basename, total_reg, starts_with("eth1_")) %>% 
  mutate_at(vars(starts_with("eth1_")), ~.x*total_reg) %>% 
  pivot_longer(cols = starts_with("eth1_"),
               names_to = "eth",
               values_to = "votes_by_eth") %>% 
  group_by(eth) %>% 
  dplyr::summarize(votes_by_eth = sum(votes_by_eth, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(total_vote = sum(votes_by_eth, na.rm = TRUE),
         perc_eth = votes_by_eth/total_vote)

##Political Composition in Missouri
party_comp_mo <- 
  missouri_data_clean %>% 
  select(county_name, basename, total_reg, starts_with("party_")) %>% 
  mutate_at(vars(starts_with("party_")), ~.x*total_reg) %>% 
  pivot_longer(cols = starts_with("party_"),
               names_to = "party",
               values_to = "votes_by_party") %>% 
  group_by(party) %>% 
  dplyr::summarize(votes_by_party = sum(votes_by_party, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(votes_by_party_perc = votes_by_party/sum(votes_by_party))

##Stargazer Table of Fit 14
# stargazer(fit14, type = "html", flip = TRUE, out = "output/fit14.html", summary = TRUE)
# tab_model(fit14, fit12, 
#           showHeaderStrings = TRUE,
#           stringB = "Estimate",
#           stringCI = "Conf. Int.",
#           stringP = "p-value",
#           stringDependentVariables = "Response",
#           stringPredictors = "Coefficients",
#           labelDependentVariables = c("Voter Turnout",
#                                       "Voter Turnout"),
#           file = "output/fit14_fit12.html")
tab_model(fit12, file = "output/fit12.html", show.aic = TRUE, show.dev = TRUE, show.r2 = FALSE)
tab_model(fit12, file = "output/fit12_original.html", transform = NULL, show.aic = TRUE, show.r2 = FALSE)
tab_model(fit12, file = "output/fit12.html", show.aic = TRUE, show.dev = TRUE, show.r2 = FALSE, 
          dv.labels = c("2020 Voter Turnout"),
          pred.labels = c("Intercept", "2020 Primary Voter Turnout", "2016 General Voter Turnout",
                          "2012 General Voter Turnout", "County Seat Mask Mandate", "COVID Cases 7-Day Rolling Average",
                          "Percentage Democrat", "Percentage Other Political Affiliation", "Percentage Ethnicity - Unknown", "Percentage Ethnicity - Hispanic",
                          "Percentage Ethnicity - Black", "Percentage Ethnicity - East/South Asian", "Percentage Ethnicity - Other"))
tab_model(fit12, file = "output/fit12_original.html", transform = NULL, show.aic = TRUE, show.r2 = FALSE, 
          dv.labels = c("2020 Voter Turnout"),
          pred.labels = c("Intercept", "2020 Primary Voter Turnout", "2016 General Voter Turnout",
                          "2012 General Voter Turnout", "County Seat Mask Mandate", "COVID Cases 7-Day Rolling Average",
                          "Democratic Party", "Other Political Affiliation", "Percentage Ethnicity - Unknown", "Percentage Ethnicity - Hispanic",
                          "Percentage Ethnicity - Black", "Percentage Ethnicity - East/South Asian", "Percentage Ethnicity - Other"))


tab_model(fit12_2, file = "output/fit12_2.html", show.aic = TRUE, show.dev = TRUE, show.r2 = FALSE,
          dv.labels =  "2020 Voter Turnout", 
          pred.labels = c("Intercept", "2020 Primary Voter Turnout", "2016 General Voter Turnout",
                          "2012 General Voter Turnout", "County Seat Mask Mandate", "COVID Cases 7-Day Rolling Average",
                          "Percentage Democrat", "Percentage Other Political Affiliation", "Percentage Ethnicity - Unknown", "Percentage Ethnicity - Hispanic",
                          "Percentage Ethnicity - Black", "Percentage Ethnicity - East/South Asian", "Percentage Ethnicity - Other",
                          "Percentage Age 25-34", "Percentage Age 35-54", "Percentage Age 55-64", "Percentage Age 65+"))










