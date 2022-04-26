rm(list = ls())

####Read in Libraries####
library(tidyverse)
library(data.table)
library(janitor)
library(rvest)


# ####Get Voting Data for Missouri####
##Precinct Level
full_precinct_data <- fread("data-raw/2020-PRESIDENT-precinct-general.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
mo_precinct_data <- fread("data-raw/2020-mo-precinct-general.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##Clean Precinct Data
full_precinct_data_clean <-
  full_precinct_data %>%
  clean_names() %>%
  mutate_all(~na_if(., "")) %>%
  filter(office == "US PRESIDENT",
         state == "MISSOURI")
rm(full_precinct_data)
mo_precinct_data_clean <-
  mo_precinct_data %>%
  clean_names() %>%
  mutate_all(~na_if(., "")) %>%
  filter(office == "US PRESIDENT")


####Get VTD Level Data from Census Bureau####
##First VTD Data
vtd_data1 <- fread("data-raw/MO_L2_2020VTDAgg/MO_l2_2020vtd_agg_20210902.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
vtd_data1_clean <- 
  vtd_data1 %>% 
  clean_names() %>% 
  select(c(geoid, countyfp, vtd, total_reg, starts_with("age"), starts_with("party"), starts_with("eth"), 
           g20201103, pp20200310, g20161108, pp20160315, g20121106, pp20120207)) %>% 
  rename("geocode" = "geoid")

##Second VTD Data
vtd_data2 <- fread("data-raw/MO_L2_Turnout_2020VTDAgg/MO_l2_turnout_stats_vtd20.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
vtd_data2_clean <- 
  vtd_data2 %>% 
  clean_names() %>% 
  select(c(vtd_geoid20, total_reg, starts_with("g20201103"), starts_with("pp20200310"))) %>% 
  rename("geocode" = "vtd_geoid20")

##VTD Identifying File
vtd_id_data <- fread("data-raw/mo_pl2020_vtd/mo_pl2020_vtd.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
vtd_id_data_clean <- 
  vtd_id_data %>% 
  clean_names() %>% 
  select(name, basename, geoid, geocode, county, arealand, pop100, hu100)

##Merge Data
vtd_data_full <-
  vtd_id_data_clean %>% 
  full_join(vtd_data1_clean, by = "geocode") %>% 
  full_join(vtd_data2_clean, by = "geocode") %>% 
  select(-total_reg.y) %>% 
  rename("total_reg" = "total_reg.x") %>% 
  filter(is.na(total_reg) == FALSE) %>% 
  mutate(fips = 
           geocode %>% 
           str_sub(1, 5) %>% 
           as.numeric()) %>% 
  relocate("fips", .after = "geocode")


####Covid Data####
##Get COVID Data by County
covid_data <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
covid_data_clean <- 
  covid_data %>% 
  filter(state == "Missouri") %>% 
  select(-state) %>% 
  filter(date >= "2020-10-01",
         date <= "2020-11-03") %>% 
  pivot_wider(names_from = date,
              names_glue = "{.value}_{date}",
              values_from = c(cases, deaths)) %>% 
  filter(is.na(fips) == FALSE) %>% 
  rename("county_name" = "county")

####Assign Mask Mandate Status####
mask_mandate_data <- 
  data.frame(county_name = c("Adair", "Andrew", "Boone", "Taney", "Stone", "Cape Girardeau", "Clay", "Clinton",
                             "Taney", "Franklin", "Callaway", "Grundy", "Jackson", "Jefferson", "Johnson", 
                             "Jasper", "Newton", "Jackson", "Clay", "Cass", "Platte", "Lafayette", "Lincoln",
                             "Livingston", "Webster", "Nodaway", "Barry", "Lawrence", "Christian", "Clay", "Christian",
                             "Pettis", "Phelps", "Platte", "Greene", "Buchanan", "St. Louis", "St. Louis City"),
             mask_mandate_effective_date = c("2020-11-25", "2020-12-01", "2020-07-06", "2020-07-31",
                                             "2020-08-12", "2020-07-13", "2020-07-19", "2020-11-23", 
                                             "2020-07-20", "2020-11-20", "2020-12-01", "2020-12-11", 
                                             "2020-07-01", "2020-11-27", "2020-10-12", "2020-11-19", 
                                             "2020-11-19", "2020-06-26", "2020-06-26", "2020-06-26", 
                                             "2020-06-26", "2020-11-19", "2020-12-11", "2020-11-20", 
                                             "2020-11-23", "2020-07-22", "2020-12-03", "2020-12-03", 
                                             "2020-10-21", "2020-07-02", "2020-10-21", "2020-08-07", 
                                             "2020-11-30", "2020-07-16", "2020-07-16", "2020-09-17", 
                                             "2020-07-03", "2020-07-03"),
             whole_county_mask_mandate = c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 
                                           0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1),
             cities_mask_mandate = c(NA, NA, NA, "Branson", "Branson West", NA, NA, NA, "Forsyth", NA,
                                     "Fulton", NA, NA, NA, NA, "Joplin", "Joplin", "Kansas City", "Kansas City",
                                     "Kansas City", "Kansas City", NA, NA, NA, "Marshfield", "Maryville",
                                     "Monett", "Monett", "Nixa", "North Kansas City", "Ozark", NA, NA, NA,
                                     "Springfield", "St. Joseph", NA, NA)) %>% 
  mutate(mask_mandate_effective_date = as.Date(mask_mandate_effective_date))

mo_county_seat_url <- "https://www.whereig.com/usa/states/missouri/counties/"
mo_counties_and_seats <-
  mo_county_seat_url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  .[[2]] %>% 
  clean_names() %>% 
  select(county, county_seat) %>% 
  mutate(county = 
           county %>% 
           str_replace_all(" County", ""),
         county_seat = 
           county_seat %>% 
           str_replace_all("Kansas City,", "Kansas City")) %>% 
  rename("county_name" = "county")

mask_mandate_data_full <- 
  mask_mandate_data %>% 
  left_join(mo_counties_and_seats, by = "county_name") %>% 
  mutate(county_seat_mask_mandate = ifelse(county_seat == cities_mask_mandate, 1, 0),
         county_seat_mask_mandate = replace_na(county_seat_mask_mandate, 0)) %>% 
  select(-c(cities_mask_mandate, county_seat)) %>% 
  arrange(county_name) %>% 
  unique()

mask_mandate_data_full_county <- 
  mask_mandate_data_full %>% 
  filter(whole_county_mask_mandate == 1) %>% 
  select(-county_seat_mask_mandate)

mask_mandate_county_seat <- 
  mask_mandate_data_full %>% 
  filter(county_seat_mask_mandate == 1) %>% 
  select(-whole_county_mask_mandate)
  

####Combine Data####
##Combine Voter Data w/ COVID Data
covid_vtd_data_combined <- 
  vtd_data_full %>% 
  left_join(covid_data_clean, by = "fips") %>% 
  relocate("county_name", .after = "basename") %>% 
  mutate(county_name = 
           county_name %>% 
           str_replace_all("St. Louis city", "St. Louis City"))

##Combine Data w/ Mask Mandate Data
mo_data_full <- 
  covid_vtd_data_combined %>% 
  left_join(mask_mandate_data_full_county, by = "county_name") %>% 
  left_join(mask_mandate_county_seat, by = "county_name") %>% 
  mutate(full_county_mask_mandate_pre_election = 
           ifelse(mask_mandate_effective_date.x <= "2020-11-03" & whole_county_mask_mandate == 1, 1, 0),
         county_seat_mask_mandate_pre_election = 
           ifelse(mask_mandate_effective_date.y <= "2020-11-03" & county_seat_mask_mandate == 1, 1, 0)) %>% 
  select(-starts_with("mask_mandate_effective_date")) %>% 
  mutate_at(vars(contains("mask_mandate")), ~replace_na(.x, 0))


####Write Final Dataset####
fwrite(mo_data_full, "data/missouri_data_clean.csv", row.names = FALSE)



