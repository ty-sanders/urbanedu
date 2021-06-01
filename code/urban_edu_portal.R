## Project: Urban Education Portal: State Funding for Universal Pre-K Programs 
## Authors: Dana Madden & Tyler Sanders 
## Source:  https://educationdata.urban.org/documentation/index.html


# API package install -----------------------------------------------------

# devtools::install_github('UrbanInstitute/education-data-package-r') 

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(urbnthemes) 
library(educationdata)
library(kableExtra)


# Query Data --------------------------------------------------------------

# 3rd grade enrollment data at the school level 2013-2017 broken out by race
enrollment_data <- get_education_data(level = "schools",
                           source = "ccd",
                           topic = "enrollment",
                           filters = list(year = 2013:2017, grade = 3),
                           subtopic = list("race"))

#write_csv(enrollment_data, "enrollment_data_race.csv") -query saved to csv

# 3rd grade assessment data at the school level 2013-2017 broken out by race
assessment_data <- get_education_data(level = "schools",
                           source = "edfacts",
                           topic = "assessments",
                           filters = list(year = 2013:2017, grade_edfacts = 3),
                           subtopic = list("race"))

#write_csv(assessment_data, "assessment_data_race.csv") -query saved to csv



# Optionally- use csv version of data instead of API after first load --------

assessment_data <- read_csv("data/assessment_data_race.csv")

enrollment_data <- read_csv("data/enrollment_data_race.csv")



# Clean,  Filter,  and Summarise Data -------------------------------------

schools_with_full_data <- assessment_data %>% 
  select(ncessch_num, year, race) %>% 
  filter(race %in% 1:4) %>% 
  group_by(ncessch_num) %>% 
  count() %>% 
  filter(n %in% 20) %>%  #all valid count is 20 for 4 race measures x 5 years
  pull(ncessch_num)

# clean data. Create state column and convert race data to a factor
combined_ed_data <- assessment_data %>% 
  filter(ncessch_num %in% schools_with_full_data) %>% 
  select(ncessch_num, school_name, year, fips, race, 
         math_test_num_valid, math_test_pct_prof_midpt,
         read_test_num_valid, read_test_pct_prof_midpt) %>% 
  left_join(y = enrollment_data, by = c("ncessch_num", "year", "fips", "race")) %>% 
  mutate(state = case_when(fips %in% 1  ~ "Alabama",                       
                           fips %in% 2  ~ "Alaska",
                           fips %in% 4  ~ "Arizona",
                           fips %in% 5  ~ "Arkansas",
                           fips %in% 6  ~ "California",
                           fips %in% 8  ~ "Colorado",
                           fips %in% 9  ~ "Connecticut",
                           fips %in% 10 ~ "Delaware",
                           fips %in% 11 ~ "District of Columbia",
                           fips %in% 12 ~ "Florida",
                           fips %in% 13 ~ "Georgia",
                           fips %in% 14 ~ "Guam",
                           fips %in% 15 ~ "Hawaii",
                           fips %in% 16 ~ "Idaho",
                           fips %in% 17 ~ "Illinois",
                           fips %in% 18 ~ "Indiana",
                           fips %in% 19 ~ "Iowa",
                           fips %in% 20 ~ "Kansas",
                           fips %in% 21 ~ "Kentucky",
                           fips %in% 22 ~ "Louisiana",
                           fips %in% 23 ~ "Maine",
                           fips %in% 24 ~ "Maryland",
                           fips %in% 25 ~ "Massachusetts",
                           fips %in% 26 ~ "Michigan",
                           fips %in% 27 ~ "Minnesota",
                           fips %in% 28 ~ "Mississippi",
                           fips %in% 29 ~ "Missouri",
                           fips %in% 30 ~ "Montana",
                           fips %in% 31 ~ "Nebraska",
                           fips %in% 32 ~ "Nevada",
                           fips %in% 33 ~ "New Hampshire",
                           fips %in% 34 ~ "New Jersey",
                           fips %in% 35 ~ "New Mexico",
                           fips %in% 36 ~ "New York",
                           fips %in% 37 ~ "North Carolina",
                           fips %in% 38 ~ "North Dakota",
                           fips %in% 39 ~ "Ohio",
                           fips %in% 40 ~ "Oklahoma",
                           fips %in% 41 ~ "Oregon",
                           fips %in% 42 ~ "Pennsylvania",
                           fips %in% 43 ~ "Puerto Rico",
                           fips %in% 44 ~ "Rhode Island",
                           fips %in% 45 ~ "South Carolina",
                           fips %in% 46 ~ "South Dakota",
                           fips %in% 47 ~ "Tennessee",
                           fips %in% 48 ~ "Texas",
                           fips %in% 49 ~ "Utah",
                           fips %in% 50 ~ "Vermont",
                           fips %in% 51 ~ "Virginia",
                           fips %in% 53 ~ "Washington",
                           fips %in% 54 ~ "West Virginia",
                           fips %in% 55 ~ "Wisconsin",
                           fips %in% 56 ~ "Wyoming",
                           TRUE         ~ NA_character_),
         race = case_when(race %in% 1 ~ "White",
                          race %in% 2 ~ "Black",
                          race %in% 3 ~ "Hispanic",
                          race %in% 4 ~ "Asian",
                          TRUE        ~ NA_character_)) %>% 
  filter(!is.na(state) | !is.na(race)) %>% 
  select(-fips)

# weighted proficiency scores by state, all races 
combined_ed_data_clean <- combined_ed_data %>% 
  group_by(state, year) %>% 
  summarise(weighted_mean_math_score = weighted.mean(x = math_test_pct_prof_midpt, w = math_test_num_valid, na.rm = TRUE),
            weighted_mean_read_score = weighted.mean(x = read_test_pct_prof_midpt, w = read_test_num_valid, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(state, year, starts_with("weighted"))

unique(combined_ed_data_clean$state) #41 states + DC have requisite race data 

mean_5_year_state <- combined_ed_data_clean %>% 
  group_by(state) %>% 
  summarise(mean_5_year_math = mean(weighted_mean_math_score,  na.rm = TRUE) / 100,
            mean_5_year_read = mean(weighted_mean_read_score,  na.rm = TRUE) / 100) %>% 
  arrange(state) 

# Range Midpoint scores by state, all races
mean_5_year_state %>% 
  mutate(mean_5_year_math   = scales::percent(mean_5_year_math, accuracy = 0.1),
         mean_5_year_read   = scales::percent(mean_5_year_read, accuracy = 0.1)) %>% 
  mutate(mean_5_year_math   = case_when(is.na(mean_5_year_math) ~ "Data Not Available", TRUE ~ mean_5_year_math),
         mean_5_year_read   = case_when(is.na(mean_5_year_read) ~ "Data Not Available", TRUE ~ mean_5_year_read))  %>% 
  
  kbl(col.names = c("State", "Average 3rd Grade Math Assessment Score 2013-2017", 
                             "Average 3rd Grade Reading Assessment Score 2013-2017")) %>% 
  kable_classic()

#calculate weighted mean range midpoint proficiency % and aggregate by state and race
combined_ed_data_clean_race <- combined_ed_data %>% 
  group_by(state, year, race) %>% 
  summarise(weighted_mean_math_score = weighted.mean(x = math_test_pct_prof_midpt, w = math_test_num_valid),
            weighted_mean_read_score = weighted.mean(x = read_test_pct_prof_midpt, w = read_test_num_valid)) %>% 
  ungroup() %>% 
  select(state, year, race, starts_with("weighted")) %>% 
  filter(!is.na(race))

unique(combined_ed_data_clean_race$state) #41 states + DC have requisite race data 


mean_5_year_state_race <- combined_ed_data_clean_race %>% 
  group_by(state, race) %>% 
  summarise(mean_5_year_math = mean(weighted_mean_math_score,  na.rm = TRUE) / 100,
            mean_5_year_read = mean(weighted_mean_read_score,  na.rm = TRUE) / 100) %>% 
  arrange(state) 

# Kable Table 3rd grade range midpoint scores by state, race, and subject
mean_5_year_state_race %>% 
  mutate(mean_5_year_math   = scales::percent(mean_5_year_math, accuracy = 0.1),
         mean_5_year_read   = scales::percent(mean_5_year_read, accuracy = 0.1)) %>% 
  mutate(mean_5_year_math   = case_when(is.na(mean_5_year_math) ~ "Data Not Available", TRUE ~ mean_5_year_math),
         mean_5_year_read   = case_when(is.na(mean_5_year_read) ~ "Data Not Available", TRUE ~ mean_5_year_read))  %>% 
  kbl(col.names = c("State", "Race", "Average 3rd Grade Math Assessment Score 2013-2017", 
                    "Average 3rd Grade Reading Assessment Score 2013-2017")) %>% 
  kable_classic()



# Build Plot --------------------------------------------------------------



# Set urban ggplot defaults 
set_urbn_defaults(style = "print")

# Build multi-category Cleveland Plot
plot <- mean_5_year_state_race %>% 
  filter(state %in% c("Maine", "New Hampshire", "Vermont", "New York",
                      "Massachusetts", "New Jersey", "Pennsylvania", "Connecticut",
                      "Rhode Island", "Delaware")) %>% 
  pivot_longer(cols = c(mean_5_year_math, mean_5_year_read), names_to = "subject", values_to = "midpoint_score") %>% 
  mutate(subject = case_when(subject %in% "mean_5_year_math" ~ "Math",
                             TRUE ~ "Reading"),
         race    = factor(race, levels = c("Black", "Hispanic", "Asian", "White")),
         label_nh  = case_when(state %in% "New Hampshire" & race %in% "Black" & subject %in% "Reading" ~ "*Only NE state with no state funded pre-k",
                               TRUE ~ NA_character_),
         label_ny  = case_when(state %in% "New York"      & race %in% "Black" & subject %in% "Reading" ~ "*Funds near-universal pre-k",
                               TRUE ~ NA_character_),
         state     = case_when(state %in% "New Hampshire" ~ "New Hampshire*",
                               state %in% "New York"      ~ "New York*",
                               TRUE ~ state),
         state     = as.factor(state)) %>%
  ggplot(aes(y = state, x = midpoint_score, color = race)) +
  geom_line(aes(group = state), alpha = 0.4, color = "black") +
  geom_point(size = 6, alpha = .9) +
  geom_text(aes(x = .22, y = state, label = label_nh), color = "black", hjust = .25, vjust = 2.2) +
  geom_text(aes(x = .35, y = state, label = label_ny), color = "black", hjust = .25, vjust = 2.2) +
  scale_color_manual(values = c("#fdbf11", "#1696d2", "#55b748", "#ec008b")) +
  labs(title = "Tracking Student Testing Proficiency by Subject and Race:",
       subtitle = "3rd grade standardized assessments: math and reading", 
       x = "Range midpoint proficiency score percent",
       y = "States (Northeast Region)") +
  theme(panel.grid.major = element_line(size = .25),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .10),
        panel.grid.minor = element_line(size = .5),
        axis.line.x = element_line(size = .75),
        axis.ticks.x = element_blank(),
        # plot.title   = element_text(hjust = .75),
        # plot.subtitle = element_text(hjust = .75)
        ) +
  scale_x_continuous(breaks = c(.10, .30, .50, .70),
                     labels = scales::percent) +
  facet_wrap(~subject)


# Add Urban special features to plot. Struggling with logo text placement 
plot_urban <- urbn_plot(urbn_logo_text(), plot, urbn_note("Midpoint proficiency is 5 year average (2013-2017) of school-level performance metrics weighted by test taking population."), urbn_source(string = "Edfacts via Urban Institute Education Data Portal, Education Commission of the States"),  ncol = 1, heights = c(1, 10, 1, 1))

# Print plot to .jpg
ggsave(plot_urban, filename = "plot_urban.jpg", dpi = 300, width = 8, height = 11)



# Full Citation -----------------------------------------------------------

"Sources: Edfacts, Education Data Portal (Version 0.11.0), Urban Institute, accessed May, 21, 2021. 
https://educationdata.urban.org/documentation/, made available under the ODC Attribution License. 
Education Commision of the States, How States Fund Pre-K, https://www.ecs.org/research-reports/key-issues/pre-k/."






  
  
  
  
  
