## Author: Tyler Sanders
devtools::install_github('UrbanInstitute/education-data-package-r')

library(tidyverse)
library(educationdata)

df <- get_education_data(level = 'schools', 
                         source = 'ccd', 
                         topic = 'enrollment', 
                         subtopic = list('race', 'sex'),
                         filters = list(year = 2008,
                                        grade = 9:12),
                         add_labels = TRUE)

glimpse(df)
unique(df$fips)

write_csv(df, "sample_api.csv")


library(tidyverse)

nj_d_dd <- read_csv("nj_district_data/nj_district_datadictionary.csv")

nj_d <- read_csv("nj_district_data/nj_district_data.csv")

nj_d_race <- read_csv("nj_district_data/nj_district_race.csv")

unique(nj_d_race$lea_name)
unique(nj_d$lea_name)

joined_data <- nj_d %>% 
  full_join(y = nj_d_race, by = c("year", "leaid", "lea_name"))

full_race_data_counties <- joined_data %>% 
  group_by(lea_name, race) %>% 
  summarise(grad_rate_midpt) %>%  
  filter(! is.na(grad_rate_midpt) & grad_rate_midpt != "Suppressed data") %>%
  ungroup() %>% 
  group_by(lea_name) %>% 
  count() %>% 
  filter(n %in% 18) %>% 
  pull(lea_name)



joined_data %>% 
  filter(lea_name %in% full_race_data_counties) %>% 
  filter(race %in% c("White", "Black", "Asian", "Hispanic")) %>% 
  group_by(year, lea_name, race) %>% 
  summarise(grad_rate_midpt = mean(as.numeric(grad_rate_midpt))) %>% 
  mutate(grad_rate_midpt  = case_when(is.na(grad_rate_midpt) ~ 0, TRUE ~ grad_rate_midpt)) %>% 
  ggplot(aes(x = lea_name, y = grad_rate_midpt, group = race, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~year)
