library(dplyr)
library(tidyverse)

caaspp_data <- read.csv("caaspp_data.csv") 
caaspp_data <- caaspp_data %>%
  select(-X) %>%
  mutate(district_code = as.character(district_code),
         county_code = as.character(sprintf("%02d",county_code)),
         CDSCode = paste0(county_code,district_code),
         year = test_year,
         test_name = ifelse(test_id==1, "ELA", "Math")) %>% 
  select(CDSCode, year, everything(), -district_code, -county_code, -test_year) %>% 
  filter(year != 2023, student_group_id == 1)

census_data <- readRDS("ca_census_2015-2022.rds")
census_data <- census_data%>% 
  filter(year != 2015)

# Left merge the caaspp data to the census by CDSCode and year
caaspp_census <- left_join(caaspp_data, census_data, by = c("CDSCode", "year")) %>% 
  filter(geoid != "NA") %>% # Empty geoid means no data for census
  select(-geoid, -test_id, -student_group_id)
# Separate the census by test name
caaspp_census_math <- caaspp_census %>% 
  filter(test_name == "Math")

# Same for ELA
caaspp_census_ela <- caaspp_census %>% 
  filter(test_name == "ELA")


# Write the data to csv and rds
write.csv(caaspp_census, "caaspp_census.csv", row.names = FALSE)
write.csv(caaspp_census_math, "caaspp_census_math.csv", row.names = FALSE)
write.csv(caaspp_census_ela, "caaspp_census_ela.csv", row.names = FALSE)
saveRDS(caaspp_census, "caaspp_census.rds")
saveRDS(caaspp_census_math, "caaspp_census_math.rds")
saveRDS(caaspp_census_ela, "caaspp_census_ela.rds")