library(tidyverse)
library(tidycensus)
library(dplyr)
library(janitor)
library(readxl)

# Set a census API key from the census website
# http://api.census.gov/data/key_signup.html
census_api_key("112c33ba637508940d93afbf4069c58caa1a7cd5")

# Data from American Community Survey
# ACS only contains informations on counties with populations of 65,000+

# ACS Variable List

# acs2019var <- load_variables(2023, "acs5", cache=F)
# acs2015var <- load_variables(2015, "acs5", cache=F)


# Race (Unified School District) --------------------------------------------------------------------
ca_race_unified <- c()
for (i in 2015:2022){
# Race Data with variable and codes
  race_vars <- c(
    White = "B03002_003",
    Black = "B03002_004",
    Native = "B03002_005",
    Asian = "B03002_006",
    HIPI = "B03002_007",
    Other = "B03002_008",
    Hispanic = "B03002_012"
  )
  
  # Retrieves county data on above variables from the ACS
  ca_race_1 <- get_acs(
    geography = "school district (unified)",
    state = "CA",
    variables = race_vars,
    summary_var = "B03002_001",
    year = i
  )
  
  # Pivot race table
  ca_race_2 <- ca_race_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Convert values into percentages
  ca_race_3 <- ca_race_2 %>% 
    mutate(across(White:Hispanic, ~.x/summary_est*100)) %>% 
    select(-moe, -summary_est, -summary_moe)
  
  # Collapses values per county, adds year, and cleans name
  assign("ca_race_temp", 
         ca_race_3 %>% 
          group_by(GEOID, NAME) %>% 
          summarize(across(White:Hispanic, ~max(.x,na.rm=T),.names="{.col}")) %>% 
          mutate(year = i) %>% 
          clean_names()
        )
  
  # Deletes the other tibbles
  rm(ca_race_1, ca_race_2, ca_race_3, race_vars)
  
  ca_race_unified <- rbind(ca_race_unified, ca_race_temp)
  
  # removes the assigned ca_race_ 
  rm(ca_race_temp)
}
# Race (Elementary School District) --------------------------------------------------------------------
ca_race_elementary = c()
for (i in 2015:2022){
  # Race Data with variable and codes
  race_vars <- c(
    White = "B03002_003",
    Black = "B03002_004",
    Native = "B03002_005",
    Asian = "B03002_006",
    HIPI = "B03002_007",
    Other = "B03002_008",
    Hispanic = "B03002_012"
  )
  
  # Retrieves county data on above variables from the ACS
  ca_race_1 <- get_acs(
    geography = "school district (elementary)",
    state = "CA",
    variables = race_vars,
    summary_var = "B03002_001",
    year = i
  )
  
  # Pivot race table
  ca_race_2 <- ca_race_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Convert values into percentages
  ca_race_3 <- ca_race_2 %>% 
    mutate(across(White:Hispanic, ~.x/summary_est*100)) %>% 
    select(-moe, -summary_est, -summary_moe)
  
  # Collapses values per county, adds year, and cleans name
  assign("ca_race_temp", 
         ca_race_3 %>% 
           group_by(GEOID, NAME) %>% 
           summarize(across(White:Hispanic, ~max(.x,na.rm=T),.names="{.col}")) %>% 
           mutate(year = i) %>% 
           clean_names()
  )
  
  # Deletes the other tibbles
  rm(ca_race_1, ca_race_2, ca_race_3, race_vars)


ca_race_elementary <- rbind(ca_race_elementary, ca_race_temp)
rm(ca_race_temp)
}

# Household (Unified School District)  ---------------------------------------------------------------
ca_household_unified = c()
for (i in 2015:2022){
  # Household variables
  household_vars <- c(
    Family_Married = "B11005_004",
    Family_Single_Male = "B11005_006",
    Family_Single_female = "B11005_007",
    Non_Family = "B11005_008"
  )
  
  # Retrieves household type data from ACS
  ca_household_1 <- get_acs(
    geography = "school district (unified)",
    state = "CA",
    variables = household_vars,
    summary_var = "B11005_002",
    year = i
  )
  
  # Pivots table
  ca_household_2 <- ca_household_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Convert values into percentages
  ca_household_3 <- ca_household_2 %>% 
    mutate(across(Family_Married:Non_Family, ~.x/summary_est*100)) %>% 
    select(-moe, -summary_est, -summary_moe)
  
  # Collapses values per county, adds year, and cleans name
  assign("ca_household_temp",
         ca_household_3 %>% 
           group_by(GEOID, NAME) %>% 
           summarize(across(Family_Married:Non_Family, ~max(.x,na.rm=T),.names="{.col}")) %>% 
           mutate(year = i) %>% 
           clean_names()
  )
  
  rm(ca_household_1, ca_household_2, ca_household_3, household_vars)
  ca_household_unified <- rbind(ca_household_unified, ca_household_temp)
  rm(ca_household_temp)
}
# Household (Elementary School District)  ---------------------------------------------------------------
ca_household_elementary = c()
for (i in 2015:2022){
  # Household variables
  household_vars <- c(
    Family_Married = "B11005_004",
    Family_Single_Male = "B11005_006",
    Family_Single_female = "B11005_007",
    Non_Family = "B11005_008"
  )
  
  # Retrieves household type data from ACS
  ca_household_1 <- get_acs(
    geography = "school district (elementary)",
    state = "CA",
    variables = household_vars,
    summary_var = "B11005_002",
    year = i
  )
  
  # Pivots table
  ca_household_2 <- ca_household_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Convert values into percentages
  ca_household_3 <- ca_household_2 %>% 
    mutate(across(Family_Married:Non_Family, ~.x/summary_est*100)) %>% 
    select(-moe, -summary_est, -summary_moe)
  
  # Collapses values per county, adds year, and cleans name
  assign("ca_household_temp",
         ca_household_3 %>% 
          group_by(GEOID, NAME) %>% 
          summarize(across(Family_Married:Non_Family, ~max(.x,na.rm=T),.names="{.col}")) %>% 
          mutate(year = i) %>% 
          clean_names()
  )
  
  rm(ca_household_1, ca_household_2, ca_household_3, household_vars)
  
  ca_household_elementary <- rbind(ca_household_elementary, ca_household_temp)
  rm(ca_household_temp)
}

# Median Income (Unified School District) --------------------------------------
# Household median income variables
ca_income_unified = c()
for(i in 2015:2022){
# Races other thn hispanic are counted as non-hispanic
  income_vars <- c(
    White_Median_Income = "B19013H_001",
    Black_Median_Income = "B19013B_001",
    Native_Median_Income = "B19013C_001",
    Asian_Median_Income = "B19013D_001",
    HIPI_Median_Income = "B19013E_001",
    Other_Median_Income = "B19013F_001",
    Hispanic_Median_Income = "B19013I_001",
    Total_Median_Income = "B19013_001"
  )
  
  
  # Retrieves median household income from ACS
  ca_income_1 <- get_acs(
    geography = "school district (unified)",
    state = "CA",
    variables = income_vars,
    year = i
  )
  
  # Pivots table
  ca_income_2 <- ca_income_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Deletes column moe
  ca_income_3 <- ca_income_2 %>% 
    select(-moe)
  
  # Collapses values per county
  assign("ca_income_temp", ca_income_3 %>% 
    group_by(GEOID, NAME) %>% 
    summarize(across(Total_Median_Income:Hispanic_Median_Income, ~max(.x,na.rm=T),.names="{.col}")) %>% 
    mutate(year = i) %>% 
    clean_names()
  )
  
  # Deletes unneeded tibbles
  rm(ca_income_1, ca_income_2, ca_income_3)
  ca_income_unified <- rbind(ca_income_unified, ca_income_temp)
  rm(ca_income_temp)
}


# Median Income (Elementary School District) --------------------------------------
# Household median income variables
ca_income_elementary = c()
for(i in 2015:2022){
  # Races other thn hispanic are counted as non-hispanic
  income_vars <- c(
    White_Median_Income = "B19013H_001",
    Black_Median_Income = "B19013B_001",
    Native_Median_Income = "B19013C_001",
    Asian_Median_Income = "B19013D_001",
    HIPI_Median_Income = "B19013E_001",
    Other_Median_Income = "B19013F_001",
    Hispanic_Median_Income = "B19013I_001",
    Total_Median_Income = "B19013_001"
  )
  
  
  # Retrieves median household income from ACS
  ca_income_1 <- get_acs(
    geography = "school district (elementary)",
    state = "CA",
    variables = income_vars,
    year = i
  )
  
  # Pivots table
  ca_income_2 <- ca_income_1 %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Deletes column moe
  ca_income_3 <- ca_income_2 %>% 
    select(-moe)
  
  # Collapses values per county
  assign("ca_income_temp", ca_income_3 %>% 
           group_by(GEOID, NAME) %>% 
           summarize(across(Total_Median_Income:Hispanic_Median_Income, ~max(.x,na.rm=T),.names="{.col}")) %>% 
           mutate(year = i) %>% 
           clean_names()
  )
  
  # Deletes unneeded tibbles
  rm(ca_income_1, ca_income_2, ca_income_3)
  ca_income_elementary <- rbind(ca_income_elementary, ca_income_temp)
  rm(ca_income_temp)
  }
# Gini Coefficient (Elementary and Unified School District) --------------------------------
for(i in 2015:2022){
  ca_gini_1_elementary <- get_acs(
    geography = "school district (elementary)",
    state = "CA",
    variables = "B19083_001E",
    year = i
  )
  
  ca_gini_1_unified <- get_acs(
    geography = "school district (unified)",
    state = "CA",
    variables = "B19083_001E",
    year = i
  )
  
  # Pivots table
  ca_gini_2_elementary <- ca_gini_1_elementary %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  ca_gini_2_unified <- ca_gini_1_unified %>% pivot_wider(
    names_from = variable,
    values_from = estimate)
  
  # Deletes column moe
  ca_gini_3_elementary <- ca_gini_2_elementary %>% 
    select(-moe)
  
  ca_gini_3_unified <- ca_gini_2_unified %>% 
    select(-moe)
  
  
  assign(paste0("ca_gini_elementary_",i), ca_gini_3_elementary %>% 
           group_by(GEOID, NAME) %>% 
           mutate(year = i) %>% 
           clean_names()
  )
  
  assign(paste0("ca_gini_unified_",i), ca_gini_3_unified %>% 
           group_by(GEOID, NAME) %>% 
           mutate(year = i) %>% 
           clean_names()
  )

  # Deletes unneeded tibbles
  rm(ca_gini_1_elementary, ca_gini_1_unified, ca_gini_2_elementary, ca_gini_2_unified,
     ca_gini_3_elementary, ca_gini_3_unified)
}

ca_gini_elementary <- rbind(ca_gini_elementary_2015, ca_gini_elementary_2016, ca_gini_elementary_2017, ca_gini_elementary_2018,
                            ca_gini_elementary_2019, ca_gini_elementary_2020, ca_gini_elementary_2021, ca_gini_elementary_2022)
ca_gini_unified <- rbind(ca_gini_unified_2015, ca_gini_unified_2016, ca_gini_unified_2017, ca_gini_unified_2018, 
                         ca_gini_unified_2019, ca_gini_unified_2020, ca_gini_unified_2021, ca_gini_unified_2022)

rm(ca_gini_elementary_2015, ca_gini_elementary_2016, ca_gini_elementary_2017, ca_gini_elementary_2018, 
   ca_gini_elementary_2019, ca_gini_elementary_2020, ca_gini_elementary_2021, ca_gini_elementary_2022,
   ca_gini_unified_2015, ca_gini_unified_2016, ca_gini_unified_2017, ca_gini_unified_2018, 
   ca_gini_unified_2019, ca_gini_unified_2020, ca_gini_unified_2021, ca_gini_unified_2022)
# Final Combination -------------------------------------------------------
ca_race <- rbind(ca_race_unified, ca_race_elementary) %>%
  filter(geoid != "0699999")
rm(ca_race_unified, ca_race_elementary)

ca_household <- rbind(ca_household_unified, ca_household_elementary) %>%
  filter(geoid != "0699999")
rm(ca_household_unified, ca_household_elementary)

ca_income <- rbind(ca_income_unified, ca_income_elementary) %>%
  filter(geoid != "0699999")
rm(ca_income_unified, ca_income_elementary)

ca_gini <- rbind(ca_gini_elementary, ca_gini_unified) %>%
  filter(geoid != "0699999")
rm(ca_gini_elementary, ca_gini_unified)

# Joining all data
ca_census1 <- ca_race %>%
  inner_join(ca_household, by = c("geoid", "year", "name")) %>%
  inner_join(ca_income, by = c("geoid", "year", "name")) %>%
  inner_join(ca_gini, by = c("geoid", "year", "name")) %>%
  rename("gini" = "b19083_001")
rm(ca_gini, ca_household, ca_income, ca_race)

ca_census2 <- ca_census1 %>% 
  select(geoid, name, year, everything()) %>% 
  mutate(across(white:gini, ~na_if(.x, -Inf)))
ca_census3 <- ca_census2 %>% 
  mutate(non_white = 100 - white,
         non_white_income = weighted.mean(c(white_median_income, black_median_income, native_median_income, 
                                            asian_median_income, hipi_median_income, other_median_income, hispanic_median_income), 
                                          c(white/100, black/100, native/100, asian/100, 
                                            hipi/100, other/100, hispanic/100), na.rm = TRUE))

# Deletes unneeded tibbles

distr_key <- read_xlsx("pubschls.xlsx", skip=5)
# Filters distr_key to only include unique NCESdist and deletes the last 7 digits of the CDSCode
distr_key <- distr_key %>% 
  distinct(NCESDist, .keep_all = TRUE) %>% 
  select(NCESDist, CDSCode) %>%
  mutate(CDSCode = substr(CDSCode, 1, 7))

ca_census <- ca_census3 %>% 
  left_join(distr_key, by = c("geoid" = "NCESDist")) %>%
  select(geoid,CDSCode, year, everything())

rm(ca_census1, ca_census2, ca_census3, distr_key)
# Writes to csv
write.csv(ca_census, file = "ca_census_2015-2022.csv")
saveRDS(ca_census, file = "ca_census_2015-2022.rds")
