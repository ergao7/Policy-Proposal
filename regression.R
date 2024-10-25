library(MASS)
library(dplyr)
library(skimr)
library(tidyverse)
library(gridExtra)
library(broom)
library(faraway)
library(modelr)
library(ggplot2)
library(GGally)
library(caret)
library(leaps)
library(conf)
library(cowplot)



caaspp_census <- read_csv("data_hack_2024/caaspp_census.csv")

all <- lm(formula = mean_scale_score ~ grade + year + black + native + asian + hipi + 
     other + hispanic + family_single_male + family_single_female + non_family + 
     log(black_median_income) + log(native_median_income) + log(asian_median_income) + 
     log(hipi_median_income) + log(other_median_income) + log(hispanic_median_income) + 
     gini, data = caaspp_census)
summary(all)


race <- lm(formula = mean_scale_score ~ grade + year + black + native + asian + hipi + 
     other + hispanic, data = caaspp_census)
summary(race)
income <- lm(formula = mean_scale_score ~ grade + year + log(black_median_income) + 
     log(native_median_income) + log(asian_median_income) + 
     log(hipi_median_income) + log(other_median_income) + log(hispanic_median_income) + 
     gini, data = caaspp_census)
summary(income)

all <- lm(mean_scale_score ~ white + black + native + asian + hipi + other 
          + hispanic + family_married + family_single_male + family_single_female
          + total_median_income + black_median_income + native_median_income 
          + asian_median_income + hipi_median_income + other_median_income 
          + white_median_income + hispanic_median_income + gini, data = caaspp_census)
summary(all)

caaspp_census_math.csv <- read_csv("caaspp_census_math.csv")

all_math <- lm(mean_scale_score ~ white + black + native + asian + hipi + other 
              + hispanic + family_married + family_single_male + family_single_female
              + total_median_income + black_median_income + native_median_income 
              + asian_median_income + hipi_median_income + other_median_income 
              + white_median_income + hispanic_median_income, data = caaspp_census_math.csv)
summary(all_math)




hispanic <- lm(mean_scale_score ~ hispanic + log(hispanic_median_income) + grade + year, data = caaspp_census)
summary(hispanic)


ggpairs(caaspp_census,  c("white",  "black", "native",    "asian", "hipi",                           
                              "other"   ,  "hispanic", "family_married",                 
                              "family_single_male", "family_single_female", 
                              "non_family", "total_median_income", 
                              "black_median_income", "native_median_income",       
                              "asian_median_income", "hipi_median_income",  "other_median_income",              
                              "white_median_income", "hispanic_median_income" ),
        upper = list(continuous = "cor"),
        lower = list(continuous = "points", combo = "box_no_facet", discrete = "facetbar"), 
        diag = list(continuous = "densityDiag"))


