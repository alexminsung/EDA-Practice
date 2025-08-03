# Load relevant libraries
library(tidyverse)
library(readr)

# Load the data
person <- read_csv("2025-08-02/FARS2023NationalCSV/person.zip")

# Select columns related to motorcyclist fatalities and filter out all non two-wheeled vehicles
person <- person %>% 
  filter(grepl("Motorcycle", BODY_TYPNAME)) %>%
  select(DOA, DRINKING, HELM_USE, INJ_SEV, BODY_TYPNAME, MONTHNAME)
 
# Replace DOA indicators from numbers to attributes
# Case_when function allows for multiple `if_else()` statements as seen below
person <- person %>% mutate(DOA = case_when(
  DOA == 0 ~ "Not Applicable",
  DOA ==  7 ~ "Died at Scene",
  DOA == 8 ~ "Died En Route",
  DOA == 9 ~ "Unknown"
))

# Replace DRINKING indicators to comprehensible attributes
person <- person %>% mutate(DRINKING = case_when(
  DRINKING == 0 ~ "No Alcohol",
  DRINKING ==  1 ~ "Alcohol Involved",
  DRINKING == 8 ~ "Not Reported",
  DRINKING == 9 ~ "Unknown"
))

# Replace HELM_USE indicators to comprehensible attributes
person <- person %>% mutate(HELM_USE = case_when(
  HELM_USE == 5 ~ "DOT-Compliant Motorcycle Helmet",
  HELM_USE ==  16 ~ "Helmet, Other than DOT-Compliant Motorcycle Helmet",
  HELM_USE == 17 ~ "No Helmet",
  HELM_USE == 19 ~ "Helmet, Unknown if DOT-Compliant ",
  HELM_USE == 20 ~ "Not Applicable",
  HELM_USE == 96 ~ "Not a Motor Vehicle Occupant",
  HELM_USE == 98 ~ "Not Reported",
  HELM_USE == 99 ~ "Unknown"
))

# Replace INJ_SEV indicators to comprehensible attributes
person <- person %>% mutate(INJ_SEV = case_when(
  INJ_SEV == 0 ~ "No Apparent Injury",
  INJ_SEV ==  1 ~ "Possible Injury",
  INJ_SEV == 2 ~ "Suspected Minor Injury",
  INJ_SEV == 3 ~ "Suspected Serious Injury",
  INJ_SEV == 4 ~ "Fatal Injury",
  INJ_SEV == 5 ~ "Injured, Severity Unknown",
  INJ_SEV == 6 ~ "Died Prior to Crash",
  INJ_SEV == 9 ~ "Unknown"
))
