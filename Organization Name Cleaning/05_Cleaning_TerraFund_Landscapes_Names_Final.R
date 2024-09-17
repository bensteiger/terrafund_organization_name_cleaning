# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 03/22/2024
# Last Updated: 03/22/2024
# Description: Clean and Upload Final Landscapes Organization Names

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)
library(janitor)
library(readxl)

# Load data ---------------------------------------------------------------

# top 100 names
cleaned_landscapes_names <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Landscapes",
    "clean_landscapes_org_names_03_22_24.xlsx"
  ))

# select variables --------------------------------------------------------

cleaned_landscapes_names <- cleaned_landscapes_names %>%
  select(airtable_unique_id_list, 
         organization_name_full,
         organization_name_abbr,
         application_type_list)

# place parentheses on either side of organization_name_abbr --------------

cleaned_landscapes_names <- cleaned_landscapes_names %>%
  mutate(organization_name_abbr = ifelse(!is.na(organization_name_abbr), paste0("(", organization_name_abbr, ")"), organization_name_abbr))

# final name merge --------------------------------------------------------

cleaned_landscapes_names <- cleaned_landscapes_names %>%
  unite(organization_name_merge, organization_name_full, organization_name_abbr, sep = " ", na.rm = TRUE)

# unlist airtable id's ----------------------------------------------------

cleaned_landscapes_names <- cleaned_landscapes_names %>%
  separate_longer_delim(c(airtable_unique_id_list, application_type_list), delim = ", ") %>%
  arrange(organization_name_merge, application_type_list)

# add cohort name ---------------------------------------------------------

cleaned_landscapes_names <- cleaned_landscapes_names %>%
  mutate(cohort = "TerraFund Landscapes") 

# save .csv ---------------------------------------------------------------

write_csv(cleaned_landscapes_names,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "Landscapes",
            "cleaned_landscapes_names_final.csv"
          ))

