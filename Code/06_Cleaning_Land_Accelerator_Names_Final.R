# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 04/11/2024
# Last Updated: 04/11/2024
# Description: Clean and Upload Final Land Accelerator Organization Names

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)
library(janitor)
library(readxl)

# Load data ---------------------------------------------------------------

# land accelerator post Amanda cleaning names and manual checking
cleaned_land_accelerator_names <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Land Accelerator",
    "land_accelerator_org_names_04_18_24.xlsx"
  ))

# remaining land accelerator org names post manual checking
cleaned_extra_land_accelerator_names <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Land Accelerator",
    "land_accelerator_extra_org_names_04_17_24.xlsx"
  ))

# select variables --------------------------------------------------------

# cleaned_land_accelerator_names
cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  select(airtable_unique_id_list, 
         organization_name_full,
         organization_name_abbr,
         land_accelerator_cohort_year_list)

# cleaned_extra_land_accelerator_names
cleaned_extra_land_accelerator_names <- cleaned_extra_land_accelerator_names %>%
  select(airtable_unique_id_list, 
         organization_name_full,
         organization_name_abbr,
         land_accelerator_cohort_year_list)


# bind data ---------------------------------------------------------------

cleaned_land_accelerator_names <-
  rbind(cleaned_land_accelerator_names,
        cleaned_extra_land_accelerator_names)

# list and regroup --------------------------------------------------------

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  group_by(organization_name_full) %>%
  mutate(land_accelerator_cohort_year_list = paste(land_accelerator_cohort_year_list, collapse = ", ")) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id_list, collapse = ", ")) %>%
  ungroup()

# land accelerator grouped

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  group_by(organization_name_full) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_full, land_accelerator_cohort_year_list)

# place parentheses on either side of organization_name_abbr --------------

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  mutate(organization_name_abbr = ifelse(!is.na(organization_name_abbr), paste0("(", organization_name_abbr, ")"), organization_name_abbr))

# final name merge --------------------------------------------------------

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  unite(organization_name_merge, organization_name_full, organization_name_abbr, sep = " ", na.rm = TRUE)

# unlist airtable id's and LA cohort ----------------------------------------------------

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  separate_longer_delim(c(airtable_unique_id_list, land_accelerator_cohort_year_list), delim = ", ") %>%
  arrange(organization_name_merge, land_accelerator_cohort_year_list)

# save .csv ---------------------------------------------------------------

write_csv(cleaned_land_accelerator_names,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "Land Accelerator",
            "cleaned_land_accelerator_names_final.csv"
          ))
