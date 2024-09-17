# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 03/22/2024
# Last Updated: 03/22/2024
# Description: Clean and Upload Final Top 100 Organization Names

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
cleaned_top_100_names <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Top 100",
    "top_100_org_names_03_22_24.xlsx"
  ))

# top 100 remaining
remain_cleaned_top_100_names <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Top 100",
    "cleaned_remaining_top_100_org_names_03_22_24.xlsx"
  ))


# select variables --------------------------------------------------------

cleaned_top_100_names <- cleaned_top_100_names %>%
  select(airtable_unique_id_list, 
         organization_name_full,
         organization_name_abbr,
         application_type_list)

remain_cleaned_top_100_names <- remain_cleaned_top_100_names %>%
  select(
    airtable_unique_id_list, 
    organization_name_full,
    organization_name_abbr,
    application_type_list
  )


# bind rows ---------------------------------------------------------------

cleaned_top_100_names <-
  bind_rows(cleaned_top_100_names,
            remain_cleaned_top_100_names)

# list and regroup --------------------------------------------------------

#cleaned_top_100_names <- cleaned_top_100_names %>%
#  group_by(organization_name_full) %>%
#  mutate(application_type_list = paste(application_type_list, collapse = ", ")) %>%
#  mutate(airtable_unique_id_list = paste(airtable_unique_id_list, collapse = ", ")) %>%
#  ungroup()
#
## top_100 grouped
#
#cleaned_top_100_names <- cleaned_top_100_names %>%
#  group_by(organization_name_full) %>%
#  slice(1) %>%
#  ungroup() %>%
#  arrange(organization_name_full, application_type_list)
#
# place parentheses on either side of organization_name_abbr --------------

cleaned_top_100_names <- cleaned_top_100_names %>%
  mutate(organization_name_abbr = ifelse(!is.na(organization_name_abbr), paste0("(", organization_name_abbr, ")"), organization_name_abbr))

# final name merge --------------------------------------------------------

cleaned_top_100_names <- cleaned_top_100_names %>%
  unite(organization_name_merge, organization_name_full, organization_name_abbr, sep = " ", na.rm = TRUE)

# unlist airtable id's ----------------------------------------------------

cleaned_top_100_names <- cleaned_top_100_names %>%
  separate_longer_delim(c(airtable_unique_id_list, application_type_list), delim = ", ") %>%
  arrange(organization_name_merge, application_type_list)


# add cohort name ---------------------------------------------------------

cleaned_top_100_names <- cleaned_top_100_names %>%
  mutate(cohort = "TerraFund Top 100") 

# save .csv ---------------------------------------------------------------

write_csv(cleaned_top_100_names,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "Top 100",
            "cleaned_top_100_names_final.csv"
          ))

