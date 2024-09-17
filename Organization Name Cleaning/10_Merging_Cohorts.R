# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 04/18/2024
# Last Updated: 04/18/2024
# Description: Identify all Cross-Cohort Applications

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)

# Load data ---------------------------------------------------------------

# all organizations and selection status

dta <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Master Table-All Data - Selection Status_04_18_24.csv"
  ))

# convert to snakecase ---------------------------------------------------

names(dta) <- to_snake_case(names(dta))


# subset to non N/A final application decision ----------------------------

dta_sub <- dta %>%
  filter(!is.na(organization_name_clean)) %>%
  group_by(organization_name_clean, cohort_year_combined) %>%
  arrange(organization_name_clean, desc(application_type)) %>%
  ungroup()

# identify cross-cohort applicants --------------------------------------------------

# paste all cohorts into one column by group
dta_sub <- dta_sub %>%
  group_by(organization_name_clean) %>%
  mutate(other_cohort_applied = paste(cohort_year_combined, collapse = ", ")) %>%
  ungroup()

# remove any mention of the same cohort in the same row as cohort_year_combined
dta_sub <- dta_sub %>%
  mutate(other_cohort_applied = ifelse(str_detect(other_cohort_applied, cohort_year_combined),
                               str_replace_all(other_cohort_applied, cohort_year_combined, ""),
                               other_cohort_applied))

# remove wrong comma placements
dta_sub <- dta_sub %>%
  mutate(other_cohort_applied = str_remove_all(other_cohort_applied, "^,\\s*"),  # Remove leading comma and space
         other_cohort_applied = str_remove_all(other_cohort_applied, ",\\s*$"),  # Remove trailing comma and space
         other_cohort_applied = str_remove_all(other_cohort_applied, " ,"))  # Remove middle commas

dta_sub <- dta_sub %>%
  mutate(other_cohort_applied = str_split(other_cohort_applied, ",\\s*") %>%              # Split strings by comma
           map_chr(~toString(unique(.x)))) 

# identify cross-cohort selections --------------------------------------------------

# paste all cohorts into one column by group
dta_sub <- dta_sub %>%
  group_by(organization_name_clean) %>%
  mutate(other_cohort_selected = 
           case_when(final_application_decision == "Selected" ~ cohort_year_combined,
                     TRUE ~ NA_character_)) %>%
  ungroup()

dta_sub <- dta_sub %>%
  group_by(organization_name_clean) %>%
  mutate(other_cohort_selected = paste(na.omit(other_cohort_selected), collapse = ",")) %>%
  ungroup()

# remove any mention of the same cohort in the same row as cohort_year_combined
dta_sub <- dta_sub %>%
  mutate(other_cohort_selected = ifelse(str_detect(other_cohort_selected, cohort_year_combined),
                                       str_replace_all(other_cohort_selected, cohort_year_combined, ""),
                                       other_cohort_selected))

# remove wrong comma placements
dta_sub <- dta_sub %>%
  mutate(other_cohort_selected = str_remove_all(other_cohort_selected, "^,\\s*"),  # Remove leading comma and space
         other_cohort_selected = str_remove_all(other_cohort_selected, ",\\s*$"),  # Remove trailing comma and space
         other_cohort_selected = str_remove_all(other_cohort_selected, " ,"))  # Remove middle commas

dta_sub <- dta_sub %>%
  mutate(other_cohort_selected = str_split(other_cohort_selected, ",\\s*") %>%              # Split strings by comma
           map_chr(~toString(unique(.x)))) 


# select data -------------------------------------------------------------

dta_sub_final <- dta_sub %>%
  select(airtable_unique_id,
         other_cohort_applied,
         other_cohort_selected)

# save data ---------------------------------------------------------------

# cross cohort applications
write_csv(dta_sub_final,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "all_cross_cohort_applications.csv"
          ))


