# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 04/12/2024
# Last Updated: 04/18/2024
# Description: Merge TerraFund and Land Accelerator Organization Names

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)
library(janitor)
library(readxl)

# Load data ---------------------------------------------------------------

# cleaned TerraFund names
cleaned_terrafund_names <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Master Table-TerraFund Org Names_04_18_24.csv"
  ))

# cleaned Land Accelerator names
cleaned_land_accelerator_names <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Land Accelerator",
    "cleaned_land_accelerator_names_final.csv"
  ))


# convert to snakecase ---------------------------------------------------

# land accelerator
names(cleaned_land_accelerator_names) <- to_snake_case(names(cleaned_land_accelerator_names))

# terrafund
names(cleaned_terrafund_names) <- to_snake_case(names(cleaned_terrafund_names))


# drop EOI applications ---------------------------

# change NAs to "Y"
cleaned_terrafund_names <- cleaned_terrafund_names %>%
  mutate(landscapes_eoi_drop_application = 
           case_when(
             is.na(landscapes_eoi_drop_application) ~ "N",
             TRUE ~ landscapes_eoi_drop_application
           ))

# drop applications

cleaned_terrafund_names <- cleaned_terrafund_names %>%
  filter(landscapes_eoi_drop_application != "Y")
  
# rename variables --------------------------------------------------------

cleaned_land_accelerator_names <- cleaned_land_accelerator_names %>%
  rename(cohort = land_accelerator_cohort_year_list,
         airtable_unique_id = airtable_unique_id_list,
         organization_name_clean_final = organization_name_merge)

# bind dataframes ---------------------------------------------------------

all_org_names <- 
  bind_rows(cleaned_terrafund_names,
            cleaned_land_accelerator_names) %>%
  arrange(organization_name_clean_final, cohort) 

# make edits to names -----------------------------------------------------

# change all apostrophes to the same type
# lowercase "the", but make sure the first letter of org name is capitalized

all_org_names <- all_org_names %>%
  mutate(organization_name_clean_final = 
           str_replace_all(organization_name_clean_final, "’", "'")
  ) %>%
  mutate(organization_name_clean_final = 
           str_replace_all(organization_name_clean_final, "The", "the"))

# capitalize first letter of word

all_org_names <- all_org_names %>%
  mutate(organization_name_clean_final = str_c(str_to_upper(str_sub(organization_name_clean_final, 1, 1)), str_sub(organization_name_clean_final, 2, -1)))

# group by organization name ----------------------------------------------

all_org_names_grouped <- all_org_names %>%
  group_by(organization_name_clean_final) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id, collapse = ", ")) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_clean_final, cohort) %>%
  select(-organization_name, -airtable_unique_id)

# select data all_org_names -----------------------------------------------

all_org_names <- all_org_names %>%
  select(-organization_name)

# save data ---------------------------------------------------------------

# grouped org names
write_csv(all_org_names_grouped,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "grouped_terrafund_land_accelerator_names_final.csv"
          ))

# ungrouped org names
write_csv(all_org_names,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "terrafund_land_accelerator_names_final.csv"
          ))
