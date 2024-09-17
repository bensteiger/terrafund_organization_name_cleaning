# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 03/22/2024
# Last Updated: 03/22/2024
# Description: Merge Top 100 and Landscapes Organization Names

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)
library(janitor)
library(readxl)

# Load data ---------------------------------------------------------------

# cleaned Top 100 names
cleaned_top_100_names <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Top 100",
    "cleaned_top_100_names_final.csv"
  ))

# cleaned Landscapes names
cleaned_landscapes_names <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Organization Name Cleaning",
    "Landscapes",
    "cleaned_landscapes_names_final.csv"
  ))

# bind dataframes ---------------------------------------------------------

terrafund_names <- 
  bind_rows(cleaned_landscapes_names,
            cleaned_top_100_names) %>%
  arrange(organization_name_merge, cohort) 


# make edits to names -----------------------------------------------------

terrafund_names <- terrafund_names %>%
  mutate(organization_name_merge = 
    str_replace_all(organization_name_merge, "’", "'")
  )

# group by organization name ----------------------------------------------

terrafund_names_grouped <- terrafund_names %>%
  group_by(organization_name_merge) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id_list, collapse = ", ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-application_type_list)


# join landscapes to top 100 to get other cohort --------------------------

cleaned_landscapes_names_final <- 
  left_join(cleaned_landscapes_names,
            cleaned_top_100_names,
            by = c("organization_name_merge")) 

# keep one airtable id x

cleaned_landscapes_names_final <- cleaned_landscapes_names_final %>%
  group_by(airtable_unique_id_list.x) %>%
  slice(1) %>%
  ungroup()

# rename cohort.y to be "other cohort"
# rename airtable_unique id
# rename cohort.x to cohort

cleaned_landscapes_names_final <- cleaned_landscapes_names_final %>%
  rename(other_cohort = cohort.y,
         airtable_unique_id = airtable_unique_id_list.x,
         cohort = cohort.x)

# select variables

cleaned_landscapes_names_final <- cleaned_landscapes_names_final %>%
  select(airtable_unique_id,
         organization_name_merge,
         cohort,
         other_cohort)

# join landscapes to top 100 to get other cohort --------------------------

cleaned_top_100_names_final <- 
  left_join(cleaned_top_100_names,
            cleaned_landscapes_names,
            by = c("organization_name_merge")) 

# keep one airtable id x

cleaned_top_100_names_final <- cleaned_top_100_names_final %>%
  group_by(airtable_unique_id_list.x) %>%
  slice(1) %>%
  ungroup()

# rename cohort.y to be "other cohort"
# rename airtable_unique id
# rename cohort.x to cohort

cleaned_top_100_names_final <- cleaned_top_100_names_final %>%
  rename(other_cohort = cohort.y,
         airtable_unique_id = airtable_unique_id_list.x,
         cohort = cohort.x)

# select variables

cleaned_top_100_names_final <- cleaned_top_100_names_final %>%
  select(airtable_unique_id,
         organization_name_merge,
         cohort,
         other_cohort)


# bind final landscapes and top 100 ---------------------------------------

terrafund_names_final <- bind_rows(cleaned_landscapes_names_final,
                                   cleaned_top_100_names_final)


# drop empty ids ----------------------------------------------------------


terrafund_names_final <- terrafund_names_final %>%
  filter(
    !airtable_unique_id %in% c(
      "recBas0sYzXmCEkrZ",
      "recN60UAhxNq41T22",
      "recT9LmmUvG2tiWLZ",
      "recd0UxPHdzgiK7ey",
      "recmxo54h01rWqWHc"
    )
  )

# save .csv ---------------------------------------------------------------

write_csv(terrafund_names_final,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "terrafund_names_final.csv"
          ))



