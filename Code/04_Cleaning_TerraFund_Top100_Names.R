# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 02/26/2024
# Last Updated: 02/28/2024
# Description: Cleaning All top_100 Organization Names in Master Table

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)
library(janitor)

# Load data ---------------------------------------------------------------

# all data
#data <- read_csv(
#  here(
#    "Unified Database",
#    "Data",
#    "All",
#    "Raw",
#    "Master Table - Repost-Org Names Cleaning.csv"
#  )
#)

# all data
data <- read_csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Master Table-Org Names Cleaning.csv"
  )
)


#
words <- read_xlsx(
  here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Organization Name Cleaning",
    "lowercase_words.xlsx"
  ))

# Change column names to snake case ---------------------------------------

# all data
names(data) <- to_snake_case(names(data))

# words
names(words) <- to_snake_case(names(words))

# create cleaned org name variable ----------------------------------------

data_sub <- data %>%
  mutate(organization_name_clean = organization_name)

# make all organization names uppercase

data_sub <- data_sub %>%
  mutate(organization_name_clean = toupper(organization_name_clean))

# define function ---------------------------------------------------------

# remove all uses of limited, LTD
# remove ""
# remove spaces after parentheses
# add a space before parentheses
# remove white space, trim ws
# remove "organization name" and variations
# replace "[]" and "«»" with parentheses
# replace "_" with a space
# replace "=" with "-"
# remove other extra special characters
# remove any use of "INC."
# remove any use of "LLC"
# remove @ and : at beginning of string
# Function to remove "." except when the string contains ".COM"

clean_organization_names <- function(data, col_name) {
  data %>%
    mutate(
      !!col_name := str_remove_all(!!sym(col_name), '"'),!!col_name := if_else(
        str_detect(!!sym(col_name), fixed(".COM")) |
          str_detect(!!sym(col_name), fixed(".ORG")),!!sym(col_name),
        str_replace_all(!!sym(col_name), "\\.", "")
      ),!!col_name := str_remove_all(
        !!sym(col_name),
        "ORGANIZATION NAME:|THE COMPANY BUSINESS NAME IS|
                                              MY COMPANY NAME IS|COMPANY NAME;|OUR COMPANY IS CALLED"
      ),!!col_name := str_remove_all(!!sym(col_name), ""),!!col_name := str_remove_all(!!sym(col_name), "\\(1\\)"),!!col_name := str_remove_all(!!sym(col_name), "•"),!!col_name := str_remove_all(!!sym(col_name), "1[.]"),!!col_name := str_remove_all(!!sym(col_name), ", INC[.]|INC[.]|\\bINC\\b"),!!col_name := str_remove_all(!!sym(col_name), "LIMITED|LTD[.]|LTD|LMITED"),!!col_name := str_remove_all(!!sym(col_name), "LLC,|LLC"),!!col_name := str_remove_all(!!sym(col_name), "PLC"),!!col_name := str_remove_all(!!sym(col_name), "ASBL"),!!col_name := str_remove_all(!!sym(col_name), "\\bSARL\\b"),!!col_name := str_remove_all(!!sym(col_name), "\\bLDA\\b"),!!col_name := str_remove_all(!!sym(col_name), "\\bNGO\\b"),!!col_name := str_remove_all(!!sym(col_name), "\\bNPC\\b"),!!col_name := str_remove_all(!!sym(col_name), "\\bPTY\\b|\\(PTY\\)"),!!col_name := str_remove_all(!!sym(col_name), "\\bLBG\\b|\\(LBG\\)"),!!col_name := str_remove_all(!!sym(col_name), "\\bONGD\\b|\\(ONGD\\)"),!!col_name := str_remove_all(!!sym(col_name), "-ONG|\\bONG\\b|\\(ONG\\)"),!!col_name := str_remove_all(
        !!sym(col_name),
        "\\(CBO\\)|CBO|COMMUNITY BASED ORGANIZATION|COMMUNITY-BASED ORGANIZATION|COMMUNITY BASED ORGANISATION"
      ),!!col_name := str_remove_all(!!sym(col_name), "^: |^@"),!!col_name := str_remove_all(!!sym(col_name), "™"),!!col_name := str_remove_all(!!sym(col_name), ", EN SIGLE|EN SIGLES|EN SIGLE"),!!col_name := str_replace_all(!!sym(col_name), "DEVELOPPEMENT", "DÉVELOPPEMENT"),!!col_name := str_replace_all(!!sym(col_name), "\\[", "("),!!col_name := str_replace_all(!!sym(col_name), "\\]", ")"),!!col_name := str_replace_all(!!sym(col_name), "\\(", " ("),!!col_name := str_replace_all(!!sym(col_name), "\\)", ") "),!!col_name := str_replace_all(!!sym(col_name), "\\«", "("),!!col_name := str_replace_all(!!sym(col_name), "\\»", ")"),!!col_name := str_replace_all(!!sym(col_name), "\\(\\s+", "("),!!col_name := str_replace_all(!!sym(col_name), "\\s+\\)", ")"),!!col_name := str_replace_all(!!sym(col_name), "\\s+\\,", ", "),!!col_name := str_replace_all(!!sym(col_name), "=", "-"),!!col_name := str_replace_all(!!sym(col_name), "&", " & "),!!col_name := str_replace_all(!!sym(col_name), "_", " "),!!col_name := str_replace_all(!!sym(col_name), "‘", "'"),!!col_name := str_replace_all(!!sym(col_name), ",\\)", ")"),!!col_name := str_replace_all(!!sym(col_name), "’", "'"),!!col_name := str_replace_all(!!sym(col_name), ", \\(", " ("),!!col_name := str_replace_all(!!sym(col_name), "-\\)", ")"),!!col_name := str_squish(!!sym(col_name)),!!col_name := str_trim(!!sym(col_name))
    )
}

# at the end of strings, remove "-", ",", and "."
# replace "N/A" and "" with NA

clean_organization_names_2 <- function(data, col_name) {
  data %>%
    mutate(
      !!col_name := str_remove_all(!!sym(col_name), "-$|,$|[.]$"),!!col_name := str_replace_all(!!sym(col_name), "/$", ")"),!!col_name := str_replace_all(!!sym(col_name), "^/|^-", ""),!!col_name := ifelse(!!sym(col_name) == ")", NA_character_,!!sym(col_name)),!!col_name := na_if(!!sym(col_name), "N/A"),!!col_name := na_if(!!sym(col_name), ""),!!col_name := str_squish(!!sym(col_name)),!!col_name := str_trim(!!sym(col_name))
    )
}

clean_organization_names_3 <- function(x) {
  case_when(
    str_detect(x, "CADEL BUSSINESS") ~ "CADEL Business",
    str_detect(x, "SOSEB|SOS ENERGIE BURKINA") ~ "SOS Énergie Burkina (SOSEB)",
    str_detect(x, "PERFECT VILLAGE COMMUNITIES") ~ "Perfect Village Communities (PVC Burundi)",
    str_detect(x, "MUNDAWATHU GARDEN") ~ "Mundawathu Garden",
    str_detect(x, "AEROBIC AGRO") ~ "Aerobic Agroforestry",
    str_detect(x, "OLIVETTE") ~ "D-Olivette Enterprise",
    str_detect(x, "KENVO") ~ "Kijabe Environment Volunteers (KENVO)",
    str_detect(x, "GREEN POT") ~ "Green Pot Enterprises",
    str_detect(x, "SAKAM SAVANA") ~ "Sakam Savana",
    str_detect(x, "PADO") ~ "PRIVATE AFFORESTATION DEVELOPERS ORGANIZATION (PADO)",
    str_detect(x, "EPECOT") ~ "Environmental Protection, Construction and General Services (EPECOTGE)",
    str_detect(x, "PROMACNUTS") ~ "Promotion of Macadamia Nuts (PROMACNUTS)",
    str_detect(x, "Y. & M REGENERATION") ~ "Y&M REGENERATION",
    TRUE ~ x
  )
}

# Define the function to convert country names to camel case
convert_to_camel_case <- function(x) {
  to_upper_camel_case(x, sep_out = " ")
}

# clean data - all_data --------------------------------------------------------------

data_sub <- data_sub %>%
  clean_organization_names(col_name = "organization_name_clean")

data_sub <- data_sub %>%
  clean_organization_names_2(col_name = "organization_name_clean")

data_sub <- data_sub %>%
  mutate_at(vars(organization_name_clean), clean_organization_names_3)


# relocate organization_name_clean variable -------------------------------

data_sub <- data_sub %>%
  relocate(organization_name_clean, .after = organization_name)

# Filter to TerraFund Top 100 ----------------------------------------------

# Landscapes
top_100 <- data_sub %>%
  filter(cohort %in% c(
    "TerraFund Top 100"
  ))

# Create top_100 cohort group listing -------------------------------------------

# merge cohort year and cohort
# then merge LA cohort years into list

top_100 <- top_100 %>%
  group_by(organization_name_clean) %>%
  mutate(application_type_list = paste(application_type, collapse = ", ")) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id, collapse = ", ")) %>%
  ungroup()

# top_100 grouped

top_100_grouped <- top_100 %>%
  group_by(organization_name_clean) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_clean, application_type_list)

# select variables

top_100_grouped <-
  top_100_grouped %>%
  select(
    airtable_unique_id_list,
    organization_name_clean,
    application_type_list,
    headquarters_country_clean,
    organization_mission_or_one_line_pitch,
    organization_name
  ) %>%
  rename(organization_name_original = organization_name)

# extract parenthetical information from org names ------------------------

# Function to extract names within parentheses
extract_names <- function(text) {
  names <- str_extract_all(text, "\\((.*?)\\)") %>%
    map_chr(~ ifelse(length(.) == 0, NA_character_, .)) %>%
    str_replace_all("\\(|\\)", "") %>%
    str_trim()
  names
}

# Extract names within parentheses into a new column
top_100_grouped <- top_100_grouped %>%
  mutate(
    organization_name_abbr = extract_names(organization_name_clean),
    organization_name_full = str_replace_all(organization_name_clean, "\\s*\\((.*?)\\)", "")
  )

# Remove parentheses from the original 'text' column
top_100_grouped$organization_name_full <- str_replace_all(top_100_grouped$organization_name_full, "\\(|\\)", "")

# convert organization name full to lowercase, then to camel --------------

# to lower

top_100_grouped <- top_100_grouped %>%
  mutate(organization_name_full = tolower(organization_name_full))

# convert organization name full to camel ---------------------------------------

# function
capitalize_after_space_hyphen <- function(text) {
  # Use regular expressions to find spaces or hyphens followed by a letter
  # Replace the matched letter with its uppercase version
  result <- str_replace_all(text, "(?<=[\\s-])([a-zA-Z])", function(match) toupper(match))
  
  # Capitalize the first letter of the string as well
  result <- str_to_title(result)
  
  return(result)
}

top_100_grouped <- top_100_grouped %>%
  mutate(organization_name_full = capitalize_after_space_hyphen(organization_name_full))

# make two words columns - upper and lower --------------------------------

words <- words %>%
  mutate(lowercase_words_all = tolower(lowercase_words_all),
         lowercase_words_all_upper = str_to_title(lowercase_words_all))

# Function to replace words
replace_words <- function(text, replacement_df) {
  for (i in 1:nrow(replacement_df)) {
    # Create regular expression with word boundaries
    pattern <- paste0("\\b", replacement_df$lowercase_words_all_upper[i], "\\b")
    
    # Replace using the regular expression
    text <- str_replace_all(text, pattern, replacement_df$lowercase_words_all[i])
  }
  return(text)
}

# Apply the function to each row of the 'organization_name_full' column
top_100_grouped$organization_name_full <- lapply(
  top_100_grouped$organization_name_full,
  replace_words,
  replacement_df = words
)

# make first letter of org name capital -----------------------------------

capitalize_first <- function(text) {
  # Split the text into words
  words <- str_split(text, "\\s")[[1]]
  
  # Capitalize the first letter of the first word
  words[1] <- str_to_title(words[1])
  
  # Combine the words back into a single string
  capitalized_text <- str_c(words, collapse = " ")
  
  return(capitalized_text)
}

# Apply the function to the column
top_100_grouped$organization_name_full <- sapply(top_100_grouped$organization_name_full, capitalize_first)

# select columns ----------------------------------------------------------

top_100_grouped_filtered <- top_100_grouped %>%
  select(airtable_unique_id_list,
         organization_name_full,
         organization_name_abbr,
         organization_name_original,
         headquarters_country_clean,
         application_type_list,
         organization_mission_or_one_line_pitch)


# group_by ----------------------------------------------------------------

top_100_grouped_filtered <- top_100_grouped_filtered %>%
  group_by(organization_name_full, headquarters_country_clean) %>%
  mutate(application_type_list = paste(application_type_list, collapse = ", ")) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id_list, collapse = ", ")) %>%
  ungroup()

# land accelerator grouped

top_100_grouped_filtered <- top_100_grouped_filtered %>%
  group_by(organization_name_full) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_full, application_type_list)

# Other edits -------------------------------------------------------------

# function to capitalize all the first letter after l'

capitalize_after_l <- function(text) {
  words <- unlist(strsplit(text, " "))
  l_words <- grep("^l'", words, value = TRUE)
  
  for (word in l_words) {
    l_position <- regexpr("^l'", word)
    
    if (l_position > 0) {
      letter_position <- l_position + 2
      
      if (letter_position <= nchar(word)) {
        new_word <- paste(substr(word, 1, l_position + 1),
                          toupper(substr(word, letter_position, letter_position)),
                          substr(word, letter_position + 1, nchar(word)),
                          sep = "")
        words[words == word] <- new_word
      }
    }
  }
  
  result <- paste(words, collapse = " ")
  return(result)
}

# function to capitalize all the first letter after d'

capitalize_after_d <- function(text) {
  words <- unlist(strsplit(text, " "))
  d_words <- grep("^d'", words, value = TRUE)
  
  for (word in d_words) {
    d_position <- regexpr("^d'", word)
    
    if (d_position > 0) {
      letter_position <- d_position + 2
      
      if (letter_position <= nchar(word)) {
        new_word <- paste(substr(word, 1, d_position + 1),
                          toupper(substr(word, letter_position, letter_position)),
                          substr(word, letter_position + 1, nchar(word)),
                          sep = "")
        words[words == word] <- new_word
      }
    }
  }
  
  result <- paste(words, collapse = " ")
  return(result)
}

top_100_grouped_filtered <- top_100_grouped_filtered %>%
  mutate(organization_name_full = sapply(organization_name_full, capitalize_after_l),
         organization_name_full = sapply(organization_name_full, capitalize_after_d))

# change NA to "" ---------------------------------------------------------

# Replace NA with ""
top_100_grouped_filtered <- top_100_grouped_filtered %>%
  mutate_all(~ifelse(is.na(.), "", .))

# Export to .csv ---------------------------------------------------------

#write_csv(top_100_grouped_filtered,
#          file = here(
#            "Unified Database",
#            "Data",
#            "All",
#            "Processed",
#            "Organization Name Cleaning",
#            "Top 100",
#            "top_100_org_names_03_04_24.csv"
#          ))

write_csv(top_100_grouped_filtered,
          file = here(
            "Unified Database",
            "Data",
            "All",
            "Processed",
            "Organization Name Cleaning",
            "Top 100",
            "remaining_top_100_org_names_03_22_24.csv"
          ))

