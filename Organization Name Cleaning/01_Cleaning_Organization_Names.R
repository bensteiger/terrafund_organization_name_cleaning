# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 02/09/2024
# Last Updated: 02/13/2024
# Description: Cleaning All Organization Names in Master Table

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(writexl)

# Load data ---------------------------------------------------------------

# all data
data <- read_csv(here(
  "Unified Database",
  "Data",
  "All",
  "Raw",
  "Master Table - Repost-Org Names Cleaning.csv"
))

# Hierarchy --------------------------------------------------------------

#1.	Landscapes (funded)
#2.	Top 100 (funded)
#3.	Land accelerator (funded)
#4.	Landscapes (applied)
#5.	Top 100 (applied)
#6.	Land accelerator (applied)  

# format orgs as follows:
# Organization Name (ON)

# To do:
# remove whitespace
# remove Inc., Ltd.
# remove special characters

# Change column names to snake case ---------------------------------------

names(data) <- to_snake_case(names(data))

# create cleaned org name variable ----------------------------------------

data_sub <- data %>%
  mutate(
    organization_name_clean = organization_name
  )

# make all organization names uppercase

data_sub <- data_sub %>%
  mutate(organization_name_clean = toupper(organization_name_clean))

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

data_sub <- data_sub %>%
  mutate(
         organization_name_clean = str_remove_all(organization_name_clean, '"'),
         organization_name_clean = if_else(str_detect(organization_name_clean, fixed(".COM")) | str_detect(organization_name_clean, fixed(".ORG")), 
                                           organization_name_clean, 
                                           str_replace_all(organization_name_clean, "\\.", "")),
         organization_name_clean = str_remove_all(organization_name_clean, "ORGANIZATION NAME:|THE COMPANY BUSINESS NAME IS|
                                                  MY COMPANY NAME IS|COMPANY NAME;|OUR COMPANY IS CALLED"),
         organization_name_clean = str_remove_all(organization_name_clean, ""),
         organization_name_clean = str_remove_all(organization_name_clean, "\\(1\\)"),
         organization_name_clean = str_remove_all(organization_name_clean, "•"),
         organization_name_clean = str_remove_all(organization_name_clean, "1[.]"),
         organization_name_clean = str_remove_all(organization_name_clean, ", INC[.]|INC[.]|\\bINC\\b"),
         organization_name_clean = str_remove_all(organization_name_clean, "LIMITED|LTD[.]|LTD|LMITED"),
         organization_name_clean = str_remove_all(organization_name_clean, "LLC,|LLC"),
         organization_name_clean = str_remove_all(organization_name_clean, "PLC"),
         organization_name_clean = str_remove_all(organization_name_clean, "ASBL"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bSARL\\b"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bLDA\\b"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bNGO\\b"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bNPC\\b"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bPTY\\b|\\(PTY\\)"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bLBG\\b|\\(LBG\\)"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\bONGD\\b|\\(ONGD\\)"),
         organization_name_clean = str_remove_all(organization_name_clean, "-ONG|\\bONG\\b|\\(ONG\\)"),
         organization_name_clean = str_remove_all(organization_name_clean, "\\(CBO\\)|CBO|COMMUNITY BASED ORGANIZATION|COMMUNITY-BASED ORGANIZATION|COMMUNITY BASED ORGANISATION"),
         organization_name_clean = str_remove_all(organization_name_clean, "^: |^@"),
         organization_name_clean = str_remove_all(organization_name_clean, "™"),
         organization_name_clean = str_remove_all(organization_name_clean, ", EN SIGLE|EN SIGLES|EN SIGLE"),
         organization_name_clean = str_replace_all(organization_name_clean, "\\[", "("),
         organization_name_clean = str_replace_all(organization_name_clean, "\\]", ")"),
         organization_name_clean = str_replace_all(organization_name_clean, "\\(", " ("),
         organization_name_clean = str_replace_all(organization_name_clean, "\\)", ") "),
         organization_name_clean = str_replace_all(organization_name_clean, "\\«", "("),
         organization_name_clean = str_replace_all(organization_name_clean, "\\»", ")"),
         organization_name_clean = str_replace_all(organization_name_clean, "\\(\\s+", "("),
         organization_name_clean = str_replace_all(organization_name_clean, "\\s+\\)", ")"),
         organization_name_clean = str_replace_all(organization_name_clean, "\\s+\\,", ", "),
         organization_name_clean = str_replace_all(organization_name_clean, "=", "-"),
         organization_name_clean = str_replace_all(organization_name_clean, "_", " "),
         organization_name_clean = str_replace_all(organization_name_clean, ",\\)", ")"),
         organization_name_clean = str_replace_all(organization_name_clean, "’", "'"),
         organization_name_clean = str_replace_all(organization_name_clean, "&", " & "),
         organization_name_clean = str_replace_all(organization_name_clean, ", \\(", " ("),
         organization_name_clean = str_squish(organization_name_clean),
         organization_name_clean = str_trim(organization_name_clean))

# at the end of strings, remove "-", ",", and "."
# replace "N/A" and "" with NA

data_sub <- data_sub %>%
  mutate(organization_name_clean = str_remove_all(organization_name_clean, "-$|,$|[.]$"),
         organization_name_clean = str_replace_all(organization_name_clean, "/$", ")"),
         organization_name_clean = str_replace_all(organization_name_clean, "^/|^-", ""),
         organization_name_clean = if_else(organization_name_clean == ")", NA_character_, organization_name_clean),
         organization_name_clean = na_if(organization_name_clean, "N/A"),
         organization_name_clean = na_if(organization_name_clean, ""),
         organization_name_clean = str_squish(organization_name_clean),
         organization_name_clean = str_trim(organization_name_clean))

# clean organization names ------------------------------------------------

data_sub <- data_sub %>%
  mutate(
    organization_name_clean = case_when(
      str_detect(organization_name_clean, "CADEL BUSSINESS") ~ "CADEL Business",
      str_detect(organization_name_clean, "SOSEB|SOS ENERGIE BURKINA") ~ "SOS Énergie Burkina (SOSEB)",
      str_detect(organization_name_clean, "PERFECT VILLAGE COMMUNITIES") ~ "Perfect Village Communities (PVC Burundi)",
      str_detect(organization_name_clean, "MUNDAWATHU GARDEN") ~ "Mundawathu Garden",
      str_detect(organization_name_clean, "AEROBIC AGRO") ~ "Aerobic Agroforestry",
      str_detect(organization_name_clean, "OLIVETTE") ~ "D-Olivette Enterprise",
      str_detect(organization_name_clean, "KENVO") ~ "Kijabe Environment Volunteers (KENVO)",
      str_detect(organization_name_clean, "GREEN POT") ~ "Green Pot Enterprises",
      str_detect(organization_name_clean, "SAKAM SAVANA") ~ "Sakam Savana",
      str_detect(organization_name_clean, "PADO") ~ "PRIVATE AFFORESTATION DEVELOPERS ORGANIZATION (PADO)",
      str_detect(organization_name_clean, "EPECOT") ~ "Environmental Protection, Construction and General Services (EPECOTGE)",
      str_detect(organization_name_clean, "PROMACNUTS") ~ "Promotion of Macadamia Nuts (PROMACNUTS)",
      str_detect(organization_name_clean, "Y. & M REGENERATION") ~ "Y & M REGENERATION",
      TRUE ~ organization_name_clean
    )
  )

# subset to top 100, landscapes, land accelerator -------------------------

# landscapes
landscapes <- data_sub %>%
  filter(cohort == "TerraFund Landscapes")

# Top 100
top_100 <- data_sub %>%
  filter(cohort == "TerraFund Top 100")

# Land Accelerator 
land_accelerator <- data_sub %>%
  filter(cohort %in% c("Land Accelerator", "Land Accelerator x GRV", "Land Accelerator x Malawi"))

# Create land accelerator cohort group listing -------------------------------------------

# merge cohort year and cohort
# then merge LA cohort years into list

land_accelerator <- land_accelerator %>%
  group_by(organization_name_clean) %>%
  unite(land_accelerator_cohort_year, cohort, cohort_year, sep = "-") %>%
  mutate(land_accelerator_cohort_year_list = paste(land_accelerator_cohort_year, collapse = ", ")) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id, collapse = ", ")) %>%
  ungroup()

# land accelerator grouped

land_accelerator_grouped <- land_accelerator %>%
  group_by(organization_name_clean) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_clean, land_accelerator_cohort_year_list)

# select variables

land_accelerator_grouped <- land_accelerator_grouped %>%
  select(airtable_unique_id_list, 
         organization_name_clean,
         land_accelerator_cohort_year_list,
         headquarters_country_clean,
         organization_mission_or_one_line_pitch,
         organization_name) %>%
  rename(organization_name_original = organization_name)


# write excel file for Amanda ---------------------------------------------

#write_xlsx(land_accelerator_grouped, 
#           path = here(
#             "Unified Database",
#             "Data",
#             "All",
#             "Processed",
#             "land_accelerator_grouped_names.xlsx"),  tempfile(fileext = ".xlsx"),
#           col_names = TRUE,
#           format_headers = TRUE)

# cleaning landscapes names -----------------------------------------------

landscapes <- landscapes %>%
  mutate(
    organization_name_clean =
      case_when(
        str_detect(organization_name_clean, "APPUI AU DEVELOPPEMENT INTEGRAL ET A LA SOLIDARITE|ADISCO") ~ "Appui au Développement Intégral et à la Solidarité sur les Collines (ADISCO)",
        str_detect(organization_name_clean, "AFPDE") ~ "Association des Femmes pour la Promotion et le Développement Endogène (AFPDE)",
        str_detect(organization_name_clean, "ACVE") ~ "Action Ceinture Verte pour l'Environnement (ACVE)",
        str_detect(organization_name_clean, "APDEV") ~ "Actions paysannes pour le développement (APDEV)",
        str_detect(organization_name_clean, "AEE RWANDA") ~ "African Evangelistic Enterprise (AEE Rwanda)",
        str_detect(organization_name_clean, "AWAN AFRIKA") ~ "African Women in Agribusiness Network (AWAN AFRIKA)",
        str_detect(organization_name_clean, "APROCUVI") ~ "Association Pour la promotion des Cultures Vivrières (APROCUVI)",
        str_detect(organization_name_clean, "APROHDIV") ~ "Association Pour la promotion de l'Hygiene et le Developpement Integral des Vulnerables (APROHDIV)",
        str_detect(organization_name_clean, "ASSOCIATION DE JEUNES VISIONNAIRES POUR LE DEVELOPPEMENT DU CONGO|AJVDC") ~ "Association de Jeunes Visionnaires pour le Developpement du Congo (AJVDC)",
        str_detect(organization_name_clean, "ANGLICAN DEVELOPMENT SERVICES- NORTH") ~ "Anglican Development Services (ADS) North Rift Region",
        str_detect(organization_name_clean, "ANGLICAN DEVELOPMENT SERVICES-KENYA SOUTHRIFT") ~ "Anglican Development Services (ADS) South Rift Region",
        str_detect(organization_name_clean, "SAIC") ~ "Stewardship Agribusiness Incubation Center (SAIC)",
        str_detect(organization_name_clean, "ASSOCIATION OF WOMEN IN AGRICULTURE|AWAK") ~ "Association of Women in Agriculture, Kenya (AWAK)",
        str_detect(organization_name_clean, "AICC") ~ "Association Inter Cafe Cacao RDC (AICC-RDC)",
        str_detect(organization_name_clean, "BIRDLIFE INTERNATIONAL") ~ "Birdlife International - Kenya",
        str_detect(organization_name_clean, "BOMA IKOLOJIA REGENERATION KANYERUS") ~ "Boma Ikolojia Regeneration Kanyerus",
        str_detect(organization_name_clean, "TRADITIONNELLE DE BUTA") ~ "Centre de médecine traditionnelle de Buta",
        str_detect(organization_name_clean, "CARBOFIX SERVICES") ~ "Carbofix Services",
        str_detect(organization_name_clean, "COOPERATIVE DES CAFEICULTEURS TUUNGANE") ~ "Cooperative des Cafeiculteurs Tuungane (COCAT)",
        str_detect(organization_name_clean, "COMMUNITIES OF HOPE") ~ "Communities of Hope (CoH)",
        str_detect(organization_name_clean, "CONGO GREEN SOCIETY") ~ "CONGO GREEN SOCIETY (CONGRES)",
        str_detect(organization_name_clean, "COOPÉRATIVE AGRICOLE|COOPABA") ~ "Coopérative Agricole pour la Commercialisation performante des aliments de base (COOPABA)",
        str_detect(organization_name_clean, "LHAGREN|LABORATOIRE D'HYDROBIOLOGIE") ~ "Laboratoire d’Hydrobiologie, d’Aquaculture et Gestion des Ressources Naturelles (LHAGREN-UOB)",
        str_detect(organization_name_clean, "EDA DRC") ~ "Agency for Education and Development in DRC (EDA RDC)",
        str_detect(organization_name_clean, "ENDAO") ~ "Endao Water Resource Users Association",
        str_detect(organization_name_clean, "FESD") ~ "Friends of Environment and Social Development (FESD)",
        str_detect(organization_name_clean, "IJED") ~ "Consortium: Initiative des Jacobins Eleveurs pour le Développement (IJED), Mikono Ya Wajenzi Cooperative",
        str_detect(organization_name_clean, "ITOMBWE") ~ "Itombwe Génération pour l'Humanité (IGH)",
        str_detect(organization_name_clean, "MILLENNIUM COMMUNITY DEVELOPMENT INITIATIVES") ~ "Millennium Community Development Initiatives",
        str_detect(organization_name_clean, "ORGANISATION DE DÉFENSE DE L'ENVIRONNEMENT ET LE DÉVELOPPEMENT DURABLE") ~ "Organisation de Défense de l'Environnement et le Développement Durable (ODEB)",
        str_detect(organization_name_clean, "OSEPCCA") ~ "Organisation for support to the environment protection and climate change adaptation (OSEPCCA)",
        str_detect(organization_name_clean, "RURAL DEVELOPMENT INITIATIVE") ~ "Rural Development Initiative (RDI)",
        str_detect(organization_name_clean, "RUMINA") ~ "Rumina",
        str_detect(organization_name_clean, "RWANDA ENVIRONMENTAL CONSERVATION ORGANIZATION|\\bRECOR\\b") ~ "Rwanda Environmental Conservation Oorganization (RECOR)",
        str_detect(organization_name_clean, "SUSTAINABLE AGROFORESTRY INITIATIVE") ~ "Sustainable Agroforestry Initiative (SAFI)",
        str_detect(organization_name_clean, "RDIS") ~ "The Rural Development Interdiocesan Service (RDIS)",
        str_detect(organization_name_clean, "WDI") ~ "WOMAN DEVELOPMENT INITIATIVE (WDI)",
        str_detect(organization_name_clean, "WORLD WIDE FUND FOR NATURE KENYA") ~ "World Wide Fund for Nature Kenya (WWF-Kenya)",
        str_detect(organization_name_clean, "OBSERVATOIRE POUR LE DÉVELOPPEMENT DE LA JEUNESSE ODJ") ~ "Observatoire pour le Développement de la Jeunesse (ODJ)",
        str_detect(organization_name_clean, "J&R ENGINEERING AND GENERAL BUSINESS") ~ "Consortium: J&R Engineering and General Business, GreenGoal Rwanda (CBC)",
        str_detect(organization_name_clean, "ARTCELELRATOR") ~ "Accelerator",
        str_detect(organization_name_clean, "AFHPD") ~ "Association des Femmes et Hommes unis pour la Paix et le Développement (AFHPD)",
        str_detect(organization_name_clean, "CAPAD") ~ "Confédération des Associations des Producteurs Agricoles pour le Développement (CAPAD)",
        str_detect(organization_name_clean, "\\bIGE\\b") ~ "Institut pour la Gouvernance et education électorale (IGE)",
        str_detect(organization_name_clean, "\\bPAEPEV\\b") ~ "Programme d’Assistance et d’Encadrement des Personnes Vulnérables (PAEPEV)",
        str_detect(organization_name_clean, "SCOOPPAA") ~ "Société Coopérative pour la Promotion des Productions Agricoles et Animales (SCOOPPAA)",
        str_detect(organization_name_clean, "TRAFFED") ~ "Travail en Réseau avec les Fédérations des Femmes et Enfants en Détresses (TRAFFED-RDC)",
        str_detect(organization_name_clean, "UPDI") ~ "Union Paysanne pour le Développement Intégral (UPDI)",
        str_detect(organization_name_clean, "AIDEM") ~ "Association d’Intervention pour le Développement et l’Environnement à Madagascar (AIDEM)",
        str_detect(organization_name_clean, "APED") ~ "Aides aux Personnes Démunies (APED)",
        TRUE ~ organization_name_clean
      )
  )

# Create landscapes cohort group listing -------------------------------------------

# landscapes grouped

landscapes_grouped <- landscapes %>%
  group_by(organization_name_clean) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id, collapse = ", ")) %>%
  mutate(application_type_list = paste(application_type, collapse = ", ")) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_clean, application_type)

# select variables

landscapes_grouped <- landscapes_grouped %>%
  select(airtable_unique_id_list, 
         organization_name_clean,
         application_type_list,
         headquarters_country_clean,
         project_location_country_clean,
         organization_name) %>%
  rename(organization_name_original = organization_name)

# export landscapes data --------------------------------------------------

# write file

#write_xlsx(landscapes_grouped, 
#           path = here(
#             "Unified Database",
#             "Data",
#             "All",
#             "Processed",
#             "landscapes_grouped_names.xlsx"),  tempfile(fileext = ".xlsx"),
#           col_names = TRUE,
#           format_headers = TRUE)




# Edit top 100 organization names -----------------------------------------

top_100 <- top_100 %>%
  mutate(
    organization_name_clean =
      case_when(
        str_detect(organization_name_clean, "AJFM") ~ "Association des Jeunes Forestiers du Mali (AJFM)",
        str_detect(organization_name_clean, "PANR") ~ "People and Nature Rwenzori Mountain (PANR)",
        str_detect(organization_name_clean, "AJEDA") ~ "Actions des Jeunes Environnementalistes pour le Développement et l’Agriculture (AJEDA)",
        str_detect(organization_name_clean, "DEMOZ") ~ "DEMOZ EYOBAB BOTANICAL GARDEN",
        str_detect(organization_name_clean, "ACCORD") ~ "Action Coopération Rurale pour le Développement en Afrique (ACCORD - Afrique)",
        str_detect(organization_name_clean, "ACPDI") ~ "Actions des communautes Paysannes pour le Développement Integre (ACPDI)",
        str_detect(organization_name_clean, "ACODEPA") ~ "Action Communautaire la Défense et le Progrès des Agriculteurs (ACODEPA)",
        str_detect(organization_name_clean, "APAF TOGO|APAF-TOGO") ~ "Association pour la Promotion des Arbres Fertilitaires de l’Agroforesterie et de la Foresterie (APAF-Togo)",
        str_detect(organization_name_clean, "APAF-BÉNIN") ~ "Association pour la Promotion des Arbres Fertilitaires de l’Agroforesterie et de la Foresterie (APAF-Bénin)",
        str_detect(organization_name_clean, "APAF SN") ~ "Association pour la Promotion des Arbres Fertilitaires de l’Agroforesterie et de la Foresterie (APAF-Senegal)",
        str_detect(organization_name_clean, "\\bADDA\\b") ~ "Action pour le Développement Durable en Afrique (ADDA)",
        str_detect(organization_name_clean, "\\bAPES\\b") ~ "Actions pour l’Espoir (APES)",
        str_detect(organization_name_clean, "\\bASOJED\\b") ~ "Actions Solidaires pour les Jeunes et l’Environnement Durable (ASOJED)",
        str_detect(organization_name_clean, "\\bAGRIPLUS\\b") ~ "Agriculture Plus (AGRIPLUS)",
        str_detect(organization_name_clean, "\\bACEFA\\b") ~ "Association Congolaise des Exploitants Forestiers (ACEFA)",
        str_detect(organization_name_clean, "\\bACPAD\\b") ~ "Association des Chrétiens pour la Promotion de l’Agriculture et du Développement (ACPAD)",
        str_detect(organization_name_clean, "\\bACUCOBA\\b") ~ "Association des Cultivateurs des Collectivités des Batangi et Bamate (ACUCOBA)",
        str_detect(organization_name_clean, "\\bAJDC\\b") ~ "Association des Jeunes pour le Développement Communautaire (AJDC)",
        str_detect(organization_name_clean, "\\bCAPADER\\b") ~ "Centre d’Animation pour la Promotion Agropastorale et le Développement rural (CAPADER)",
        str_detect(organization_name_clean, "\\bCADIMR\\b") ~ "Centre d’Appui Pour le Développement Intégral en Milieu Rural (CADIMR)",
        str_detect(organization_name_clean, "\\bCENED\\b") ~ "Centre d'Éducation Nutritionelle et Environnementale pour le Développement (CENED)",
        str_detect(organization_name_clean, "ACDD|ACTION COMMUNAUTAIRE POUR LE DÉVELOPPEMENT DURABLE") ~ "Action Communautaire pour le Développement Durable (ACDD)",
        str_detect(organization_name_clean, "ACDI-CG") ~ "Action Communautaire pour le Développement Intégral du Congo et Grands-Lacs (ACDI-CG)",
        str_detect(organization_name_clean, "ACTION D'AIDE HUMANITAIRE POUR LE DÉVELOPPEMENT") ~ "Action d'aide Humanitaire pour le Développement (AHD)",
        str_detect(organization_name_clean, "\\bAJP\\b") ~ "Action Jeunesse et Paix (AJP)",
        str_detect(organization_name_clean, "ACTION LE VERT") ~ "Action Le Vert (ALV)",
        str_detect(organization_name_clean, "ACB") ~ "Action pour la Conservation de la Biodiversité en Côte d’Ivoire (ACB - Côte d’Ivoire)",
        str_detect(organization_name_clean, "ACTIONS DES FEMMES ENGAGÉES POUR LE DÉVELOPPEMENT INTÉGRAL") ~ "Actions des Femmes Engagées pour le Développement Intégral (AFEDI)",
        str_detect(organization_name_clean, "\\bADCB\\b") ~ "Action pour le Développement Coopeératif à la Base (ADCB)",
        str_detect(organization_name_clean, "\\bADENYA\\b") ~ "Association pour le Développement de Nyabimata (ADENYA)",
        str_detect(organization_name_clean, "\\bADET\\b") ~ "Amis des Étrangers au Togo (ADET)",
        str_detect(organization_name_clean, "\\ADIF\\b") ~ "Association pour le Développement des Initiatives Féminines (ADIF)",
        str_detect(organization_name_clean, "\\ADOSGET\\b") ~ "ADSOGET Ethiopia",
        str_detect(organization_name_clean, "\\bAIDD\\b") ~ "Actions Intégrées pour un Développement Durable (AIDD)",
        str_detect(organization_name_clean, "\\bAPEFA\\b") ~ "Action pour la Protection de L'Environnement et la Promotion des Filieres Agricoles (APEFA)",
        str_detect(organization_name_clean, "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION (ACEF) CAMEROON CHAPTER") ~ "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION (ACEF) CAMEROON",
        str_detect(organization_name_clean, "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION SIERRA LEONE") ~ "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION (ACEF) SIERRA LEONE",
        str_detect(organization_name_clean, "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION") & project_location_country_clean == "Liberia" ~ "AFRICA CLIMATE AND ENVIRONMENT FOUNDATION (ACEF) LIBERIA",
        str_detect(organization_name_clean, "AFRICA BIOENERGY PROGRAMS") ~ "Africa Bioenergy Programs (ABL)",
        str_detect(organization_name_clean, "AFRICAN RESEARCH ASSOCIATION MANAGING DEVELOPMENT IN NIGERIA") ~ "African Research Association managing Development in Nigeria (ARADIN)",
        str_detect(organization_name_clean, "AJESH") ~ "Ajemalebu Self Help (AJESH)",
        str_detect(organization_name_clean, "\\bARCOS\\b") ~ "Albertine Rift Conservation Society (ARCOS Network)",
        str_detect(organization_name_clean, "\\bALDIPE\\b") ~ "Association de Lutte pour un Développement Intégré et pour la Protection (ALDIPE)",
        str_detect(organization_name_clean, "\\bAMADESE\\b") ~ "Association Malagasy pour le Développement Economique, social et Environnemental (AMADESE)",
        str_detect(organization_name_clean, "APPUI AUX INITIATIVES DE DÉVELOPPEMENT \\(AIDE") ~ "Appui aux Initiatives de Développement (AIDE)",
        str_detect(organization_name_clean, "AIDES AUX PERSONNES DÉMUNIES, APED|APED AIDES AUX PERSONNES DÉMUNIES") ~ "Aides aux Personnes Démunies (APED)",
        str_detect(organization_name_clean, "APPUI POUR LA PROTECTION DE L'ENVIRONNEMENT ET LE DEVELOPPEMENT") ~ "Appui pour la Protection de l'Environnement et le Développement (APED)",
        str_detect(organization_name_clean, "ASSOCIATION DES PRODUCTEURS DE LA VALLÉE DU FLEUVE GAMBIE") ~ "Association des producteurs de la vallée du fleuve Gambie (APROVAG)",
        str_detect(organization_name_clean, "ASAPID") ~ "Association d'Appui aux Initiatives de Paix et de Développement (ASAPID)",
        str_detect(organization_name_clean, "ASSOCIACAO MOCAMBICANA PARA A AJUDA DE DESENVOLVIMENTO DE POVO PARA POVO") ~ "Associacao Mocambicana para a Ajuda de Desenvolvimento de Povo para Povo (ADPP Mocambique)",
        str_detect(organization_name_clean, "ACPE") ~ "Association Action communautaire pour la protection de l’environnement (ACPE)",
        str_detect(organization_name_clean, "ASSOCIATION BURUNDAISE POUR LA PROTECTION DE LA NATURE") ~ "ASSOCIATION BURUNDAISE POUR LA PROTECTION DE LA NATURE (ABN)",
        str_detect(organization_name_clean, "ASSOCIATION D'APPUI AU DÉVELOPPEMENT COMMUNAUTAIRE") ~ "Association d'Appui au Développement Communautaire (AADECO)",
        str_detect(organization_name_clean, "AFPDE") ~ "Association des Femmes pour la Promotion et le Développement Endogène (AFPDE)",
        str_detect(organization_name_clean, "ASSOCIATION DES JEUNES POUR LA PROTECTION DE L'ENVIRONNEMENT DANS LA COMMUNE DE KITA-OUEST AJPE") ~ "Association des jeunes pour la protection de l'environnement dans la commune de kita-ouest (AJPE)",
        str_detect(organization_name_clean, "ASSOCIATION FANANTENANA D'ANTSAMPANANA") ~ "Association Fanantenana d’Antsampanana (AFA)",
        str_detect(organization_name_clean, "AMEDD") ~ "Association Malienne d'Eveil au Développement Durable (AMEDD)",
        str_detect(organization_name_clean, "MPSF") ~ "Association Mouvement des Paysans Sans Frontière (MPSF) - Burkina Faso",
        str_detect(organization_name_clean, "PROTECTION DES PYG") ~ "Association Paysanne pour la Réhabilitation et Protection des Pygmées (PREPPYG)",
        str_detect(organization_name_clean, "AJEDD") ~ "Association des Jeunes pour l'Environnement et le Développement Durable (AJEDD)",
        str_detect(organization_name_clean, "ATDERBI") ~ "Association des Techniciens de Développement Rural et Environnementalistes Ressortissants de Bikoro (ATDERBI)",
        str_detect(organization_name_clean, "ASSOCIATION POUR L'AIDE AUX FEMMES EN DETRESSE ET AUX ENFANTS") ~ "Association pour l’Aide aux Femmes en Détresse et aux Enfants",
        str_detect(organization_name_clean, "ACCB") ~ "Association pour la Conservation Communautaire de la Biodiversité (ACCB)",
        

        
       TRUE ~ organization_name_clean
      )
  )

top_100 <- top_100 %>%
  mutate(
    organization_name_clean = str_replace_all(organization_name_clean, ", CEPED", " (CEPED)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", CFAD", " (CFAD)"),
    organization_name_clean = str_replace_all(organization_name_clean, "; CAF BENI", " (CAF BENI)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bCLCE\\b", "(CLCE)"),
    organization_name_clean = str_replace_all(organization_name_clean, "CONSERTIUM", "CONSORTIUM"),
    organization_name_clean = str_replace_all(organization_name_clean, "CODELUS", "(CODELUS)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bEDS-SUD\\b", "(EDS-SUD)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", EPF", " (EPF)"),
    organization_name_clean = str_replace_all(organization_name_clean, "BDA-RDC", "(BDA-RDC)"),
    organization_name_clean = str_replace_all(organization_name_clean, "FOMINA", "(FOMINA)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", FORAF", "(FORAF)"),
    organization_name_clean = str_replace_all(organization_name_clean, "FGCA", "(FGCA)"),
    organization_name_clean = str_replace_all(organization_name_clean, "ICPEDH", "(ICPEDH)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\) ' ‘'IRCFI''", " (IRCFI)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bIDIC\\b", "(IDIC)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", IFVS", " (IFVS)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bIFDP\\b", "(IFDP)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", \\(IDPE\\)", " (IDPE)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", LOFEPACO", " (LOFEPACO)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", OCGL", " (OCGL)"),
    organization_name_clean = str_replace_all(organization_name_clean, "RADF", "(RADF)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", RECOF", " (RECOF)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", RÉSEAU CREF", " (Réseau CREF)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", SENAD", " (SENAD)"),
    organization_name_clean = str_replace_all(organization_name_clean, "SOPEVUDECO", "(SOPEVUDECO)"),
    organization_name_clean = str_replace_all(organization_name_clean, "UPADERI", "(UPADERI)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", ASOFED-MR", " (ASOFED-MR)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\*AVUDS\\*", "(AVUDS)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bAFAL\\b", "(AFAL)"),
    organization_name_clean = str_replace_all(organization_name_clean, "- ACSET", "(ACSET)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bARP\\b", "(ARP)"),
    organization_name_clean = str_replace_all(organization_name_clean, "\\bADGRN\\b", "(ADGRN)"),
    organization_name_clean = str_replace_all(organization_name_clean, ", ADEPDH", "(ADEPDH)"),
    organization_name_clean = str_replace_all(organization_name_clean, "- APPCO", "(APPCO)"),
    organization_name_clean = str_replace_all(organization_name_clean, "ADIS/MALI", "(ADIS - MALI)"),
    
    organization_name_clean = str_squish(organization_name_clean),
    organization_name_clean = str_trim(organization_name_clean))

# Create landscapes cohort group listing -------------------------------------------

# landscapes grouped

top_100_grouped <- top_100 %>%
  group_by(organization_name_clean) %>%
  mutate(airtable_unique_id_list = paste(airtable_unique_id, collapse = ", ")) %>%
  mutate(application_type_list = paste(application_type, collapse = ", ")) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(organization_name_clean, application_type)

# select variables

top_100_grouped <- top_100_grouped %>%
  select(airtable_unique_id_list, 
         organization_name_clean,
         application_type_list,
         headquarters_country_clean,
         project_location_country_clean,
         organization_name) %>%
  rename(organization_name_original = organization_name)

# subset landscapes dataset -----------------------------------------------

## subset landscapes to rfp
#landscapes_rfp <- landscapes %>%
#  filter(application_type == "RFP")
#
## subset to landscapes EOI not in RFP
#landscapes_eoi <- landscapes %>%
#  filter(!organization_name %in% landscapes_rfp$organization_name_eoi_link)

# step 1 - match top 100 to landscapes RFP -------------------------------

#landscapes_top_100_rfp <- 
#  left_join(
#    landscapes,
#    top_100,
#    by = "organization_name_clean"
#  )
  

# step 2 - match LA to landscapes  -----------------------------------

# check previous success

# step 3 - match LA to top 100 RFP ----------------------------------------

# step 4 - match LA to landscapes EOI ----------------------------------------

# step 5 - match LA to Top 100 EOI ----------------------------------------



