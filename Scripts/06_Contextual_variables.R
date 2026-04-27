
# 06 Additional contextual data ################################################

# Load packages
library(readxl)
library(writexl)

## 6.1 Load QOF Smoking Data ###################################################

QOF_smoking_data <- read_excel("Data/QOF smoking statistics.xlsx", sheet = "SMOK")

smoking_data <- QOF_smoking_data[-c(1:9, 6199:6202), ]
colnames(smoking_data) <- as.character(smoking_data[1, ])
smoking_data <- smoking_data[-c(1), ]

smoking_data <- smoking_data %>%
  select(`PCN ODS code`, `PCN name`, `Practice code`, `Practice name`, c(9,32))

colnames(smoking_data) <- c("PCN_code","PCN_name","Practice_code","Practice_name","Total_practice_pop","Total_current_smokers")

smoking_data <- smoking_data %>%
  mutate(Practice_smoking_prev = as.numeric(Total_current_smokers)/as.numeric(Total_practice_pop))

## 6.2 Profile practices to LSOA ###############################################

join_practices_smoking_counts <- function(
    practices_df = patients_by_practice,
    smoking_df = smoking_data){
  
  practices_joined <- dplyr::left_join(
    practices_df,
    smoking_df,
    by = c("PRACTICE_CODE" = "Practice_code")) %>%
    dplyr::mutate(
      Total_current_smokers = as.numeric(Total_current_smokers),
      Total_practice_pop = as.numeric(Total_practice_pop),
      
      Total_current_smokers = tidyr::replace_na(Total_current_smokers, 0),
      Total_practice_pop = tidyr::replace_na(Total_practice_pop, 0),
      
      smokers_lsoa = Total_current_smokers * PATIENT_PROPORTION,
      population_lsoa = Total_practice_pop * PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    dplyr::group_by(LSOA_CODE) %>%
    dplyr::summarise(
      smokers = sum(smokers_lsoa, na.rm = TRUE),
      population = sum(population_lsoa, na.rm = TRUE),
      smoking_prevalence = smokers / population) %>%
    dplyr::ungroup()
  
return(LSOA_summed)}

LSOA_smoking_prevalence <- join_practices_smoking_counts(practices_df = patients_by_practice, smoking_df = smoking_data)

# Overcrowding data
overcrowding_data <- read_excel("Data/Overcrowding_data.xlsx", sheet = "1c")
colnames(overcrowding_data) <- as.character(overcrowding_data[2, ])
overcrowding_data <- overcrowding_data[-c(1:2), ]

overcrowding_data <- overcrowding_data %>%
  group_by(`LSOA code`) %>%
  mutate(Total_households = sum(as.numeric(`Occupancy rating of -1 or less`),
                                as.numeric(`Occupancy rating of 0`),
                                as.numeric(`Occupancy rating of +1`),
                                as.numeric(`Occupancy rating of +2 or more`)),
         Overcrowding_prev = as.numeric(`Occupancy rating of -1 or less`)/as.numeric(Total_households))

# Air pollution data


# Ethnicity data
ethnicity_data <- read_csv("Data/Ethnicity by LSOA.csv")

ethnicity_data <- ethnicity_data %>%
  group_by(`Lower layer Super Output Areas Code`,
           `Ethnic group (20 categories)`) %>%
  summarise(Observation = sum(Observation), .groups = "drop") %>%
  pivot_wider(
    names_from = `Ethnic group (20 categories)`,
    values_from = Observation,
    values_fill = 0) %>%
  rename(LS)

# CLEAN ########################################################################

ethnicity_data <- ethnicity_data %>%
  rename(LSOA_CODE = "Lower layer Super Output Areas Code")

overcrowding_data <- overcrowding_data %>%
  rename(LSOA_CODE = "LSOA code")

smoking_data <- LSOA_smoking_prevalence

rm(LSOA_smoking_prevalence)
rm(patients_by_practice)
rm(QOF_smoking_data)

write.csv(ethnicity_data, 'Outputs/ethnicity_lsoa_data.csv')
write.csv(overcrowding_data, 'Outputs/overcrowding_lsoa_data.csv')
write.csv(smoking_data, 'Outputs/smoking_lsoa_data.csv')



