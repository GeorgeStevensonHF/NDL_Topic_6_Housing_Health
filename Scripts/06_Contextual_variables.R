
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
eng_ethnicity_data <- read_csv("Data/Ethnicity by LSOA.csv")

eng_ethnicity_data <- eng_ethnicity_data %>%
  group_by(`Lower layer Super Output Areas Code`,
           `Ethnic group (20 categories)`) %>%
  summarise(Observation = sum(Observation), .groups = "drop") %>%
  pivot_wider(
    names_from = `Ethnic group (20 categories)`,
    values_from = Observation,
    values_fill = 0)

eng_ethnicity_data <- eng_ethnicity_data %>%
  rename(LSOA_CODE = "Lower layer Super Output Areas Code")

groups <- tibble( 
  col = colnames(eng_ethnicity_data)[-1],
  group = str_extract(col, "^[^:]+"))
    
eng_ethnicity_data <- eng_ethnicity_data %>%
  pivot_longer(
    cols = -LSOA_CODE,
    names_to = "col",
    values_to = "count"
  ) %>%
  left_join(groups, by = "col")
  
eng_ethnicity_data <- eng_ethnicity_data %>%
  group_by(LSOA_CODE, group) %>%
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop")

eng_ethnicity_data <- eng_ethnicity_data %>%
  pivot_wider(
  names_from = `group`,
  values_from = total,
  values_fill = 0)

eng_ethnicity_data <- eng_ethnicity_data %>%
  group_by(LSOA_CODE) %>%
  mutate(Total = sum(`Asian, Asian British or Asian Welsh`, `Black, Black British, Black Welsh, Caribbean or African`, `Does not apply`, `Mixed or Multiple ethnic groups`, White, `Other ethnic group`),
         All_other_ethnic_groups = sum(`Asian, Asian British or Asian Welsh`, `Black, Black British, Black Welsh, Caribbean or African`, `Does not apply`, `Mixed or Multiple ethnic groups`, `Other ethnic group`),
         Perc_other_than_white = All_other_ethnic_groups/Total)

eng_ethnicity_data <- eng_ethnicity_data %>%
  mutate( 
    across(
      .cols = where(is.numeric) & !c(Perc_other_than_white),
      .fns = ~ {
        x <- as.character(.x)
        ifelse(as.numeric(x)< 10, "*", x)}))

# SCOTLAND
sco_ethnicity_data <- read.xlsx("Data/scotland_ethnicity_datazone_data.xlsx")
colnames(sco_ethnicity_data) <- as.character(sco_ethnicity_data[7, ])
sco_ethnicity_data <- sco_ethnicity_data[-c(1:8), ]
sco_ethnicity_data <- sco_ethnicity_data[-c(6977:6979), ]

sco_ethnicity_data <- sco_ethnicity_data %>%
  select(-`All People`, -`White: Total`, -`Asian, Asian Scottish or Asian British: Total`, -`African: Total`, -`Caribbean or Black: Total`, -`Other ethnic groups: Total`) %>%
  rename(`White: Other` = "Other White")

sco_ethnicity_data <- sco_ethnicity_data %>%
  rename(DZ_2011 = "Ethnic Group")
  
groups <- tibble( 
    col = colnames(sco_ethnicity_data)[-1],
    group = str_extract(col, "^[^:]+"))

sco_ethnicity_data <- sco_ethnicity_data %>%
  pivot_longer(
    cols = -DZ_2011,
    names_to = "col",
    values_to = "count"
  ) %>%
  left_join(groups, by = "col")

sco_ethnicity_data <- sco_ethnicity_data %>%
  group_by(DZ_2011, group) %>%
  summarise(total = sum(as.numeric(count), na.rm = TRUE), .groups = "drop")

sco_ethnicity_data <- sco_ethnicity_data %>%
  pivot_wider(
    names_from = `group`,
    values_from = total,
    values_fill = 0)

sco_ethnicity_data <- sco_ethnicity_data %>%
  group_by(DZ_2011) %>%
  mutate(Total = sum(African, `Asian, Asian Scottish or Asian British`, `Caribbean or Black`, `Mixed or multiple ethnic group`, `Other ethnic groups`, White),
         All_other_ethnic_groups = sum(African, `Asian, Asian Scottish or Asian British`, `Caribbean or Black`, `Mixed or multiple ethnic group`, `Other ethnic groups`),
         Perc_other_than_white = All_other_ethnic_groups/Total)
  
# CLEAN ########################################################################

overcrowding_data <- overcrowding_data %>%
  rename(LSOA_CODE = "LSOA code")

smoking_data <- LSOA_smoking_prevalence

rm(LSOA_smoking_prevalence)
rm(patients_by_practice)
rm(QOF_smoking_data)

write.csv(eng_ethnicity_data, 'Outputs/ethnicity_lsoa_data.csv')
write.csv(overcrowding_data, 'Outputs/overcrowding_lsoa_data.csv')
write.csv(smoking_data, 'Outputs/smoking_lsoa_data.csv')
write.csv(sco_ethnicity_data, 'Outputs/sco_ethnicity_data.csv')



