
#zip_url <- 'https://files.digital.nhs.uk/72/D18B98/gp-reg-pat-prac-all.zip'

#temp <- tempfile()

#download.file(zip_url, destfile = temp)

#unzip(temp, exdir = 'Data/GP_practice_data/')

# This now uses the LSOA-level extract from July 2025 - closest to Aug 2025 we can get.
patients_by_practice <- fread('Data/GP_practice_data/gp-reg-pat-prac-lsoa-all.csv')

head(patients_by_practice)

patients_by_practice <- patients_by_practice %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS)


####### Joining function

join_practices_prescriptions <- function(practices_df = patients_by_practice, prescription_df, label){ 
  practices_joined <- left_join(practices_df, prescription_df, join_by(PRACTICE_CODE == row_id)) %>%
    mutate_at(c('total_items', 'total_quantity', 'total_cost'), ~replace_na(.,0)) %>%
    mutate(items_per_LSOA = total_items*PATIENT_PROPORTION) %>%
    mutate(quantity_per_LSOA = total_quantity*PATIENT_PROPORTION) %>%
    mutate(cost_per_LSOA = total_cost*PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(items = sum(items_per_LSOA), quantity = sum(quantity_per_LSOA), cost = sum(cost_per_LSOA))
  
  LSOA_summed$drug_type <- label
  
  return(LSOA_summed)
}

###################
##### ANTIANX 
###################

LSOA_antianx_summed <- join_practices_prescriptions(prescription_df = antianx_grouped, label = 'anti_anxiety')

#######################
##### ANTIPSYCHOTICS
######################

LSOA_antipsych_summed <- join_practices_prescriptions(prescription_df = antipsych_grouped, label = 'anti_psychotic')

#######################
##### ANTIDEPRESSANTS
######################

LSOA_antidepress_summed <- join_practices_prescriptions(prescription_df = antidepress_grouped, label = 'anti_depressants')

################################
##### INHALERS
################################

inhalers_df <- read_excel("Data/inhalers_list.xlsx") 

inhaler_codelist <- unique(inhalers_df$bnf_presentation_code)

inhalers_joined_list <- list()

for (i in 1:length(inhaler_codelist)) {
  
  filtered_inhaler <- inhalers_grouped %>% filter(bnf_code == inhaler_codelist[[i]])
  
  practices_joined <- left_join(patients_by_practice, filtered_inhaler, join_by(PRACTICE_CODE == row_id)) %>%
    mutate(items_per_LSOA = total_items*PATIENT_PROPORTION) %>%
    mutate(quantity_per_LSOA = total_quantity*PATIENT_PROPORTION) %>%
    mutate(cost_per_LSOA = total_cost*PATIENT_PROPORTION)
  
  practices_joined$items_per_LSOA[is.na(practices_joined$items_per_LSOA)] <- 0
  practices_joined$quantity_per_LSOA[is.na(practices_joined$quantity_per_LSOA)] <- 0
  practices_joined$cost_per_LSOA[is.na(practices_joined$cost_per_LSOA)] <- 0
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(items = sum(items_per_LSOA), quantity = sum(quantity_per_LSOA), cost = sum(cost_per_LSOA))
  
  LSOA_summed$drug_type <- inhaler_codelist[[i]]
  
  inhalers_joined_list <- append(inhalers_joined_list, list(LSOA_summed))
}

LSOA_inhaler_summed <- bind_rows(inhalers_joined_list) %>%
  group_by(LSOA_CODE) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  mutate(drug_type = 'inhaler')

#######################
##### PAIN MEDICATIONS
######################

painmed_joined_list <- list()

for (i in 1:length(pain_codelist)) {
  
  filtered_painmed <- painmed_grouped %>% filter(bnf_code == pain_codelist[[i]])
  
  practices_joined <- left_join(patients_by_practice, filtered_painmed, join_by(PRACTICE_CODE == row_id)) %>%
    mutate(items_per_LSOA = total_items*PATIENT_PROPORTION) %>%
    mutate(quantity_per_LSOA = total_quantity*PATIENT_PROPORTION) %>%
    mutate(cost_per_LSOA = total_cost*PATIENT_PROPORTION)
  
  practices_joined$items_per_LSOA[is.na(practices_joined$items_per_LSOA)] <- 0
  practices_joined$quantity_per_LSOA[is.na(practices_joined$quantity_per_LSOA)] <- 0
  practices_joined$cost_per_LSOA[is.na(practices_joined$cost_per_LSOA)] <- 0
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(items = sum(items_per_LSOA), quantity = sum(quantity_per_LSOA), cost = sum(cost_per_LSOA))
  
  LSOA_summed$drug_type <- pain_codelist[[i]]
  
  painmed_joined_list <- append(painmed_joined_list, list(LSOA_summed))
}

LSOA_painmed_summed <- bind_rows(painmed_joined_list) %>%
  group_by(LSOA_CODE) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  mutate(drug_type = 'painmed')



# Final dataset ################################################################
all_eng_pres <- rbind(LSOA_antianx_summed, LSOA_antidepress_summed, LSOA_antipsych_summed, LSOA_inhaler_summed, LSOA_painmed_summed)

write.csv(all_eng_pres, 'Outputs/england_updated_practice_prescriptions.csv')

hist((LSOA_inhaler_summed$items[LSOA_inhaler_summed$LSOA_CODE != 'EMPTY']))
hist((LSOA_antianx_summed$items[LSOA_antianx_summed$LSOA_CODE != 'EMPTY']))
hist((LSOA_antidepress_summed$items[LSOA_antidepress_summed$LSOA_CODE != 'EMPTY']))
hist((LSOA_antipsych_summed$items[LSOA_antipsych_summed$LSOA_CODE != 'EMPTY']))
hist((LSOA_painmed_summed$items[LSOA_painmed_summed$LSOA_CODE != 'EMPTY']))





