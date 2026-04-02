
inhalers_df <- read_excel("Data/inhalers_list.xlsx") 

inhaler_codelist <- unique(inhalers_df$bnf_presentation_code)

dates <- c('202408', '202409', '202410', '202411', '202412', '202501',
           '202502', '202503', '202504', '202505', '202506', '202507')

### SCOTLAND

scotland_data_list <- lapply(1:length(dates), function(i){
  
  df <- read_csv(paste0('Data/Scottish_prescription_data/pitc', dates[[i]], '.csv'),
                 col_types = list(HBT = col_character(),
                                  GPPractice = col_character(),
                                  DMDCode = col_character(),
                                  BNFItemCode = col_character(),
                                  BNFItemDescription = col_character(),
                                  PrescribedType = col_character(),
                                  NumberOfPaidItems = col_double(),
                                  PaidQuantity = col_double(),
                                  GrossIngredientCost = col_double(),
                                  PaidDateMonth = col_character()
                 ) )
  
  filtered_df <- df %>% filter(BNFItemCode %in% inhaler_codelist | BNFItemCode %in% pain_codelist | str_detect(BNFItemCode, '^040102') | str_detect(BNFItemCode, '^0402') | str_detect(BNFItemCode, '^0403'))
  
  return(filtered_df)
  
})

all_scotland_data <- bind_rows(scotland_data_list)

sct_dz_to_hscp <- read_csv('Data/Scottish_prescription_data/datazone_to_hscp_locality_lookup_2024-05.csv')

sct_dz_populations <- read_csv('Data/Scottish_prescription_data/scotland_dz_populations.csv') %>%
  select(Data_zone_code, Total_population)

sct_practice_size <- read_csv('Data/Scottish_prescription_data/practice_size_scotland.csv')

sct_dz_to_hscp <- left_join(sct_dz_to_hscp, sct_dz_populations, by = join_by(DataZone == Data_zone_code)) %>%
  group_by(SubHSCPName) %>%
  mutate(total_sub_hscp_population = sum(Total_population)) %>%
  ungroup() %>%
  group_by(HSCP) %>%
  mutate(total_hscp_population = sum(Total_population)) %>%
  ungroup() %>%
  mutate(prop_of_subHSCP = Total_population/total_sub_hscp_population) %>%
  mutate(prop_of_HSCP = Total_population/total_hscp_population)

practices_to_HSCP <- sct_dz_to_hscp %>%
  rename(HSCP_dz = HSCP) %>%
  right_join(., sct_practice_size, by = 'DataZone') %>%
  mutate(HSCP_check = case_when(HSCP == HSCP_dz ~ 1, TRUE ~ 0)) %>%
  select(DataZone, SubHSCPName, HSCP_dz, Total_population, total_sub_hscp_population, total_hscp_population,
         prop_of_subHSCP, prop_of_HSCP, PracticeCode, GPPracticeName, PracticeListSize, HSCP, GPCluster)

practices_to_HSCP$PracticeCode <- as.character(practices_to_HSCP$PracticeCode)



sct_inhalers_by_dz <- all_scotland_data %>%
  filter(BNFItemCode %in% inhaler_codelist) %>%
  group_by(GPPractice) %>%
  summarise(items = sum(NumberOfPaidItems), quantity = sum(PaidQuantity), cost = sum(GrossIngredientCost)) %>%
  mutate(item = 'inhaler') %>%
  full_join(., practices_to_HSCP, by = join_by(GPPractice == PracticeCode)) %>%
  group_by(SubHSCPName, item) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  full_join(., sct_dz_to_hscp, by = 'SubHSCPName') %>%
  mutate(items_per_dz = items*prop_of_subHSCP) %>%
  mutate(quantity_per_dz = quantity*prop_of_subHSCP) %>%
  mutate(cost_per_dz = cost*prop_of_subHSCP)


sct_antidep_by_dz <- all_scotland_data %>%
  filter(str_detect(BNFItemCode, '^0403')) %>%
  group_by(GPPractice) %>%
  summarise(items = sum(NumberOfPaidItems), quantity = sum(PaidQuantity), cost = sum(GrossIngredientCost)) %>%
  mutate(item = 'antidep') %>%
  full_join(., practices_to_HSCP, by = join_by(GPPractice == PracticeCode)) %>%
  group_by(SubHSCPName, item) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  full_join(., sct_dz_to_hscp, by = 'SubHSCPName') %>%
  mutate(items_per_dz = items*prop_of_subHSCP) %>%
  mutate(quantity_per_dz = quantity*prop_of_subHSCP) %>%
  mutate(cost_per_dz = cost*prop_of_subHSCP)

sct_antianx_by_dz <- all_scotland_data %>%
  filter(str_detect(BNFItemCode, '^040102')) %>%
  group_by(GPPractice) %>%
  summarise(items = sum(NumberOfPaidItems), quantity = sum(PaidQuantity), cost = sum(GrossIngredientCost)) %>%
  mutate(item = 'antianx') %>%
  full_join(., practices_to_HSCP, by = join_by(GPPractice == PracticeCode)) %>%
  group_by(SubHSCPName, item) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  full_join(., sct_dz_to_hscp, by = 'SubHSCPName') %>%
  mutate(items_per_dz = items*prop_of_subHSCP) %>%
  mutate(quantity_per_dz = quantity*prop_of_subHSCP) %>%
  mutate(cost_per_dz = cost*prop_of_subHSCP)

sct_antipsych_by_dz <- all_scotland_data %>%
  filter(str_detect(BNFItemCode, '^0402')) %>%
  group_by(GPPractice) %>%
  summarise(items = sum(NumberOfPaidItems), quantity = sum(PaidQuantity), cost = sum(GrossIngredientCost)) %>%
  mutate(item = 'antipsych') %>%
  full_join(., practices_to_HSCP, by = join_by(GPPractice == PracticeCode)) %>%
  group_by(SubHSCPName, item) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  full_join(., sct_dz_to_hscp, by = 'SubHSCPName') %>%
  mutate(items_per_dz = items*prop_of_subHSCP) %>%
  mutate(quantity_per_dz = quantity*prop_of_subHSCP) %>%
  mutate(cost_per_dz = cost*prop_of_subHSCP)

sct_painmed_by_dz <- all_scotland_data %>%
  filter(BNFItemCode %in% pain_codelist) %>%
  group_by(GPPractice) %>%
  summarise(items = sum(NumberOfPaidItems), quantity = sum(PaidQuantity), cost = sum(GrossIngredientCost)) %>%
  mutate(item = 'painmed') %>%
  full_join(., practices_to_HSCP, by = join_by(GPPractice == PracticeCode)) %>%
  group_by(SubHSCPName, item) %>%
  summarise(items = sum(items), quantity = sum(quantity), cost = sum(cost)) %>%
  full_join(., sct_dz_to_hscp, by = 'SubHSCPName') %>%
  mutate(items_per_dz = items*prop_of_subHSCP) %>%
  mutate(quantity_per_dz = quantity*prop_of_subHSCP) %>%
  mutate(cost_per_dz = cost*prop_of_subHSCP)


sct_allpres <- rbind(sct_inhalers_by_dz, sct_antianx_by_dz, sct_antidep_by_dz, sct_antipsych_by_dz, sct_painmed_by_dz)

write.csv(sct_allpres, 'Outputs/sct_all_prescriptions_new.csv')


###############################
###  Wales
###############################


locality_codes <- read_csv('Data/Welsh_prescription_data/wales_locality_codes.csv')

wales_data_list <- lapply(1:length(dates), function(i){
  
  df <- read_csv(paste0('Data/Welsh_prescription_data/GPData', dates[[i]], '.csv'),
                 col_types = list(HB = col_character(),
                                  Locality = col_character(),
                                  PracticeID = col_character(),
                                  BNFCode = col_character(),
                                  BNFName = col_character(),
                                  Items = col_double(),
                                  NIC = col_double(),
                                  ActCost = col_double(),
                                  Quantity = col_double(),
                                  DDD = col_double(),
                                  ADQ = col_double(),
                                  Period = col_character()
                 ) )
  
  filtered_df <- df %>% filter(BNFCode %in% inhaler_codelist | BNFCode %in% pain_codelist | str_detect(BNFCode, '^040102') | str_detect(BNFCode, '^0402') | str_detect(BNFCode, '^0403'))
  
  return(filtered_df)
  
})

all_wales_data <- bind_rows(wales_data_list)

wales_hb_reg_by_lsoa <- read_csv('Data/Welsh_prescription_data/wales_practices_registered_by_LSOA.csv')

wales_hb_reg_by_lsoa$patients <- as.numeric(wales_hb_reg_by_lsoa$patients)

wales_hb_reg_by_lsoa$patients[is.na(wales_hb_reg_by_lsoa$patients)] <- 0

wales_hb_reg_by_lsoa$Locality <- str_sub(wales_hb_reg_by_lsoa$LSOA_name, 1, str_length(wales_hb_reg_by_lsoa$LSOA_name)-5)

wales_hb_reg_by_lsoa <- wales_hb_reg_by_lsoa %>%
  group_by(Locality) %>%
  mutate(total_locality_pop = sum(patients)) %>%
  ungroup() %>%
  mutate(prop_of_locality = patients/total_locality_pop) %>%
  left_join(., locality_codes, by = 'Locality')

wales_inhalers_by_lsoa <- all_wales_data %>%
  filter(BNFCode %in% inhaler_codelist) %>%
  group_by(Locality) %>%
  summarise(items = sum(Items), cost = sum(ActCost), quantity = sum(Quantity)) %>%
  mutate(item = 'inhalers') %>%
  right_join(wales_hb_reg_by_lsoa, ., by = join_by(Code == Locality)) %>%
  mutate(items_per_lsoa = items*prop_of_locality) %>%
  mutate(cost_per_lsoa = cost*prop_of_locality) %>%
  mutate(quantity_per_lsoa = quantity*prop_of_locality)

wales_antidep_by_lsoa <- all_wales_data %>%
  filter(str_detect(BNFCode, '^0403')) %>%
  group_by(Locality) %>%
  summarise(items = sum(Items), cost = sum(ActCost), quantity = sum(Quantity)) %>%
  mutate(item = 'antidep') %>%
  right_join(wales_hb_reg_by_lsoa, ., by = join_by(Code == Locality)) %>%
  mutate(items_per_lsoa = items*prop_of_locality) %>%
  mutate(cost_per_lsoa = cost*prop_of_locality) %>%
  mutate(quantity_per_lsoa = quantity*prop_of_locality)


wales_antianx_by_lsoa <- all_wales_data %>%
  filter(str_detect(BNFCode, '^040102')) %>%
  group_by(Locality) %>%
  summarise(items = sum(Items), cost = sum(ActCost), quantity = sum(Quantity)) %>%
  mutate(item = 'antianx') %>%
  right_join(wales_hb_reg_by_lsoa, ., by = join_by(Code == Locality)) %>%
  mutate(items_per_lsoa = items*prop_of_locality) %>%
  mutate(cost_per_lsoa = cost*prop_of_locality) %>%
  mutate(quantity_per_lsoa = quantity*prop_of_locality)

wales_antipsych_by_lsoa <- all_wales_data %>%
  filter(str_detect(BNFCode, '^0402')) %>%
  group_by(Locality) %>%
  summarise(items = sum(Items), cost = sum(ActCost), quantity = sum(Quantity)) %>%
  mutate(item = 'antipsych') %>%
  right_join(wales_hb_reg_by_lsoa, ., by = join_by(Code == Locality)) %>%
  mutate(items_per_lsoa = items*prop_of_locality) %>%
  mutate(cost_per_lsoa = cost*prop_of_locality) %>%
  mutate(quantity_per_lsoa = quantity*prop_of_locality)

wales_painmed_by_lsoa <- all_wales_data %>%
  filter(BNFCode %in% pain_codelist) %>%
  group_by(Locality) %>%
  summarise(items = sum(Items), cost = sum(ActCost), quantity = sum(Quantity)) %>%
  mutate(item = 'painmed') %>%
  right_join(wales_hb_reg_by_lsoa, ., by = join_by(Code == Locality)) %>%
  mutate(items_per_lsoa = items*prop_of_locality) %>%
  mutate(cost_per_lsoa = cost*prop_of_locality) %>%
  mutate(quantity_per_lsoa = quantity*prop_of_locality)


wales_allpres <- rbind(wales_inhalers_by_lsoa, wales_antianx_by_lsoa, wales_antidep_by_lsoa, wales_antipsych_by_lsoa, wales_painmed_by_lsoa)

write.csv(wales_allpres, 'Outputs/wales_all_prescriptions_new.csv')
