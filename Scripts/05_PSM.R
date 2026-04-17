
library(marginaleffects)
library(MatchIt)

#Fuel poverty data
fp_data <- read_csv('Outputs/ndl_fp_data.csv')

lsoa_ages <- read_excel('Data/uk_age_lsoa.xlsx', sheet = 'Mid-2024 LSOA 2021', skip = 3)

ef_young <- paste0(rep('F', 18), 0:17)
em_young <- paste0(rep('M', 18), 0:17)
ef_wa <- paste0(rep('F', 47), 18:64)
em_wa <- paste0(rep('M', 47), 18:64)
ef_senior <- paste0(rep('F', 26), 65:90)
em_senior <- paste0(rep('M', 26), 65:90)

lsoa_ages <- lsoa_ages %>%
  select(!c(`LAD 2023 Code`, `LAD 2023 Name`, `LSOA 2021 Name`))%>%
  pivot_longer(2:length(colnames(.)), names_to = 'age_cat', values_to = 'value')

lsoa_ages_agg <- lsoa_ages %>%
  mutate(agg_cat = case_when(age_cat %in% ef_young | age_cat %in% em_young ~ '<=17',
                             age_cat %in% ef_wa | age_cat %in% em_wa ~ '18-64',
                             age_cat %in% ef_senior | age_cat %in% em_senior ~ '65+',
                             age_cat == 'Total' ~ 'Total',
                             TRUE ~ NA
  )) %>%
  group_by(`LSOA 2021 Code`, agg_cat) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = agg_cat, values_from = value) %>%
  mutate(young_perc = `<=17`/Total,
         wa_perc = `18-64`/Total,
         senior_perc = `65+`/Total)


# Ages for Scotland

dz_22_lookup <- read_csv('Data/sct_oa_dz_lookup.csv')

scot_data_ages <- read_csv('Data/sct_oa_ages.csv', skip = 3) %>%
  rename(oa_code = 1) %>%
  mutate(`All people` = as.character(`All people`)) %>%
  pivot_longer(2:length(colnames(.)), names_to = 'age_cat', values_to = 'value') %>%
  mutate(value = case_when(value == '-' ~ '0',
                           TRUE ~ value)) %>%
  mutate(agg_cat = case_when(age_cat == 'Under 1' | age_cat %in% paste0(1:17) ~ '<=17',
                             age_cat %in% paste0(18:64) ~ '18-64',
                             age_cat %in% paste0(65:99) | age_cat == '100 and over' ~ '65+',
                             age_cat == 'All people' ~ 'Total',
                             TRUE ~ NA
  )) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!(is.na(value))) %>%
  group_by(oa_code, agg_cat) %>%
  summarise(value = sum(value)) %>%
  left_join(., dz_22_lookup, by = join_by(oa_code == OA22)) %>%
  group_by(DZ22, agg_cat) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = agg_cat, values_from = value) %>%
  mutate(young_perc = `<=17`/Total,
         wa_perc = `18-64`/Total,
         senior_perc = `65+`/Total) %>%
  rename(`LSOA 2021 Code` = DZ22)


ages_data <- rbind(lsoa_ages_agg, scot_data_ages)

sct_urban_rural <- read.csv('Data/sct_urban_rural.csv') %>%
  select(DZ22_Code, UR6_Name) %>%
  rename(LSOA21CD = DZ22_Code, ru_sct = UR6_Name)

fp_data <- fp_data %>%
  mutate(
    income_dist_median = quantcut(LSOA_incmedian, labels=c('1','2','3','4')),
    fp_quint_mean = quantcut(LSOA_FPmean, 5, labels=c('1','2','3','4', '5')),
    fp_dec_mean = quantcut(LSOA_FPmean, 10, labels=c('1','2','3','4', '5', 
                                                     '6', '7', '8', '9', '10')),
    top5_or_not = case_when(fp_quint_mean == '5' ~ 'Highest fuel poverty',
                            TRUE ~ 'Elsewhere'),
    top10_or_not = case_when(fp_dec_mean == '10' ~ 'Highest fuel poverty',
                             TRUE ~ 'Elsewhere'),
    top_20_or_not = case_when(fp_dec_mean %in% c('9', '10') ~ 1,
                              TRUE ~ 0)
  ) %>%
  left_join(., ages_data, by = join_by(LSOA21CD == `LSOA 2021 Code`)) %>%
  left_join(., sct_urban_rural, by = 'LSOA21CD') %>%
  mutate(urb_rur = case_when(
    (str_detect(LSOA21CD, 'E') | str_detect(LSOA21CD, 'W'))  ~ ruc21ind,
    str_detect(LSOA21CD, 'S') ~ ru_sct
  )) %>%
  mutate(country = case_when(
    str_detect(LSOA21CD, 'E') ~ 'England',
    str_detect(LSOA21CD, 'W') ~ 'Wales',
    str_detect(LSOA21CD, 'S') ~ 'Scotland'
  )) %>%
  mutate(urb_or_not = case_when(urb_rur %in% c('Urban', 'Urban near town/city',
                                               'Large Urban Areas', 'Other Urban Areas') ~ 'Urban',
                                TRUE ~ 'Small towns and rural')) %>%
  select(!c(ru_sct, `...1`))


#Initial imbalance

m.out0 <- matchit(top_20_or_not ~ LSOA_incmedian + wa_perc + urb_rur,
                  data = fp_data,
                  method = NULL,
                  distance = "glm")
summary(m.out0)

m.out1 <- matchit(top_20_or_not ~ LSOA_incmedian + wa_perc + urb_rur,
                  data = fp_data,
                  method = 'nearest',
                  distance = "glm")

summary(m.out1, un = FALSE)

plot(m.out1, type = "jitter", interactive = FALSE)

plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~LSOA_incmedian + wa_perc + urb_rur)

plot(summary(m.out1))

mdata <- match_data(m.out1)
head(mdata)

fit <- lm(inhaler_prv ~ top_20_or_not * (LSOA_incmedian + wa_perc + urb_rur),
          data = mdata,
          weights = weights)

avg_comparisons(fit,
                variables = "top_20_or_not",
                vcov = ~subclass,
                newdata = subset(top_20_or_not == 1))
