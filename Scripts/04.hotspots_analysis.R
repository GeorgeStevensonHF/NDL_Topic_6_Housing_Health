

fp_data <- read_csv('Outputs/ndl_fp_data.csv')

fp_data <- fp_data %>%
  mutate(
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")),
    top_or_not = case_when(classification %in% c('Very hot', 'Hot') ~ 'Hotspots',
                           TRUE ~ 'Elsewhere'),
    income_dist_median = quantcut(LSOA_incmedian, labels=c('1','2','3','4'))
  ) 

outliers <- c('E01009406', 'E01015180')

fp_data_no_outliers <- fp_data %>% filter(!(LSOA21CD %in% outliers))

fp_data_low_income <- fp_data %>% filter(income_dist_median == '1')



# Charts to include 

#1. Map of fuel poverty hotspots UK
 # Possible zoom in on Scotland

ggplot(data = fp_data, aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Tree Equity Hot Spots in Tucson"
  )

#2.Demographic features of hotspots
 # Urban/rural
fp_data %>% tabyl(ruc21ind, top_or_not) %>% adorn_percentages(denominator = 'col')

fp_data %>% tabyl(ruc21ind, classification) %>% adorn_percentages(denominator = 'col')

 # Income distribution

ggplot(fp_data) +
  aes(x = classification, y = LSOA_incmedian, color = classification) +
  geom_boxplot()

ggplot(fp_data) +
  aes(x = top_or_not, y = LSOA_incmedian, color = top_or_not) +
  geom_boxplot()

income_dist_df <- fp_data %>% select(LSOA21CD, LSOA_incmedian, classification, top_or_not)

write.csv(income_dist_df, 'Outputs/income_distribution_classification.csv')

 # Age Distribution


 # Ethnicity


#3. SDE: Comp of secondary care contacts per group
 # Plus: stats test for diff between those groups
 
 # Comp of secondary care contacts per group divided by income and age


#4. Comp of prescription types per group (barplot)

prescriptions_groups <- fp_data %>%
  drop_na() %>%
  group_by(classification) %>%
  summarise(inhaler = mean(inhaler_prv)*100,
            antidep = mean(antidep_prv)*100,
            antianx = mean(antianx_prv)*100,
            antipsych = mean(antipsych_prv)*100)

write.csv(prescriptions_groups, 'Outputs/prescriptions_by_classification.csv')


#5. Demonstration of statistically significant difference in inhalers (boxplot)

ggplot(fp_data) +
           aes(x = fct_reorder(top_or_not, -inhaler_prv), y = inhaler_prv*100, color = fct_reorder(top_or_not, -inhaler_prv)) +
           geom_boxplot(show.legend = FALSE) +
  theme_minimal()+
  xlab('Fuel poverty hotspot status of LSOA') +
  ylab('Inhaler prescriptions per 100 people, 2024')

leveneTest(inhaler_prv ~ top_or_not, data = fp_data)

#Inhalers (significant)
oneway.test(inhaler_prv ~ top_or_not, 
            data = fp_data,
            var.equal = FALSE # assuming unequal variances
)

#Anti depressants (significant)
oneway.test(antidep_prv ~ top_or_not, 
            data = fp_data,
            var.equal = FALSE # assuming unequal variances
)


#Anti anxiety (significant)
oneway.test(antianx_prv ~ top_or_not, 
            data = fp_data,
            var.equal = FALSE # assuming unequal variances
)



inhaler_classification <- fp_data %>%
  drop_na() %>%
  group_by(top_or_not) %>%
  summarise(inhaler = mean(inhaler_prv)*100)

write.csv(inhaler_classification, 'Outputs/inhaler_classification.csv')

# Sensivity check 
leveneTest(inhaler_prv ~ top_or_not, data = fp_data_no_outliers)

oneway.test(inhaler_prv ~ top_or_not, 
            data = fp_data_no_outliers,
            var.equal = FALSE # assuming unequal variances
)


#6. Prescriptions by group split by income

# Test within low income

ggplot(fp_data) +
  aes(x = income_dist_median, y = inhaler_prv*100, color = fct_reorder(top_or_not, -inhaler_prv)) +
  geom_boxplot() +
  theme_minimal() +
  xlab('Median Income Qartile of LSOA (1 = lowest)') +
  ylab('Inhaler prescriptions per 100 people, 2024')+
  labs(color='')

leveneTest(inhaler_prv ~ top_or_not, data = fp_data_low_income)

summary(aov(antipsych_prv ~ top_or_not, 
                   data = fp_data_low_income
    #,
     #              var.equal = FALSE # assuming unequal variances
))

oneway.test(antidep_prv ~ top_or_not, 
            data = fp_data_low_income,
                         var.equal = FALSE # assuming unequal variances
)







