
# 01 Loading prescriptions data ################################################
# updated 31/03/2026 to take August 24 - August 25 matching FP data ############


library(openxlsx)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)

# Note: dates covered: July 2024 - June 2025

# Cat 1: inhalers
# Cat 2: Anti-anxiety meds
# Cat 3: Anti-depressants


####################
########### INHALERS
####################


#Read in BNF code file
inhalers_df <- read_excel("Data/inhalers_list.xlsx") 

inhaler_codelist <- unique(inhalers_df$bnf_presentation_code)

dates <- list('2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01', 
              '2025-01-01', '2025-02-01', '2025-03-01', '2025-04-01', '2025-05-01', '2025-06-01', '2025-07-01')


full_inhaler_list <- list()


read_presc_function <- function(codes){
  
  full_list <- list()
  
  for (i in 1:length(codes)){
    
    minilist <- list()
    
    for (x in 1:length(dates)){
      
      single_df <- read_csv(paste0("https://openprescribing.net/api/1.0/spending_by_org/?org_type=practice&code=", codes[[i]], "&date=", dates[[x]],"&format=csv"),
                            col_types = list(ccg = col_character(),
                                             row_id = col_character(),
                                             row_name = col_character(),
                                             actual_cost = col_double(),
                                             items = col_double(),
                                             quantity = col_double(),
                                             setting = col_character(),
                                             date = col_date()
                            ))
      
      single_df$bnf_code <- codes[[i]]
      
      minilist <- append(minilist, list(single_df))
      
    }
    
    combined_df <- bind_rows(minilist)
    
    full_list <- append(full_list, list(combined_df))
    
  }
  
  return(full_list)
  
}

full_inhaler_list <- read_presc_function(codes = inhaler_codelist)

# Create df

full_inhaler_df <- bind_rows(full_inhaler_list)

full_inhaler_df <- full_inhaler_df %>% select(!(X1:X8))

# Group df

inhalers_grouped <- full_inhaler_df %>%
  group_by(bnf_code, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


########################
########### ANTI ANXIETY
########################


#Read in BNF code file

full_antianx_list <- read_presc_function(codes = list('040102'))

# Create df
antianx_df <- full_antianx_list[[1]]

# Group df
antianx_grouped <- antianx_df %>%
  group_by(bnf_code, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))

############################
########### ANTI PSYCHOTICS
############################

#Read in BNF code file

full_antipsych_list <- read_presc_function(codes = list('0402'))

# Create df
antipsych_df <- full_antipsych_list[[1]]

# Group df
antipsych_grouped <- antipsych_df %>%
  group_by(bnf_code, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))

#############################
########### ANTI DEPRESSANTS
#############################

#Read in BNF code file

full_antidepress_list <- read_presc_function(codes = list('0403'))

# Create df
antidepress_df <- full_antidepress_list[[1]]

#Group df
antidepress_grouped <- antidepress_df %>%
  group_by(bnf_code, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


#############################
########### PAIN MEDICATIONS
#############################

#Read in BNF code file
pain_df <- read_excel("Data/pain_medication_list.xlsx") 

pain_df <- pain_df %>%
  select(table, code, code_description, bnf_presentation_code)

pain_codelist <- unique(pain_df$bnf_presentation_code)

#full_pain_list <- read_presc_function(codes = pain_codelist)
#
# Create df
#full_pain_df <- unique(full_pain_list[[1]])
#
#Group df
#painmed_grouped <- full_pain_df %>%
#  group_by(bnf_code, row_id, row_name) %>%
#  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))

setwd("C:/Users/georges/OneDrive - The Health Foundation/Documents/NDL")
getwd()

LSOA_painmed_summed <- read.csv("LSOA_painmeds.csv")
LSOA_painmed_summed <- LSOA_painmed_summed %>%
  select(!X)
