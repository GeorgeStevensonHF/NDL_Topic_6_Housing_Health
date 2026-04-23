
getwd()

# Load packages

library(readxl)
library(writexl)

# Load QOF Smoking Data ########################################################

QOF_smoking_data <- read_excel("Data/QOF smoking statistics.xlsx", sheet = "SMOK")

smoking_data <- QOF_smoking_data[-c(1:9, 6199:6202), ]
colnames(smoking_data) <- as.character(smoking_data[1, ])
smoking_data <- smoking_data[-c(1), ]

smoking_data <- smoking_data %>%
  select(`PCN ODS code`, `PCN name`, `Practice code`, `Practice name`, c(9,32))

colnames(smoking_data) <- c("PCN_code","PCN_name","Practice_code","Practice_name","Total_practice_pop","Total_current_smokers")

smoking_data <- smoking_data %>%
  mutate(Practice_smoking_prev = as.numeric(Total_current_smokers)/as.numeric(Total_practice_pop))
