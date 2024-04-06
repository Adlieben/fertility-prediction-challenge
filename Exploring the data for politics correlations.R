# Settings
setwd('C:/Users/adama/OneDrive/Documents/Utrecht/PreFer challenge/5e8ab08c-b634-4948-8b47-8792d36d753f') # Enter your directory
Sys.setenv(LANG = "en")

# Libraries¨
library(readr)
library(tidyverse)
library(data.table)

# Create function to automate data loading
fread2 <- function(data){
  table <- data.table::fread(data, 
                             keepLeadingZeros = TRUE, # if FALSE adds zeroes to some dates
                             data.table = FALSE) # returns a data.frame object rather than data.table 
  return(table)
}

# Load the data
PreFer_train_data <- fread2("training_data/PreFer_train_data.csv")
PreFer_train_outcome <- fread2("training_data/PreFer_train_outcome.csv")

# Load in bulk
filenames <- list.files(path = "other_data", full.names=TRUE) %>% str_replace("other_dat./", "other_data/")
ldf <- lapply(filenames, fread2)
names(ldf) <- filenames %>% str_replace("other_data/", "")

# Load the codebooks
codebook <- read_csv('codebooks/PreFer_codebook.csv')
codebook_summ <- read_csv('codebooks/PreFer_codebook_summary.csv')

# Look at summaries
summary(as.factor(codebook_summ$survey))

# Checking for missingness
hist(codebook$prop_missing)

# Check politics ------------------------------------------------------------
no_missing <- codebook %>% filter(prop_missing != 1)
politics <- no_missing %>% filter(survey == 'Politics and Values')

hist(politics$prop_missing) # Not many missings

# Collect politics variables
names <- as.vector(list(politics$var_name))

# Filter in the data
politics_data <- PreFer_train_data %>% select(all_of(names[[1]]))
politics_outcome <- data.frame(politics_data, PreFer_train_outcome)

# Try 19 as an example
pol_19 <- politics_outcome[ , grepl("19" , names(politics_outcome))]
pol_19 <- data.frame(pol_19, politics_outcome$new_child) # Add the outcome back to the dataframe

# Looking for correlations
library(lares)

results <- corr_var(pol_19, # name of dataset
         politics_outcome.new_child, # name of variable to focus on
         ceiling = 0.05,
         plot = FALSE,
         top = 257
)

# Using the results, we then get:
common_columns <- intersect(results$variables, names(politics_data))
common_columns <- common_columns %>% str_replace("cv19k", "")
matches <- paste(common_columns, collapse = "|")
politics_reduced <- politics_data %>% select(-contains(common_columns))
  
  
  