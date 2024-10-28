# Load the necessary libraries
library(janitor)
library(lubridate)
library(readr)
library(stringr)
library(dplyr)

# load csv file
combined_data <- read_csv("combined_data.csv")
View(combined_data)

# Assuming combined_data is your dataset
combined_clean_data <- combined_data %>%
  clean_names() %>%                # Clean column names
  mutate(across(everything(), ~ str_replace_all(., " ", "_"))) %>%  # Replace spaces with underscores
  mutate(across(everything(), tolower)) %>%  # Convert all to lowercase
  mutate(scientific_name = str_to_title(scientific_name))  # Convert scientific_name to title case

# View the cleaned dataset
View(combined_clean_data)
