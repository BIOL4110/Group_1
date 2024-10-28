# Load the necessary libraries
library(janitor)
library(lubridate)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

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

# remove all na. rows
combined_clean_data <- na.omit(combined_clean_data)
View(combined_clean_data)


# Create the histogram for trophic position
ggplot(combined_data, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(combined_data, aes(x = body_mass_in_grams)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()

# Create the histogram for Metabolic Rate
ggplot(combined_data, aes(x = mr_raw_mg_per_kg_per_h)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Metabolic Rate", x = "Metabolic Rate", y = "Frequency") +
  theme_minimal()
