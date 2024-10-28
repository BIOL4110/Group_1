# Load the necessary libraries
library(janitor)
library(lubridate)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# load csv file
combined_data <- read_csv("combined_data.csv")
View(combined_data)

# Assuming combined_data is your dataset
combined_clean_data <- combined_data %>%
  clean_names() %>%                # Clean column names
  mutate(across(everything(), ~ str_replace_all(., " ", "_"))) %>%  # Replace spaces with underscores
  mutate(across(everything(), tolower)) %>%  # Convert all to lowercase
  mutate(scientific_name = str_to_title(scientific_name))  # Convert scientific_name to title cas
# View the cleaned dataset
View(combined_clean_data)

# remove all na. rows
combined_clean_data <- na.omit(combined_clean_data)
View(combined_clean_data)


# Create the histogram for trophic position
ggplot(combined_clean_data, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(combined_clean_data, aes(x = body_mass_in_grams)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()


# Create the histogram for Metabolic Rate
ggplot(combined_clean_data, aes(x = mr_raw_mg_per_kg_per_h)) +
  geom_histogram(binwidth = 1.0, fill = "blue", color = "black") +
  labs(title = "Histogram of Metabolic Rate", x = "Metabolic Rate", y = "Frequency") +
  theme_minimal()



# Remove rows with mr_raw_mg_per_kg_per_h equal to 1258240.0000
combined_clean_data <- combined_clean_data[combined_clean_data$mr_raw_mg_per_kg_per_h != 1258240.0000, ]

# Apply IQR outlier detection to each numeric column
outliers_list <- lapply(combined_clean_data[, sapply(combined_clean_data, is.numeric)], function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outliers <- column[column < lower_bound | column > upper_bound]
  
  # Return outliers with their column name
  return(outliers)
})

# Print outliers for each column
print(outliers_list)

outliers_removed_data <- subset(
  combined_clean_data,
  mr_raw_mg_per_kg_per_h >= lower_bound & mr_raw_mg_per_kg_per_h <= upper_bound
)

# Create the histogram for trophic position
ggplot(outliers_removed_data, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(outliers_removed_data, aes(x = body_mass_in_grams)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()

# Create the histogram for Metabolic Rate
ggplot(outliers_removed_data, aes(x = mr_raw_mg_per_kg_per_h)) +
  geom_histogram(binwidth = 1.0, fill = "blue", color = "black") +
  labs(title = "Histogram of Metabolic Rate", x = "Metabolic Rate", y = "Frequency") +
  theme_minimal()




