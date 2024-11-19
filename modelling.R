#load libraries
library(janitor)
library(lubridate)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)


## load data 
# load csv file
main_dataset3 <- read_csv("processed_data/main_dataset3.csv")
View(main_dataset3)

# Check the data type of the column
str(main_dataset3$whole_organism_mr_watts)

#remove na values
main_dataset3 <- na.omit(main_dataset3)

# Convert MR to numeric
main_dataset3$whole_organism_mr_watts <- as.numeric(as.character(main_dataset3$whole_organism_mr_watts))

# Shapiro-Wilk test
shapiro.test(main_dataset3$whole_organism_mr_watts)

# Log-transform the data
main_dataset3$log_whole_organism_mr_watts <- log(main_dataset3$whole_organism_mr_watts + 1)

# Create a density plot (line plot)
ggplot(main_dataset3, aes(x = log_whole_organism_mr_watts)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of MR", x = "MR", y = "Frequency") +
  theme_minimal()









# Create the histogram for trophic position
ggplot(main_dataset3, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(main_dataset3, aes(x = mr_mass_grams)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()


