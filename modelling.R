library(ggplot2)
library(readr)

## load data 
main_dataset3 <- read_csv("main_dataset3.csv")

# Check the data type of the column
str(main_dataset3$whole_organism_mr_watts)

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
