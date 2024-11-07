library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)

FULL_Rmax_LHT_MR_ms <- read_csv("FULL_Rmax_LHT_MR_ms.csv")
View(FULL_Rmax_LHT_MR_ms)

# find column names
str(FULL_Rmax_LHT_MR_ms)
column_names <- names(FULL_Rmax_LHT_MR_ms)
print(column_names)

# clean datasets
main_dataset1 <- FULL_Rmax_LHT_MR_ms %>%
  select(-TeleostOrElasmo, -MROriginLocation, -MRCitation, -DFtype )

# Convert column names to snake case
colnames(main_dataset1) <- make_clean_names(names(main_dataset1), case = "snake")
print(names(main_dataset1))

View(main_dataset1)

# removing upper case letters and adding "_"
main_dataset2 <- main_dataset1 %>%
  clean_names() %>%                # Clean column names
  mutate(across(everything(), ~ str_replace_all(., " ", "_"))) %>%  # Replace spaces with underscores
  mutate(across(everything(), tolower)) %>%  # Convert all to lowercase
  mutate(scientific_name = str_to_title(scientific_name))  # Convert scientific_name to title case
View(main_dataset2)

### Fishbase trophic position data:

# reading excel data and cleaning up names
df <- read_xlsx("trophic_position.xlsx") %>% 
  clean_names()

