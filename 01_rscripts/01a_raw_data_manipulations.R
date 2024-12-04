#load packages#
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(tidyr)
library(stringr)
library(readxl)

### Gravel et al. data:

#load dataset
gravel_dataset <- read_csv("00_raw/00c_gravel_dataset.csv") %>% 
  mutate(unique_id = row.names(.))
View(gravel_dataset)



#view data#
str(gravel_dataset)
column_names <- names(gravel_dataset)
print(column_names)

#clean datasets#
gravel_clean <- gravel_dataset %>%
  select(-ScientificNamePopID, -SerenaOrSarah, TeleostOrElasmo, -MROriginLocation, -SampleSize, -RawMetabolicRateCitation, -Temperature, -TemperatureUnits, -FactorialAerobicScope, -binTemp, )
View(gravel_clean)

# Convert all weights to grams
gravel_WeightFix <- gravel_clean %>%
  mutate(BodyMassInGrams = case_when(
    BodyMassUnits == "kg" ~ BodyMass * 1000,
    BodyMassUnits == "g"  ~ BodyMass,
    TRUE ~ NA_real_  # Handle unexpected units
  ))

gravel_cleanWeightFix <- gravel_WeightFix %>%
  select(-BodyMass, -BodyMassUnits)

#convert all metabolic rates to same units #
unique_units <- unique(gravel_cleanWeightFix$RawMetabolicRateUnits)
print(unique_units)

# Define the conversion function
convert_to_mg_per_kg_per_h <- function(value, unit) {
  if (unit == "mg O2 kg^-1 h^-1") {
    return(value)  # No conversion needed
  } else if (unit == "mg O2 kg^-1 min^-1") {
    return(value * 60)  # Convert min to h
  } else if (unit == "mg O2 kg^-0.82 h^-1") {
    return(value * (1 / 0.82))  # Adjust for exponent
  } else if (unit == "mg O2 h^-1") {
    return(value * 1000)  # Convert to per kg
  } else if (unit == "cal g^-1 day^-1") {
    return(value * 4184 / 1000 / 24)  # Convert to mg O2 kg^-1 h^-1
  } else if (unit == "mg O2 kg^-0.89 h^-1") {
    return(value * (1 / 0.89))  # Adjust for exponent
  } else if (unit == "mg O2 kg^-0.8 h^-1") {
    return(value * (1 / 0.8))  # Adjust for exponent
  } else if (unit == "umol O2 g^-1 h^-1") {
    return(value * 32 * 1000)  # Convert to mg O2 kg^-1
  } else if (unit == "umol O2 min^-1") {
    return(value * 60 * 32 / 1000)  # Convert to mg O2 kg^-1 h^-1
  } else if (unit == "mmol O2 kg^-1 h^-1") {
    return(value * 1000)  # Convert to mg O2 kg^-1
  } else if (unit == "umol O2 g^-1 min^-1") {
    return(value * 60 * 32)  # Convert to mg O2 kg^-1
  } else if (unit == "ul O2 mg^-1 h^-1") {
    return(value * 1)  # Assuming 1 ul = 1 mg
  } else if (unit == "mg O2 g^-1 h^-1") {
    return(value * 1000)  # Convert to mg O2 kg^-1
  } else if (unit == "umol O2 kg^-1 min^-1") {
    return(value * 60 * 32)  # Convert to mg O2 kg^-1 h^-1
  } else if (unit == "g O2 day^-1") {
    return(value * 1000 / 24)  # Convert to mg O2 kg^-1 h^-1
  } else if (unit == "mg O2 kg^-0.81 h^-1") {
    return(value * (1 / 0.81))  # Adjust for exponent
  } else if (unit == "mg O2 min^-1") {
    return(value * 60)  # Convert to h
  } else {
    return(NA)  # Handle unknown units
  }
}

# Convert all units to mg O2 kg^-1 h^-1
gravel_cleanWeightFix$ConvertedValues <- mapply(
  convert_to_mg_per_kg_per_h, 
  gravel_cleanWeightFix$RawMetabolicRate, 
  gravel_cleanWeightFix$RawMetabolicRateUnits
)

gravel_clean <- gravel_cleanWeightFix %>%
  select(-RawMetabolicRate)

gravel_clean <- gravel_clean %>%
  rename(mr_raw_mg_per_kg_per_h = ConvertedValues ,
         family = Family, 
         common_name = CommonName, 
         teleost_or_elasmo = TeleostOrElasmo, 
         mr_raw_data_or_mean = MRRawDataOrMean, 
         body_mass_in_grams = BodyMassInGrams, 
         mass_raw_data_or_mean = MassRawDataOrMean,
         scientific_name = ScientificName) %>%   # Rename the columns to snakecase 
  select(scientific_name, family, common_name, teleost_or_elasmo, 
         mr_raw_mg_per_kg_per_h, mr_raw_data_or_mean, body_mass_in_grams, unique_id, RawMetabolicRateUnits, 
         mass_raw_data_or_mean) # Rearrange the columns

# View the updated dataset
View(gravel_clean)

#scatterplot#
ggplot(gravel_clean, aes(x = body_mass_in_grams, y = mr_raw_mg_per_kg_per_h)) +
  geom_point(alpha = 0.3, size = 5) +  # Add points to the plot
  labs(x = "Body Mass (grams)",
       y = "MR Raw (mg O2 kg^-1 h^-1)") +
  theme_minimal() +  # Use a minimal theme for better aesthetics
  theme(title.text = element_text(size = 12), axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

### Fishbase trophic position data:

# reading excel data and cleaning up names
df <- read_xlsx("00_raw/00e_trophic_position.xlsx") %>% 
  clean_names()

# cleaning up trophic position column
df2 <- df %>%
  separate(trophic_level, into = c("trophic_position", "extra"), sep = "\\+") %>%
  mutate(trophic_position = trimws(trophic_position, which = "both")) %>%
  mutate(trophic_position = gsub("[^0-9.]", "", trophic_position)) %>%   # Remove non-numeric characters, allowing for decimals
  mutate(tro = as.numeric(trophic_position))   # Convert to numeric

# cleaning up standard error column
trophic_data <- df2 %>% 
  select(scientific_name, family, tro, extra) %>%
  mutate(extra = str_extract(extra, "\\d\\.\\d{1,2}")) %>% 
  rename(trophic_position = tro, tro_standard_error = extra) %>% 
  mutate(tro_standard_error = as.numeric(tro_standard_error))

str(trophic_data)

### Joining datasets

combined_data <- left_join(gravel_clean, trophic_data, by = "scientific_name") %>% 
  select(-family.y) %>% 
  rename(family = family.x)
