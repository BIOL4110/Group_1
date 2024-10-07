#load packages#
library(dplyr)
library(ggplot2)

#load datasets#
gravel_dataset <- read_csv("gravel_dataset.csv")
View(gravel_dataset)

#view data#
str(gravel_dataset)
column_names <- names(gravel_dataset)
print(column_names)

#clean datasets#
gravel_clean <- gravel_dataset %>%
  select(-ScientificNamePopID, -SerenaOrSarah, TeleostOrElasmo, -MROriginLocation, -SampleSize, -RawMetabolicRateCitation, -Temperature, -TemperatureUnits, -FactorialAerobicScope, -binTemp, )
print(gravel_clean)

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
  select(-RawMetabolicRate, -RawMetabolicRateUnits)

gravel_clean <- gravel_clean %>%
  rename(MRRawMgPerKgPerH = ConvertedValues) %>%   # Rename the column
  select(ScientificName, Family, CommonName, TeleostOrElasmo, 
         MRRawMgPerKgPerH, MRRawDataOrMean, BodyMassInGrams, MassRawDataOrMean)  # Rearrange the columns

# View the updated dataset
print(gravel_clean)

#scatterplot#
ggplot(gravel_clean, aes(x = BodyMassInGrams, y = MRRawMgPerKgPerH)) +
  geom_point() +  # Add points to the plot
  labs(title = "Body Mass vs. Raw Metabolic Rate",
       x = "Body Mass (grams)",
       y = "MR Raw (mg O2 kg^-1 h^-1)") +
  theme_minimal()  # Use a minimal theme for better aesthetics
