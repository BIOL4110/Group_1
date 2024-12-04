### data cleaning in class

library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

### New approach: cleaning directly from excel, then merge datasets

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
  rename(trophic_position = tro, standard_error = extra) %>% 
  mutate(standard_error = as.numeric(standard_error))
  
str(trophic_data)
