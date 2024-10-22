

### data cleaning in class


library(tidyverse)
library(janitor)


df <- read_csv("combined_data_incomplete.csv") %>% 
  clean_names()


str(df)


unique(df$trophic_level)



df2 <- df %>% 
  tidyr::separate(trophic_level, into = c("trophic_position", "extra"), sep = "\xa") %>%
  select(-extra)

decimals <- df %>% 
  mutate(contains_decimal = ifelse(grepl("\\.", trophic_level), "yes", "no")) %>%
  select(trophic_level, contains_decimal, everything()) %>% 
  filter(contains_decimal == "yes")


no_decimals <- df %>% 
  mutate(contains_decimal = ifelse(grepl("\\.", trophic_level), "yes", "no")) %>%
  select(trophic_level, contains_decimal, everything()) %>% 
  filter(contains_decimal == "no")


decimals2 <- decimals %>%
  separate(trophic_level, into = c("trophic_position", "extra"), sep = 3)


  

grepl("\\.", df$trophic_level)


str(df2)
unique(df2$trophic_position)


df3 <- df2 %>% 
  mutate(trophic_position = as.numeric(trophic_position))



library(readxl)

df3 <- read_xlsx("trophic_position.xlsx") %>% 
  clean_names()


df4 <- df3 %>% 
  separate(trophic_level, into = c("trophic_position", "extra"), sep = "\\+") %>%
  mutate(trophic_position = trimws(trophic_position, which = "both")) %>% 
  mutate(tro = as.numeric(trophic_position))



?trimws

str(df4)  
