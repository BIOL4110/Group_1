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
ggplot(main_dataset4, aes(x = whole_organism_mr_watts)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of MR", x = "MR", y = "Frequency") +
  theme_minimal()

# Create the histogram for trophic position
ggplot(main_dataset4, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(main_dataset4, aes(x = mr_mass_grams)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()



### PGLS Modelling: 

library(piecewiseSEM)
library(tidyverse)
library(rotl)
library(ape)
library(stargazer)
library(janitor)
library(MuMIn)
library(broom)
library(forcats)
library(tidyverse)
library(xtable)
library(arm)
library(nlme)
library(cowplot)
library(visreg)

library(stargazer)
library(rotl)
library(rfishbase)
library(readxl)
library(rr2)

# model selection RMR ----------------------------------------------------


kb <- 8.617333262*10^-5 ## boltzman's constant

### transforming the variables 
transDF <- main_dataset4 %>% 
  mutate(whole_organism_mr_watts_fourth_root = whole_organism_mr_watts^(1/4), # power transformation
         mass_fourth_root = mr_mass_grams^(1/4)) %>%
  clean_names()

# Plot histogram of the transformed metabolic rate
ggplot(transDF, aes(x = whole_organism_mr_watts_fourth_root)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Cube Root Transformed whole_organism_mr_watts", x = "Cube Root of Whole Organism MR Watts", y = "Frequency")

# Plot histogram of the transformed body mass
ggplot(transDF, aes(x = mass_fourth_root)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Transformed Reciprocal Mass", y = "Frequency")

transDF$std_whole_organism_mr_watts_fourth <- as.numeric(scale(transDF$whole_organism_mr_watts_fourth_root))
transDF$std_mass_fourth_root <- as.numeric(scale(transDF$mass_fourth_root))
transDF$std_trophic_position <- as.numeric(scale(transDF$trophic_position))

# Identify outliers in "std_mass_fourth_root" column
outliers_std_mass <- transDF$std_mass_fourth_root
Q1 <- quantile(outliers_std_mass, 0.25)
Q3 <- quantile(outliers_std_mass, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers_std_mass

# Identify outliers in "std_whole_mr_watts_fourth"
outliers_std_whole_mr <- transDF$std_whole_mr_watts_fourth
Q1 <- quantile(outliers_std_whole_mr, 0.25)
Q3 <- quantile(outliers_std_whole_mr, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers_std_whole_mr

# Identify outliers in "std_trophic_position"

outliers_std_trophic_position <- transDF$std_trophic_position
Q1 <- quantile(outliers_std_trophic_position, 0.25)
Q3 <- quantile(outliers_std_trophic_position, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers_std_trophic_position

# resting metabolic rate --------------------------------------------------



rmr <- transDF %>% 
  filter(m_rtype == "RMR") %>% 
  as.data.frame() %>% 
  mutate(std_log_mr_mass = as.character(std_log_mr_mass)) %>% 
  mutate(std_log_mr_mass = as.numeric(std_log_mr_mass)) %>% 
  mutate(std_log_amat = as.character(std_log_amat)) %>% 
  mutate(std_log_amat = as.numeric(std_log_amat)) %>% 
  mutate(std_log_amax = as.character(std_log_amax)) %>% 
  mutate(std_log_amax = as.numeric(std_log_amax))



# PGLS for MR data  ------------------------------------------------------------

df_full <- rmr
df_full$species1 <- str_to_lower(df_full$species1)

df2 <- df_full %>% 
  filter(species1 != "urobatis halleri")

length(unique(df2$species1)) ## 81 species

df_taxa <- tnrs_match_names(unique(df2$species1), context="Animals", names = unique(df2$species1), do_approximate_matching = TRUE) 
## note rostroraja eglanteria are not matched 


## Returns the induced subtree on the synthetic tree that relates a list of nodes.
tr_fishes <- tol_induced_subtree(ott_ids = ott_id(df_taxa), label_format="name") 

### computes branch lengths of a tree
tr_bl_fishes <- compute.brlen(tr_fishes)

phylo <- tr_bl_fishes

fishes2 <- df_full %>% 
  left_join(., df_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(scientific_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_fishes$tip.label)) ### trim the MR dataset to the species with tree tips


data <- fishes2
data$sp_name <- data$unique_name2
# check overlap between tree and species in data
data$Phylospecies <- "not in tree" 
for(i in 1:nrow(data)){
  species <- data$sp_name[i]
  if(rlang::is_empty(phylo$tip.label[grepl(species,phylo$tip.label)])){ # if sp is not in tree leave "not in tree"
  } else {data$Phylospecies[i]<-phylo$tip.label[grepl(species,phylo$tip.label)]} # else put the sp name from the tree
}

length(unique(data$Phylospecies[which(!data$Phylospecies=="not in tree")])) # 77 species in tree


#prune tree to match overlapping taxa
nameslist <- phylo$tip.label
treenameslist <- as.data.frame(table(data$Phylospecies))
Speciestoretain <- intersect(treenameslist$Var1, nameslist)
pruned.tree <- drop.tip(phylo,phylo$tip.label[-match(Speciestoretain,phylo$tip.label)])
fishes_tree <- pruned.tree
plot(fishes_tree)
is.ultrametric(fishes_tree) # has to be TRUE
is.rooted(fishes_tree) # TRUE

any(duplicated(fishes_tree$node.label)) # FALSE

fishes_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% fishes_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 



write.tree(fishes_tree, "data-processed/gravel-fishes.tre")

plot(fishes_tree)


pd <- Phylodata1 
pd <- pd[match(fishes_tree$tip.label, pd$species),]
row.names(pd) <- pd$species





library(phylolm)
library(ape)
library(phytools)
mod1p <- phylolm(log_mr ~ std_log_amat + std_log_mr_mass + inv_mr_temp + lifestyle, phy = fishes_tree, data = pd, model = "lambda")
summary(mod1p) 



trait_m <- as.matrix((pd)[,c("log_mr")])
phylosig(fishes_tree, trait_m, method = "lambda")


mod1p <- phylolm(log_mr ~ std_log_amat + std_log_mr_mass + lifestyle + inv_mr_temp, phy = fishes_tree, data = pd, model = "lambda")
summary(mod1p) #### tells us lamba ~ 0, i think


mod1 <- gls(log_mr ~ std_log_amat + std_log_mr_mass + inv_mr_temp + lifestyle, correlation = corPagel(1, fishes_tree, fixed = FALSE), data = pd, method = "ML")
summary(mod1)


mod1 <- gls(log_mr ~ std_log_amat + std_log_mr_mass, correlation = corPagel(1, fishes_tree, fixed = FALSE), data = pd, method = "ML")
summary(mod1)


mod2 <- gls(log_mr ~ std_log_amat + std_log_mr_mass + inv_mr_temp, correlation = corBrownian(form = ~ sp_name, value = 1, phy = fishes_tree), data = pd, method = "ML")
summary(mod2)


###  using the caper packaage
library(caper)
cdata <- comparative.data(fishes_tree, pd ,names.col = "sp_name") 
cdata

mod3 <- pgls(log_mr ~ std_log_amat + std_log_mr_mass, data = cdata,lambda= "ML") 
summary(mod3)



##### end model selection here. 

