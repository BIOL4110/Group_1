#load libraries
library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(ape)
library(ggplot2)
library(phylolm)
library(phytools)
library(tidyverse)
library(MuMIn)
library(viridis)



-------------------------------------------------------------------------------------
### CLEANING ###


FULL_Rmax_LHT_MR_ms <- read_csv("00_raw/00b_FULL_Rmax_LHT_MR_ms.csv")

# find column names
str(FULL_Rmax_LHT_MR_ms)
column_names <- names(FULL_Rmax_LHT_MR_ms)

# clean datasets
main_dataset1 <- FULL_Rmax_LHT_MR_ms %>%
  select(-TeleostOrElasmo, -MROriginLocation, -MRCitation, -DFtype )

# Convert column names to snake case
colnames(main_dataset1) <- make_clean_names(names(main_dataset1), case = "snake")

# removing upper case letters and adding "_"
main_dataset2 <- main_dataset1 %>%
  clean_names() %>%                # Clean column names
  mutate(across(everything(), ~ str_replace_all(., " ", "_"))) %>%  # Replace spaces with underscores
  mutate(across(everything(), tolower)) %>%  # Convert all to lowercase
  mutate(scientific_name = str_to_title(scientific_name))  # Convert scientific_name to title case

### Fishbase trophic position data:

trophic_position_clean <- read_csv("01_processed_datasets/01c_trophic_position_clean.csv")

## adding underscores to match main_dataset2
trophic_position_clean <- trophic_position_clean %>%
  mutate(scientific_name = gsub(" ", "_", scientific_name))


main_dataset3 <- left_join(main_dataset2, trophic_position_clean, by = "scientific_name")
str(main_dataset3)

main_dataset3$mr_mass_grams <- as.numeric(as.character(main_dataset3$mr_mass_grams))



-----------------------------------------------------------------------------------------
### TRANSFORMATIONS ###


#remove na values
main_dataset3 <- na.omit(main_dataset3)

# Convert MR to numeric
main_dataset3$whole_organism_mr_watts <- as.numeric(as.character(main_dataset3$whole_organism_mr_watts))

# Shapiro-Wilk test
shapiro.test(main_dataset3$whole_organism_mr_watts)

# Log-transform the data
main_dataset3$log_whole_organism_mr_watts <- log(main_dataset3$whole_organism_mr_watts + 1)

# Create a density plot (line plot)
ggplot(main_dataset3, aes(x = whole_organism_mr_watts)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of MR", x = "MR", y = "Frequency") +
  theme_minimal()

# Create the histogram for trophic position
ggplot(main_dataset3, aes(x = trophic_position)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Trophic Position", x = "Trophic Position", y = "Frequency") +
  theme_minimal()

# Create the histogram for body mass
ggplot(main_dataset3, aes(x = mr_mass_grams)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Body Mass", x = "Body Mass", y = "Frequency") +
  theme_minimal()

### transforming the variables
transDF <- main_dataset3 %>%
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
outliers_std_whole_mr <- transDF$std_whole_organism_mr_watts_fourth
Q1 <- quantile(outliers_std_whole_mr, 0.25)
Q3 <- quantile(outliers_std_whole_mr, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers_std_whole_mr



-------------------------------------------------------------------------------------------
### OLS MODELLING ###


# Linear regression with trophic as the explanatory variable
ols_trophic_position <- lm(std_whole_organism_mr_watts_fourth ~ std_trophic_position, data = transDF)
summary(ols_trophic_position)

# scatter plot for linear model trophic position
ggplot(transDF, aes(x = std_trophic_position, y = std_whole_organism_mr_watts_fourth)) +
  geom_point() +  # Scatter plot of the data points
  geom_smooth(method = "lm", col = "red") +  # Add the regression line
  labs(title = "Linear Regression: Whole Organism MR vs. Trophic Position",
       x = "Trophic Position",
       y = "Whole Organism MR (Fourth Root)") +
  theme_minimal()


# Linear regression with body mass as the explanatory variable
ols_body_mass <- lm(std_whole_organism_mr_watts_fourth ~ std_mass_fourth_root, data = transDF)
summary(ols_body_mass)

# scatter plot for linear model with body mass
ggplot(transDF, aes(x = std_mass_fourth_root, y = std_whole_organism_mr_watts_fourth)) +
  geom_point() +  # Scatter plot of the data points
  geom_smooth(method = "lm", col = "red") +  # Add the regression line
  labs(title = "Linear Regression: Whole Organism MR vs. Mass (Fourth Root)",
       x = "Mass (Fourth Root)",
       y = "Whole Organism MR (Fourth Root)") +
  theme_minimal()

# Multiple linear regression with trophic position and body mass as explanatory variables
ols_trophic_and_mass <- lm(std_whole_organism_mr_watts_fourth ~ std_trophic_position + std_mass_fourth_root, data = transDF)
summary(ols_trophic_and_mass)

# plotting residuals from linear model
## Counting Residuals
residuals_trophic_mass <- resid(ols_trophic_and_mass)
length(residuals_trophic_mass)

sum(residuals_trophic_mass > 0)
sum(residuals_trophic_mass < 0)

# Add residuals to the data
transDF$residuals <- residuals(ols_trophic_and_mass)

## Plot the residuals to check for patterns

# Add residuals to the data
transDF$residuals <- residuals(ols_trophic_and_mass)

# Plot the residuals to check for patterns for trophic position
ggplot(transDF, aes(x = std_trophic_position, y = residuals)) +
  geom_point() +
  labs(title = "Residuals of the Linear Model: Trophic Position vs Residuals",
       x = "Trophic Position",
       y = "Residuals") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()

# Plot the residuals to check for patterns for mass
ggplot(transDF, aes(x = std_mass_fourth_root, y = residuals)) +
  geom_point() +
  labs(title = "Residuals of the Linear Model: Mass vs Residuals",
       x = "Mass (Fourth Root)",
       y = "Residuals") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()

# Diagnostic plot (fitted values vs residuals)
ggplot(transDF, aes(x = ols_trophic_and_mass$fitted.values, y = residuals)) +
  geom_point() +
  labs(title = "Fitted Values vs Residuals",
       x = "Fitted Values",
       y = "Residuals") +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal()



------------------------------------------------------------------------------------------------
### PHYLOGENETIC TREE ###


# Load supertree (full teleost chronogram and the chondro subset)
tree_all <- read.tree("00_raw_phylogenetic_data/00i_fullTeleostChrono_wChondroSubset.tre")

# Update some sp names in supertree:
tree_all$tip.label[tree_all$tip.label== "Dasyatis_lata"] <- "Bathytoshia_lata"
tree_all$tip.label[tree_all$tip.label== "Dasyatis_sabina"] <- "Hypanus_sabinus"
tree_all$tip.label[tree_all$tip.label== "Dasyatis_americana"] <- "Hypanus_americanus"
tree_all$tip.label[tree_all$tip.label== "Myliobatis_californicus"] <- "Myliobatis_californica"
tree_all$tip.label[tree_all$tip.label== "Raja_eglanteria"] <- "Rostroraja_eglanteria"
tree_all$tip.label[tree_all$tip.label=="Theragra_chalcogramma"] <- "Gadus_chalcogrammus"
tree_all$tip.label[tree_all$tip.label=="Chrysophrys_major"] <- "Pagrus_major"
tree_all$tip.label[tree_all$tip.label=="Chrysophrys_auratus"] <- "Pagrus_auratus"
tree_all$tip.label[tree_all$tip.label=="Clupea_pallasii_pallasii"] <- "Clupea_pallasii"


# Extract species names from both sources
csv_species <- main_dataset3$scientific_name
tree_species_tree_all <- tree_all$tip.label

# Compare species is csv to tree
# Species in CSV but not in the tree
missing_in_tree_all <- setdiff(csv_species, tree_species_tree_all)
# Species in the tree but not in the CSV
extra_in_tree_all <- setdiff(tree_species_tree_all, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_all, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_all, "\n")

# Check if all species match
if (length(missing_in_tree_all) == 0 && length(extra_in_tree_all) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}

#Prune the tree to match only the tips in csv_species
pruned_tree <- drop.tip(tree_all, setdiff(tree_all$tip.label, csv_species))

#Visualize the pruned tree
plot(pruned_tree, main = "Pruned Phylogenetic Tree")

#extract species names from pruned tree
tree_species_pruned <- pruned_tree$tip.label

# Compare species lists to pruned_tree
# Species in CSV but not in the tree
missing_in_tree_pruned <- setdiff(csv_species, tree_species_pruned)
# Species in the tree but not in the CSV
extra_in_tree_pruned <- setdiff(tree_species_pruned, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_pruned, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_pruned, "\n")
# Check if all species match
if (length(missing_in_tree_pruned) == 0 && length(extra_in_tree_pruned) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}



-------------------------------------------------------------------------------------
### PGLS MODELLING ###


#get mean of values
transDF_avg <- transDF %>%
  group_by(scientific_name) %>%
  summarise(
    avg_mr = mean(std_whole_organism_mr_watts_fourth, na.rm = TRUE),
    avg_trophic_position = mean(std_trophic_position, na.rm = TRUE),
    avg_mass = mean(std_mass_fourth_root, na.rm = TRUE)
  )

# Extract the tip labels from the tree
tree_tips <- pruned_tree$tip.label

# Reorder the data frame based on the order of the tree tips
transDF_ordered <- transDF_avg %>%
  filter(scientific_name %in% tree_tips) %>%  # Filter out any species not in the tree (optional)
  arrange(match(scientific_name, tree_tips))  # Reorder rows based on the tree tip labels

pruned_tree$tip.label == transDF_ordered$scientific_name


# Check the root edge length of your phylogenetic tree
pruned_tree$root.edge
# fix root edge
pruned_tree$root.edge <- 0

class(transDF_ordered)
transDF_2 <- transDF_ordered %>% 
  column_to_rownames(var = "scientific_name")

# Fit the PGLS model
mod1p <- phylolm(avg_mr ~ avg_trophic_position + avg_mass,
                 phy = pruned_tree,
                 data = transDF_2,
                 model = "lambda")

mod2p <- phylolm(avg_mr ~ avg_trophic_position,
                 phy = pruned_tree,
                 data = transDF_2,
                 model = "lambda")

mod3p <- phylolm(avg_mr ~ avg_mass,
                 phy = pruned_tree,
                 data = transDF_2,
                 model = "lambda")

# Summarize the model output
summary(mod1p)

# Model comparison using MuMIn: 

(model_comparison <- dredge(mod1p))
# Extract the models from the dredge output
# Get models with deltaAIC < 2
best_models <- get.models(model_comparison, subset = delta < 2)

# View the best models
best_models

# Remove intercept-only model
model_comparison_filtered <- model_comparison[!grepl("^1$", rownames(model_comparison)), ]

# Plotting the 3 model comparison
model_comparison_df <- as.data.frame(model_comparison_filtered)

# Add labels for the models
model_comparison_df$label <- c("BM-only", "Combined", "TP-only")

ggplot(model_comparison_df, aes(x = delta, y = weight)) +
  geom_point(size = 4, aes(color = factor(delta < 2))) + # Highlights models within delta AIC < 2
  geom_text(aes(label = label), vjust = -0.8, size = 3) +
  scale_color_manual(values = c("black", "red")) +         # Color best models
  labs(title = "Model Comparison (AICc vs. Weight)",
       x = "Delta AIC", 
       y = "Model Weight",
       color = "Best Models") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        legend.position = "top")



--------------------------------------------------------------------------------------------

### TRAIT MAPPING ###


# Compute Phylogenetic Signal
trait_m <- setNames(as.matrix(transDF_2$avg_mr), transDF_2$scientific_name)
trait_tp <- setNames(as.matrix(transDF_2$avg_trophic_position), transDF_2$scientific_name)
trait_mass <- setNames(as.matrix(transDF_2$avg_mass), transDF_2$scientific_name)

phylosig(pruned_tree, trait_m, method = "lambda")
phylosig(pruned_tree, trait_tp, method = "lambda")
phylosig(pruned_tree, trait_mass, method = "lambda")


# Aggregate Data
transDF_avg <- transDF %>%
  group_by(scientific_name) %>%
  summarise(
    avg_mr = mean(std_whole_organism_mr_watts_fourth, na.rm = TRUE),
    avg_trophic_position = mean(std_trophic_position, na.rm = TRUE),
    avg_mass = mean(std_mass_fourth_root, na.rm = TRUE)
  )

# Align Data with Tree
transDF_ordered <- transDF_avg %>%
  filter(scientific_name %in% pruned_tree$tip.label) %>%
  arrange(match(scientific_name, pruned_tree$tip.label))

# plot Average Metabolic Rate
x_mr <- setNames(transDF_ordered$avg_mr, transDF_ordered$scientific_name)
x_mr <- na.omit(x_mr)  # Remove NA values
trait_mapped_tree_mr <- plotBranchbyTrait(
  pruned_tree, 
  x_mr, 
  mode = "tips", 
  cols = viridis(100),  
  legend = TRUE,                      
  xlims = range(x_mr, na.rm = TRUE)
)

# Add title
title(main = "Average Metabolic Rate Across Phylogeny")

# plot Average Trophic Position
x_tp <- setNames(transDF_ordered$avg_trophic_position, transDF_ordered$scientific_name)
x_tp <- na.omit(x_tp)  # Remove NA values
trait_mapped_tree_tp <- plotBranchbyTrait(
  pruned_tree, 
  x_tp, 
  mode = "tips", 
  cols = viridis::viridis(100),  
  legend = TRUE,                      
  xlims = c(min(x_tp, na.rm = TRUE), max(x_tp, na.rm = TRUE)),  
  main = "Average Trophic Position Across Phylogeny", 
  xlab = "Average Trophic Position",           
  ylab = "Tree Branch",      
  branch.length = TRUE,                
  cex.legend = 0.8
)

# Add title
title(main = "Average Trophic Position Across Phylogeny")

# plot Average Body Mass
x_mass <- setNames(transDF_ordered$avg_mass, transDF_ordered$scientific_name)
x_mass <- na.omit(x_mass)  # Remove NA values
trait_mapped_tree_mass <- plotBranchbyTrait(
  pruned_tree, 
  x_mass, 
  mode = "tips", 
  cols = viridis::viridis(100),  
  legend = TRUE,                      
  xlims = c(min(x_mass, na.rm = TRUE), max(x_mass, na.rm = TRUE)), 
  main = "Average Body Mass Across Phylogeny", 
  xlab = "Body Mass",           
  ylab = "Tree Branch",      
  branch.length = TRUE,                
  cex.legend = 0.8
)

# Add title
title(main = "Average Body Mass Across Phylogeny")




  
  








  
  
  
  
  
  
  
  
