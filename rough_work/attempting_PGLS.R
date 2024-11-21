#load libraries
library(phylolm)
library(ape)
library(phytools)
library(readr)
library(tidyverse)
library(dplyr)

#load datasets
transDF_taxa_removed <- read_csv("processed_data/transDF_taxa_removed.csv")
pruned_tree<- read.tree("processed_data/pruned_fish_tree.tre")

#get mean of values
transDF_avg <- transDF_taxa_removed %>%
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

# Save the reordered data frame to a new CSV
write.csv(transDF_ordered, "transDF_ordered.csv", row.names = FALSE)

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

# Summarize the model output
summary(mod1p)

unique(transDF_ordered$avg_trophic_position)

library(caper)

comparative_data <- comparative.data(pruned_tree, trait_data, species, vcv=TRUE)
model <- pgls(trophic_position ~ metabolic_rate, comparative_data)









