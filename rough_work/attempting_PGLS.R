#load libraries
library(phylolm)
library(ape)
library(phytools)
library(readr)

#load datasets
transDF_taxa_removed <- read_csv("processed_data/transDF_taxa_removed.csv")
pruned_tree<- read.tree("processed_data/pruned_fish_tree.tre")

str(transDF_taxa_removed)

# Fit the PGLS model
mod1p <- phylolm(std_whole_organism_mr_watts_fourth ~ std_mass_fourth_root + std_trophic_position,
                 phy = pruned_tree,
                 data = transDF_taxa_removed,
                 model = "lambda")

# Summarize the model output
summary(mod1p)

# Check the first few species names in your dataset
transDF_taxa_removed$scientific_name

# Check the tip labels in the phylogenetic tree
head(pruned_tree$tip.label)

# Check which species in the dataset are missing from the tree
missing_species <- setdiff(transDF_taxa_removed$scientific_name, pruned_tree$tip.label)

# Print the missing species names
missing_species

# Check for missing data in the relevant columns
summary(transDF_taxa_removed$std_whole_organism_mr_watts_fourth)
summary(transDF_taxa_removed$std_mass_fourth_root)
summary(transDF_taxa_removed$std_trophic_position)

# Check for missing data in the key variables
any(is.na(transDF_taxa_removed$std_whole_organism_mr_watts_fourth))
any(is.na(transDF_taxa_removed$std_mass_fourth_root))
any(is.na(transDF_taxa_removed$std_trophic_position))

# Double-check the column names in the dataset
names(transDF_taxa_removed)



# Fit a simpler model with just one predictor to test
mod_simple <- phylolm(std_whole_organism_mr_watts_fourth ~ std_mass_fourth_root,
                      phy = pruned_tree,
                      data = transDF_taxa_removed,
                      model = "lambda")

# Check the summary of the simpler model
summary(mod_simple)


# Reorder the dataset to match the order of species in the tree
transDF_taxa_removed <- transDF_taxa_removed[match(pruned_tree$tip.label, transDF_taxa_removed$scientific_name), ]

# Fit a simpler model with just one predictor to test
mod_simple <- phylolm(std_whole_organism_mr_watts_fourth ~ std_mass_fourth_root,
                      phy = pruned_tree,
                      data = transDF_taxa_removed,
                      model = "lambda")

# Check the summary of the simpler model
summary(mod_simple)

# Check for missing data in the key variables
sum(is.na(transDF_taxa_removed$std_whole_organism_mr_watts_fourth))
sum(is.na(transDF_taxa_removed$std_mass_fourth_root))


# Remove leading/trailing spaces from the tree's tip labels and dataset names
pruned_tree$tip.label <- trimws(pruned_tree$tip.label)
transDF_taxa_removed$scientific_name <- trimws(transDF_taxa_removed$scientific_name)

# Fit the PGLS model again
mod_simple <- phylolm(std_whole_organism_mr_watts_fourth ~ std_mass_fourth_root,
                      phy = pruned_tree,
                      data = transDF_taxa_removed,
                      model = "lambda")

# Check the model summary
summary(mod_simple)

# Identify species in the dataset that are missing from the tree
missing_in_tree <- setdiff(dataset_species, tree_species)

# Identify species in the tree that are missing from the dataset
missing_in_dataset <- setdiff(tree_species, dataset_species)

# Print out missing species
missing_in_tree
missing_in_dataset


