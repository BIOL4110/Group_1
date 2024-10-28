# Load necessary packages
library(ape)  # For reading tree files
library(dplyr) # For data manipulation
library(readr) # for reading csv files

# Read csv file
combined_data <- read_csv("combined_data.csv")
View(combined_data)

# Read rmax AS tree
pruned_fish_tree_rmaxAS_ms <- read.tree("pruned_fish_tree_rmaxAS_ms.tre")
# Read rmax MMR tree
pruned_fish_tree_rmaxMMR_ms <- read.tree("pruned_fish_tree_rmaxMMR_ms.tre")
# Read rmax RMR tree
pruned_fish_tree_rmaxRMR_ms <- read.tree("pruned_fish_tree_rmaxRMR_ms.tre")

# Visualize the phylogenetic trees #
plot(pruned_fish_tree_rmaxMMR_ms)
plot(pruned_fish_tree_rmaxRMR_ms)
plot(pruned_fish_tree_rmaxAS_ms)

# Extract species names from both sources
csv_species <- combined_data$scientific_name
tree_species_rmaxAS_ms <- pruned_fish_tree_rmaxAS_ms$tip.label

# Compare species lists AS to combined
# Species in CSV but not in the tree
missing_in_tree_rmaxAS_ms <- setdiff(csv_species, tree_species_rmaxAS_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxAS_ms <- setdiff(tree_species_rmaxAS_ms, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxAS_ms, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxAS_ms, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxAS_ms) == 0 && length(extra_in_tree_rmaxAS_ms) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}

# Extract species names from tree MMR
tree_species_rmaxMMR_ms <- pruned_fish_tree_rmaxMMR_ms$tip.label

# Compare species lists MMR to combined
# Species in CSV but not in the tree
missing_in_tree_rmaxMMR_ms <- setdiff(csv_species, tree_species_rmaxMMR_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxMMR_ms <- setdiff(tree_species_rmaxMMR_ms, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxMMR_ms, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxMMR_ms, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxMMR_ms) == 0 && length(extra_in_tree_rmaxMMR_ms) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}

# Extract species names from tree RMR
tree_species_rmaxRMR_ms <- pruned_fish_tree_rmaxRMR_ms$tip.label

# Compare species lists RMR to combined
# Species in CSV but not in the tree
missing_in_tree_rmaxRMR_ms <- setdiff(csv_species, tree_species_rmaxRMR_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxRMR_ms <- setdiff(tree_species_rmaxRMR_ms, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxRMR_ms, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxRMR_ms, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxRMR_ms) == 0 && length(extra_in_tree_rmaxRMR_ms) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}











# Read cleaned csv file
combined_clean_data <- read_csv("combined_clean_data.csv")
View(combined_clean_data)

# Extract species names from both sources
csv_clean_species <- combined_clean_data$scientific_name

# Compare species lists AS to cleaned
# Species in CSV but not in the tree
missing_in_tree_rmaxAS_ms_cleaned <- setdiff(csv_clean_species, tree_species_rmaxAS_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxAS_ms_cleaned <- setdiff(tree_species_rmaxAS_ms, csv_clean_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxAS_ms_cleaned, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxAS_ms_cleaned, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxAS_ms_cleaned) == 0 && length(extra_in_tree_rmaxAS_ms_cleaned) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}

# Compare species lists MMR to cleaned
# Species in CSV but not in the tree
missing_in_tree_rmaxMMR_ms_cleaned <- setdiff(csv_clean_species, tree_species_rmaxMMR_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxMMR_ms_cleaned <- setdiff(tree_species_rmaxMMR_ms, csv_clean_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxMMR_ms_cleaned, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxMMR_ms_cleaned, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxMMR_ms_cleaned) == 0 && length(extra_in_tree_rmaxMMR_ms_cleaned) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}

# Compare species lists RMR to cleaned
# Species in CSV but not in the tree
missing_in_tree_rmaxRMR_ms_cleaned <- setdiff(csv_clean_species, tree_species_rmaxRMR_ms)
# Species in the tree but not in the CSV
extra_in_tree_rmaxRMR_ms_cleaned <- setdiff(tree_species_rmaxRMR_ms, csv_clean_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree_rmaxRMR_ms_cleaned, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree_rmaxRMR_ms_cleaned, "\n")

# Check if all species match
if (length(missing_in_tree_rmaxRMR_ms_cleaned) == 0 && length(extra_in_tree_rmaxRMR_ms_cleaned) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}





# Combine all tree species into one vector and remove duplicates
all_tree_species <- unique(c(tree_species_rmaxRMR_ms, tree_species_rmaxMMR_ms, tree_species_rmaxAS_ms))
print(all_tree_species)

# Check if each species in the CSV is present in at least one of the trees
matches <- csv_clean_species[csv_clean_species %in% all_tree_species]

# Find species in the CSV that are NOT in any of the trees
mismatches <- setdiff(csv_clean_species, all_tree_species)

# Print results
cat("Species found in at least one of the phylogenetic trees:\n")
print(matches)
cat("\nSpecies not found in any of the phylogenetic trees:\n")
print(mismatches)





csv_clean_unique_species <- unique(csv_clean_species)
matches_unique <- csv_clean_unique_species[csv_clean_unique_species %in% all_tree_species]
mismatches_unique <- setdiff(csv_clean_unique_species, all_tree_species)
cat("Species found in at least one of the phylogenetic trees:\n")
print(matches_unique)
cat("\nSpecies not found in any of the phylogenetic trees:\n")
print(mismatches_unique)
