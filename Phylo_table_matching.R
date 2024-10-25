# Load necessary packages
library(ape)  # For reading tree files
library(dplyr) # For data manipulation
library(readr) # for reading csv files

# Read csv file
combined_data <- read_csv("combined_data.csv")
View(combined_data)

# Read rmaxAS tree
pruned_fish_tree_rmaxAS_ms <- read.tree("pruned_fish_tree_rmaxAS_ms.tre")

# Extract species names from both sources
csv_species <- combined_data$scientific_name
tree_species_rmaxAS_ms <- pruned_fish_tree_rmaxAS_ms$tip.label

# Compare species lists
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

# Read rmax MMR tree
pruned_fish_tree_rmaxMMR_ms <- read.tree("pruned_fish_tree_rmaxMMR_ms.tre")

# Extract species names from tree
tree_species_rmaxMMR_ms <- pruned_fish_tree_rmaxMMR_ms$tip.label

# Compare species lists
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

# Read rmax RMR tree
pruned_fish_tree_rmaxRMR_ms <- read.tree("pruned_fish_tree_rmaxRMR_ms.tre")

# Extract species names from tree
tree_species_rmaxRMR_ms <- pruned_fish_tree_rmaxRMR_ms$tip.label

# Compare species lists
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
