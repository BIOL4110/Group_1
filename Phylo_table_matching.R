# Load necessary packages
library(ape)  # For reading tree files
library(dplyr) # For data manipulation
library(readr) # for reading csv files

# Read files
combined_data <- read_csv("combined_data.csv")
View(combined_data)

phylo_tree <- read.tree("C:\\Users\\emmab\\Documents\\BIOL4110\\Group_1\\pruned_fish_tree_rmaxAS_ms.tre")

# Extract species names from both sources
csv_species <- combined_data$scientific_name
tree_species <- phylo_tree$tip.label

# Compare species lists
# Species in CSV but not in the tree
missing_in_tree <- setdiff(csv_species, tree_species)
# Species in the tree but not in the CSV
extra_in_tree <- setdiff(tree_species, csv_species)

# Output results
cat("Species in CSV but not in the tree:\n", missing_in_tree, "\n")
cat("Species in the tree but not in CSV:\n", extra_in_tree, "\n")

# Check if all species match
if (length(missing_in_tree) == 0 && length(extra_in_tree) == 0) {
  cat("All species in the CSV are found in the tree, and vice versa.\n")
} else {
  cat("There are mismatches between the CSV and the tree.\n")
}
