#load libraries
library(ape)  # For reading tree files
library(dplyr) # For data manipulation
library(readr) # for reading csv files

# Read csv file


# Load supertree
tree_all <- read.tree("fullTeleostChrono_wChondroSubset.tre")

# Update some sp names in supertree:found from Gravel et al.
treeAll$tip.label[treeAll$tip.label== "Dasyatis_lata"] <- "Bathytoshia_lata"
treeAll$tip.label[treeAll$tip.label== "Dasyatis_sabina"] <- "Hypanus_sabinus"
treeAll$tip.label[treeAll$tip.label== "Dasyatis_americana"] <- "Hypanus_americanus"
treeAll$tip.label[treeAll$tip.label== "Myliobatis_californicus"] <- "Myliobatis_californica"
treeAll$tip.label[treeAll$tip.label== "Raja_eglanteria"] <- "Rostroraja_eglanteria"
treeAll$tip.label[treeAll$tip.label=="Theragra_chalcogramma"] <- "Gadus_chalcogrammus"
treeAll$tip.label[treeAll$tip.label=="Chrysophrys_major"] <- "Pagrus_major"
treeAll$tip.label[treeAll$tip.label=="Chrysophrys_auratus"] <- "Pagrus_auratus"
treeAll$tip.label[treeAll$tip.label=="Clupea_pallasii_pallasii"] <- "Clupea_pallasii"

# Visualize the phylogenetic tree #
plot(tree_all)

# Extract species names from both sources
csv_species <- _________ $scientific_name
tree_species_tree_all <- tree_all$tip.label

# Compare species lists AS to combined
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
