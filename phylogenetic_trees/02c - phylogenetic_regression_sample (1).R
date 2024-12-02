install.packages("phytools")
install.packages("ape")
install.packages("caper")

library(phytools)
library(ape)
library(caper)

# Create a data frame with species, metabolic rate, and trophic position #
trait_data <- data.frame(
  species = c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E",
              "Species_F", "Species_G", "Species_H", "Species_I", "Species_J"),
  metabolic_rate = c(5.1, 4.7, 6.2, 5.5, 4.9, 6.0, 5.2, 4.8, 5.7, 6.1),
  trophic_position = c(3.2, 2.9, 3.7, 3.4, 3.1, 3.6, 3.3, 3.0, 3.5, 3.8)
)

# Save the data frame as a CSV file in your working directory #
write.csv(trait_data, "bee_traits.csv", row.names = FALSE)

# View the data frame to ensure it's correct
print(trait_data)

# Simulate a phylogenetic tree with 10 species that match the dataset #
species_names <- trait_data$species
tree <- rtree(length(species_names), tip.label = species_names)

# Visualize the phylogenetic tree #
plot(tree)

# Continue with the phylogenetic regression process #
# Use the same code as before to perform PGLS with this trait data and tree #
comparative_data <- comparative.data(tree, trait_data, species, vcv=TRUE)
model <- pgls(trophic_position ~ metabolic_rate, comparative_data)

# View the summary of the regression model #
summary(model)
