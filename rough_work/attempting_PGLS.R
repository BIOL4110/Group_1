#load libraries
library(phylolm)
library(ape)
library(phytools)
library(readr)
library(tidyverse)
library(dplyr)
library(MuMIn)

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
# write.csv(transDF_ordered, "transDF_ordered.csv", row.names = FALSE)

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

# Plotting model comparison
model_comparison_df <- as.data.frame(model_comparison)

ggplot(model_comparison_df, aes(x = delta, y = weight)) +
  geom_point(size = 4, aes(color = factor(delta < 2))) +  # Highlights models within delta AIC < 2
  scale_color_manual(values = c("black", "red")) +         # Color best models
  labs(title = "Model Comparison (AICc vs. Weight)",
       x = "Delta AIC", 
       y = "Model Weight",
       color = "Best Models") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        legend.position = "top")


#phylogenetic signal
trait_m <- as.matrix((transDF_2)[,c("avg_mr")])
phylosig(pruned_tree, trait_m, method = "lambda")

library(phytools)

# Ensure that the species names match between tree and trait data
trait_data <- trait_data[trait_data$scientific_name %in% tree$tip.label, ]

transDF_avg <- transDF_taxa_removed %>%
  group_by(scientific_name) %>%
  summarise(
    scientific_name = first(scientific_name),  # Retains the scientific_name
    avg_mr = mean(std_whole_organism_mr_watts_fourth, na.rm = TRUE),
    avg_trophic_position = mean(std_trophic_position, na.rm = TRUE),
    avg_mass = mean(std_mass_fourth_root, na.rm = TRUE)
  )

# Select the numeric trait (e.g., avg_mr) for visualization
x <- transDF_ordered$avg_mr 

# Visualize the trait distribution across the tree
trait_mapped_tree <- plotBranchbyTrait(pruned_tree, 
                  x,  # Pass the correctly defined 'x' (avg_mr)
                  mode = c("edges", "tips", "nodes"),  
                  palette = "rainbow",                 
                  legend = TRUE,                      
                  xlims = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)),  
                  main = "Trait Distribution Across Tree", 
                  xlab = "Trait Value",           
                  ylab = "Tree Branch",      
                  branch.length = TRUE,                
                  cex.legend = 0.8) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, "cm"))# Adjust legend size

dev.copy(png,'graphs/trait_mapped_tree')
dev.off()

rm(trait_mapped_tree)