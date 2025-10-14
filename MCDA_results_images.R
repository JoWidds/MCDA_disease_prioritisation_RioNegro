#### Code for plot based upon final attribute weights. Attributes that were categorised as less than or 
#equal to 2 per criteria had the highest hierarchical weights assigned to them.   

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tibble)
library(RColorBrewer)

Criteria <- data.frame(
  Criteria = c("Epidemiology", "Prevention_control", "Public_health", "Economic_impact", "Societal_impact"), 
  Rank = c(3,2,1,4,5), 
  Points = c(100, 100, 100, 80, 80)
)

# Define Attributes data
Attributes <- data.frame(
  Attributes = c(
    "Epi_incidence", "Epi_lethality", "Epi_spread", 
    "Prev_control_diagnostics", "Prev_control_treatments", "Prev_control_tools", "Prev_control_knowledge", 
    "Public_health_intervention",  "Public_health_epidemic", 
    "Economic_impact_control_costs", "Economic_impact_health_costs", "Economic_impact_tourism_costs", 
    "Societal_impact_QOL", "Societal_impact_public"
  ), 
  Rank = c(3,1,2, 4,2,1,3,1,2,1,3,2, 1,2), 
  Points = c(70, 100, 90, 50, 90, 100, 90, 100, 85, 100, 40, 100, 100, 80)
)

# Calculate Weights by sub-group
Attributes$Weight <- c(
  Attributes[1,3]/sum(Attributes[1:3,3]), 
  Attributes[2,3]/sum(Attributes[1:3,3]), 
  Attributes[3,3]/sum(Attributes[1:3,3]), 
  Attributes[4,3]/sum(Attributes[4:7,3]), 
  Attributes[5,3]/sum(Attributes[4:7,3]), 
  Attributes[6,3]/sum(Attributes[4:7,3]), 
  Attributes[7,3]/sum(Attributes[4:7,3]), 
  Attributes[8,3]/sum(Attributes[8:9,3]), 
  Attributes[9,3]/sum(Attributes[8:9,3]), 
  Attributes[10,3]/sum(Attributes[10:12,3]), 
  Attributes[11,3]/sum(Attributes[10:12,3]), 
  Attributes[12,3]/sum(Attributes[10:12,3]), 
  Attributes[13,3]/sum(Attributes[13:14,3]), 
  Attributes[14,3]/sum(Attributes[13:14,3])
)

# Assign groups manually
Attributes$Criteria <- c(
  rep("Epidemiology", 3),
  rep("Prevention_control", 4),
  rep("Public_health", 2),
  rep("Economic_impact", 3),
  rep("Societal_impact", 2)
)

# Custom labels for bars
Labels <- c(
  "Incidence", "Case fatality", "Spread/transmission", 
  "Diagnostics", "Treatments", "Tools for control", "Knowledge", 
  "Interventions", "Epidemic potential", 
  "Control Costs", "Healthcare Costs", "T & P losses", 
  "Quality of Life", "Public perception"
)
Attributes$Label <- Labels

# Add empty bars
empty_bar <- 1  # number of empty bars between groups
to_add <- data.frame(
  Attributes = rep(NA, empty_bar * length(unique(Attributes$Criteria))),
  Rank = NA,
  Points = 0,
  Weight = 0,
  Criteria = rep(unique(Attributes$Criteria), each = empty_bar),
  Label = rep("", empty_bar * length(unique(Attributes$Criteria)))
)

# Combine Attributes and empty bars
Attributes_full <- rbind(Attributes, to_add) %>%
  arrange(Criteria, Rank) %>%
  mutate(id = row_number())

# Calculate angles and hjust
total_bars <- nrow(Attributes_full)
Attributes_full <- Attributes_full %>%
  mutate(
    angle = 90 - 360 * (id - 0.5) / total_bars,
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

# Custom Criteria names for the legend
criteria_names <- c(
  Epidemiology = "Epidemiology",
  Prevention_control = "Prevention & Control",
  Public_health = "Public Health",
  Economic_impact = "Economic Impact",
  Societal_impact = "Societal Impact"
)

# Plot
p <- ggplot(Attributes_full, aes(x = factor(id), y = Weight, fill = Criteria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(start = 0) +
  geom_text(
    aes(x = factor(id), y = Weight + 0.01, label = Label, hjust = hjust, angle = angle),
    color = "black", size = 3, na.rm = TRUE
  ) +
  scale_fill_manual(
    name = "Criteria",
    values = RColorBrewer::brewer.pal(n = 5, name = "Set2"),
    labels = criteria_names
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")
  ) +
  guides(fill = guide_legend(title = "Criteria"))



svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/MCDA elicitation Argentina/Manuscript/Figure_Circular.svg",   
    width = 10,height = 10)
# Print plot
print(p)


dev.off()



