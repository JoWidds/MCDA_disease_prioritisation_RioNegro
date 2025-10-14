setwd ("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/MCDA elicitation Argentina/Manuscript")
rm(list=ls())

# BETO_values <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Post doc/03 - EUPAH&W/SOA12_Better tools for diagnosis of infectious diseases/BETO - MDCA Consolidated.responses.csv")
# rows_to_drop <- c(1, 5, 6, 10, 11, 15, 16, 20, 21, 25, 26, 30) ## exclude 0 and 100 as lower/upper bounds remain the same  
# BETO_values_clean <- BETO_values[-rows_to_drop, ]
# BETO_values_clean <- BETO_values_clean[,-1]
# 
# 
# BETO_values_clean[BETO_values_clean == 0] <- NA
# row_sd <- apply(BETO_values_clean, 1, function(row) sd(row, na.rm = TRUE))
# BETO_sd <- row_sd[7] 

#### Original analysis ####
## Lethality and Transmission is normalized 
Epi_data <- data.frame( 
  Dx = c("Rabies", "Hanta", "Echino", "Trich", "Lepto"),
  Incidence = c(0,25,93.75, 93.75, 100),
  Lethality = c(100,50.50505050, 1.01010101,0, 1.414141414),
  Transmission = c(0,100,80, 40, 40))

Criteria <- data.frame(
  Criteria = c("Epidemiology", "Prevention_control", "Public_health", "Economic_impact", "Societal_impact"), 
  Rank = c(3,2,1,4,5), 
  Points = c(100, 100, 100, 80, 80))

Weight_criteria <- Criteria$Points/sum(Criteria$Points)

Attributes <- data.frame(
  Attributes = c("Epi_incidence", "Epi_lethality", "Epi_spread", 
               "Prev_control_diagnostics", "Prev_control_treatments", "Prev_control_tools", "Prev_control_knowledge", 
               "Public_health_intervention",  "Public_health_epidemic", 
               "Economic_impact_control_costs", "Economic_impact_health_costs", "Economic_impact_tourism_costs", 
               "Societal_impact_QOL", "Societal_impact_public"), 
  Rank = c(3,1,2, 4,2,1,3,1,2,1,3,2, 1,2), 
  Points = c(70, 100, 90, 50, 90, 100, 90, 100, 85, 100, 40, 100, 100, 80))
  
  Weight_attributes <- c(Attributes[[1,3]]/sum(Attributes[c(1:3), 3]), 
                         Attributes[[2,3]]/sum(Attributes[c(1:3), 3]), 
                         Attributes[[3,3]]/sum(Attributes[c(1:3), 3]), 
                         Attributes[[4,3]]/sum(Attributes[c(4:7), 3]), 
                         Attributes[[5,3]]/sum(Attributes[c(4:7), 3]), 
                         Attributes[[6,3]]/sum(Attributes[c(4:7), 3]), 
                         Attributes[[7,3]]/sum(Attributes[c(4:7), 3]), 
                         Attributes[[8,3]]/sum(Attributes[c(8:9), 3]), 
                         Attributes[[9,3]]/sum(Attributes[c(8:9), 3]), 
                         Attributes[[10,3]]/sum(Attributes[c(10:12), 3]), 
                         Attributes[[11,3]]/sum(Attributes[c(10:12), 3]), 
                         Attributes[[12,3]]/sum(Attributes[c(10:12), 3]), 
                         Attributes[[13,3]]/sum(Attributes[c(13:14), 3]), 
                         Attributes[[14,3]]/sum(Attributes[c(13:14), 3])) 
  
  Weight_subattributes <- c(0,50,85,100, ##Criteria prevention/control 1-4
                            0,10,85,100,
                            0,14,60,15,100,
                            0,15,100,
                            0,20,100, ## criteria Public health 17-19
                            0,50,20,100,
                            0,20,100, ## Criteria economic impact 24-26
                            0,15,100,
                            0,85,100,
                            0,80,100, ##Criteria social 33- 35
                            0,25,100)
                         
#Criteria Epi  
Value_incidence <- (Weight_criteria[1]*Weight_attributes[1])*Epi_data$Incidence 
Value_lethality <- (Weight_criteria[1]*Weight_attributes[2])*Epi_data$Lethality
Value_transmission <- (Weight_criteria[1]*Weight_attributes[3])*Epi_data$Transmission

Total_criteria_score_Epi <- Value_incidence + Value_lethality + Value_transmission #Reorder diseases so the same as rest of criteria scores 
Total_criteria_score_Epi <- c(Total_criteria_score_Epi[3], Total_criteria_score_Epi[5], 
                              Total_criteria_score_Epi[4], Total_criteria_score_Epi[1], Total_criteria_score_Epi[2])

#Criteria prevention/control 
Value_diagnostics <- (Weight_criteria[2]*Weight_attributes[4])*
  c(Weight_subattributes[1], Weight_subattributes[2],Weight_subattributes[2],Weight_subattributes[2],Weight_subattributes[1])

Value_treatments <- (Weight_criteria[2]*Weight_attributes[5])*
  c(Weight_subattributes[6], Weight_subattributes[6],Weight_subattributes[7],Weight_subattributes[7],Weight_subattributes[8])

Value_tools_control <- (Weight_criteria[2]*Weight_attributes[6])*
  c(Weight_subattributes[9], Weight_subattributes[10],Weight_subattributes[10],Weight_subattributes[10],Weight_subattributes[12])

Value_knowledge_pathogen <- (Weight_criteria[2]*Weight_attributes[7])*
  c(Weight_subattributes[14], Weight_subattributes[16],Weight_subattributes[15],Weight_subattributes[15],Weight_subattributes[15])

Total_criteria_score_prevention <- Value_diagnostics + Value_treatments + Value_tools_control + Value_knowledge_pathogen

#Criteria public health 
Value_efective_intervention <- (Weight_criteria[3]*Weight_attributes[8])*
  c(Weight_subattributes[18], Weight_subattributes[19],Weight_subattributes[18],Weight_subattributes[18],Weight_subattributes[19])

Value_epidemic_potential <- (Weight_criteria[3]*Weight_attributes[9])*
  c(Weight_subattributes[20], Weight_subattributes[22],Weight_subattributes[22],Weight_subattributes[20],Weight_subattributes[21])

Total_criteria_public_health <- Value_efective_intervention + Value_epidemic_potential

#Criteria economic 
Value_cost_eradication <- (Weight_criteria[4]*Weight_attributes[10])*
  c(Weight_subattributes[24], Weight_subattributes[26],Weight_subattributes[24],Weight_subattributes[25],Weight_subattributes[25])

Value_cost_healthcare <- (Weight_criteria[4]*Weight_attributes[11])*
  c(Weight_subattributes[28], Weight_subattributes[27],Weight_subattributes[28],Weight_subattributes[27],Weight_subattributes[28])

Value_cost_productivity <- (Weight_criteria[4]*Weight_attributes[12])*
  c(Weight_subattributes[30], Weight_subattributes[30],Weight_subattributes[31],Weight_subattributes[30],Weight_subattributes[32])

Total_criteria_economic <- Value_cost_eradication + Value_cost_healthcare + Value_cost_productivity 

#Criteria Social 
Value_QOL <- (Weight_criteria[5]*Weight_attributes[13])*
  c(Weight_subattributes[35], Weight_subattributes[33],Weight_subattributes[35],Weight_subattributes[35],Weight_subattributes[33])

Value_public_perception <- (Weight_criteria[5]*Weight_attributes[14])*
  c(Weight_subattributes[37], Weight_subattributes[37],Weight_subattributes[37],Weight_subattributes[37],Weight_subattributes[38])

Total_criteria_social <- Value_QOL+ Value_public_perception 

rbind(Total_criteria_score_Epi, Total_criteria_score_prevention, Total_criteria_public_health, Total_criteria_economic, Total_criteria_social)
### Summed totals of all criteria 
Final_score_original <- Total_criteria_score_Epi + Total_criteria_score_prevention + Total_criteria_public_health + Total_criteria_economic + Total_criteria_social
names(Final_score_original) <- c("Echino","Lepto","Trich","Rabies","Hanta")
    

#### Analysis with sensitivity analysis ####  
rm(list=ls())

Epi_data <- data.frame( 
  Dx = c("Rabies", "Hanta", "Echino", "Trich", "Lepto"),
  Incidence = c(0,25,93.75, 93.75, 100),
  Lethality = c(100,50.50505050, 1.01010101,0, 1.414141414),
  Transmission = c(0,100,80, 40, 40))

Criteria <- data.frame(
  Criteria = c("Epidemiology", "Prevention_control", "Public_health", "Economic_impact", "Societal_impact"), 
  Rank = c(3,2,1,4,5), 
  Points = c(100, 100, 100, 80, 80))


# Jiggle criteria 
Criteria_jiggle <- T ## Can turn on and off the criteria noise so can assess noise in sub attributes independently if desired 

if (Criteria_jiggle == T) {
  set.seed(2)
  
  Points_sim_matrix <- matrix(
    nrow = 100,
    ncol = nrow(Criteria)
  )
  
  for (i in 1:nrow(Criteria)) {
    Points_sim_matrix[, i] <- pmax(0, rnorm(100, mean = Criteria$Points[i], sd = 7))
  }
  
  # Normalise rows so each row sums to 1
  Weight_criteria_sim <- t(apply(Points_sim_matrix, 1, function(x) x / sum(x)))
  
} else {
  # Use fixed weights
  Weight_criteria_sim <- matrix(
    rep(Criteria$Points / sum(Criteria$Points), 100),
    nrow = 100,
    byrow = TRUE
  )
}


## Jiggle attributes 
Attribute_jiggle <- T ## Can turn on and off the attribute noise

Attributes <- data.frame(
  Attributes = c("Epi_incidence", "Epi_lethality", "Epi_spread", 
                 "Prev_control_diagnostics", "Prev_control_treatments", "Prev_control_tools", "Prev_control_knowledge", 
                 "Public_health_intervention",  "Public_health_epidemic", 
                 "Economic_impact_control_costs", "Economic_impact_health_costs", "Economic_impact_tourism_costs", 
                 "Societal_impact_QOL", "Societal_impact_public"), 
  Rank = c(3,1,2, 4,2,1,3,1,2,1,3,2, 1,2), 
  Points = c(70, 100, 90, 50, 90, 100, 90, 100, 85, 100, 40, 100, 100, 80))


normalise_groups <- function(row) {
  row[1:3] <- row[1:3] / sum(row[1:3])
  row[4:7] <- row[4:7] / sum(row[4:7])
  row[8:9] <- row[8:9] / sum(row[8:9])
  row[10:12] <- row[10:12] / sum(row[10:12])
  row[13:14] <- row[13:14] / sum(row[13:14])
  return(row)}

if (Attribute_jiggle == TRUE) {  
  set.seed(2)
  
  Attribute_jiggle_matrix <- matrix(
    nrow = 100,
    ncol = length(Attributes$Points)
  )
  
  for (i in 1:length(Attributes$Points)) {
    Attribute_jiggle_matrix[, i] <- pmin(100, pmax(0, rnorm(100, mean = Attributes$Points[i], sd = 7)))
  }
  
   Attribute_jiggle_matrix <- t(apply(Attribute_jiggle_matrix, 1, normalise_groups))
  
  Weight_attributes_list <- lapply(1:nrow(Attribute_jiggle_matrix), function(i) {
    row <- Attribute_jiggle_matrix[i, ]
    c(
      row[1:3],
      row[4:7],
      row[8:9],
      row[10:12],
      row[13:14]
    )
  })
  
} else {
  # Use fixed weights
  Weight_criteria_sim <- matrix(
    rep(Attributes$Points, 100),
    nrow = 100,
    byrow = TRUE
  )
  
  Weight_criteria_sim <- t(apply(Weight_criteria_sim, 1, normalise_groups))
  
  Weight_attributes_list <- lapply(1:nrow(Weight_criteria_sim), function(i) {
    row <- Weight_criteria_sim[i, ]
    c(
      row[1:3],
      row[4:7],
      row[8:9],
      row[10:12],
      row[13:14]
    )
  })
  }


## Jiggle subattributes 

Subattributes_jiggle <- T

Weight_subattributes <- c(0,50,85,100, ##Criteria prevention/control 1-4
                          0,10,85,100,
                          0,14,60,15,100,
                          0,15,100,
                          0,20,100, ## criteria Public health 17-19
                          0,50,20,100,
                          0,20,100, ## Criteria economic impact 24-26
                          0,15,100,
                          0,85,100,
                          0,80,100, ##Criteria social 33- 35
                          0,25,100)

if (Subattributes_jiggle == T) {
  set.seed(2)
  
  generate_values <- function(x) {
  if (x == 0 || x == 100) {
    
    rep(x, 100)
  } else {
    
    pmin(100, pmax(0, rnorm(100, mean = x, sd = 7)))
  }
}

Weight_subattributes <- lapply(Weight_subattributes, generate_values)

} else {
  Weight_subattributes <- lapply(Weight_subattributes, function(x) rep(x, 100))
  }



Final_score_all <- vector("list", 100)
  
  for (i in 1:100) {
  
  
#Criteria Epi  
Value_incidence <- (Weight_criteria_sim[i,1]*Weight_attributes_list[[i]][1])*Epi_data$Incidence 
Value_lethality <- (Weight_criteria_sim[i,1]*Weight_attributes_list[[i]][2])*Epi_data$Lethality
Value_transmission <- (Weight_criteria_sim[i,1]*Weight_attributes_list[[i]][3])*Epi_data$Transmission

Total_criteria_score_Epi <- Value_incidence + Value_lethality + Value_transmission #Reorder diseases so the same as rest of criteria scores 
Total_criteria_score_Epi <- c(Total_criteria_score_Epi[3], Total_criteria_score_Epi[5], 
                              Total_criteria_score_Epi[4], Total_criteria_score_Epi[1], Total_criteria_score_Epi[2])

#Criteria prevention/control 
Value_diagnostics <- (Weight_criteria_sim[i,2]*Weight_attributes_list[[i]][4])*
  rbind(Weight_subattributes[[1]][i], Weight_subattributes[[2]][i],Weight_subattributes[[2]][i],Weight_subattributes[[2]][i],Weight_subattributes[[1]][i])

Value_treatments <- (Weight_criteria_sim[i,2]*Weight_attributes_list[[i]][5])*
  rbind(Weight_subattributes[[6]][i], Weight_subattributes[[6]][i],Weight_subattributes[[7]][i],Weight_subattributes[[7]][i],Weight_subattributes[[8]][i])

Value_tools_control <- (Weight_criteria_sim[i,2]*Weight_attributes_list[[i]][6])*
  rbind(Weight_subattributes[[9]][i], Weight_subattributes[[10]][i],Weight_subattributes[[10]][i],Weight_subattributes[[10]][i],Weight_subattributes[[12]][i])

Value_knowledge_pathogen <- (Weight_criteria_sim[i,2]*Weight_attributes_list[[i]][7])*
  rbind(Weight_subattributes[[14]][i], Weight_subattributes[[16]][i],Weight_subattributes[[15]][i],Weight_subattributes[[15]][i],Weight_subattributes[[15]][i])

Total_criteria_score_prevention <- Value_diagnostics + Value_treatments + Value_tools_control + Value_knowledge_pathogen

#Criteria public health 
Value_efective_intervention <- (Weight_criteria_sim[i,3]*Weight_attributes_list[[i]][8])*
  rbind(Weight_subattributes[[18]][i], Weight_subattributes[[19]][i],Weight_subattributes[[18]][i],Weight_subattributes[[18]][i],Weight_subattributes[[19]][i])

Value_epidemic_potential <- (Weight_criteria_sim[i,3]*Weight_attributes_list[[i]][9])*
  rbind(Weight_subattributes[[20]][i], Weight_subattributes[[22]][i],Weight_subattributes[[22]][i],Weight_subattributes[[20]][i],Weight_subattributes[[21]][i])

Total_criteria_public_health <- Value_efective_intervention + Value_epidemic_potential

#Criteria economic 
Value_cost_eradication <- (Weight_criteria_sim[i,4]*Weight_attributes_list[[i]][10])*
  rbind(Weight_subattributes[[24]][i], Weight_subattributes[[26]][i],Weight_subattributes[[24]][i],Weight_subattributes[[25]][i],Weight_subattributes[[25]][i])

Value_cost_healthcare <- (Weight_criteria_sim[i,4]*Weight_attributes_list[[i]][11])*
  rbind(Weight_subattributes[[28]][i], Weight_subattributes[[27]][i],Weight_subattributes[[28]][i],Weight_subattributes[[27]][i],Weight_subattributes[[28]][i])

Value_cost_productivity <- (Weight_criteria_sim[i,4]*Weight_attributes_list[[i]][12])*
  rbind(Weight_subattributes[[30]][i], Weight_subattributes[[30]][i],Weight_subattributes[[31]][i],Weight_subattributes[[30]][i],Weight_subattributes[[32]][i])

Total_criteria_economic <- Value_cost_eradication + Value_cost_healthcare + Value_cost_productivity 

#Criteria Social 
Value_QOL <- (Weight_criteria_sim[i,5]*Weight_attributes_list[[i]][13])*
  rbind(Weight_subattributes[[35]][i], Weight_subattributes[[33]][i],Weight_subattributes[[35]][i],Weight_subattributes[[35]][i],Weight_subattributes[[33]][i])

Value_public_perception <- (Weight_criteria_sim[i,5]*Weight_attributes_list[[i]][14])*
  rbind(Weight_subattributes[[37]][i], Weight_subattributes[[37]][i],Weight_subattributes[[37]][i],Weight_subattributes[[37]][i],Weight_subattributes[[38]][i])

Total_criteria_social <- Value_QOL+ Value_public_perception 

### Summed totals of all criteria 
Final_score_all[[i]]<- Total_criteria_score_Epi + 
  Total_criteria_score_prevention + 
  Total_criteria_public_health + 
  Total_criteria_economic + 
  Total_criteria_social
}

# Final summary

Final_score_matrix <- do.call(cbind, Final_score_all)

Final_summary <- data.frame(
  Disease = c("Echino", "Lepto", "Trich", "Rabies", "Hanta"),
  Mean = rowMeans(Final_score_matrix),
  SD = apply(Final_score_matrix, 1, sd),
  CI_lower = apply(Final_score_matrix, 1, function(x) quantile(x, 0.025)),
  CI_upper = apply(Final_score_matrix, 1, function(x) quantile(x, 0.975))
)

Final_summary <- Final_summary[order(-Final_summary$Mean),]

colours <- c("steelblue4","slategray3","#FFFFFF", "slategray1", "gray80")

svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/MCDA elicitation Argentina/Manuscript/FigureSA.svg",   
    width = 10,height = 6)

barCenters <-barplot(Final_summary$Mean, beside=T, 
                     col= colours,
                     xlab = "",
                     ylab = "Value for prioritisation",
                     xaxt="n", 
                     ylim = c(0, 70))#

axis(side = 1, at = barCenters, tick = F, labels = Final_summary$Disease)

arrows(barCenters, Final_summary$CI_lower, barCenters, Final_summary$CI_upper,
       lwd = 0.8, angle = 90, #arrow + angle gives you top to error bar
       code = 3, length = 0.03)#length is width or error bar top

dev.off()


Final_score_means <- lapply(Final_score_all, rowMeans)
Final_score_matrix_1 <- do.call(cbind, Final_score_means)

x_vals <- 1:length(Final_score_matrix_1[1, ])


svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/MCDA elicitation Argentina/Manuscript/FigureSA_all_sims.svg",   
    width = 10,height = 6)
# Set up empty plot with correct limits
plot(x_vals, Final_score_matrix_1[5, ], type = "l", col = colours[1], lwd = 2,
     ylim = c(0, 70),
     xlim = c(0, 100), xaxs = "i", yaxs = "i",
     xlab = "Simulation number", ylab = "Mean prioritisation value",
     main = "", 
     bty = "n")

lines(x_vals, Final_score_matrix_1[2, ], col = colours[2], lwd = 2)
lines(x_vals, Final_score_matrix_1[3, ], col = "black", lwd = 2)
lines(x_vals, Final_score_matrix_1[4, ], col = colours[4], lwd = 2)
lines(x_vals, Final_score_matrix_1[1, ], col = colours[5], lwd = 2)

# Add a legend
legend("topright", legend = c("Hanta", "Lepto", "Trich", "Rabies", "Echino"), inset=c(0, -0.12), 
       col = c(colours[1],colours[2],"black",colours[4], colours[5]), lwd = 2, bty = "n", xpd=TRUE)

dev.off()

