# Por matriz de correlación 
round(cor(x = universidad_data, method = "pearson"), 3)

# Por gráfica

library(psych)  
multi.hist(x = universidad_data, dcol = c("blue", "orange"), dlty = c("dotted", "solid"), main = "")

library(GGally)
ggpairs(universidad_data, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Identificar mejores predictores

modelox <- lm(universidad_data$score ~ universidad_data$Founded_year + universidad_data$UK_rank + universidad_data$World_rank + universidad_data$Minimum_IELTS_score + universidad_data$fees + universidad_data$Student_satisfaction + universidad_data$`Estimated_cost_of_living_per_year_(in_pounds)` + universidad_data$Latitude)

step(object = modelox, direction = "both", trace=1)

# Calcular colinealidad en el modelo
library(car)
vif(modelox)

#En 3D
library(rgl)
plot3d(universidad_data$`Estimated_cost_of_living_per_year_(in_pounds)`, universidad_data$Student_satisfaction, universidad_data$score, pch = ".", size = 0.5)