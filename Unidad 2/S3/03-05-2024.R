
# Por matriz de correlación 
round(cor(x = diabetes_data, method = "pearson"), 3)

# Por gráfica

library(psych)  
multi.hist(x = diabetes_data, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")

library(GGally)
ggpairs(diabetes_data, lower = list(continuous = "smooth"),           diag = list(continuous = "barDiag"), axisLabels = "none")

# Identificar mejores predictores

modelox = lm(diabetes_data$BloodPressure ~ diabetes_data$Pregnancies+diabetes_data$SkinThickness
             +diabetes_data$Insulin+diabetes_data$BMI+diabetes_data$DiabetesPedigreeFunction+diabetes_data$Age)

step(object = modelox, direction = "both", trace=1)

# Calcular colinealidad en el modelo
library(car)
vif(modelox)

# Grafica en 3D
library(rgl)
plot3d(diabetes_data$BMI, diabetes_data$Age, diabetes_data$BloodPressure, pch = ".", size = 0.5)

