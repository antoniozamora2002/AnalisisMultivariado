# Data
guns_data <- read.csv("E:/UPeU/2024 - 1/Análisis multivariado/Guns.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

library("lattice")
xyplot(log(violent) ~ as.numeric(as.character(year)) | state, data = guns_data, type = "l")

# Eliminar las columnas no numericas usando dplyr
guns_data <- subset(guns_data, select = -rownames)
guns_data <- subset(guns_data, select = -year)
guns_data <- subset(guns_data, select = -state)
guns_data <- subset(guns_data, select = -law)

# Por matriz de correlación
round(cor(x = guns_data, method = "pearson"), 3)

library(GGally)
ggpairs(guns_data, lower = list(continuous = "smooth"),           diag = list(continuous = "barDiag"), axisLabels = "none")

library(GGally)
ggcorr(guns_data, label = TRUE, label_size = 3, label_round = 3)

library(psych)  
multi.hist(x = guns_data, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")

# Identificar mejores predictores
modelx <- lm(log(violent) ~ murder + robbery + prisoners + afam + cauc + male + population + income + density, data = guns_data)
step(object = modelx, direction = "both", trace=1)

modelo <- lm(guns_data$violent ~ guns_data$prisoners + guns_data$male + guns_data$population + guns_data$income + guns_data$density)
modelo2 <- lm(guns_data$violent ~ guns_data$murder + guns_data$robbery + guns_data$afam + guns_data$cauc)
modelo2_arreglado <- lm(guns_data$violent ~ guns_data$murder + guns_data$robbery)

# Calcular colinealidad en el modelo
library(car)
vif(modelo)
vif(modelo2)
vif(modelo2_arreglado)

# En 3D
library(rgl)
plot3d(guns_data$murder, guns_data$robbery, guns_data$violent, pch = ".", size = 0.5)

# Ejecuto anova para saber si hay diferencia
anova(modelo)
anova(modelo2_arreglado)
anova(modelo, modelo2_arreglado)

summary(modelo)
summary(modelo2_arreglado)

# Guardo residuos
residuos = resid(modelo)
print(residuos)

residuos2 = resid(modelo2_arreglado)
print(residuos2)

# Graficos
plot(density(residuos))
plot(density(residuos2))

qqnorm(residuos)
qqline(residuos)

qqnorm(residuos2)
qqline(residuos2)

shapiro.test(residuos)
shapiro.test(residuos2)

ks.test(residuos, residuos2)
