
modelo = lm(diabetes_data$BloodPressure ~ diabetes_data$BMI+diabetes_data$Age+diabetes_data$SkinThickness)

# summary(modelo)

y = mx +b

#Formula para predecir
# Presion = Age*()+BMI*()+Skintickness*()+Coeficiente
#Presion = Age*(0.4101)+BMI*(0.5231)+Skintickness*(0.1846)+34.9501
#Age = 25
#BMI = 35
#ST= 4

# Ejecuto anova para saber si hay diferencia
anova(modelo)

# Guardo residuos
residuos = resid(modelo)
print(residuos)

# Graficos

# 1
plot(density(residuos))

# 2
qqnorm(residuos)
qqline(residuos)

# Confirmar si tienen una Distribuciòn normal
shapiro.test(residuos)