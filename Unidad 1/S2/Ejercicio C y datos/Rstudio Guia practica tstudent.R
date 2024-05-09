library(readxl)
library(stats)

# Carga la data
births <- read_excel("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S2/Ejercicio C y datos/births_ulttbhbmip.xlsx")

View(births)

dim(births) #mostrar filas y columnas
str(births) #tipo de variable del dato
names(births) #nombres de las variables
summary(births) #descripción de los datos

# Convertir la variable "smoke" en numérica
births$smoke_numeric <- ifelse(births$smoke == "smoker", 1, 0)

# Realizar la prueba t para comparar los pesos de los bebés entre padres fumadores y no fumadores
t_test_result <- t.test(weight ~ smoke_numeric, data = births)

# Imprimir el resultado
print(t_test_result)

#2do ejercicio

# Convertir la variable "sex_baby" en numérica
births$sex_numeric <- ifelse(births$sex_baby == "male", 1, 0)

# Realizar la prueba t para comparar los pesos de los bebés entre sexos masculino y femenino
t_test_result_sex <- t.test(weight ~ sex_numeric, data = births)

# Imprimir el resultado
print(t_test_result_sex)
