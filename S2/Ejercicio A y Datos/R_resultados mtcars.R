library(readr)
library(corrplot)
library(RColorBrewer) #Definir colores

mtcars = read_delim("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S2/Ejercicio A y Datos/mtcars.csv", 
                    ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,

View(mtcars)

dim(mtcars) #mostrar filas y columnas
str(mtcars) #tipo de variable del dato
names(mtcars) #nombres de las variables
summary(mtcars) #descripción de los datos

# Calcular la matriz de correlación solo para las variables numéricas
mtcars_numeric <- mtcars[, sapply(mtcars, is.numeric)]

# Definir una paleta de colores para la matriz de correlación
colorMatriz <- colorRampPalette(c("red", "white", "green"))(200)

# Ejecutar Matriz de correlacion
matriz_mtcars <- cor(mtcars_numeric)

corrplot(matriz_mtcars, method = "ellipse", col = colorMatriz) # Elipse
corrplot(matriz_mtcars, method = "circle", col = colorMatriz) # Circulo
corrplot(matriz_mtcars, method = "square", col = colorMatriz) #Cuadrado
corrplot(matriz_mtcars, method = "number", col = colorMatriz) #Numerico
corrplot(matriz_mtcars, method = "shade", col = colorMatriz) #Sombra
corrplot(matriz_mtcars, method = "color", col = colorMatriz) # Color
corrplot(matriz_mtcars, method = "pie", col = colorMatriz) #Pai o pie