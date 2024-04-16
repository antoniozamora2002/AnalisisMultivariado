install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(readr)
library(corrplot)
library(RColorBrewer)

Peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~Peso,col="blue")


df = data.frame(Peso,largo)
chart.Correlation(df)

# Descargar y mostar datos
mtcars = read_delim("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S2/A/mtcars.csv", 
                    ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
View(mtcars)

str(mtcars) #ver que tipo de variable son las columnas
mtcars_numeric <- mtcars[, sapply(mtcars, is.numeric)] #excluir caracteres y solo admitir numeros

#Ejecutar matriz de correlaciones
colorMatriz <- colorRampPalette(c("blue", "white", "green"))(200) #definimos los colores

# Ejecutar Matriz de correlacion
M <- cor(mtcars_numeric)
corrplot(M, method = "ellipse", col = colorMatriz) # Forma Elipse
corrplot(M, method = "circle", col = my_colors) # Forma Circulo
corrplot(M, method = "square", col = my_colors) # Forma Cuadrado
corrplot(M, method = "number", col = my_colors) # Forma Numerico
corrplot(M, method = "shade", col = my_colors) # Forma Sombra
corrplot(M, method = "color", col = my_colors) # Forma Color
corrplot(M, method = "pie", col = my_colors) # Forma Pai o pie










shapiro.test(Peso)
shapiro.test(largo)

#Valor de la correlacion

cor(Peso, largo)
cor.test(Peso,largo)

#Correlacion de Spearm











#Ejemplo de Clase

edad =c(26, 18, 20, 19, 25, 22,  37, 56, 78)
talla=c (1.56, 1.73, 1.65, 1.44, 1.69, 1.66, 1.51, 1.62, 1.42)
pairs(edad~talla,col="blue")

df = data.frame(edad,talla)
chart.Correlation(df)

shapiro.test(edad)
shapiro.test(talla)

#Valor de la correlacion

cor(edad, talla)
cor.test(edad, talla)

#Correlacion de Spearm

