# Cargar el paquete readxl para leer archivos Excel
library(readxl)

# Importar el conjunto de datos desde un archivo Excel
bookstore <- read_excel("C:/Users/HP/OneDrive/Documentos/AnalisisMultivariada/AnalisisMultivariado/Unidad 2/S3/Bookstore/bookstore.xlsx")

# Mostrar las primeras filas del conjunto de datos
head(bookstore)

# Mostrar la estructura del conjunto de datos
str(bookstore)

# Exportar el conjunto de datos a un archivo CSV sin incluir los números de fila
write.csv(bookstore, "bookstore_limpio.csv", row.names = FALSE)

# Seleccionar solo las columnas necesarias
bookstore <- subset(bookstore, select = c(title, author, price, pages, reviews, 
                                          n_reviews, star5, star4, star3, star2, star1))

# Matriz de correlación
round(cor(bookstore[, c("price", "pages", "reviews", "n_reviews", "star5", "star4", 
                        "star3", "star2", "star1")], method = "pearson"), 3)

# Gráfico de histogramas múltiples
library(psych)
multi.hist(bookstore[, c("price", "pages", "reviews", "n_reviews", "star5", 
                         "star4", "star3", "star2", "star1")], 
                          dcol = c("red", "orange"), 
                          dlty = c("dotted", "solid"), main = "")

# Gráfico de pares
library(GGally)
ggpairs(bookstore[, c("price", "pages", "reviews", "n_reviews", "star5", 
                      "star4", "star3", "star2", "star1")], 
                       lower = list(continuous = "smooth"), 
                       diag = list(continuous = "barDiag"), 
                       axisLabels = "none")

# Ajuste de modelo de regresión lineal
modelo <- lm(price ~ pages + reviews + n_reviews + star5 + star4 + star3 + star2 + star1, data = bookstore)

# Selección de variables con stepwise
step(modelo, direction = "both", trace = 1)

# Colinealidad en el modelo
library(car)
vif(modelo)

# Gráfico en 3D
library(rgl)
colores <- rainbow(length(bookstore$price))
plot3d(bookstore$pages, bookstore$reviews, bookstore$price, pch = ".", size = 0.5, col = colores)