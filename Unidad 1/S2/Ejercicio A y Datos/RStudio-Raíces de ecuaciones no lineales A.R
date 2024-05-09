install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~Peso,col="blue")


df = data.frame(Peso,largo)
chart.Correlation(df)

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

