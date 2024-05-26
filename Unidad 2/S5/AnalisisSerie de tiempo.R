#libreria fpp2
library(fpp2)
library(readxl)

# Data
file <- read.csv("C:/Users/zambr/Documents/AnalisisMultivariada/DemandaElectrica.csv", 
                 header = TRUE, sep = ",", stringsAsFactors = FALSE)

colnames(file) <- c('tiempo','demanda')
data <- file[1:180,]
test <- file[181:188,]
head(data,3)
tail(data,3)
head(test,3)

#creación de la serie de tiempo histórica

data <- ts(data$demanda, frequency=12,start=1980)

#creación de la serie de tiempo para validación
test <- ts(test$demanda, frequency=12,start=1995)



#Analisis de la gráfica resultante de la descomposición de la serie de tiempo

#Gráfica ACF
ggAcf(data)

#Descomposición STL
autoplot(stl(data, s.window=7))


#Analisis de pronostico con ARIMA
m6 <- auto.arima(data)
m6

#grafico de datos
autoplot(forecast(m6),h=16)+ xlab("Tiempo") + ylab("Demanda")+ autolayer(test, serie="Real")

#análisis del modelo
autoplot(data)+    
  autolayer(fitted(m6), series="Ajuste")

accuracy(m6)
checkresiduals(m6)
