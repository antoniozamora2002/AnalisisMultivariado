library(beeswarm)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)
library(data.table)

resultados2014 = read_delim("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S1/datos/resultados2014.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
  
head(resultados2014) #mostrar datos de resultados2014

resultados2018 = read_delim("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S1/datos/resultados2018.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
head(resultados2018) #mostrar datos de resultados2018

dim(resultados2014) #mostrar filas y columnas
str(resultados2014) #tipo de variable del dato
names(resultados2014) #nombres de las variables
summary(resultados2014) #descripción de los datos

#Actualizar los nombres de las columnas con el 
#nombre de los partidos politicos
partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

cam_nombre = function(dataframe){
  for (i in 1:length(partidos_nombre)){
    names(dataframe)[names(dataframe) == paste0('votos', i)] =
      partidos_nombre[i]
  }
  return(dataframe)
}
resultados2018 = cam_nombre(resultados2018) #ejecutar funcion
head(resultados2018, 5) # ver el resultado del cambio de nombre

#calcular porcentaje

votos_porcentaje = function(dataframe){
  x = dataframe%>%
    group_by(codigo)%>%
    mutate_all(funs((. / votos_validos)*100))%>%
    select(-votos_validos)
  return(x)
}

resultados2014_porcen = votos_porcentaje(resultados2014)
resultados2018_porcen = votos_porcentaje(resultados2018)
head(resultados2014_porcen)
head(resultados2018_porcen)


resultados2014_porcen = resultados2014 %>% #Metodo para volver el porcentaje a normal
  group_by(codigo) %>%
  mutate_all(list(~ mean(., trim = .2), ~ median(., na.rm = TRUE)))

##por_resultados2018 = votos_porcentaje(resultados2018)
#Metodo para volver el porcentaje a normal
resultados2018_porcen = resultados2018 %>%
  group_by(codigo) %>%
  mutate_all(list(~ mean(., trim = .2), ~ median(., na.rm = TRUE)))


# funcion para sacar partido ganador por zona
winner = function(dataframe, periodo){
  x = dataframe%>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo)%>%
    filter(votos==max(votos))%>%
    separate(partido, c(paste0("partido", periodo)),
             sep="1")%>%
    select(-votos)
  return(x)
  
}

winner2014=winner(resultados2014_porcen, 14)
winner2018 =winner(resultados2018_porcen, 18)
table(winner2014$partido14)
table(winner2018$partido18)

# ¿Como cambio la distribucion de los cantones ganados por 
#cada partido politico en comparación con las elecciones 2014

cambio = winner2018%>%
  left_join(winner2014, by="codigo")%>%
  mutate(cambio=ifelse(partido18==partido14,"sin
cambio", "cambio"),
         robo=ifelse(cambio=="cambio",
                     paste(partido18, partido14, sep=" al "), "sin cambio"))

table(cambio$cambio)
table(cambio$robo)

# sacar grafico de acuerdo al partido politico
grafico_votos = function(partido, color){
  x = resultados2018_porcen%>%
    select(codigo, paste0(partido,18))%>%
    left_join(
      (resultados2014_porcen%>%
         select(codigo, paste0(partido,14))),
      by="codigo")%>%
    gather(anio, votos, - codigo)%>%
    mutate(anio=ifelse(anio==paste0(partido,14), 2014, 2018))
  
  par(las=1, bty="l", family="mono", font=1, bg="transparent")
  
  return(
    beeswarm(votos ~ anio, data=x, col=color, pch=16, method="hex", 
             cex=0.8, horizontal=TRUE, ylab="", xlab=paste("Porcentaje de votos del", toupper(partido)), 
             main=paste("Porcentaje de votos del", toupper(partido)), xlim=c(0, 60))
  )
}
#mostrar gráfico y definir color
grafico_votos("pac", "red")
grafico_votos("prn", "red")

####MAPAS 
library("sf")
library("raster")
library("ggplot2")
library("readxl")
library("dplyr")
library(geojsonsf)
codigohasc = read_excel("E:/UPeU/2024 - 1/Análisis multivariado/AnalisisMultivariado/S1/datos/codigohasc2.xlsx")
View(codigohasc)

# Para mapas
mapa14 = left_join(resultados2014_porcen, codigohasc, by="codigo")
mapa18 = left_join(resultados2018_porcen, codigohasc, by="codigo")

# Gráfico
cr = getData("GADM", country = "CRI", level = 2)
print(class(cr))
# Transformar geodatos
cr_sf = st_as_sf(cr)

mapa_resultados <- function(dataframe_mapa, partido, color_high, titulo) {
  # Unir los datos
  cr_mapa = full_join(cr_sf, dataframe_mapa, by = c("HASC_2" = "HASC"))
  
  # Revisar las columnas disponibles
  print(names(cr_mapa))
  
  # Asegurarse de que 'order' (o el nombre de la columna correcta) está disponible
  if ("order" %in% names(cr_mapa)) {
    cr_mapa = arrange(cr_mapa, desc(order))
  } else {
    # Si 'order' no está disponible, elige otra columna o emite una advertencia
    warning("Columna 'order' no encontrada, ajusta el código de ordenamiento.")
  }
  
  # Generar el mapa
  return(
    ggplot() +
      geom_sf(data = cr_mapa, aes(fill = {{ partido }}), color = "white") +
      coord_sf() +
      scale_fill_gradient(low = "#E0E0E0", high = color_high, limits = c(0, 70)) +
      labs(x = NULL, y = NULL, title = titulo) +
      theme_void()
  )
}
mapa_resultados(mapa14, pln14, "red", "PLN 2014")
mapa_resultados(mapa14, pln14, "red", "PLN 2014")
mapa_resultados(mapa18, pln18, "red", "PLN 2018")


#RESPONDIENDO LAS PREGUNTAS DE LA PRACTICA 01

# ¿Cómo logró un partido político, que contaba con un solo diputado en 2014, obtener 
# el mayor porcentaje de votos en las elecciones de 2018?
# R: Para lograrlo se gestó al apropiarse de cantones clave de sus principales rivales 
#le arrebató 26 municipios al PLN, 9 al PAC y dos al Frente Amplio.

# ¿Cuántos cantones ganó cada partido político y cómo se compara este resultado con 
# las elecciones de 2014?
table(winner2018$partido18)

# ¿Cómo cambió la distribución de los cantones ganados por cada partido político en 
#  comparación con las elecciones de 2014?
table(cambio$cambio)

# ¿En qué cantones logró el PRN superar a los demás 
#  partidos políticos?
table(cambio$robo)

# ¿Cómo varió el porcentaje de votos obtenidos por el PLN, PUSC, PAC, PRN y PIN?
#Metodo grafico_votos esta mas arribita
grafico_votos("pln", "blue")
grafico_votos("pusc", "red")
grafico_votos("pac", "green")
grafico_votos("prn", "pink")
grafico_votos("pin", "violet")

# ¿Qué partido político resultó ganador en los seis cantones con la mayor cantidad 
#  de electores?
# El partido fue PRN, Fabricio Alvarado se impuso en cinco de los seis cantones con más 
#electores.

# ¿Qué partidos políticos resultaron ganadores en cada uno de los cantones?
library("gpclib")
library("raster")
library("maptools")
library("broom")
library(mapproj)
library(rlang)
gpclibPermit()
cantones= resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)
sum(cantones$votos_validos)/sum(resultados2018$votos_validos)*100

resultados2018_porcen%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)%>%
  gather(partido, votos, -codigo)%>%
  group_by(codigo)%>%
  filter(votos==max(votos))
