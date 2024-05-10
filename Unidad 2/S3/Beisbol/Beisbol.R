#Matriz de correlación

round(cor(x = beisbol, method = "pearson"), 3)
#Si hay colinealidad mayor a 0.7

#Gráfica 01

library(psych) 
multi.hist(x = beisbol, dcol = c("pink", "green"),
           dlty = c("dotted", "solid"), main = "")

#Gráfica 02
library(GGally) 
ggpairs(beisbol, lower = list(continuous ="smooth"), 
        diag = list(continuous = "barDiag"), axisLabels ="none")

#Mejores predictores
modelox = lm(beisbol$Points ~ beisbol$Age+beisbol$Games+beisbol$`Minutes Played`+
               beisbol$`Fields Goal`+beisbol$Assists+beisbol$Steals)

step(object = modelox, direction = "both", trace=1)

#Colinealidad en el modelo: si la hay
library(car)
vif(modelox)

#Gráfico en 3D
library(rgl)
colores <- rainbow(length(beisbol$Points))
plot3d(beisbol$Age,beisbol$Games,beisbol$Points, pch = ".", size = 0.5, col = colores)