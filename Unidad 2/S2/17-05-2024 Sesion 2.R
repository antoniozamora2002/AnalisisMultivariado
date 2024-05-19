horizonte = 7

rango_pronostico =  seq(nrow(st_apple), nrow(st_apple) + horizonte-1)

# Grafica 1

library(ggplot2)
ggplot(st_apple, aes(x = periodo, y = valor)) +  
  geom_line(color = "pink") +  
  geom_point(color = "purple") +  
  geom_line(data = data.frame(fecha = rango_pronostico, valor = rep(tail(st_apple$valor, n = 1),  times = horizonte)),             
            aes(x = fecha, y = valor), color = "turquoise") +  
  geom_point(data = data.frame(fecha = rango_pronostico, valor = rep(tail(st_apple$valor, n = 1), times = horizonte)),              
             aes(x = fecha, y = valor), color = "turquoise") +  labs(x = "\nDía de Cotización", y = "Dólares Estadounidenses") +  theme(plot.title = element_text(hjust = 0.5))

# Datos
st_apple$periodo =c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 )
View(st_apple)

# Grafico 2

ggplot(st_apple, aes(x = periodo, y = valor), group =1)  +  
  geom_line(color = "red") +  geom_point(color = "red") +  
  geom_line(data = data.frame(fecha = rango_pronostico, valor = rep(tail(st_apple$valor, n = 1), horizonte)),             
            aes(x = fecha, y = valor),             color = "turquoise") +  
  geom_point(data = data.frame(fecha = rango_pronostico, valor = rep(tail(st_apple$valor, n = 1), horizonte)),              
             aes(x = fecha, y = valor),   
             color = "turquoise") +  
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  labs(x = "\nDía de Cotización", y = "Dólares Estadounidenses") +  theme_minimal() +  theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 0.5))
