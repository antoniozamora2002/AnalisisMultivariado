#INDEPENDIENTES

EA = c(2,3,1,4,2,3,2,1,3,2)
EB = c(3,2,1,3,2,2,4,2,3,1)

boxplot(EA,EB, names= c ("EA", "EB"))
medias=c(mean(EA), mean(EB))
points(medias,pch=18,col="red")

par(mar = c(2, 2, 2, 2))
par(mfrow =c(1, 2))
qqnorm(EA, xlab = "", ylab="", main = "EA")
qqline(EA)
qqnorm(EB, xlab = "", ylab="", main = "EB")
qqline(EB)

## Prueba t

t.test(EA, EB, paired = FALSE)

##### CASO 2

AT = c (7, 6, 5, 6, 7)
DT = c (8, 7, 8, 8, 9)

boxplot(AT,DT, names= c ("AT", "DT"))
medias=c(mean(AT), mean(DT))
points(medias,pch=18,col="red")

par(mar = c(2, 2, 2, 2))
par(mfrow =c(1, 2))
qqnorm(AT, xlab = "", ylab="", main = "AT")
qqline(AT)
qqnorm(DT, xlab = "", ylab="", main = "DT")
qqline(DT)

## Prueba t

t.test(AT, DT, paired = TRUE)
