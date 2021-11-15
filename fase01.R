#creando funcion con 1 parametro de entrada
estadistica <- function(x){
  z <- ceiling(length(x)/2)
  tabla <- data.frame(x)
  tabla[z,"Media"] <- mean(x)
  tabla[z,"DE"] <- sd(x)
  tabla
}
estadistica(a)

#Creando funcion para experimento aceleración gravitacional 

propagacion <- function(x,y){
  z<- ceiling(length(y)/2)
  tabla <- data.frame(x,y)
  tabla[,"g"] <- 4*pi^2*x/y^2
  tabla[z,"gprom"] <- mean(tabla$g)
  tabla[z, "sigmag"] <- sd(tabla$g)/sqrt(length(x))
  print(tabla)
  library(xtable)
  xtable(tabla)
}

l <- c(0.512, 0.597, 0.682, 0.797, 0.883)
T <- c(1.448, 1.566, 1.669, 1.804, 1.896)
propagacion(l,T)
