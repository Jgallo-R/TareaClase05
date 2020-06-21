############################################# EVALUACION ##########################################
# Describir modelos AR(2), graficarlos para valores diferentes
# de los argumentos (ar=c(p1,p2))
#AR(2)

AR2 <- arima.sim(model=list(order=c(2,0,0), ar=c(0.1,0.2)),
                                  n=100, sd=0.1)

#Probar varias combinaciones de p1 y p2, graficas las series de tiempo
# simularlas y sus correspondientes funciones de autocorrelacion simple
# y funciones de autocorrelacion parcial

# repetir lo mismo con los procesos MA


############################################## SOLUCION ######################################

### Cargamos las librerias necesarias
library(ggplot2)
library(tseries)
library(forecast)
library(quantmod)


######################################## Para N=100 (N GRANDE)###########################################


##1.-Simulando Modelo AR(2)

#Para que el proceso sea estacionario se debe cumplir: 
# -1 < phi(2) < 1 ó phi(1) + phi(2)< 1 ó phi(2)-phi(1)< 1
# Con este criterio generemos algunos coeficientes phi(1)y phi(2)

#definimos la semilla
set.seed(999)
##Generamos los modelos con diferentes valores de phi1 y phi2 con valores altos, bajos y negativos
AR2.1 <- arima.sim(n = 100 , model = list(order = c(2,0,0) , ar=c(0.1,0.2) , sd=0.1))
AR2.2 <- arima.sim(n = 100 , model = list(order = c(2,0,0) , ar=c(-0.1,0.2) , sd=0.1))
AR2.3 <- arima.sim(n = 100 , model = list(order = c(2,0,0) , ar=c(0.1,-0.2) , sd=0.1))
AR2.4 <- arima.sim(n = 100 , model = list(order = c(2,0,0) , ar=c(-0.1,-0.2) , sd=0.1))
AR2.5 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(0.1,0.8) , sd=0.1))
AR2.6 <- arima.sim(n = 100,model = list(order = c(2,0,0) , ar=c(-0.1,0.8) , sd=0.1))
AR2.7 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(0.1,-0.8) , sd=0.1))
AR2.8 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(-0.1,-0.8) , sd=0.1))
AR2.9 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(0.8,0.1) , sd=0.1))
AR2.10 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(-0.8,0.1) , sd=0.1))
AR2.11 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(0.8,-0.1) , sd=0.1))
AR2.12 <- arima.sim(n = 100, model = list(order = c(2,0,0) , ar=c(-0.8,-0.1) , sd=0.1))


# grafiquemos estas series de tiempo simuladas
graphics.off()
par(mfrow = c(3,4))
ylm <- c(min(AR2.1, AR2.2, AR2.3, AR2.4, AR2.5, AR2.6, AR2.7, AR2.8, AR2.9, AR2.10, AR2.11, AR2.12) , 
         max(AR2.1, AR2.2, AR2.3, AR2.4, AR2.5, AR2.6, AR2.7, AR2.8, AR2.9, AR2.10, AR2.11, AR2.12))

plot.ts(AR2.1, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = 0.2")
plot.ts(AR2.2, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = 0.2")
plot.ts(AR2.3, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = -0.2")
plot.ts(AR2.4, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = -0.2")

plot.ts(AR2.5, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = 0.8")
plot.ts(AR2.6, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = 0.8")
plot.ts(AR2.7, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = -0.8")
plot.ts(AR2.8, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = -0.8")

plot.ts(AR2.9, ylim = ylm, main = "AR(2) phi[1] = 0.8 y  phi[2] = 0.1")
plot.ts(AR2.10, ylim = ylm, main = "AR(2) phi[1] = -0.8 y  phi[2] = 0.1")
plot.ts(AR2.11, ylim = ylm, main = "AR(2) phi[1] = 0.8 y  phi[2] = -0.1")
plot.ts(AR2.12, ylim = ylm, main = "AR(2) phi[1] = -0.8 y  phi[2] = -0.1")

##Graficamos las AFC (Funciones de autocorrelacion simples) para el AR(2)

graphics.off()
par(mfrow = c(3,4))

acf(AR2.1,main="AR(2) phi[1] = 0.1 y  phi[2] = 0.2")
acf(AR2.2,main="AR(2) phi[1] = -0.1 y  phi[2] = 0.2")
acf(AR2.3,main="AR(2) phi[1] = 0.1 y  phi[2] = -0.2")
acf(AR2.4,main="AR(2) phi[1] = -0.1 y  phi[2] = -0.2")

acf(AR2.5,main="AR(2) phi[1] = 0.1 y  phi[2] = 0.8")
acf(AR2.6,main="AR(2) phi[1] = -0.1 y  phi[2] = 0.8")
acf(AR2.7,main="AR(2) phi[1] = 0.1 y  phi[2] = -0.8")
acf(AR2.8,main="AR(2) phi[1] = -0.1 y  phi[2] = -0.8")

acf(AR2.9,main="AR(2) phi[1] = 0.8 y  phi[2] = 0.1")
acf(AR2.10,main="AR(2) phi[1] = -0.8 y  phi[2] = 0.1")
acf(AR2.11,main="AR(2) phi[1] = 0.8 y  phi[2] = -0.1")
acf(AR2.12,main="AR(2) phi[1] = -0.8 y  phi[2] = -0.1")


##Graficamos las PAFC (Funciones de autocorrelacion simples) para el AR(2)

graphics.off()
par(mfrow = c(3,4))

pacf(AR2.1,main="phi[1] = 0.1 y  phi[2] = 0.2")
pacf(AR2.2,main="phi[1] = -0.1 y  phi[2] = 0.2")
pacf(AR2.3,main="phi[1] = 0.1 y  phi[2] = -0.2")
pacf(AR2.4,main="phi[1] = -0.1 y  phi[2] = -0.2")

pacf(AR2.5,main="phi[1] = 0.1 y  phi[2] = 0.8")
pacf(AR2.6,main="phi[1] = -0.1 y  phi[2] = 0.8")
pacf(AR2.7,main="phi[1] = 0.1 y  phi[2] = -0.8")
pacf(AR2.8,main="phi[1] = -0.1 y  phi[2] = -0.8")

pacf(AR2.9,main="phi[1] = 0.8 y  phi[2] = 0.1")
pacf(AR2.10,main="phi[1] = -0.8 y  phi[2] = 0.1")
pacf(AR2.11,main="phi[1] = 0.8 y  phi[2] = -0.1")
pacf(AR2.12,main="phi[1] = -0.8 y  phi[2] = -0.1")


##1.-Simulando Modelo MA(2)

# Los Modelos MA siempre son estacionarios

#definimos la semilla
set.seed(999)
##Generamos los modelos con diferentes valores de phi1 y phi2 con valores altos, bajos y negativos
MA2.1 <- arima.sim(n = 100 , model = list(order = c(0,0,2) , ma=c(0.1,0.2) , sd=0.1))
MA2.2 <- arima.sim(n = 100 , model = list(order = c(0,0,2) , ma=c(-0.1,0.2) , sd=0.1))
MA2.3 <- arima.sim(n = 100 , model = list(order = c(0,0,2) , ma=c(0.1,-0.2) , sd=0.1))
MA2.4 <- arima.sim(n = 100 , model = list(order = c(0,0,2) , ma=c(-0.1,-0.2) , sd=0.1))
MA2.5 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,0.8) , sd=0.1))
MA2.6 <- arima.sim(n = 100,model = list(order = c(0,0,2) , ma=c(-0.1,0.8) , sd=0.1))
MA2.7 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,-0.8) , sd=0.1))
MA2.8 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,-0.8) , sd=0.1))
MA2.9 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.8,0.1) , sd=0.1))
MA2.10 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.8,0.1) , sd=0.1))
MA2.11 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.8,-0.1) , sd=0.1))
MA2.12 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.8,-0.1) , sd=0.1))


# grafiquemos estas series de tiempo simuladas
grathetacs.off()
par(mfrow = c(3,4))
ylm <- c(min(MA2.1, MA2.2, MA2.3, MA2.4, MA2.5, MA2.6, MA2.7, MA2.8, MA2.9, MA2.10, MA2.11, MA2.12) , 
         max(MA2.1, MA2.2, MA2.3, MA2.4, MA2.5, MA2.6, MA2.7, MA2.8, MA2.9, MA2.10, MA2.11, MA2.12))

plot.ts(MA2.1, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = 0.2")
plot.ts(MA2.2, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = 0.2")
plot.ts(MA2.3, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = -0.2")
plot.ts(MA2.4, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = -0.2")

plot.ts(MA2.5, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = 0.8")
plot.ts(MA2.6, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = 0.8")
plot.ts(MA2.7, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = -0.8")
plot.ts(MA2.8, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = -0.8")

plot.ts(MA2.9, ylim = ylm, main = "MA(2) theta[1] = 0.8 y  theta[2] = 0.1")
plot.ts(MA2.10, ylim = ylm, main = "MA(2) theta[1] = -0.8 y  theta[2] = 0.1")
plot.ts(MA2.11, ylim = ylm, main = "MA(2) theta[1] = 0.8 y  theta[2] = -0.1")
plot.ts(MA2.12, ylim = ylm, main = "MA(2) theta[1] = -0.8 y  theta[2] = -0.1")

##Graficamos las AFC (Funciones de autocorrelacion simples)

grathetacs.off()
par(mfrow = c(3,4))

acf(MA2.1,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = 0.2")
acf(MA2.2,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = 0.2")
acf(MA2.3,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = -0.2")
acf(MA2.4,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = -0.2")

acf(MA2.5,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = 0.8")
acf(MA2.6,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = 0.8")
acf(MA2.7,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = -0.8")
acf(MA2.8,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = -0.8")

acf(MA2.9,main="ACF MA(2) theta[1] = 0.8 y  theta[2] = 0.1")
acf(MA2.10,main="ACF MA(2) theta[1] = -0.8 y  theta[2] = 0.1")
acf(MA2.11,main="ACF MA(2) theta[1] = 0.8 y  theta[2] = -0.1")
acf(MA2.12,main="ACF MA(2) theta[1] = -0.8 y  theta[2] = -0.1")


##Graficamos las PAFC (Funciones de autocorrelacion simples)

grathetacs.off()
par(mfrow = c(3,4))

pacf(MA2.1,main="PACF theta[1] = 0.1 y  theta[2] = 0.2")
pacf(MA2.2,main="PACF theta[1] = -0.1 y  theta[2] = 0.2")
pacf(MA2.3,main="PACF theta[1] = 0.1 y  theta[2] = -0.2")
pacf(MA2.4,main="PACF theta[1] = -0.1 y  theta[2] = -0.2")

pacf(MA2.5,main="PACF theta[1] = 0.1 y  theta[2] = 0.8")
pacf(MA2.6,main="PACF theta[1] = -0.1 y  theta[2] = 0.8")
pacf(MA2.7,main="PACF theta[1] = 0.1 y  theta[2] = -0.8")
pacf(MA2.8,main="PACF theta[1] = -0.1 y  theta[2] = -0.8")

pacf(MA2.9,main="PACF theta[1] = 0.8 y  theta[2] = 0.1")
pacf(MA2.10,main="PACF theta[1] = -0.8 y  theta[2] = 0.1")
pacf(MA2.11,main="PACF theta[1] = 0.8 y  theta[2] = -0.1")
pacf(MA2.12,main="PACF theta[1] = -0.8 y  theta[2] = -0.1")



######################################## Para N=30 (N PEQUEÑO)###########################################


##1.-Simulando Modelo AR(2)

#Para que el proceso sea estacionario se debe cumplir: 
# -1 < phi(2) < 1 ó phi(1) + phi(2)< 1 ó phi(2)-phi(1)< 1
# Con este criterio generemos algunos coeficientes phi(1)y phi(2)

#definimos la semilla
set.seed(999)
##Generamos los modelos con diferentes valores de phi1 y phi2 con valores altos, bajos y negativos
AR2.1 <- arima.sim(n = 30 , model = list(order = c(2,0,0) , ar=c(0.1,0.2) , sd=0.1))
AR2.2 <- arima.sim(n = 30 , model = list(order = c(2,0,0) , ar=c(-0.1,0.2) , sd=0.1))
AR2.3 <- arima.sim(n = 30 , model = list(order = c(2,0,0) , ar=c(0.1,-0.2) , sd=0.1))
AR2.4 <- arima.sim(n = 30 , model = list(order = c(2,0,0) , ar=c(-0.1,-0.2) , sd=0.1))
AR2.5 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(0.1,0.8) , sd=0.1))
AR2.6 <- arima.sim(n = 30,model = list(order = c(2,0,0) , ar=c(-0.1,0.8) , sd=0.1))
AR2.7 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(0.1,-0.8) , sd=0.1))
AR2.8 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(-0.1,-0.8) , sd=0.1))
AR2.9 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(0.8,0.1) , sd=0.1))
AR2.10 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(-0.8,0.1) , sd=0.1))
AR2.11 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(0.8,-0.1) , sd=0.1))
AR2.12 <- arima.sim(n = 30, model = list(order = c(2,0,0) , ar=c(-0.8,-0.1) , sd=0.1))


# grafiquemos estas series de tiempo simuladas
graphics.off()
par(mfrow = c(3,4))
ylm <- c(min(AR2.1, AR2.2, AR2.3, AR2.4, AR2.5, AR2.6, AR2.7, AR2.8, AR2.9, AR2.10, AR2.11, AR2.12) , 
         max(AR2.1, AR2.2, AR2.3, AR2.4, AR2.5, AR2.6, AR2.7, AR2.8, AR2.9, AR2.10, AR2.11, AR2.12))

plot.ts(AR2.1, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = 0.2")
plot.ts(AR2.2, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = 0.2")
plot.ts(AR2.3, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = -0.2")
plot.ts(AR2.4, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = -0.2")

plot.ts(AR2.5, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = 0.8")
plot.ts(AR2.6, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = 0.8")
plot.ts(AR2.7, ylim = ylm, main = "AR(2) phi[1] = 0.1 y  phi[2] = -0.8")
plot.ts(AR2.8, ylim = ylm, main = "AR(2) phi[1] = -0.1 y  phi[2] = -0.8")

plot.ts(AR2.9, ylim = ylm, main = "AR(2) phi[1] = 0.8 y  phi[2] = 0.1")
plot.ts(AR2.10, ylim = ylm, main = "AR(2) phi[1] = -0.8 y  phi[2] = 0.1")
plot.ts(AR2.11, ylim = ylm, main = "AR(2) phi[1] = 0.8 y  phi[2] = -0.1")
plot.ts(AR2.12, ylim = ylm, main = "AR(2) phi[1] = -0.8 y  phi[2] = -0.1")

##Graficamos las AFC (Funciones de autocorrelacion simples) para el AR(2)

graphics.off()
par(mfrow = c(3,4))

acf(AR2.1,main="AR(2) phi[1] = 0.1 y  phi[2] = 0.2")
acf(AR2.2,main="AR(2) phi[1] = -0.1 y  phi[2] = 0.2")
acf(AR2.3,main="AR(2) phi[1] = 0.1 y  phi[2] = -0.2")
acf(AR2.4,main="AR(2) phi[1] = -0.1 y  phi[2] = -0.2")

acf(AR2.5,main="AR(2) phi[1] = 0.1 y  phi[2] = 0.8")
acf(AR2.6,main="AR(2) phi[1] = -0.1 y  phi[2] = 0.8")
acf(AR2.7,main="AR(2) phi[1] = 0.1 y  phi[2] = -0.8")
acf(AR2.8,main="AR(2) phi[1] = -0.1 y  phi[2] = -0.8")

acf(AR2.9,main="AR(2) phi[1] = 0.8 y  phi[2] = 0.1")
acf(AR2.10,main="AR(2) phi[1] = -0.8 y  phi[2] = 0.1")
acf(AR2.11,main="AR(2) phi[1] = 0.8 y  phi[2] = -0.1")
acf(AR2.12,main="AR(2) phi[1] = -0.8 y  phi[2] = -0.1")


##Graficamos las PAFC (Funciones de autocorrelacion simples) para el AR(2)

graphics.off()
par(mfrow = c(3,4))

pacf(AR2.1,main="phi[1] = 0.1 y  phi[2] = 0.2")
pacf(AR2.2,main="phi[1] = -0.1 y  phi[2] = 0.2")
pacf(AR2.3,main="phi[1] = 0.1 y  phi[2] = -0.2")
pacf(AR2.4,main="phi[1] = -0.1 y  phi[2] = -0.2")

pacf(AR2.5,main="phi[1] = 0.1 y  phi[2] = 0.8")
pacf(AR2.6,main="phi[1] = -0.1 y  phi[2] = 0.8")
pacf(AR2.7,main="phi[1] = 0.1 y  phi[2] = -0.8")
pacf(AR2.8,main="phi[1] = -0.1 y  phi[2] = -0.8")

pacf(AR2.9,main="phi[1] = 0.8 y  phi[2] = 0.1")
pacf(AR2.10,main="phi[1] = -0.8 y  phi[2] = 0.1")
pacf(AR2.11,main="phi[1] = 0.8 y  phi[2] = -0.1")
pacf(AR2.12,main="phi[1] = -0.8 y  phi[2] = -0.1")


##1.-Simulando Modelo MA(2)

# Los Modelos MA siempre son estacionarios

#definimos la semilla
set.seed(999)
##Generamos los modelos con diferentes valores de phi1 y phi2 con valores altos, bajos y negativos
MA2.1 <- arima.sim(n = 30 , model = list(order = c(0,0,2) , ma=c(0.1,0.2) , sd=0.1))
MA2.2 <- arima.sim(n = 30 , model = list(order = c(0,0,2) , ma=c(-0.1,0.2) , sd=0.1))
MA2.3 <- arima.sim(n = 30 , model = list(order = c(0,0,2) , ma=c(0.1,-0.2) , sd=0.1))
MA2.4 <- arima.sim(n = 30 , model = list(order = c(0,0,2) , ma=c(-0.1,-0.2) , sd=0.1))
MA2.5 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(0.1,0.8) , sd=0.1))
MA2.6 <- arima.sim(n = 30,model = list(order = c(0,0,2) , ma=c(-0.1,0.8) , sd=0.1))
MA2.7 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(0.1,-0.8) , sd=0.1))
MA2.8 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(-0.1,-0.8) , sd=0.1))
MA2.9 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(0.8,0.1) , sd=0.1))
MA2.10 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(-0.8,0.1) , sd=0.1))
MA2.11 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(0.8,-0.1) , sd=0.1))
MA2.12 <- arima.sim(n = 30, model = list(order = c(0,0,2) , ma=c(-0.8,-0.1) , sd=0.1))


# grafiquemos estas series de tiempo simuladas
grathetacs.off()
par(mfrow = c(3,4))
ylm <- c(min(MA2.1, MA2.2, MA2.3, MA2.4, MA2.5, MA2.6, MA2.7, MA2.8, MA2.9, MA2.10, MA2.11, MA2.12) , 
         max(MA2.1, MA2.2, MA2.3, MA2.4, MA2.5, MA2.6, MA2.7, MA2.8, MA2.9, MA2.10, MA2.11, MA2.12))

plot.ts(MA2.1, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = 0.2")
plot.ts(MA2.2, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = 0.2")
plot.ts(MA2.3, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = -0.2")
plot.ts(MA2.4, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = -0.2")

plot.ts(MA2.5, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = 0.8")
plot.ts(MA2.6, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = 0.8")
plot.ts(MA2.7, ylim = ylm, main = "MA(2) theta[1] = 0.1 y  theta[2] = -0.8")
plot.ts(MA2.8, ylim = ylm, main = "MA(2) theta[1] = -0.1 y  theta[2] = -0.8")

plot.ts(MA2.9, ylim = ylm, main = "MA(2) theta[1] = 0.8 y  theta[2] = 0.1")
plot.ts(MA2.10, ylim = ylm, main = "MA(2) theta[1] = -0.8 y  theta[2] = 0.1")
plot.ts(MA2.11, ylim = ylm, main = "MA(2) theta[1] = 0.8 y  theta[2] = -0.1")
plot.ts(MA2.12, ylim = ylm, main = "MA(2) theta[1] = -0.8 y  theta[2] = -0.1")

##Graficamos las AFC (Funciones de autocorrelacion simples)

grathetacs.off()
par(mfrow = c(3,4))

acf(MA2.1,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = 0.2")
acf(MA2.2,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = 0.2")
acf(MA2.3,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = -0.2")
acf(MA2.4,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = -0.2")

acf(MA2.5,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = 0.8")
acf(MA2.6,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = 0.8")
acf(MA2.7,main="ACF MA(2) theta[1] = 0.1 y  theta[2] = -0.8")
acf(MA2.8,main="ACF MA(2) theta[1] = -0.1 y  theta[2] = -0.8")

acf(MA2.9,main="ACF MA(2) theta[1] = 0.8 y  theta[2] = 0.1")
acf(MA2.10,main="ACF MA(2) theta[1] = -0.8 y  theta[2] = 0.1")
acf(MA2.11,main="ACF MA(2) theta[1] = 0.8 y  theta[2] = -0.1")
acf(MA2.12,main="ACF MA(2) theta[1] = -0.8 y  theta[2] = -0.1")


##Graficamos las PAFC (Funciones de autocorrelacion simples)

grathetacs.off()
par(mfrow = c(3,4))

pacf(MA2.1,main="PACF theta[1] = 0.1 y  theta[2] = 0.2")
pacf(MA2.2,main="PACF theta[1] = -0.1 y  theta[2] = 0.2")
pacf(MA2.3,main="PACF theta[1] = 0.1 y  theta[2] = -0.2")
pacf(MA2.4,main="PACF theta[1] = -0.1 y  theta[2] = -0.2")

pacf(MA2.5,main="PACF theta[1] = 0.1 y  theta[2] = 0.8")
pacf(MA2.6,main="PACF theta[1] = -0.1 y  theta[2] = 0.8")
pacf(MA2.7,main="PACF theta[1] = 0.1 y  theta[2] = -0.8")
pacf(MA2.8,main="PACF theta[1] = -0.1 y  theta[2] = -0.8")

pacf(MA2.9,main="PACF theta[1] = 0.8 y  theta[2] = 0.1")
pacf(MA2.10,main="PACF theta[1] = -0.8 y  theta[2] = 0.1")
pacf(MA2.11,main="PACF theta[1] = 0.8 y  theta[2] = -0.1")
pacf(MA2.12,main="PACF theta[1] = -0.8 y  theta[2] = -0.1")
