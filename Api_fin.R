# Carga Librerias
library(zoo)
library(rugarch)
library(xts)
library(ggplot2)
library(psych)
library(tseries)
library(forecast)
library(xtable)
library(jsonlite)
library(forecast)

#RUN Fondo	Nombre Fondo
#9730	FONDO MUTUO FINTUAL VERY CONSERVATIVE STREEP
#9570	FONDO MUTUO FINTUAL RISKY NORRIS
#9569	FONDO MUTUO FINTUAL MODERATE PITT
#9568	FONDO MUTUO FINTUAL CONSERVATIVE CLOONEY

# Conservative Clooney: principalmente renta fija. id: 188
# Moderate Pit: la justa mezcla de renta fija y ETFs accionarios. id: 187
# Risky Norris: casi solamente ETFs accionarios. id: 186



### Selecciona Fondo y rango de fechas 
cod <- 186
from <- '2010-07-01'
to <- Sys.Date()

### Extrae datos
url <- paste("https://fintual.cl/api/real_assets/",cod,"/days?to_date=",to,"&from_date=",from,sep="")
url1 <- paste("https://fintual.cl/api/real_assets/",cod,sep="")

x <- fromJSON(txt=url)
y <- fromJSON(txt=url1)
nombre <- paste(y[["data"]][["attributes"]][["name"]],y[["data"]][["attributes"]][["symbol"]],sep= " ")

### genera serie de precios y fechas
serie <- xts(x=x[["data"]][["attributes"]][["price"]],order.by=as.Date(x[["data"]][["attributes"]][["date"]]))
serie <-na.omit(merge(serie, (diff(log(serie), lag=1))))
colnames(serie) <- (c(y[["data"]][["attributes"]][["symbol"]],y[["data"]][["attributes"]][["symbol"]]))


### grafica
plot(x=as.zoo(serie[,1]),xlab="fecha", ylab = "$ CLP", col = c("blue"), screens = 2)
legend(x = "topleft", legend =nombre, lty = 1, col = c("blue"))
plot(x=as.zoo(serie[,2]),xlab="fecha", ylab = "ret %", col = c("red"), screens = 1)
legend(x = "topleft", legend =nombre, lty = 1, col = c("red"))


### se verifica estacionariedad
adf.test(serie[,2])

### arima optimo para proceso garch
arima_opt <- auto.arima(serie[,2], trace=TRUE, test="adf",  ic="bic")
# ARIMA(1,0,0) with zero mean     : -7037.265
Box.test(arima_opt$residuals^2,lag=12, type="Ljung-Box")

forecast(arima_opt,h=4)

####

model = c('sGARCH','gjrGARCH','apARCH','iGARCH','fGARCH')
submodel = c(NA, NA, NA, NA, 'AVGARCH')  


n_modelos = length(model) 

spec1 = vector(mode = 'list', length = n_modelos)
spec2 = vector(mode = 'list', length = n_modelos)
spec3 = vector(mode = 'list', length = n_modelos)
spec4 = vector(mode = 'list', length = n_modelos)
spec5 = vector(mode = 'list', length = n_modelos)


for (i in 1:n_modelos) 
  spec1[[i]] = ugarchspec(mean.model = list(armaOrder = c(1, 0)),variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 4)
        submodel[i]
      else
        NULL
    ),
    distribution = 'std'
  )

for (i in 1:n_modelos)
  spec2[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 0)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 4)
        submodel[i]
      else
        NULL
    ),
    distribution = 'ged'
  )

for (i in 1:n_modelos)
  spec3[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 0)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 4)
        submodel[i]
      else
        NULL
    ),
    distribution = 'nig'
  )

for (i in 1:n_modelos)
  spec4[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 0)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 4)
        submodel[i]
      else
        NULL
    ),
    distribution = 'jsu'
  )

for (i in 1:n_modelos)
  spec5[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 0)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 4)
        submodel[i]
      else
        NULL
    ),
    distribution = 'norm'
  )

spec = c(spec1, spec2, spec3, spec4, spec5)
n_spec = length(spec)



# Estimar modelo
fit = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) {
  fit[[i]] = ugarchfit(spec = spec[[i]], data = serie[,2])
}

for (i in 30:n_spec) {
  fit[[i]] = ugarchfit(spec = spec[[i]], data = serie[,2])
}



# Grafica
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit[[6]], which = i)
}
