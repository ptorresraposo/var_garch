# Permite cargar y estimar VaR al 5% a través de distintos modelos GARCH y con distintas 
# distribuciones del error condicionales para los fondos A, C y E de las AFP


# Carga Librerias
library("zoo")
library("rugarch")
library("xts")
library("ggplot2")
library("psych")
library("tseries")
library("forecast")
library("xtable")


# carga_datos y retornos -------------------------------------------------------------

ColClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
data_A <- as.xts(read.zoo("https://raw.githubusercontent.com/ptorresraposo/data/main/fondoA2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))
data_C <- as.xts(read.zoo("https://raw.githubusercontent.com/ptorresraposo/data/main/fondoC2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))
data_E <- as.xts(read.zoo("https://raw.githubusercontent.com/ptorresraposo/data/main/fondoE2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))

# genera retornos

ret_A <- na.omit(data_A["20020927/20190729","ret_pond_A"])
ret_C <- na.omit(data_C["20020927/20190729","ret_pond_C"])
ret_E <- na.omit(data_E["20020927/20190729","ret_pond_E"])

# Data descriptiva y test

descriptiva <- describe(merge(ret_A,ret_C,ret_E))
descriptiva

jarque.bera.test(ret_A)
jarque.bera.test(ret_C)
jarque.bera.test(ret_E)

jarque.bera.test(ret_A)
jarque.bera.test(ret_C)
jarque.bera.test(ret_E)

adf.test(ret_A)
adf.test(ret_C)
adf.test(ret_E)

# grafica

plot(x=as.zoo(merge(data_A[,17],data_C[,17],data_E[,17])/1000000000), xlab="Time", ylab = "MM$ CLP",col = c("red", "blue", "darkgreen"), screens = 1)
legend(x = "topleft", legend = c("A", "C", "E"), lty = 1, col = c("red", "blue", "darkgreen"))

plot(x=as.zoo(merge(data_A[,35],data_C[,35],data_E[,35])), xlab="Time", ylab = c("A", "C", "E"), main = "" , col = c("red", "blue", "darkgreen"), ylim=c(-0.06,0.06))



# arima y lung box --------------------------------------------------------


arima_a <- auto.arima(ret_A, trace=TRUE, test="adf",  ic="bic")
# Best model: ARIMA(0,0,1) with non-zero mean 
Box.test(arima_a$residuals^2,lag=12, type="Ljung-Box")

arima_c <- auto.arima(ret_C, trace=TRUE, test="adf",  ic="bic")
# Best model: ARIMA(0,0,1) with non-zero mean 
Box.test(arima_c$residuals^2,lag=12, type="Ljung-Box")

arima_e <- auto.arima(ret_E, trace=TRUE, test="adf",  ic="bic")
#  Best model: ARIMA(1,0,2) with non-zero mean 
Box.test(arima_e$residuals^2,lag=12, type="Ljung-Box")

# Modelos a Estimar -------------------------------------------------------
# Especifica
# "sGARCH', 'eGARCH', 'gjrGARCH', 'apARCH', 'iGARCH' 
# garchOrder The ARCH (q) and GARCH (p) orders.
# submodel If the model is 'fGARCH', valid submodels 
# are 'TGARCH','AVGARCH', 'NGARCH', 'NAGARCH'
#

model = c('sGARCH','eGARCH','gjrGARCH','apARCH','iGARCH','fGARCH','fGARCH','fGARCH','fGARCH')
submodel = c(NA, NA, NA, NA, NA, 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH')  

# Fondo A -----------------------------------------------------------------
# “norm”, “snorm”, “std”, “sstd”, “ged”, “sged”, “nig”, “jsu”.

n_modelos = length(model) 
spec1_A = vector(mode = 'list', length = n_modelos)
spec2_A = vector(mode = 'list', length = n_modelos)
spec3_A = vector(mode = 'list', length = n_modelos)
spec4_A = vector(mode = 'list', length = n_modelos)
spec5_A = vector(mode = 'list', length = n_modelos)


for (i in 1:n_modelos) 
  spec1_A[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'std'
  )

for (i in 1:n_modelos)
  spec2_A[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'ged'
  )

for (i in 1:n_modelos)
  spec3_A[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'nig'
  )

for (i in 1:n_modelos)
  spec4_A[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'jsu'
  )

for (i in 1:n_modelos)
  spec5_A[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'norm'
  )

spec_A = c(spec1_A, spec2_A, spec3_A, spec4_A, spec5_A)
n_spec = length(spec_A)



# Estimar modelo
fit_A = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) {
  fit_A[[i]] = ugarchfit(spec = spec_A[[i]], data = ret_A)
}

# Grafica
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_A[[45]], which = i)
}



# Fondo C -----------------------------------------------------------------


n_modelos = length(model) 
spec1_C = vector(mode = 'list', length = n_modelos)
spec2_C = vector(mode = 'list', length = n_modelos)
spec3_C = vector(mode = 'list', length = n_modelos)
spec4_C = vector(mode = 'list', length = n_modelos)
spec5_C = vector(mode = 'list', length = n_modelos)

for (i in 1:n_modelos) 
  spec1_C[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'std'
  )

for (i in 1:n_modelos)
  spec2_C[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'ged'
  )

for (i in 1:n_modelos)
  spec3_C[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'nig'
  )

for (i in 1:n_modelos)
  spec4_C[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'jsu'
  )

for (i in 1:n_modelos)
  spec5_C[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'norm'
  )

spec_C = c(spec1_C, spec2_C, spec3_C, spec4_C, spec5_C)
n_spec = length(spec_C)



# Estimar modelo
fit_C = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) {
  fit_C[[i]] = ugarchfit(spec = spec_C[[i]], data = ret_C)
}

# Grafica
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_C[[45]], which = i)
}



# Fondo E -----------------------------------------------------------------

n_modelos = length(model) 
spec1_E = vector(mode = 'list', length = n_modelos)
spec2_E = vector(mode = 'list', length = n_modelos)
spec3_E = vector(mode = 'list', length = n_modelos)
spec4_E = vector(mode = 'list', length = n_modelos)
spec5_E = vector(mode = 'list', length = n_modelos)

for (i in 1:n_modelos) 
  spec1_E[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 2)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'std'
  )

for (i in 1:n_modelos)
  spec2_E[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 2)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'ged'
  )

for (i in 1:n_modelos)
  spec3_E[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 2)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'nig'
  )

for (i in 1:n_modelos)
  spec4_E[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 2)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'jsu'
  )

for (i in 1:n_modelos)
  spec5_E[[i]] = ugarchspec(
    mean.model = list(armaOrder = c(1, 2)),
    variance.model = list(
      garchOrder = c(1, 1),
      model = model[i],
      submodel = if (i > 5)
        submodel[i]
      else
        NULL
    ),
    distribution = 'norm'
  )
spec_E = c(spec1_E, spec2_E, spec3_E, spec4_E,spec5_E)
n_spec = length(spec_E)



# Estimar modelo
fit_E = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) {
  fit_E[[i]] = ugarchfit(spec = spec_E[[i]], data = ret_E)
}

# Grafica
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_E[[45]], which = i)
}

