library("zoo")
library("rugarch")
library("xts")
library("ggplot2")
library("psych")
library("tseries")
library("forecast")
#library("foreign")
#library("xtable")
#library("car") 
#library("forecast")
#library("FinTS")

# getwd()
# setwd("C:/Users/patricio.torres/Google Drive/Educaci?n/Magister en Finanzas - Fen Uchile/Cursos/13. Tesis/VC - Spensiones")

ColClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
data_A <- as.xts(read.zoo("C:/Users/patricio.torres/Google Drive/Educación/Magister en Finanzas - Fen Uchile/Cursos/13. Tesis/VC - Spensiones/fondoA2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))
data_A_tmp <- as.xts(read.zoo(xxxx, header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))



data_C <- as.xts(read.zoo("C:/Users/patricio.torres/Google Drive/Educación/Magister en Finanzas - Fen Uchile/Cursos/13. Tesis/VC - Spensiones/fondoC2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))
data_E <- as.xts(read.zoo("C:/Users/patricio.torres/Google Drive/Educación/Magister en Finanzas - Fen Uchile/Cursos/13. Tesis/VC - Spensiones/fondoE2002-2019.csv", header = TRUE, index.column = 1 , sep = ";", tz="", format = "%d-%m-%Y", colClasses = ColClasses))




head(data_A)
colnames(data_A)

# [1] "vc_A_capital"        "vc_A_cuprum"         "vc_A_habitat"        "vc_A_planvital"      "vc_A_provida"        "vc_A_modelo"        
# [7] "vc_A_santa.maria"    "vc_A_magister"       "vp_A_capital"        "vp_A_cuprum"         "vp_A_habitat"        "vp_A_planvital"     
# [13] "vp_A_provida"        "vp_A_modelo"         "vp_A_santa.maria"    "vp_A_magister"       "vp_A_total"          "uf"                 
# [19] "ret_vc_A_capital"    "ret_vc_A_cuprum"     "ret_vc_A_habitat"    "ret_vc_A_planvital"  "ret_vc_A_provida"    "ret_vc_A_modelo"    
# [25] "ret_vc_A_sm"         "ret_vc_A_magister"   "pond_vp_A_capital"   "pond_vp_A_cuprum"    "pond_vp_A_habitat"   "pond_vp_A_planvital"
# [31] "pond_vp_A_provida"   "pond_vp_A_modelo"    "pond_vp_A_sm"        "pond_vp_A_magister"  "ret_pond_A"          "ret_anual_pond_A"   
# [37] "ds_anual_pond_A"     "ret_riesgo_A"     

# plot(merge(data_A[,1],data_C[,1],data_E[,1]))
# summary(data_A)
# plot(merge(data_A[,"ret_pond_A"],data_C[,"ret_pond_C"],data_E[,"ret_pond_E"]))
# plot(merge(data_A[,"ret_riesgo_A"],data_C[,"ret_riesgo_C"],data_E[,"ret_riesgo_E"]))
# plot(data_A[,"ret_pond_A"])
# plot(data_C[,"ret_pond_C"])
# plot(data_E[,"ret_pond_E"])

# plot(merge(data_A[,"ret_anual_pond_A"],data_C[,"ret_anual_pond_C"],data_E[,"ret_anual_pond_E"]))
# plot(merge(data_A[,"ds_anual_pond_A"],data_C[,"ds_anual_pond_C"],data_E[,"ds_anual_pond_E"]))
# plot(merge(data_A[,35],data_A[,36]))

# vp<- as.zoo(merge(data_A[,17],data_C[,17],data_E[,17]))
# tsRainbow <- rainbow(ncol(vp))
# myColors <- c("red", "blue", "green")

# plot(x=as.zoo(merge(data_A[,17],data_C[,17],data_E[,17])/1000000000), xlab="Time", ylab = "MM$ CLP", main = "Valor Patrimonial",col = c("red", "blue", "darkgreen"), screens = 1)
plot(x=as.zoo(merge(data_A[,17],data_C[,17],data_E[,17])/1000000000), xlab="Time", ylab = "MM$ CLP",col = c("red", "blue", "darkgreen"), screens = 1)
legend(x = "topleft", legend = c("A", "C", "E"), lty = 1, col = c("red", "blue", "darkgreen"))

plot(x=as.zoo(merge(data_A[,35],data_C[,35],data_E[,35])), xlab="Time", ylab = c("A", "C", "E"), main = "" , col = c("red", "blue", "darkgreen"), ylim=c(-0.06,0.06))

plot(x=as.zoo(na.omit(merge(data_A[,38],data_C[,38],data_E[,38]))), xlab="Time", ylab = c("A", "C", "E"), main = "Retorno Ajustado por Riesgo", col = c("red", "blue", "darkgreen"), ylim=c(-0.4,0.5))

plot(x=as.zoo(na.omit(merge(data_A[,37],data_C[,37],data_E[,37]))), xlab="Time", ylab = c("A", "C", "E"), main = "DS Anual Ponderada", col = c("red", "blue", "darkgreen"), ylim=c(0,0.0145))


summary(ret_A)
descriptiva <- describe(merge(ret_A,ret_C,ret_E))

jarque.bera.test(ret_A)
jarque.bera.test(ret_C)
jarque.bera.test(ret_E)

jarque.bera.test(ret_A)
jarque.bera.test(ret_C)
jarque.bera.test(ret_E)

adf.test(ret_A)
adf.test(ret_C)
adf.test(ret_E)

testnomr <- jarque.bera.test(na.omit(data_A[,"ret_pond_A"]))
test_tstd <- t.test(na.omit(data_A[,"ret_pond_A"]), mu = 0, alternative = "two.sided")




qqPlot(na.omit(data_A[,"ret_pond_A"]))


qqnorm(na.omit(data_A[,"ret_pond_A"]))
qqline(na.omit(data_A[,"ret_pond_A"]))

hist(data_A[,"ret_pond_A"])
acf(arima_a[["residuals"]]^2)
qplot(na.omit(data_A[,"ret_pond_A"]), geom="histogram") 

#
ggplot(data=data_A, aes(data_A[,"ret_pond_A"])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram Retornos Fondo A") +
  labs(x="retornos", y="Count")
#

#
ggplot(data=data_C, aes(data_C[,"ret_pond_C"])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram Retornos Fondo C") +
  labs(x="retornos", y="Count")
#

#
ggplot(data=ret_E, aes(ret_E[,"ret_pond_E"])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram Retornos Fondo E") +
  labs(x="retornos", y="Count")
#


ggplot(data=data_A, aes(data_A[,"ret_pond_A"])) + 
  geom_histogram(color="black", fill="white")








arima_a <- auto.arima(ret_A, trace=TRUE, test="adf",  ic="bic")
Box.test(arima_a$residuals^2,lag=12, type="Ljung-Box")

arima_c <- auto.arima(ret_C, trace=TRUE, test="adf",  ic="bic")
Box.test(arima_c$residuals^2,lag=12, type="Ljung-Box")

arima_a <- auto.arima(ret_E, trace=TRUE, test="adf",  ic="bic")
Box.test(arima_e$residuals^2,lag=12, type="Ljung-Box")

# otra forma

# fit = autoarfima(data = na.omit(data_A[,"ret_pond_A"]), ar.max = 3, ma.max = 3, criterion = c("AIC","BIC","SIC","HQIC"), method = "full", distribution.model = "std")
arima = autoarfima(data = na.omit(data_A[,"ret_pond_A"]), ar.max = 3, ma.max = 3, criterion = "BIC", method = "full", distribution.model = "std")


##################

ret_a <- na.omit(data_A["20020927/20190729","ret_pond_A"])


serie_a_sgarch_spec <- ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,1)), distribution.model ="std")
serie_a_sgarch_fit <- ugarchfit(spec = serie_a_sgarch_spec, ret_a)

# numderiv.control = list(grad.eps=1e-4, grad.d=0.0001)

ctrl = list(tol = 1e-7, delta = 1e-9)


capital_sgarch_roll <- ugarchroll(serie_a_sgarch_spec, ret_a, 
                                  n.ahead = 1, 
                                  n.start = 494, 
                                  refit.every = 20, 
                                  window.size = NULL, 
                                  refit.window = "moving", 
                                  solver = "gosolnp", 
                                  calculate.VaR = TRUE, 
                                  VaR.alpha = c(0.01), 
                                  keep.coef = TRUE, 
                                  solver.control = ctrl, 
                                  fit.control = list())



# capital_sgarch_roll = resume(capital_sgarch_roll, solver="nlminb")
# One of either “nlminb”, “solnp”, “lbfgs”, “gosolnp”, “nloptr” or “hybrid” (see notes)
# warnings()

report(capital_sgarch_roll, type="VaR", VaR.alpha = 0.01, conf.level = 0.99) 
report(capital_sgarch_roll, type="fpm")

# Kupiec's Unconditional Coverage and the Christoffersen Test
report(capital_sgarch_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)

# Plotting the VaR Limits

plot(capital_sgarch_roll)

capital_sgarch_fcst <- ugarchforecast(capital_sgarch_fit, n.ahead = 12)
capital_sgarch_fcst




###################




ret_a <- na.omit(data_A["20020927/20190729","ret_pond_A"])

model = c('sGARCH', 'gjrGARCH', 'eGARCH', 'apARCH', 'csGARCH', 'fGARCH', 'fGARCH', 'fGARCH')
submodel = c(NA, NA, NA, NA, NA, 'AVGARCH', 'NGARCH', 'NAGARCH')
spec1 = vector(mode = 'list', length = 16)
for (i in 1:8) spec1[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(model = model[i], submodel = if (i > 5) submodel[i] else NULL), distribution = 'sstd')
for (i in 9:16) spec1[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(garchOrder = c(1, 1), model = model[i - 8], submodel = if ((i - 8) > 5) submodel[i - 8] else NULL), distribution = 'norm') 
spec2 = vector(mode = 'list', length = 16)
for (i in 1:8) spec2[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(model = model[i], submodel = if (i > 5) submodel[i] else NULL), distribution = 'sstd')
for (i in 9:16) spec2[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(garchOrder = c(1, 1), model = model[i - 8], submodel = if ((i - 8) > 5) submodel[i - 8] else NULL), distribution = 'sstd')
spec3 = vector(mode = 'list', length = 16)
for (i in 1:8) spec3[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(model = model[i], submodel = if (i > 5) submodel[i] else NULL), distribution = 'nig')
for (i in 9:16) spec3[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(garchOrder = c(1, 1), model = model[i - 8], submodel = if ((i - 8) > 5) submodel[i - 8] else NULL), distribution = 'nig')
spec4 = vector(mode = 'list', length = 16)
for (i in 1:8) spec4[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(model = model[i], submodel = if (i > 5) submodel[i] else NULL), distribution = 'jsu')
for (i in 9:16) spec4[[i]] = ugarchspec(mean.model = list(armaOrder = c(0, 1)), variance.model = list(garchOrder = c(1, 1), model = model[i - 8], submodel = if ((i - 8) > 5) submodel[i - 8] else NULL), distribution = 'jsu')
spec = c(spec1, spec2, spec3, spec4)
cluster = makePSOCKcluster(15)
clusterExport(cluster, c('spec', 'R'))
clusterEvalQ(cluster, library(rugarch))

# Out of sample estimation
n = length(spec)
fitlist = vector(mode = 'list', length = n)
for (i in 1:n) {
  tmp = ugarchroll(spec[[i]], ret_a, n.ahead = 1, n.start = 494, refit.every = 1000, refit.window = 'moving', windows.size = NULL, solver = 'solnp', calculate.VaR = FALSE, keep.coef = FALSE)
  if (!is.null(tmp@model$noncidx)) {
    tmp = resume(tmp, solver = 'nlminb')
    if (!is.null(tmp@model$noncidx))
      fitlist[[i]] = NA
  } else {
    fitlist[[i]] = as.data.frame(tmp, which = 'density')
  }
}


vmodels = c('sGARCH(1,1)', 'gjrGARCH(1,1)', 'eGARCH(1,1)', 'apARCH(1,1)', 'csGARCH(1,1)','AVGARCH(1,1)', 'NGARCH(1,1)', 'NAGARCH(1,1)', 'sGARCH(2,1)', 'gjrGARCH(2,1)','eGARCH(2,1)', 'apARCH(2,1)', 'csGARCH(2,1)', 'AVGARCH(2,1)', 'NGARCH(2,1)','NAGARCH(2,1)')
modelnames = c(paste(vmodels, '-N', sep = ''), paste(vmodels, '-sstd', sep = ''), paste(vmodels, '-nig', sep = ''), paste(vmodels, '-jsu', sep = ''))
q1 = q5 = px = matrix(NA, ncol = 64, nrow = 1500)
dist = c(rep('norm', 16), rep('sstd', 16), rep('nig', 16), rep('jsu', 16))
# use apply since nig and gh distributions are not yet vectorized
for (i in 1:64) {
  q1[, i] = as.numeric(apply(fitlist[[i]], 1, function(x) qdist(dist[i], 0.01, mu = x['Mu'], sigma = x['Sigma'], skew = x['Skew'], shape = x['Shape'])))
  q5[, i] = as.numeric(apply(fitlist[[i]], 1, function(x) qdist(dist[i], 0.05, mu = x['Mu'], sigma = x['Sigma'], skew = x['Skew'], shape = x['Shape'])))
  px[, i] = as.numeric(apply(fitlist[[i]], 1, function(x) pdist(dist[i], x['Realized'], mu = x['Mu'], sigma = x['Sigma'], skew = x['Skew'], shape = x['Shape'])))
}
VaR1cc = apply(q1, 2, function(x) VaRTest(0.01, actual = fitlist[[1]][, 'Realized'], VaR = x)$cc.LRp)
VaR5cc = apply(q5, 2, function(x) VaRTest(0.05, actual = fitlist[[1]][, 'Realized'], VaR = x)$cc.LRp)
BT1 = apply(px, 2, function(x) BerkowitzTest(qnorm(x), tail.test = TRUE, alpha = 0.01)$LRp)
BT5 = apply(px, 2, function(x) BerkowitzTest(qnorm(x), tail.test = TRUE, alpha = 0.05)$LRp)
VTable = cbind(VaR1cc, VaR5cc, BT1, BT5)
rownames(VTable) = modelnames
colnames(VTable) = c('VaR.CC(1%)', 'VaR.CC(5%)', 'BT(1%)', 'BT(5%)')