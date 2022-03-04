
prv <- fromJSON(txt="https://fintual.cl/api/asset_providers")
ca <- fromJSON(txt="https://fintual.cl/api/conceptual_assets")
View(ca)


# model = c('sGARCH','eGARCH','gjrGARCH','apARCH','iGARCH','fGARCH','fGARCH','fGARCH','fGARCH')
# submodel = c(NA, NA, NA, NA, NA, 'TGARCH', 'AVGARCH', 'NGARCH', 'NAGARCH')  
#distr= c('norm', 'std', 'ged', 'nig', 'jsu')