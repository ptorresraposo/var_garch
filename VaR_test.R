# Condicional
# no condicional
# resuem + fpm
# VaR Loss
# VaR test
# Var Dur(ation)


report(roll_E[[31]], type="fpm")
report(roll_E[[27]], type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
report(roll_A_norm[[3]])
plot(roll_E[[27]])

plot(roll_A[[5]])

var_A <- as.data.frame(roll_A_total[[1]]@forecast[["VaR"]])
for (i in 1:45) 
  var_A[,i+2] <- as.data.frame(roll_A_total[[i]]@forecast[["VaR"]][["alpha(1%)"]])
var_A <- as.xts(var_A[,2:47])
colnames(var_A) <- c("realized", "GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_C <- as.data.frame(roll_C_total[[1]]@forecast[["VaR"]])
for (i in 1:45) 
  var_C[,i+2] <- as.data.frame(roll_C_total[[i]]@forecast[["VaR"]][["alpha(1%)"]])
var_C <- as.xts(var_C[,2:47])
colnames(var_C) <- c("realized", "GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_E <- as.data.frame(roll_E_total[[1]]@forecast[["VaR"]])
for (i in 1:45) 
  var_E[,i+2] <- as.data.frame(roll_E_total[[i]]@forecast[["VaR"]][["alpha(1%)"]])
var_E <- as.xts(var_E[,2:47])
colnames(var_E) <- c("realized", "GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")


# print(VaRDurTest(0.05, actual, VaR))

var_dur_A <- as.data.frame(VaRDurTest(0.01, var_A[,"realized"], var_A[,2],0.99),stringsAsFactors=FALSE)
i=1
for (i in 2:46) 
  var_dur_A[i,] <- as.data.frame(VaRDurTest(0.01, var_A[,"realized"],var_A[,i+1],0.99),stringsAsFactors=FALSE)
rownames(var_dur_A) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_dur_C <- as.data.frame(VaRDurTest(0.01, var_C[,"realized"], var_C[,2],0.99),stringsAsFactors=FALSE)
i=1
for (i in 2:46) 
  var_dur_C[i,] <- as.data.frame(VaRDurTest(0.01, var_C[,"realized"],var_C[,i+1],0.99),stringsAsFactors=FALSE)
rownames(var_dur_C) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_dur_E <- as.data.frame(VaRDurTest(0.01, var_E[,"realized"], var_E[,2],0.99),stringsAsFactors=FALSE)
i=1
for (i in 2:46) 
  var_dur_E[i,] <- as.data.frame(VaRDurTest(0.01, var_E[,"realized"],var_E[,i+1],0.99),stringsAsFactors=FALSE)
rownames(var_dur_C) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")



# VaRTest(alpha = 0.01, actual, var, conf.level = 0.99)
var_test_A <- as.data.frame(VaRTest(alpha = 0.01, var_A[,"realized"], var_A[,2], conf.level = 0.99),stringsAsFactors=FALSE)
for (i in 2:46) 
var_test_A[i,] <- as.data.frame(VaRTest(alpha = 0.01, var_A[,"realized"], var_A[,i+1], conf.level = 0.99),stringsAsFactors=FALSE)
rownames(var_test_A) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_test_C <- as.data.frame(VaRTest(alpha = 0.01, var_C[,"realized"], var_C[,2], conf.level = 0.99),stringsAsFactors=FALSE)
for (i in 2:46) 
  var_test_C[i,] <- as.data.frame(VaRTest(alpha = 0.01, var_C[,"realized"], var_C[,i+1], conf.level = 0.99),stringsAsFactors=FALSE)
rownames(var_test_C) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")

var_test_E <- as.data.frame(VaRTest(alpha = 0.01, var_E[,"realized"], var_E[,2], conf.level = 0.99),stringsAsFactors=FALSE)
for (i in 2:46) 
  var_test_E[i,] <- as.data.frame(VaRTest(alpha = 0.01, var_E[,"realized"], var_E[,i+1], conf.level = 0.99),stringsAsFactors=FALSE)
rownames(var_test_E) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")



sel <- c("actual.exceed","uc.LRstat","uc.Decision","cc.LRstat","cc.Decision")
var_test_A_res <- var_test_A[sel]
var_test_A_res[,6] <- (var_test_A_res[,1]/3922)
names(var_test_A_res)
colnames(var_test_A_res) <- c("N° Excep", "UC (Kupiec)","H0 UC","CC (Christoffersen)","H0 CC","% Excep")
xtable(var_test_A_res)

sel <- c("actual.exceed","uc.LRstat","uc.Decision","cc.LRstat","cc.Decision")
var_test_C_res <- var_test_C[sel]
var_test_C_res[,6] <- (var_test_C_res[,1]/3922)
names(var_test_C_res)
colnames(var_test_C_res) <- c("N° Excep", "UC (Kupiec)","H0 UC","CC (Christoffersen)","H0 CC","% Excep")
xtable(var_test_C_res)

sel <- c("actual.exceed","uc.LRstat","uc.Decision","cc.LRstat","cc.Decision")
var_test_E_res <- var_test_E[sel]
var_test_E_res[,6] <- (var_test_E_res[,1]/3922)
names(var_test_E_res)
colnames(var_test_E_res) <- c("N° Excep", "UC (Kupiec)","H0 UC","CC (Christoffersen)","H0 CC","% Excep")
xtable(var_test_E_res)

# write.xlsx(var_test_A_res, file = "var_test_A.xlsx", sheetName = "A", append = FALSE)


#####
par(mfrow = c(2, 2))
plot(fit_A[[4]], which = 8)
#plot(fit_A[[4]], which = 9)
plot(fit_A[[9]], which = 8)
plot(fit_A[[1]], which = 8)
plot(fit_A[[1]], which = 8)

par(mfrow = c(2, 2))
plot(fit_C[[5]], which = 8)
plot(fit_C[[4]], which = 8)
plot(fit_C[[8]], which = 8)
plot(fit_C[[6]], which = 8)


par(mfrow = c(2, 2))
plot(fit_E[[21]], which = 8) #gjr
# plot(fit_E[[21]], which = 9) #gjr
plot(fit_E[[19]], which = 8) #garch
plot(fit_E[[26]], which = 8)
plot(fit_E[[27]], which = 8)



#
par(mfrow = c(1, 4))
plot(fit_E[[4]], which = 8)
plot(fit_E[[13]], which = 8)
plot(fit_E[[22]], which = 8)
plot(fit_E[[31]], which = 8)



# varloss

var_loss_A <- as.data.frame(sum(VaRloss(0.01, var_A[,"realized"],var_A[,2])))
for (i in 2:46) 
  var_loss_A[i,] <- as.data.frame(sum(VaRloss(0.01, var_A[,"realized"],var_A[,i+1])),stringsAsFactors=FALSE)
rownames(var_loss_A) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")
colnames(var_loss_A) <- c("VaR loss")


var_loss_C <- as.data.frame(sum(VaRloss(0.01, var_C[,"realized"],var_C[,2])))
for (i in 2:46) 
  var_loss_C[i,] <- as.data.frame(sum(VaRloss(0.01, var_C[,"realized"],var_C[,i+1])),stringsAsFactors=FALSE)
rownames(var_loss_C) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")
colnames(var_loss_C) <- c("VaR loss")

var_loss_E <- as.data.frame(sum(VaRloss(0.01, var_E[,"realized"],var_E[,2])))
for (i in 2:46) 
  var_loss_E[i,] <- as.data.frame(sum(VaRloss(0.01, var_E[,"realized"],var_E[,i+1])),stringsAsFactors=FALSE)
rownames(var_loss_E) <- c("GARCH(1,1)-std",	"eGARCH(1,1)-std",	"gjrGARCH(1,1)-std",	"apARCH(1,1)-std",	"iGARCH(1,1)-std",	" TGARCH(1,1)-std",	" AVGARCH(1,1)-std",	" NGARCH(1,1)-std",	" NAGARCH(1,1)-std",	"GARCH(1,1)-ged",	"eGARCH(1,1)-ged",	"gjrGARCH(1,1)-ged",	"apARCH(1,1)-ged",	"iGARCH(1,1)-ged",	" TGARCH(1,1)-ged",	" AVGARCH(1,1)-ged",	" NGARCH(1,1)-ged",	" NAGARCH(1,1)-ged",	"GARCH(1,1)-nig",	"eGARCH(1,1)-nig",	"gjrGARCH(1,1)-nig",	"apARCH(1,1)-nig",	"iGARCH(1,1)-nig",	" TGARCH(1,1)-nig",	" AVGARCH(1,1)-nig",	" NGARCH(1,1)-nig",	" NAGARCH(1,1)-nig",	"GARCH(1,1)-jsu",	"eGARCH(1,1)-jsu",	"gjrGARCH(1,1)-jsu",	"apARCH(1,1)-jsu",	"iGARCH(1,1)-jsu",	" TGARCH(1,1)-jsu",	" AVGARCH(1,1)-jsu",	" NGARCH(1,1)-jsu",	" NAGARCH(1,1)-jsu","GARCH(1,1)-norm",	"eGARCH(1,1)-norm",	"gjrGARCH(1,1)-norm",	"apARCH(1,1)-norm",	"iGARCH(1,1)-norm",	" TGARCH(1,1)-norm",	" AVGARCH(1,1)-norm",	" NGARCH(1,1)-norm",	" NAGARCH(1,1)-norm")
colnames(var_loss_E) <- c("VaR loss")

var_test_a_res2 <- var_test_A_res
var_test_a_res2[,7] <- var_loss_A

var_test_C_res2 <- var_test_C_res
var_test_C_res2[,7] <- var_loss_C

var_test_E_res2 <- var_test_E_res
var_test_E_res2[,7] <- var_loss_E




plot(roll_A_total[[4]], which=1)
plot(roll_A_total[[4]])
report(roll_A_total[[4]], type ='fpm')