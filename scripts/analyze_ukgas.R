#run this code if you don't have ForecastDLM installed
#devtools::install_github("mbending418/ForecastDLM")

library(forecast)
library(dlm)
library(ForecastDLM)

#prepare data

lGas <- log(UKgas)
data.full  <- ts(lGas, start=c(1960, 1), frequency=4)
data.train <- ts(lGas[1:96], start = c(1960, 1), end = c(1983,4), frequency = 4)
data.test  <- ts(lGas[97:length(lGas)], start = c(1984,1), end=c(1986,4), frequency=4)


#show all the data on one plot to make sure it's the right regions
plot(data.full, col="blue")
lines(data.train, col="red")
lines(data.test, col="green")
legend(x = "topleft", legend=c("first 20 years (train)", "next 6 years (test)"),
       fill = c("red", "green"))
title("Training/Test Split")

#we want to forecast one month ahead
nAhead = length(data.test)

#FIT BATS

bats.fit = bats(data.train, use.box.cox = FALSE, use.parallel = FALSE, use.arma.errors = FALSE, seasonal.periods=c(24))
bats.results = get.bats.results(bats.fit, nAhead=nAhead)

training.comparison.plots(data.train=data.train,
                         fitted.values=bats.results$fitted.values,
                         residuals=bats.results$residuals,
                         model.name="BATS")

test.comparision.plots(data.test=data.test,
                      forecasted.values=bats.results$forecast,
                      model.name="BATS")

#calculate training MSE and MAE
MSE.bats.train = get.mse(bats.results$residuals)
MAE.bats.train = get.mae(bats.results$residuals)

#calculate testing MSPE and MAE
diff.bats = bats.results$forecast - data.test
MSPE.bats = get.mse(diff.bats)
MAE.bats = get.mae(diff.bats)

#FIT TBATS

tbats.fit = tbats(data.train, use.box.cox = FALSE, use.parallel = FALSE, use.arma.errors = FALSE, seasonal.periods=c(24))
tbats.results = get.tbats.results(tbats.fit, nAhead=nAhead)

training.comparison.plots(data.train=data.train,
                         fitted.values=tbats.results$fitted.values,
                         residuals=tbats.results$residuals,
                         model.name="TBATS")

test.comparision.plots(data.test=data.test,
                      forecasted.values=tbats.results$forecast,
                      model.name="TBATS")

#calculate training MSE and MAE
MSE.tbats.train = get.mse(tbats.results$residuals)
MAE.tbats.train = get.mae(tbats.results$residuals)

#calculate testing MSPE and MAE
diff.tbats = tbats.results$forecast - data.test
MSPE.tbats = get.mse(diff.tbats)
MAE.tbats = get.mae(diff.tbats)

#FIT DLM

dlm.fit = dlm.fit.trend.seas(data.train, seasonal.period=4, hessian=FALSE)
dlm.parms = dlm.fit$par
dlm.mod = dlm.build.trend.seas(dlm.parms, seasonal.period=4)

dlm.results = get.dlm.results(fit=dlm.mod, data=data.train, nAhead=nAhead)

training.comparison.plots(data.train=data.train,
                         fitted.values = dlm.results$fitted.values,
                         residuals = dlm.results$residuals,
                         model.name="DLM")

test.comparision.plots(data.test=data.test,
                      forecasted.values = dlm.results$forecast,
                      model.name="DLM")

#calculate training MSE and MAE
MSE.dlm.train = get.mse(dlm.results$residuals)
MAE.dlm.train = get.mae(dlm.results$residuals)

#calculate testing MSPE and MAE
diff.dlm = dlm.results$forecast - data.test
MSPE.dlm = get.mse(diff.dlm)
MAE.dlm = get.mae(diff.dlm)

#Comparison full

#compare the predictions on plots

par(mfrow=c(1,1))

plot(data.test, col="black")
lines(bats.results$forecast, col="red")
lines(tbats.results$forecast, col="green")
lines(dlm.results$forecast, col="blue")
legend(x = "topleft", legend=c("test.set", "bats", "tbats", "dlmMLE"),
       fill = c("black", "red", "green", "blue"))

#compare "residuals"
plot(diff.bats, col="red")
lines(diff.tbats, col="green")
lines(diff.dlm, col="blue")
legend(x = "topleft", legend=c("bats", "tbats", "dlmMLE"),
       fill = c("red", "green", "blue"))

#compare run times
n = 10

bats.total = 0
for (i in seq(1,n)) {
  time = system.time(bats(data.train, use.box.cox = FALSE, use.parallel = FALSE, use.arma.errors = FALSE, seasonal.periods=c(24)))
  cpu.time =  unname(time["user.self"])
  bats.total = bats.total + cpu.time
}

tbats.total = 0
for (i in seq(1,n)) {
  time = system.time(tbats(data.train, use.box.cox = FALSE, use.parallel = FALSE, use.arma.errors = FALSE, seasonal.periods=c(24)))
  cpu.time =  unname(time["user.self"])
  tbats.total = tbats.total + cpu.time
}

dlm.total = 0
for (i in seq(1,n)) {
  time = system.time(dlm.fit.trend.seas(data.train, seasonal.period=24))
  cpu.time =  unname(time["user.self"])
  dlm.total = dlm.total + cpu.time
}

table = data.frame(TrainMSE=c(MSE.bats.train,MSE.tbats.train, MSE.dlm.train),
                   TRAINMAE=c(MAE.bats.train, MAE.tbats.train, MAE.dlm.train),
                   TestMSPE=c(MSPE.bats, MSPE.tbats, MSPE.dlm),
                   TestMAE =c( MAE.bats,  MAE.tbats,  MAE.dlm),
                   #AvgRunTime = c(bats.total/n, tbats.total/n, dlm.total/n),
                   row.names=c("bats", "tbats", "dlmMLE"))

print(table)
