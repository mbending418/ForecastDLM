#run this code if you don't have ForecastDLM installed
#devtools::install_github("mbending418/ForecastDLM")

library(forecast)
library(dlm)
library(ForecastDLM)

#prepare data

electricity2016 = read.csv("data/SolarGenAndElectricityDemand2016.csv")
demand2016 = electricity2016$IT_load_new

t.five.months = 3648
t.six.months = 4368
data.full = ts(data=demand2016[1:t.six.months],
                 start=1,
                 frequency=24)
data.train = ts(data = demand2016[1:t.five.months],
                  start=1, 
                  frequency=24) 
data.test = ts(data = demand2016[t.five.months+1:t.six.months],
                 start=153,
                 end=183,
                 frequency=24)

#show all the data on one plot to make sure it's the right regions
plot(data.full, col="blue")
lines(data.train, col="red")
lines(data.test, col="green")
legend(x = "topleft", legend=c("all 6 months", "first 5 months (train)", "month 6 (test)"),
       fill = c("blue", "red", "green"))
title("Training/Test Split")

#we want to forecast one month ahead
nAhead = 30*24

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
dlm.fit = dlm.fit.trend.seas(data.train, seasonal.period=24)
dlm.parms = dlm.fit$par
dlm.mod = dlm.build.trend.seas(dlm.parms, seasonal.period=24)

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
