library(forecast)
library(dlm)
library(ForecastDLM)

#prepare data

electricity2016 = read.csv("data/SolarGenAndElectricityDemand2016.csv")
demand2016 = electricity2016$IT_load_new

demand.full = ts(data=demand2016, start=1, frequency=24)
plot(demand.full)

t.five.months = 3648
t.six.months = 4368
demand.full = ts(data=demand2016,
                 start=1,
                 frequency=24)
demand.train = ts(data = demand2016[1:t.five.months],
                  start=1, 
                  frequency=24) 
demand.test = ts(data = demand2016[t.five.months + 1:t.six.months],
                 start=152,
                 end=182,
                 frequency=24)

#show all the demand data on one plot to make sure it's the right regions
plot(demand.full, col="blue")
lines(demand.train, col="red")
lines(demand.test, col="green")
legend(x = "topleft", legend=c("all 12 months", "first 5 months", "month 6"),
       fill = c("blue", "red", "green"))

#FIT BATS

bats.fit = bats(demand.train, lambda=FALSE, use.parallel = FALSE, seasonal.periods=c(24))
plot(residuals(bats.fit))
plot(x=bats.fit$fitted.values, y=residuals(bats.fit))

MSPE.bats.train = sum((residuals(bats.fit)^2)/length(residuals(bats.fitth?sy)))
MAE.bats.train = sum(abs(residuals(bats.fit))/length(residuals(bats.fit)))

bats.forecast = forecast(bats.fit,h=30*24)
plot(bats.forecast)

bats.predicted = bats.forecast$mean
diff.bats = bats.predicted-demand.test
MSPE.bats = sum(diff.bats^2)/length(diff.bats)
MAE.bats = sum(abs(diff.bats))/length(diff.bats)

plot(diff.bats)

#FIT TBATS

tbats.fit = tbats(demand.train, use.box.cox=FALSE, use.parallel = FALSE)
plot(residuals(tbats.fit))
plot(x=tbats.fit$fitted.values, y=residuals(tbats.fit))

MSPE.tbats.train = sum((residuals(bats.fit)^2))/length(residuals(tbats.fit))
MSPE.tbats.train = sum(abs(residuals(tbats.fit)))/length(residuals(tbats.fit))

tbats.forecast = forecast(tbats.fit, h=30*24)
plot(tbats.forecast)

tbats.predicted = tbats.forecast$mean
diff.tbats = tbats.forecast$mean-demand.test
MSPE.tbats = sum(diff.tbats^2)/length(diff.tbats)
MAE.tbats = sum(abs(diff.tbats))/length(diff.tbats)

plot(diff.tbats)


#FIT DLM

dlm.fit = create.fitted.model(data=demand.train, trend.order=1, seasonal.periods=c(24))

dlm.forecast = dlmForecast(dlm.fit$filtered, nAhead =30*24)
dlm.predicted = dlm.forecast$f

diff.dlm = dlm.predicted-demand.test
MSPE.dlm = sum(diff.dlm^2)/length(diff.dlm)
MAE.dlm  = sum(abs(diff.dlm))/length(diff.dlm)
plot(diff.dlm)

plot(unlist(dlm.forecast$f))

#Comparison full

#compare the predictions on plots

plot(demand.test, col="black")
lines(bats.predicted, col="red")
lines(tbats.predicted, col="green")
lines(dlm.predicted, col="blue")
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
  time = system.time(bats(demand.train, lambda=FALSE, use.parallel = FALSE, seasonal.periods=c(24)))
  cpu.time =  unname(time["user.self"])
  bats.total = bats.total + cpu.time
}

tbats.total = 0
for (i in seq(1,n)) {
  time = system.time(tbats(demand.train, lambda=FALSE, use.parallel = FALSE, seasonal.periods=c(24)))
  cpu.time =  unname(time["user.self"])
  tbats.total = tbats.total + cpu.time
}

dlm.total = 0
for (i in seq(1,n)) {
  time = system.time(create.fitted.model(data=demand.train, trend.order=1, seasonal.periods=c(24)))
  cpu.time =  unname(time["user.self"])
  dlm.total = dlm.total + cpu.time
}

table = data.frame(MSPE=c(MSPE.bats, MSPE.tbats, MSPE.dlm),
                   MAE =c( MAE.bats,  MAE.tbats,  MAE.dlm),
                   AvgRunTime = c(bats.total/n, tbats.total/n, dlm.total/n),
                   row.names=c("bats", "tbats", "dlmMLE"))

print(table)
