library(dlm)
library(ForecastDLM)

n = 10

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

time.general = 0
for (i in seq(1,n)) {
  time = system.time(create.fitted.model(data.train, trend.order = 1, seasonal.periods=c(seasonal.period)))
  cpu.time =  unname(time["user.self"])
  time.general = time.general + cpu.time
}
(time.general=time.general/n)

time.normal = 0
for (i in seq(1,n)) {
  time = system.time(dlm.fit.trend.seas(data.train, seasonal.period=seasonal.period))
  cpu.time =  unname(time["user.self"])
  time.normal = time.normal + cpu.time
}
(time.normal=time.normal/n)

time.prealloc = 0
for (i in seq(1,n)) {
  time = system.time(dlm.fit.prealloc(data.train, seasonal.period=seasonal.period))
  cpu.time =  unname(time["user.self"])
  time.prealloc = time.prealloc + cpu.time
}
(time.prealloc=time.prealloc/n)

table = data.frame(create.fitted.model=c(time.general),
                   dlm.fit.trend.seas=c(time.normal),
                   dlm.fit.prealloc=c(time.prealloc),
                   row.names=c("time"))
print(table)
