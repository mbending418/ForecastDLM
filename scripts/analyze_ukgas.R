#run this code if you don't have tsm installed
#devtools::install_github("KevinKotze/tsm")

#run this code if you don't have ForecastDLM installed
#devtools::install_github("mbending418/ForecastDLM")

library(ForecastDLM)
library(dlm)
library(tsm)

lGas <- ts(log(UKgas), start = c(1960, 2), frequency = 4)
plot(lGas)

#fit model
model = create.fitted.model(data=lGas, trend.order=1, seasonal.periods=c(12))

#plot results
y.hats = model$data - model$resids #I'm not sure if this is right

par(mfrow = c(2, 2))

plot.ts(model$data, col="blue", xlab = "t", ylab = "y", lwd = 2)
legend("topright", legend = c("observed values (y)"))

plot.ts(model$resids, col="blue", ylab="e.hat", xlab="t", lwd=2)
abline(h=0)
legend("topright", legend = "Residuals", lwd=2, bty="n", col="blue")

plot.ts(y.hats, col="blue", ylab="y.hat", xlab="t", lwd=2)
legend("topright", legend = "Y Hat Values", lwd=2, bty="n")

plot(as.vector(y.hats), as.vector(model$resids), col="blue", xlab="y.hats", ylab="resids")
legend("topleft", legend = "Resids vs Y Hats", lwd=2, bty="n")

#plot the smoothed and filtered values
par(mfrow = c(1,1))
plot.ts(model$smoothed$s[,1], main="Smoothed")

plot.ts(model$filtered$a[,1], main="Filtered")

#residual historgram
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
hist(model$resids, prob = TRUE, col = "grey", main = "", breaks = seq(-4.5, 
                                                                      7, length.out = 30))

ac(model$resids)

#some tests
Box.test(model$resids, lag = 12, type = "Ljung", fitdf = 2)

shapiro.test(model$resids)  # normality

