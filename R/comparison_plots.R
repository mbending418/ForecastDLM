library(glue)
 
#' @title training.comparison.plots
#' @description
#' show plots for comparing training data to fitted values and residuals
#' @name training.comparison.plots
#' @param data.train training data
#' @param fitted.values fitted values
#' @param residuals residuals
#' @param model.name the name of the model you trained
#' @return NONE
#' 
#' @export
training.comparison.plots <- function(data.train,
                                     fitted.values,
                                     residuals,
                                     model.name) {
  #plot fit during training set
  par(mfrow=c(1,1))
  plot(data.train, col="blue")
  lines(fitted.values, col="red")
  title(glue("{model.name}: Actual and Predicted vs Time (In Training Set)"))
  legend(x = "topleft", legend=c("Actual Training Data", glue("{model.name} Predictions")),
         fill = c("blue", "red"))
  
  invisible(readline(prompt="Press [enter] to continue"))
  
  #training set residual plots
  par(mfrow=c(1,2))
  plot(residuals, col="blue")
  title(glue("{model.name}: Residuals vs. Time \n (In Training Set)"))
  
  plot(x=fitted.values, y=residuals, col="blue")
  title(glue("{model.name}: Residuals vs. Fitted \n (In Training Set)"))

  par(mfrow=c(1,1))  
}

#' @title test.comparison.plots
#' @description
#' show plots for comparing test data to forecast values
#' @name test.comparison.plots
#' @param data.test test data
#' @param forecasted.values forecasted values
#' @param model.name the name of the model you trained
#' @return NONE
#' 
#' @export
test.comparision.plots <- function(data.test,
                                  forecasted.values,
                                  model.name) {
  par(mfrow=c(1,1))
  plot(data.test, col="blue")
  lines(forecasted.values, col="red")
  title(glue("{model.name}: Actual and Predicted vs Time (In Test Set)"))
  legend(x = "topleft", legend=c("Actual Test Data", glue("{model.name} Predictions")),
         fill = c("blue", "red"))
  
  diff = forecasted.values - data.test
  
  invisible(readline(prompt="Press [enter] to continue"))
  
  par(mfrow=c(1,2))
  plot(diff, col="blue")
  title(glue("{model.name}: Errors vs Time \n (In Test Set)"))
  
  plot(forecasted.values, diff, col="blue")
  title(glue("{model.name}: Errors vs. Predicted \n (In Test Set)"))
  
  par(mfrow=c(1,1))
}
