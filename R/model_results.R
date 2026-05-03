library(dlm)
library(forecast)

#' @title get.bats.results
#' @description
#' produces fitted values, predicted values, etc for the BATS model
#' @name get.bats.results
#' @param fit the fitted bats model
#' @param nAhead how many values ahead to forecast
#' @return results as a named list with fields $fitted.values, $residuals, and optionally $forecast
#' @export
get.bats.results <- function(fit, nAhead=NULL) {
  if (is.null(nAhead)) {
    list(fitted.values=fit$fitted.values,
         residuals = residuals(fit)) 
  } else {
    forecasted.values = forecast(fit, h=nAhead)$mean
    list(fitted.values=fit$fitted.values,
         residuals = residuals(fit),
         forecast = forecasted.values)
  }
}

#' @title get.tbats.results
#' @description
#' produces fitted values, predicted values, etc for the TBATS model
#' @name get.tbats.results
#' @param fit the fitted tbats model
#' @param nAhead how many values ahead to forecast
#' @return results as a named list with fields $fitted.values, $residuals, and optionally $forecast 
#' @export
get.tbats.results <- function(fit, nAhead=NULL) {
  if (is.null(nAhead)) {
    list(fitted.values=fit$fitted.values,
         residuals = residuals(fit)) 
  } else {
    forecasted.values = forecast(fit, h=nAhead)$mean
    list(fitted.values=fit$fitted.values,
         residuals = residuals(fit),
         forecast = forecasted.values)
  }
}

#' @title get.dlm.results
#' @description
#' produces fitted values, predicted values, etc for a DLM model
#' @name get.dlm.results
#' @param fit the fitted dlm model
#' @param data the training data for the dlm
#' @param nAhead how many values ahead to forecast
#' @return results as a named list with fields $fitted.values, $residuals, and optionally $forecast 
#' @export
get.dlm.results <- function(fit, data, nAhead=NULL) {
  filtered = dlmFilter(y=data, mod=fit)
  if (is.null(nAhead)) {
    list(fitted.values=filtered$f,
         residuals = residuals(filtered, sd=FALSE)) 
  } else {
    list(fitted.values=filtered$f,
         residuals = residuals(filtered, sd=FALSE),
         forecast = dlmForecast(filtered, nAhead=nAhead)$f)
  }
}

#' @title get.mse
#' @description
#' take in a list of errors and return the mean squared error
#' @name get.mse
#' @param errors the error values
#' @return the MSE
#' @export
get.mse <- function(errors) sum(errors^2)/length(errors)

#' @title get.mae
#' @description
#' take in a list of errors and return the mean absolute error
#' @name get.mae
#' @param errors the error values
#' @return the MAE
#' @export
get.mae <- function(errors) sum(abs(errors))/length(errors)
