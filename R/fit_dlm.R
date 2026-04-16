library(dlm)

#' @title create.fitted.model
#' @description
#' fit a DLM model on data with trend and seasonal components
#' 
#' @name create.fitted.model
#' 
#' @param data the data fit the model with
#' 
#' @param trend.order the order of the trend (set to NULL or 0 for no trend)
#' 
#' @param seasonal.periods the period of the seasonal components as a list
#' 
#' @return returns a "fitted.dlm" object
#' 
#' @export
create.fitted.model <- function(data,
                                trend.order = NULL,
                                seasonal.periods= NULL) {
    create.dlm <- function() {
      if (is.null(trend.order) || trend.order == 0) {
        mod = NULL
      } else {
        mod = dlmModPoly(order=trend.order)
      }
      for (s in seasonal.periods) {
        if (is.null(mod)) {
          mod = dlmModSeas(s)
        } else {
          mod = mod + dlmModSeas(s)
        }
      }
      mod
    }
    
    build.fn <- function(param) {
      if (is.null(trend.order) || trend.order == 0) {
        W.diag = c()
        p.index = 1
      } else {
        W.diag = c(exp(param[2:(1+trend.order)]))
        p.index = 2+trend.order
      }
      
      for (s in seasonal.periods) {
        W.diag = c(W.diag, exp(param[p.index]), rep(0, s-2))
        p.index = p.index + 1
      }
      
      mod = create.dlm()
      V(mod) <- exp(param[1])
      diag(W(mod)) <- W.diag
      return(mod)
    }
    if (is.null(trend.order)) {
      trend.order.number = 0
    } else {
      trend.order.number = trend.order
    }
    n.coef = 1 + trend.order.number + length(seasonal.periods)
    fit <- dlmMLE(data, rep(0, n.coef), build=build.fn, hessian=TRUE)
    
    loglik <- dlmLL(data, create.dlm())
    fit.mod <- build.fn(fit$par)
    
    filtered <- dlmFilter(data, mod=fit.mod)
    smoothed <- dlmSmooth(filtered)
    resids <- residuals(filtered, sd=FALSE)
    
    model <- list(
      data = data,
      trend.order = trend.order,
      seasonal.periods = seasonal.periods,
      mod = fit.mod,
      obs.error.var = V(fit.mod),
      state.error.var = diag(W(fit.mod)),
      
      n.coef = n.coef,
      loglik = loglik,
      aic = (2 * (loglik)) + 2 * (sum(n.coef)),
      bic = (2 * (loglik)) + (log(length(data))) * (n.coef),
      
      filtered = filtered,
      smoothed = smoothed,
      resids = resids
    )
    
    class(model) <- "fitted.dlm"
    model
}

#' @title create.fitted.st.model
#' @description
#' fit a DLM model on data with 1st order trend and a single seasonal component
#' 
#' @name create.fitted.st.model
#' 
#' @param data the data fit the model with
#' 
#' @param seasonal.period the period of the seasonal component
#' 
#' @return returns a "fitted.dlm" object
#' 
#' @export
create.fitted.st.model <- function(data, seasonal.period) {
  build.fn <- function(parm) {
    mod <- dlmModPoly(order = 1) + dlmModSeas(seasonal.period)
    V(mod) <- exp(parm[1])
    diag(W(mod))[1:2] <- exp(parm[2:3])
    return(mod)
  }
  n.coef = 3
  fit <- dlmMLE(data, rep(0, n.coef), build=build.fn, hessian=TRUE)
  loglik <- dlmLL(data, dlmModPoly(1) + dlmModSeas(4))
  mod <- build.fn(fit$par)
  
  filtered <- dlmFilter(data, mod=mod)
  smoothed <- dlmSmooth(filtered)
  resids <- residuals(filtered, sd=FALSE)
  
  model <- list(
    data = data,
    mod = mod,
    obs.error.var = V(mod),
    state.error.var = diag(W(mod)),
    
    n.coef = n.coef,
    loglik = loglik,
    aic = (2 * (loglik)) + 2 * (sum(n.coef)),
    bic = (2 * (loglik)) + (log(length(data))) * (n.coef),
    
    filtered = filtered,
    smoothed = smoothed,
    resids = resids
  )
  
  class(model) <- "fitted.dlm"
  model
}