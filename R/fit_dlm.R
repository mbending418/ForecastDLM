library(dlm)

#' @title dlm.get.build.trend.seas
#' @description
#' returns a build function that builds a DLM with a trend and a seasonal component
#' The build function takes in a vector of parameters parm
#' 
#' @name dlm.get.build.trend.seas
#' 
#' @param data the data to fit the model with
#' 
#' @param seasonal.period the period of the seasonal component
#' 
#' @return returns a build function that takes in a list of 3 parameters as a list and returns a dlm
#' 
#' @export
dlm.get.build.trend.seas <- function(seasonal.period) {
  build.fn <- function(parm) {
    mod <- dlmModPoly(order=1) + dlmModSeas(seasonal.period)
    V(mod) <- exp(parm[1])
    diag(W(mod))[1:2] <- exp(parm[2:3])
    mod
  }
  build.fn
}

#' @title dlm.build.trend.seas
#' @description
#' build a DLM with a trend and a seasonal component
#' 
#' @name dlm.build.trend.seas
#'
#' @param parm a list of three parameters that parameterize the dlm
#' 
#' @param seasonal.period the period of the seasonal component
#' 
#' @return returns a dlm object
#' 
#' @export
dlm.build.trend.seas <- function(parm, seasonal.period) {
  build.fn = dlm.get.build.trend.seas(seasonal.period)
  build.fn(parm)
}

#' @title dlm.fit.trend.seas
#' @description
#' fit a DLM with a trend and seasonal component
#' 
#' @name dlm.fit.trend.seas
#' 
#' @param data the data to fit the model on
#' 
#' @param seasona.period the period of the seasonal component
#'
#' @return returns the MLE fit for the DLM
#' 
#' @export
dlm.fit.trend.seas <- function(data, seasonal.period, hessian=TRUE) {
  dlmMLE(data,
         rep(0, 3),
         build=dlm.get.build.trend.seas(seasonal.period),
         hessian=hessian)
}

#' @title dlm.fit.prealloc
#' @description
#' fit a DLM with a trend and seasonal component while preallocating memory for the DLM matricies
#' 
#' @name dlm.fit.prealloc
#' 
#' @param data the data to fit the model on
#' 
#' @param seasona.period the period of the seasonal component
#'
#' @return returns the MLE fit for the DLM
#' 
#' @export
dlm.fit.prealloc <- function(data, seasonal.period, hessian=TRUE) {
  mod <- dlmModPoly(order=1) + dlmModSeas(seasonal.period)
  build.fn <- function(parm) {
    V(mod) <- exp(parm[1])
    diag(W(mod))[1:2] <- exp(parm[2:3])
    mod
  }
  dlmMLE(data,
         rep(0, 3),
         build=build.fn,
         hessian=hessian)
}

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
