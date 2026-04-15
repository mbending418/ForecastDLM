create.fitted.model <- function(data,
                                trend.order = NULL,
                                seasonal.periods= NULL) {
  
    build.fn <- function(param) {
      if (is.null(trend.order) || trend.order == 0) {
        mod = NULL
        W.diag = c()
        p.index = 1
      } else {
        mod = dlmModPoly(order = trend.order)
        W.diag = c(exp(param[2:(1+trend.order)]))
        p.index = 2+trend.order
      }
      
      for (s in seasonal.periods) {
        if (is.null(mod)) {
          mod = dlmModSeas(frequency=s)
        } else {
          mod = mod + dlmModSeas(frequency=s)
        }
        
        W.diag = c(W.diag, exp(param[p.index]), rep(0, s-2))
        p.index = p.index + 1
      }
      
      V(mod) <- exp(param[1])
      diag(W(mod)) <- W.diag
      return(mod)
    }
    if (is.null(trend.order)) {
      trend.order.number = 0
    } else {
      trend.order.number = trend.order
    }
    n.coef = trend.order.number + sum(seasonal.periods) - length(seasonal.periods) - 1
    fit <- dlmMLE(data, rep(0, n.coef), build=build.fn, hessian=TRUE)
    
    if (trend.order.number == 0) {
      mod = NULL
    } else {
      mod = dlmModPoly(trend.order.number)
    }
    
    for (s in seasonal.periods) {
      if (is.null(mod)) {
        mod = dlmModSeas(s)
      } else {
        mod = mod + dlmModSeas(s)
      }
    }
    
    loglik <- dlmLL(data, dlmModPoly(1) + dlmModSeas(4))
    fit.mod <- build.fn(fit$par)
    
    filtered <- dlmFilter(data, mod=fit.mod)
    smoothed <- dlmSmooth(filtered)
    resids <- residuals(filtered, sd=FALSE)
    
    model <- list(
      data = data,
      trend.order = trend.order,
      seasonal.periods = seasonal.periods,
      mod = fit.mod,
      obs.error.var = V(mod),
      state.error.var = diag(W(mod)),
      
      n.coef = n.coef,
      loglik = loglik,
      aic = (2 * (loglik)) + 2 * (sum(n.coef)),
      bic = (2 * (loglik)) + (log(length(data))) * (n.coef),
      
      filtered = filtered,
      smoothed = smoothed,
      resids = resids,
    )
    
    class(model) <- "fitted.dlm"
    model
}

create.fitted.st.model <- function(data) {
  build.fn <- function(parm) {
    mod <- dlmModPoly(order = 1) + dlmModSeas(frequency = 4)
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
    resids = resids,
  )
  
  class(model) <- "fitted.dlm"
  model
}