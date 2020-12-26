theil.wage <- function(ts, period, par1, par2, par3, trend=TRUE){
    n <- length(ts)
    s <- period
    
    regr <- lm(vals~indx, data=data.frame(vals=ts[1:period], indx=1:s))
    a_prev <- regr$coefficients[1]
    b_prev <- if (trend) regr$coefficients[2] else 0
    if (!trend) par3 <- 0
    
    regr2 <- lm(vals~indx, data=data.frame(vals=ts[(s+1):(2*s)], indx=(s+1):(2*s)))
    theta <- ((ts[1:s]-predict(regr)) + (ts[(s+1):(2*s)]-predict(regr2)))/2
    
    control <- NULL
    for (i in 1:n){
        a_nxt <- par1*(ts[i]-theta[i]) + (1-par1)*(a_prev-b_prev)
        b_nxt <- par3*(a_nxt-a_prev) + (1-par3)*b_prev
        theta_nxt <- par2*(ts[i]-a_nxt) + (1-par2)*theta[i]
        
        control <- c(control, a_prev + b_prev + theta_nxt)
        
        a_prev <- a_nxt
        b_prev <- b_nxt
        theta <- c(theta, theta_nxt)
    }
    return(list(
        a = a_nxt,
        b = b_nxt,
        theta = theta[(n+1):(n+s)],
        period = s,
        control = control
    ))
}


theil.wage.predict <- function(tw.mod, horizon=1){
    tw.mod$a + horizon*tw.mod$b + tw.mod$theta[(horizon-1)%%tw.mod$period+1]
}
