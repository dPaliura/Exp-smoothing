holt <- function(ts, par1, par2, trend=TRUE){
    n <- length(ts)
    if (n<5){
        stop(paste0("holt: number of observations in time series ",
                    "expected to be not less than 5. ", n, " given."))
    }
    # Get approximate values for a_0 and b_0
    regr <- lm(vals~indx, data=data.frame(vals=ts[1:5], indx=1:5))
    a_prev <- regr$coefficients[1]
    b_prev <- if (trend) regr$coefficients[2] else 0
    if (!trend) par2 <- 0 
    
    control <- NULL
    for (i in 1:n){
        a_nxt <- par1*ts[i] + (1-par1)*(a_prev-b_prev)
        b_nxt <- par2*(a_nxt-a_prev) + (1-par2)*b_prev
        
        control <- c(control, a_prev + b_prev)
        
        a_prev <- a_nxt
        b_prev <- b_nxt
    }
    pars = c(a_nxt, b_nxt)
    names(pars) = c("a", "b")
    return(list(
        a = a_nxt,
        b = b_nxt,
        control = control
    ))
}


holt.predict <- function(holt.mod, horizon=1) holt.mod$a + horizon*holt.mod$b
