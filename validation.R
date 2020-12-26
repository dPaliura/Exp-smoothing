check.ts <- function(values, prints=FALSE){
    if (length(values)<5) {
        if (prints) cat("At least 5 elements expected in time series.\n")
        return(FALSE)
    }
    if (!is.numeric(values)){
        cat("Time series must be numeric.\n")
        return(FALSE)
    }
    if (any(!is.finite(values))){
        cat("Time series mustn't contain non-finite values (NA/NaN/Inf...).\n")
        return(FALSE)
    }
    return(TRUE)
}


check.single.numeric <- function(value, prints=FALSE){
    if (length(value)!=1) return(FALSE)
    
    tryCatch(
        expr = {
            num <- NA; num <- as.numeric(value)
        },
        error = function(cond){
            if (prints) {
                cat("Error occured on convertation into numeric",
                    "\nOriginal message:\n")
                print(cond)
            }
        },
        warning = function(cond){
            if (prints) {
                cat("Warning occured on convertation into numeric",
                    "\nOriginal message:\n")
                print(cond)
            }
        },
        finally = {return(ifelse(is.numeric(num), TRUE, FALSE))},
        silent = TRUE
    )
    
}


check.param <- function(value, prints=FALSE){
    if (!check.single.numeric(value)){
        if (prints) cat("Invalid number\n")
        return(FALSE)
    }
    
    num <- as.numeric(value)
    
    if (!is.finite(num)){
        if (prints) cat("Parameter must be finite (from 0 to 1)\n")
        return(FALSE)
    }
    if (num<0 | num>1){
        if (prints) cat("Parameter must be in range [0, 1]\n")
        return(FALSE)
    }
    return(TRUE)
}


check.integer <- function(value, prints=FALSE){
    if (!check.single.numeric(value)){
        if (prints) cat("Invalid value\n")
        return(FALSE)
    }
    
    num <- as.numeric(value)
    
    if (!is.finite(num)){
        if (prints) cat("Value must be finite (from 0 to 1)\n")
        return(FALSE)
    }
    if (num %% 1){
        if (prints) cat("Value must have no decimal or zero decimal\n")
        return(FALSE)
    }
    return(TRUE)
}
