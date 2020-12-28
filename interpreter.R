interpret <- function(input, result){
    tb <- "\t"
    nl <- "\n"
    sp <- " "
    
    ts.size <- length(input$ts)
    pred.size <- input$forecast.length
    
    cat(paste0(tb, tb, "RESULT", nl,
               tb, result$method, sp, "model",
               nl,nl,
               "Input:", nl,
               "time-series of length", sp, ts.size, sp,
               ifelse(input$is.trended, "with", "without"),
               sp, "trend and", sp,
               ifelse(input$is.seasonal, 
                      paste0("with seasonal component of period", sp,
                             input$season.period), 
                      "without seasonal component"), nl,
               "alpha =", sp, input$alpha, sp, "- smooth parameter", nl,
               ifelse(input$is.trended,
                      paste0("beta =", sp, input$beta, sp, 
                             "- trend parameter", nl),
                      ""),
               ifelse(input$is.seasonal,
                      paste0("theta =", sp, input$theta, sp, 
                             "- seasonal parameter"),
                      ""),
               nl, nl))
    
    cat(paste0("Output:", nl,
               "a =", sp, result$a, sp, "- smoothing component", nl,
               "b =", sp, result$b, sp, "- trend component", nl,
               ifelse(input$is.seasonal,
                      paste0("theta (last period):", nl,
                             paste(result$theta, collapse=", "),
                             nl),
                      ""),
               "Predicted next", sp, pred.size, sp, "values:", nl,
               paste0(result$predict, collapse=", "), nl,
               "Max absolute error", sp,
               max(abs(input$ts-result$control)), nl,
               "See plot to view result visualisation",
               nl, nl))
    
    ltype <- ifelse(ts.size>30, 'l', "b")
    
    ts.pred <- c(input$ts, result$predict)
    
    plot(NA, 
         xlim=c(0, ts.size+pred.size), 
         ylim=range(ts.pred),
         xlab = "index", ylab = "value",
         main = "Time series with predicted values")
    
    lines(y = input$ts, 
         x = 1:ts.size, 
         type = ltype,
         pch = 20,
         col = 1)
    
    lines(y = result$predict,
          x = (ts.size+1):(pred.size+ts.size),
          type = ltype,
          pch = 20,
          col = 3)
    
    lines(y = c(result$control, result$predict[1]),
          x = 1:(ts.size+1),
          type = ltype, pch = 20,
          col = 2)
    
    legend.pos <- ifelse(result$b>0, "topleft", "topright")
    legend(x = legend.pos,
           legend = c("initial", "predicted", "control"),
           pch = 20,
           col = c(1, 3, 2))
}
