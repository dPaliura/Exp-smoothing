interpret <- function(result){
    input <- result$input
    ts.size <- length(input$ts)
    pred.size <- input$forecast.length

    ltype <- ifelse(ts.size>30, 'l', "b")

    ts.pred <- c(input$ts, result$predict)

    sde <- sd(abs(input$ts-result$control))

    plot(NA,
         xlim=c(0, ts.size+pred.size),
         ylim=range(ts.pred),
         xlab = "index", ylab = "value",
         main = paste0("Time series with forecasted values via method - ",
                       result$method,
                       "(SDE = ", sde,")"))

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
