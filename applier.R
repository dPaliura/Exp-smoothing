source("holt.R", echo=FALSE)
source("theil-wage.R", echo=FALSE)


apply.method <- function(input){
    if (input$is.seasonal) {
        res <- theil.wage(ts = input$ts,
                          period = input$season.period,
                          par1 = input$alpha,
                          par2 = input$theta,
                          par3 = input$beta,
                          trend = input$is.trended)
        res$method <- "Theil-Wage"
        res$predict <- theil.wage.predict(res, 1:input$forecast.length)
    }
    else{
        res <- holt(ts = input$ts,
                    par1 = input$alpha,
                    par2 = input$beta,
                    trend = input$is.trended)
        res$method <- "Holt"
        res$predict <- holt.predict(res, 1:input$forecast.length)
    }

    res$input <- input
    return(res)
}
