library(shiny)

source('applier.R', echo = FALSE)
source('interpreter.R', echo = FALSE)

input.buffer.dir <- 'inputBuff'

ui <- fluidPage(
    fluidRow(
        # Getting path to file with data
        column(
            width = 4,
            checkboxInput(
                inputId = 'use_abs_path',
                label = 'Use absolute path to folder with file',
                value = FALSE
            ),
            conditionalPanel(
                condition = 'input.use_abs_path == true',
                textInput(
                    inputId = 'abs_path',
                    label = 'Enter path to file',
                    value = '~/'
                )
            ),
            uiOutput(
                outputId = 'file_select'
            ),
        ),

        # Setting parameters for file reading
        column(
            width = 4,
            checkboxInput(
                inputId = 'use_def_sep_n_dec',
                label = 'Use comma \',\' as separator and dot \'.\' as decimal',
                value = TRUE
            ),
            conditionalPanel(
                condition = 'input.use_def_sep_n_dec == false',
                textInput(
                    inputId = 'separator',
                    label = 'Write your separator',
                    value = ';'
                ),
                textInput(
                    inputId = 'decimal',
                    label = 'Write your decimal',
                    value = ','
                )
            ),
            span(
                textOutput(
                    outputId = 'sep_dec_err'
                ),
                style = 'color:red'
            )
        )
    ),

    # Button to read file
    uiOutput(
        outputId = 'read_file'
    ),

    # Choose variable
    uiOutput(
        outputId = 'var_choose'
    ),

    # Model parameters
    uiOutput(
        outputId = 'model_params'
    ),

    # Series preview
    fluidRow(
        plotOutput(
            outputId = 'preview'
        )
    ),

    # Build forecast button
    uiOutput(
        outputId = 'build_forecast'
    ),

    # Forecast plot
    plotOutput(
        outputId = 'forecast'
    ),

    title = 'Exponential smoothing'
)



server <- function(input, output){
    output$file_select <- renderUI(
        {
            path <- if (input$use_abs_path) input$abs_path else input.buffer.dir
            path_dir <- dir(path)
            path_dir <- path_dir[grep('\\.csv$', path_dir)]

            if (length(path_dir)) {
                choices <- c('', path_dir)
                names(choices) <- c('<Not chosen>', path_dir)
                selectInput(
                    inputId = 'file_select',
                    label = 'Select file with data',
                    choices = choices,
                )
            }
        }
    )

    output$sep_dec_err <- renderText(
        {
            if (!input$use_def_sep_n_dec){
                paste0(
                    ifelse(
                        test = trimws(input$separator) == '',
                        yes  = 'Separator field can\'t be empty. ',
                        no   = ifelse(
                            test = trimws(input$separator) ==
                                   trimws(input$decimal),
                            yes  = 'Separator and decimal can\'t be the same.',
                            no   = ''
                        )
                    ),
                    ifelse(
                        test = trimws(input$decimal) == '',
                        yes  = 'Decimal field can\'t be empty.',
                        no   = ''
                    )
                )
            }
        }
    )

    output$read_file <- renderUI(
        {
            if (!is.null(input$file_select)){
                if (input$file_select != ''){
                    actionButton(
                        inputId = 'read_file',
                        label = 'Read file'
                    )
                }
            }
        }
    )

    df <- eventReactive(
        eventExpr = input$read_file,
        valueExpr = {
            if (input$use_def_sep_n_dec){
                sep <- ','
                dec <- '.'
            }
            else {
                sep <- input$separator
                dec <- input$decimal
            }
            read.csv(
                file = paste0(
                            ifelse(input$use_abs_path,
                                   input$abs_path,
                                   input.buffer.dir),
                            '/',
                            input$file_select),
                sep = sep, dec = dec
            )
        }
    )

    output$var_choose <- renderUI(
        {
            data <- df()
            vars <- colnames(data)
            names(vars) <- vars
            choices <- as.list(vars)
            selectInput(
                inputId = 'var_choose',
                label = 'Choose variable of table',
                choices = choices
            )
        }
    )

    output$preview <- renderPlot(
        {
            data <- df()
            var <- input$var_choose
            if (!is.null(var) & !is.null(data)){
                if (var %in% colnames(data)){
                    plot(
                        x = 1:nrow(data), y = data[,var],
                        type = 'l',
                        main = 'Preview of data',
                        xlab = 'time', ylab = var
                    )
                }
            }
        }
    )

    output$model_params <- renderUI(
        {
            if (!is.null(input$var_choose) & !is.null(df())){
                div(
                    column(
                        width = 6,
                        sliderInput(
                            inputId = 'alpha',
                            label = 'Model smooth parameter (alpha)',
                            min = 0, max = 1, value = 0.5,
                            step = 1e-2
                        ),

                        checkboxInput(
                            inputId = 'has_trend',
                            label = 'Series has a trend',
                            value = TRUE
                        ),
                        conditionalPanel(
                            condition = 'input.has_trend == true',
                            sliderInput(
                                inputId = 'beta',
                                label = 'Model trend parameter (beta)',
                                min = 0, max = 1, value = 0.5,
                                step = 1e-2
                            )
                        )
                    ),
                    column(
                        width = 6,
                        checkboxInput(
                            inputId = 'has_season',
                            label = 'Series is seasonal',
                            value = FALSE
                        ),
                        conditionalPanel(
                            condition = 'input.has_season == true',
                            sliderInput(
                                inputId = 'theta',
                                label = 'Model seasonal parameter (theta)',
                                min = 0, max = 1, value = 0.5,
                                step = 1e-2
                            ),
                            uiOutput(
                                outputId = 'season_period',
                            )
                        )
                    ),
                    uiOutput(
                        outputId = 'forecast_len',
                    )
                )
            }
        }
    )

    output$season_period <- renderUI(
        {
            n <- nrow(df())
            if (n > 4) sliderInput(
                inputId = 'season_period',
                label = 'Series season period',
                min = 1, max = floor(n/2), value = floor(n/4),
                step = 1
            )
        }
    )

    output$forecast_len <- renderUI(
        {
            if(!is.null(input$var_choose) & !is.null(df())){
                n <- nrow(df())
                if (n > 4) sliderInput(
                    inputId = 'forecast_len',
                    label = 'Forecast size',
                    min = 1, max = n, value = floor(n/4),
                    step = 1
                )
            }
        }
    )

    output$build_forecast <- renderUI(
        {
            if (!is.null(input$var_choose) & !is.null(df()))
            actionButton(
                inputId = 'build_forecast',
                label = 'Build forecast'
            )
        }
    )

    forecast <- eventReactive(
        eventExpr = {input$build_forecast},
        valueExpr = {
            ts <- df()[,input$var_choose]
            print(ts)
            input <- list(
                ts = ts,
                is.trended = input$has_trend,
                is.seasonal = input$has_season,
                alpha = input$alpha,
                beta = ifelse(input$has_trend, input$beta, NA),
                theta = ifelse(input$has_season, input$theta, NA),
                season.period = ifelse(input$has_season,input$season_period,NA),
                forecast.length = input$forecast_len
            )
            apply.method(input)
        }
    )

    output$forecast <- renderPlot(
        {
            interpret(forecast())
        }
    )
}
