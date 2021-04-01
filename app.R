library(shiny)

input.buffer.dir <- 'inputBuff'

ui <- fluidPage(
    # Getting path to file with data
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

    # Setting parameters for file reading
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
    ),

    # Button to read file
    uiOutput(
        outputId = 'read_file'
    ),

    # Choose variable
    uiOutput(
        outputId = 'var_choose'
    ),

    # Series preview
    plotOutput(
        outputId = 'preview'
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


}


shinyApp(server = server, ui = ui)
