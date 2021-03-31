library(shiny)


ui <- fluidPage(
    checkboxInput(
        inputId = 'use_abs_path',
        label = 'Use absolute path to folder with file',
        value = FALSE
    ),
    conditionalPanel(
        condition = 'input.use_abs_path == true',
        textInput(
            inputId = 'abs_path',
            label = 'Enter path to file'
        )
    ),
    uiOutput(
        outputId = 'file_select'
    ),
    title = 'Exponential smoothing'
)
