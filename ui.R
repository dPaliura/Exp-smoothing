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
            label = 'Enter path to file',
            value = '~/'
        )
    ),
    uiOutput(
        outputId = 'file_select'
    ),

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

    uiOutput(
        outputId = 'read_file'
    ),

    title = 'Exponential smoothing'
)
