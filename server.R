library(shiny)

server <- function(input, output){
    output$file_select <- renderUI(
        {
            if (input$use_abs_path){

            }
            else
        }
    )


}


shinyApp(server = server, ui = ui)
