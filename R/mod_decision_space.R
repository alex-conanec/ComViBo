#' decision_space UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
mod_decision_space_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Espace de décision", 
      closable = FALSE, 
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      sidebar = boxSidebar(
        id = ns("var_sel"),
        width = 25,
        startOpen = TRUE,
        pickerInput(inputId = ns("x1"), label = "X1", choices = NULL,
                    multiple = FALSE),
        pickerInput(inputId = ns("x2"), label = "X2", choices = NULL,
                    multiple = FALSE),
        pickerInput(inputId = ns("x_cate"), label = "Couleur",
                    choices = NULL, multiple = FALSE),
        radioGroupButtons(inputId = ns("plot_type"), label = "",
                          choices = c("Observations","Space"), justified = TRUE)
      ),
      plotOutput(ns("plot"))
    )
  )
}
    
#' decision_space Server Functions
#'
#' @noRd 
mod_decision_space_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observe({
      updatePickerInput(session = session, inputId = "x1",
                        choices = prefix$static_data$var_decision_quanti_name,
                        selected = prefix$static_data$var_decision_quanti_name[1])
      
      updatePickerInput(session = session, inputId = "x2",
                        choices = prefix$static_data$var_decision_quanti_name,
                        selected = prefix$static_data$var_decision_quanti_name[2])
      
      updatePickerInput(session = session, inputId = "x_cate",
                        choices = prefix$static_data$var_decision_quali_name)
    })
    
    
    output$plot = renderPlot({
      if (input$plot_type == "Observations"){
        ggplot(data = prefix$data, aes_string(x = input$x1, y = input$x2)) +
          geom_point(aes_string(colour = input$x_cate)) +
          theme_bw()
      }else{
        print("working on it")
      }

    })
    
  })
}
    
## To be copied in the UI
# mod_decision_space_ui("decision_space_ui_1")
    
## To be copied in the server
# mod_decision_space_server("decision_space_ui_1")
