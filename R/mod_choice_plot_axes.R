#' choice_plot_axes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choice_plot_axes_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("radio_buttons_ui"))
  )
}
    
#' choice_plot_axes Server Functions
#'
#' @noRd 
mod_choice_plot_axes_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$radio_buttons_ui = renderUI({
      fluidRow(
        column(width = 6,
               radioGroupButtons(inputId = ns("x"), label = "x",
                                 choiceNames = paste0("Obj N°", 1:prefix$p),
                                 choiceValues = paste0("objectif_", 1:prefix$p),
                                 justified = TRUE, direction = "vertical",
                                 selected = "objectif_1")),
        column(width = 6,
               radioGroupButtons(inputId = ns("y"), label = "y",
                                 choiceNames = paste0("Obj N°", 1:prefix$p),
                                 choiceValues = paste0("objectif_", 1:prefix$p),
                                 justified = TRUE, direction = "vertical",
                                 selected = "objectif_2"))
      )
    })
  
    
    observe({
      req(input$y)
      mask = paste0("objectif_", 1:prefix$p) == input$y
      updateRadioGroupButtons(
        session = session, inputId = "x",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })
    
    observe({
      req(input$x)
      mask = paste0("objectif_", 1:prefix$p) == input$x
      updateRadioGroupButtons(
        session = session, inputId = "y",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })
    
    observe({
      prefix$r$tradeoff_plot$choice_plot_axes$x[[prefix$id]] = input$x
    })
    observe({
      prefix$r$tradeoff_plot$choice_plot_axes$y[[prefix$id]] = input$y
    })
    
    observeEvent(input$y,{
      prefix$r$tradeoff_plot$choice_plot_axes$changed = runif(1)
    })
    
    observeEvent(input$x,{
      prefix$r$tradeoff_plot$choice_plot_axes$changed = runif(1)
    })
    
    prefix$r
    
  })
}
    
## To be copied in the UI
# mod_choice_plot_axes_ui("choice_plot_axes_ui_1")
    
## To be copied in the server
# mod_choice_plot_axes_server("choice_plot_axes_ui_1")
