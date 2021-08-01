#' constraint_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_constraint_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      id = ns("mybox"),
      title = "Contrainte N°",
      status = "warning",
      width = 12,
      closable = TRUE,
      collapsible = TRUE,
      tagList(
        uiOutput(ns("indicators_ui"))
      )
    )
  )
}
    
#' constraint_form Server Functions
#'
#' @noRd 
mod_constraint_form_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$mybox$visible, {
      
      if (!input$mybox$visible){
        prefix$r$constraint_form$closed[prefix$id] = TRUE
      }
      
      updateBox("mybox", action = "update",
                options = list(title = h2(paste0("Contrainte N°", id_new())) ))
    })
    
    observe({
      updateBox("mybox", action = "update",
                  options = list(title =  h2(paste0("Contrainte N°", id_new())) ))
    })

    #update number
    id_new = reactive({
      prefix$id - sum(prefix$r$constraint_form$closed[1:prefix$id])
    })
    
    
    #make the indicator picker with the given list of item
    output$indicators_ui = renderUI({
      pickerInput(inputId = ns("indicators"), label = "Indicateurs",
                  choices = LETTERS[1:4], multiple = TRUE)
    })
    
    prefix$r
    
  })
}
    
## To be copied in the UI
# mod_constraint_form_ui("constraint_form_ui_1")
    
## To be copied in the server
# mod_constraint_form_server("constraint_form_ui_1")
