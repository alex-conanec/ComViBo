#' objectif_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_objectif_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      id = ns("mybox"),
      title = "Objectif N°",
      status = "warning",
      width = 12,
      closable = TRUE,
      collapsible = TRUE,
      numericInputIcon(ns("n_objectifs"), label = "Nombre d'objectifs",
                       value = 2, min = 2, max = 5, step = 1)
    )
  )
}
    
#' objectif_form Server Functions
#' 
#' @import shinydashboardPlus
#' @noRd 
mod_objectif_form_server <- function(id, prefix = NULL){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$mybox$visible, {
      
      if (!input$mybox$visible){
        prefix$r$closed[prefix$id] = TRUE
      }
      
      updateBox("mybox", action = "update",
                options = list(title = h2(paste0("Objectif N°", id_new())) ))
    })
    
    # closable become FALSE when the number of box is lower or equal than 2
    observe({
      if (sum(!prefix$r$closed) <= 2){
        updateBox("mybox", action = "update",
                  options = list(title = h2(paste0("Objectif N°", id_new())),
                                 closable = FALSE))
      }else{
        updateBox("mybox", action = "update",
                  options = list(title =  h2(paste0("Objectif N°", id_new())),
                                 closable = TRUE))
      }
    })
    
    #update number
    id_new = reactive({
      prefix$id - sum(prefix$r$closed[1:prefix$id])
    })
    
    
    prefix$r
  })
}
    
## To be copied in the UI
# mod_objectif_form_ui("objectif_form_ui_1")
    
## To be copied in the server
# mod_objectif_form_server("objectif_form_ui_1")
