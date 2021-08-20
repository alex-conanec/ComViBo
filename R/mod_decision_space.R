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
      status = "danger", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      plotlyOutput(ns("plot"))
    )
  )
}
    
#' decision_space Server Functions
#'
#' @noRd 
mod_decision_space_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    res_p = plot(prefix$res$X_space_csrt, as_list_plot = TRUE)
    
    nrow_react = reactive({
      length(res_p) %/% prefix$r$n_x_plot + 1
    })
    
    output$plot = renderPlotly({
      
      plotly::ggplotly(plotly::subplot(res_p, titleX = TRUE,
                                       nrows = nrow_react(),
                                       margin = c(0.05, 0.05, 0.05, 0.2))) %>%
        plotly::highlight(on = "plotly_selected", off= "plotly_deselect",
                          color = "red")
    })
   
  })
}
    
## To be copied in the UI
# mod_decision_space_ui("decision_space_ui_1")
    
## To be copied in the server
# mod_decision_space_server("decision_space_ui_1")
