#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  n_objectif_max = 5
  r = reactiveValues(closed = c(FALSE, FALSE),
                     formule_ok = c(FALSE, FALSE),
                     globale = NULL,
                     tau = NULL,
                     new_tau = NULL)

  
  r = mod_objectif_form_server("objectif_form_ui_1",
                               prefix = list(id = 1, r = r))
  r = mod_objectif_form_server("objectif_form_ui_2",
                               prefix = list(id = 2, r = r))
  

  
  
  observeEvent(input$add_objectif,{

    #get the last id of the objectif # un peu bourrin...
    last_id = grep(names(input), pattern = "objectif_form_ui_", value = TRUE) %>%
      strsplit("-") %>% sapply(function(x){
        strsplit(x[1], "") %>%
          unlist() %>% tail(1) %>% as.numeric()
      }) %>% max()

    insertUI(
      selector = "#fluidRow_button",
      where = c("beforeBegin"),
      ui = mod_objectif_form_ui(paste0("objectif_form_ui_", last_id+1))
    )
    r$closed[last_id+1] = FALSE
    r$formule_ok[last_id+1] = FALSE
    r = mod_objectif_form_server(paste0("objectif_form_ui_", last_id+1),
                                 prefix = list(id = last_id+1, r = r))
  })
  
  observe({
    if (sum(!r$closed) >= n_objectif_max){
      shinyjs::disable("add_objectif")
    }else{
      shinyjs::enable("add_objectif")
    }
  }) 
  
  observe({
    if (sum(r$formule_ok[!r$closed]) < sum(!r$closed)){
      shinyjs::disable("run_simu")
    }else{
      shinyjs::enable("run_simu")
    }
  })
  
}
