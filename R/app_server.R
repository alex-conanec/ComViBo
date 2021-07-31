#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  r = reactiveValues(closed = c(FALSE, FALSE))

  
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
      selector = "#add_objectif",
      where = c("beforeBegin"),
      ui = mod_objectif_form_ui(paste0("objectif_form_ui_", last_id+1))
    )
    r$closed[last_id+1] = FALSE
    r = mod_objectif_form_server(paste0("objectif_form_ui_", last_id+1),
                                 prefix = list(id = last_id+1, r = r))
  })
  
   
}
