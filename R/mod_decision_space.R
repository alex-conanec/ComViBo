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
      sidebar = boxSidebar(
        id = ns("var_sel"),
        width = 25,
        startOpen = TRUE,
        materialSwitch(inputId = ns("data_plot"), label = "Données",
                       value = TRUE, status = "primary"),
        pickerInput(inputId = ns("x1"), label = "X1", choices = NULL,
                    multiple = FALSE),
        pickerInput(inputId = ns("x2"), label = "X2", choices = NULL,
                    multiple = FALSE),
        uiOutput(ns("x_cate_lev_ui"))
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
    })
    
    output$x_cate_lev_ui = renderUI({
      if (input$data_plot){
        tagList(
          pickerInput(inputId = ns("x_cate"), label = "Couleur",
                      choices = prefix$static_data$var_decision_quali_name,
                      multiple = FALSE)
        )
      }else{
        tagList(
          lapply(prefix$static_data$var_decision_quali_name, function(x){
            pickerInput(inputId = ns(paste0(x, "_lev")), label = x,
                        choices = unique(prefix$data[,x]),
                        selected = unique(prefix$data[,x])[1],
                        multiple = FALSE)
          })
        )
      }
    })
    
    X_feasible = reactive({
      n_MC = 10^4
      x_quali = prefix$static_data$var_decision_quali_name
      names(x_quali) = prefix$static_data$var_decision_quali_name
      x_quanti = prefix$static_data$var_decision_quanti_name
      names(x_quanti) = prefix$static_data$var_decision_quanti_name
      
      X_MC = cbind(
        lapply(x_quali, function(x){
          sample(x = unique(prefix$data[,x, drop = TRUE]), size = n_MC, 
                 replace = TRUE)
        }) %>% as.data.frame(),
        lapply(x_quanti, function(x){
          runif(n = n_MC, min(prefix$data[,x, drop = TRUE]),
                max(prefix$data[,x, drop = TRUE]))
        }) %>% as.data.frame()
      )
      
      X_MC$feasible = prefix$res$g$cstr_X_space(X_MC)
      X_MC
    })
    
    df = reactive({
      mask = lapply(prefix$static_data$var_decision_quali_name, function(x){
        X_feasible()[,x, drop = TRUE] == input[[paste0(x, "_lev")]]
      }) %>% as.data.frame() %>% apply(1, all)

      X_feasible()[mask,]
    })
    
    output$plot = renderPlot({
      if (input$data_plot){
        ggplot(data = prefix$data, aes_string(x = input$x1, y = input$x2)) +
          geom_point(aes_string(colour = input$x_cate)) +
          theme_bw()
      }else{
        ggplot(df(), aes_string(x = input$x1, y = input$x2)) +
          geom_point(aes(colour = feasible), size = 10) +
          theme_bw()
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_decision_space_ui("decision_space_ui_1")
    
## To be copied in the server
# mod_decision_space_server("decision_space_ui_1")
