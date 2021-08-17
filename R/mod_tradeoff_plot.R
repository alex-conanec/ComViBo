#' tradeoff_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
mod_tradeoff_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Résultats optimisation", 
      closable = FALSE, 
      status = "danger", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      sidebar = boxSidebar(
        id = ns("var_sel"),
        width = 25,
        startOpen = TRUE,
        uiOutput(ns("choice_Y_ui")),
        uiOutput(ns("choice_X_ui"))
      ),
      plotlyOutput(ns("plot"))
    )
  )
}
    
#' tradeoff_plot Server Functions
#'
#' @noRd 
mod_tradeoff_plot_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    n_X_max = 5
    x_var_names = c(prefix$static_data$var_decision_quali_name,
                   prefix$static_data$var_decision_quanti_name)
    
    output$choice_Y_ui = renderUI({
      if (prefix$p > 2){
        tagList(
          tags$h4("Plot gauche"),
          fluidRow(
            column(width = 6,
                   radioGroupButtons(inputId = ns("left_x"), label = "x",
                                     choiceNames = paste0("Objectif N°", 1:prefix$p),
                                     choiceValues = paste0("objectif_", 1:prefix$p),
                                     justified = TRUE, direction = "vertical",
                                     selected = "objectif_1")),
            column(width = 6,
                   radioGroupButtons(inputId = ns("left_y"), label = "y",
                                     choiceNames = paste0("Objectif N°", 1:prefix$p),
                                     choiceValues = paste0("objectif_", 1:prefix$p),
                                     justified = TRUE, direction = "vertical",
                                     selected = "objectif_2"))
          ),
          tags$h4("Plot gauche"),
          fluidRow(
            column(width = 6,
                   radioGroupButtons(inputId = ns("right_x"), label = "x",
                                     choiceNames = paste0("Objectif N°", 1:prefix$p),
                                     choiceValues = paste0("objectif_", 1:prefix$p),
                                     justified = TRUE, direction = "vertical",
                                     selected = "objectif_1")),
            column(width = 6,
                   radioGroupButtons(inputId = ns("right_y"), label = "y",
                                     choiceNames = paste0("Objectif N°", 1:prefix$p),
                                     choiceValues = paste0("objectif_", 1:prefix$p),
                                     justified = TRUE, direction = "vertical",
                                     selected = "objectif_3"))
            )
          )
      }
    })

    observe({
      mask = paste0("objectif_", 1:prefix$p) == input$left_x
      updateRadioGroupButtons(
        session = session, inputId = "left_y",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })

    observe({
      mask = paste0("objectif_", 1:prefix$p) == input$left_y
      updateRadioGroupButtons(
        session = session, inputId = "left_x",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })

    observe({
      mask = paste0("objectif_", 1:prefix$p) == input$right_y
      updateRadioGroupButtons(
        session = session, inputId = "right_x",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })

    observe({
      mask = paste0("objectif_", 1:prefix$p) == input$right_x
      updateRadioGroupButtons(
        session = session, inputId = "right_y",
        disabledChoices = paste0("objectif_", 1:prefix$p)[mask]
      )
    })
    
    output$choice_X_ui = renderUI({
      checkboxGroupButtons(inputId = ns("X_to_plot"),
                           choices = x_var_names,
                           selected = x_var_names[1:n_X_max],
                           label = "Variable de décision",
                           justified = TRUE,
                           direction = "vertical")
    })
    
    observe({
      if (!is.null(input$X_to_plot)){
        if (length(input$X_to_plot) >= n_X_max){
          choice_to_disable = x_var_names[!x_var_names %in% input$X_to_plot]
          updateCheckboxGroupButtons(session = session, inputId = "X_to_plot",
                                     disabledChoices = choice_to_disable)
        }else{
          updateCheckboxGroupButtons(session = session, inputId = "X_to_plot",
                                     disabledChoices = NULL)
        }
      }
    })
    
    res_p = plot(prefix$res, as_list_plot = TRUE)
    
    x_to_diplay = reactive({ 
      if (!is.null(input$X_to_plot)){
        which(x_var_names %in% input$X_to_plot)
      }else{
          1:n_X_max
      }
    })
    
    y_to_diplay = reactive({
      if (!is.null(input$right_x)){
        df = as.data.frame(t(res_p$combi)) %>%
          tibble::rownames_to_column("id") %>%
          mutate(id = as.numeric(id))

        c(
          df %>% filter(V1 == input$left_x & V2 == input$left_y |
                          V2 == input$left_x & V1 == input$left_y) %>% pull(id),
          df %>% filter(V1 == input$right_x & V2 == input$right_y |
                          V2 == input$right_x & V1 == input$right_y) %>% pull(id)
        )
      }else{
        1
      }
    })
    
    sel = reactiveVal() 
    sel_store = reactiveVal()
    
    output$plot = renderPlotly({
      res_p = plot(prefix$res, as_list_plot = TRUE)
      
      s_y = plotly::subplot(res_p$p_y[y_to_diplay()], titleY = TRUE,
                            titleX = TRUE, margin = c(0.02, 0.1, 0.02, 0.2))
      s_x = plotly::subplot(res_p$p_x[x_to_diplay()], titleX = TRUE, 
                            margin = c(0.02, 0.1, 0.02, 0.2))
      
      plotly::ggplotly(plotly::subplot(s_y, s_x, nrows = 2, margin = c(0.05, 0.05, 0.05, 0.2),
                                       titleY = TRUE, titleX = TRUE)) %>%
        plotly::highlight(defaultValues = sel(),
                          on = "plotly_selected", off = "plotly_deselect",
                          color = "red", persistent = FALSE) %>%
        plotly::layout(annotations = list(
          list(x = 0 , y = 1.05, text = "Front de Pareto",
               showarrow = F, xref='paper', yref='paper'),
          list(x = 0 , y = 0.48, text = "Decision solution",
               showarrow = F, xref='paper', yref='paper'))
        ) 
    
    })
    
    
    observeEvent(input$X_to_plot, {
      sel(sel_store())
    })
    observeEvent(input$left_x, {
      sel(sel_store())
    })
    observeEvent(input$left_y, {
      sel(sel_store())
    })
    observeEvent(input$right_x, {
      sel(sel_store())
    })
    observeEvent(input$right_y, {
      sel(sel_store())
    })
    
    
    observe({
      deselected <- event_data("plotly_deselect")
      if (!is.null(deselected)){
        sel_store(NULL)
      }
    })
    
    observe({
      selected <- event_data("plotly_selected")
      if (!is.null(selected)){
        sel_store(selected$key)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_tradeoff_plot_ui("tradeoff_plot_ui_1")
    
## To be copied in the server
# mod_tradeoff_plot_server("tradeoff_plot_ui_1")
