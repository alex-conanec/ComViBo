#' tradeoff_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tradeoff_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("mybox_ui"))
  )
}
    
#' tradeoff_plot Server Functions
#' @importFrom plotly ggplotly subplot highlight event_data plotlyOutput
#' @importFrom ggplot2 ggplot theme_bw theme coord_flip element_blank
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom dplyr mutate
#' @noRd 
mod_tradeoff_plot_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    y_plot_height = 300
    x_plot_height = 250
    ratio_plot_pareto = 1
    x_width = y_plot_height * ratio_plot_pareto
    
    Y_names = isolate(prefix$r$objectif_form$names)
    
    # combi_y = as.data.frame(t(combn(colnames(prefix$res$Y), 2))) %>%
    #   tibble::rownames_to_column("id") %>%
    #   mutate(id = as.numeric(id))
    combi_y = as.data.frame(t(combn(Y_names, 2))) %>%
      tibble::rownames_to_column("id") %>%
      mutate(id = as.numeric(id))
    
    # combi_y = combi_y %>% left_join(static_data$choice_names, c("V1"="choice_name"))
    # combi_y = combi_y %>% left_join(static_data$choice_names, c("V2"="choice_name"))
    
    colnames(combi_y)[-1] = c("x", "y")
    
    p_empty = ggplot(data.frame(x=rnorm(1), y=rnorm(1))) +
      theme_bw() +
      theme(panel.border = element_blank())
    
    
    n_combi_plot = reactive({ choose(NCOL(prefix$res$Y), 2) })
    
    n_y = reactive({
      min(n_combi_plot(), prefix$r$window_width %/% x_width ) 
    })
    
    output$mybox_ui = renderUI({
      
      # if (prefix$p > 2){
      if (n_combi_plot() > n_y()){
        prefix$r$tradeoff_plot$n_y = n_y()
        box(
          id = ns("mybox"),
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
              lapply(1:n_y(), function(i){
                tagList(
                  tags$h4(paste0("Plot ", i)),
                  mod_choice_plot_axes_ui(paste0("choice_plot_axes_ui_", i))
                )
              })
          ),
          plotlyOutput(ns("plot"), height = y_plot_height + x_plot_height * nrow_react())
        )
      }else{
        prefix$r$tradeoff_plot$n_y = NULL
        box(
          id = ns("mybox"),
          width = 12,
          title = "Résultats optimisation", 
          closable = FALSE, 
          status = "danger", 
          solidHeader = FALSE, 
          collapsible = TRUE,
          plotlyOutput(ns("plot"), height = 300 * nrow_react())
        )
      }
    })
    
    
    # res_p = reactive({ plot(prefix$res, as_list_plot = TRUE) })
    
    
    nrow_react = reactive({
      NCOL(prefix$res$X) %/% prefix$r$n_x_plot + 1
    })

    y_to_diplay = reactive({
      if (n_combi_plot() > n_y()){
        req(prefix$r$tradeoff_plot$choice_plot_axes$y[[n_y()]])

        sapply(1:n_y(), function(i){
          combi_y %>% filter(x == prefix$r$tradeoff_plot$choice_plot_axes$x[[i]] &
                            y == prefix$r$tradeoff_plot$choice_plot_axes$y[[i]] |
                            y == prefix$r$tradeoff_plot$choice_plot_axes$x[[i]] &
                            x == prefix$r$tradeoff_plot$choice_plot_axes$y[[i]]) %>%
            pull(id)
        })
        
        
      }else{
        1:n_combi_plot()
      }
    })
    
    respect_order_asked = reactive({
      req(y_to_diplay())
      
      if (n_y() < NROW(combi_y)){
        sapply(1:n_y(), function(i){
          combi_y$x[y_to_diplay()[i]] == prefix$r$tradeoff_plot$choice_plot_axes$x[[i]]
        })
      }else{
        rep(TRUE, n_y())
      }
      
    })
    
    sel = reactiveVal() 
    sel_store = reactiveVal()
    
    
    prop_plot_y = reactive({
      1 - x_plot_height * nrow_react()/(x_plot_height * nrow_react() + y_plot_height ) 
    })
    
    vide_y_prop = reactive({
      (prefix$r$window_width - n_y()*x_width) / (2*prefix$r$window_width)
    })
    
    width_y_prop = reactive({
      (1-2*vide_y_prop()) / n_y()
    })
    
    output$plot = renderPlotly({
      res_p = plot(prefix$res, as_list_plot = TRUE)

      for (i in 1:length(res_p$p_y)){
        res_p$p_y[[i]] = res_p$p_y[[i]] + xlab(as.character(combi_y$x[i])) +
          ylab(as.character(combi_y$y[i]))
      }
      
      
      p_y = lapply(1:n_y(), function(i){
        if (respect_order_asked()[i]){
          res_p$p_y[[y_to_diplay()[i]]]
        }else{
          res_p$p_y[[y_to_diplay()[i]]] + coord_flip()
        }
      })


      s_y = subplot(c(list(p_empty), p_y, list(p_empty)),
                            titleY = TRUE, titleX = TRUE, nrows = 1,
                            widths = c(vide_y_prop(),
                                       rep(width_y_prop(), n_y()),
                                       vide_y_prop()),
                            margin = c(0.02, 0.1, 0.02, 0.02))
      
      x_names = as.character(prefix$static_data$choice_names[
        prefix$static_data$var_decision_idx, 2, drop = TRUE])
      for (i in 1:length(res_p$p_x)){
        res_p$p_x[[i]] = res_p$p_x[[i]] + xlab(x_names[i]) 
      }
      
      s_x = subplot(res_p$p_x, titleX = TRUE,
                            nrows = nrow_react(),
                            margin = c(0.02, 0.1, 0.02, 0.3/nrow_react()))

      ggplotly(subplot(s_y, s_x, nrows = 2,
                                       margin = c(0.05, 0.05, 0.05,
                                                  0.2/nrow_react()),
                                       titleY = TRUE, titleX = TRUE,
                                       heights = c(prop_plot_y(), 1-prop_plot_y())
                                       )) %>%
        highlight(defaultValues = sel(),
                          on = "plotly_selected", off = "plotly_deselect",
                          color = "red", persistent = FALSE)

    })
    

    observeEvent(prefix$r$tradeoff_plot$choice_plot_axes$changed,{
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
        selected = selected$key
        sel_store(selected[!is.na(selected)])
      }
    })
    
    prefix$r
    
  })
}
    
## To be copied in the UI
# mod_tradeoff_plot_ui("tradeoff_plot_ui_1")
    
## To be copied in the server
# mod_tradeoff_plot_server("tradeoff_plot_ui_1")
