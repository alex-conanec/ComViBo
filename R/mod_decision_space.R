#' decision_space UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_decision_space_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("box_ui"))
  )
}
    
#' decision_space Server Functions

#' @importFrom plotly plotlyOutput renderPlotly highlight_key ggplotly subplot
#' @importFrom ggplot2 ggplot theme_bw theme coord_flip element_blank geom_text geom_point aes_string aes
#' @noRd 
mod_decision_space_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  
    output$box_ui = renderUI(
      box(
        width = 12,
        title = paste0("Espace de décision (n=",NROW(prefix$res$X0), ")"), 
        closable = FALSE, 
        status = "danger", 
        solidHeader = FALSE, 
        collapsible = TRUE,
        uiOutput(ns("plot_ui"))
      )
    )
    
    # res_p = plot(prefix$res$X_space_csrt, as_list_plot = TRUE)
    
    nrow_react = reactive({
      NCOL(prefix$res$X0) %/% prefix$r$n_x_plot + 1
    })
    
    
    output$plot_ui = renderUI({
      plotlyOutput(ns("plot"), height = 300 * nrow_react())
    })
    
    output$plot = renderPlotly({
      
      #plot
      df = prefix$res$X0
      # if (!is.null(prefix$res$g)){
      #   mask = lapply(prefix$res$g, function(gg){
      #     gg(df)
      #   }) %>% bind_cols() #marche pas a mon avis
      #   df = df[mask,]
      # }
      
      df$id = 1:NROW(df)
      
      dd = highlight_key(df, ~id)
      lapply(colnames(df)[-NCOL(df)], function(X_i){
        if (is.numeric(df[, X_i,T]) | is.integer(df[, X_i,T])){
          p = ggplot(dd) +
            geom_point(aes_string(x = 1, y = X_i)) +
            xlab(prefix$static_data$choice_names %>% filter(names == X_i) %>% 
                   pull(choice_name) %>% as.character()) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
        }else{
          p = ggplot(dd) + geom_text(
            aes(x = 1, y = as.numeric(!! rlang::sym(X_i)),
                label = !! rlang::sym(X_i))) +
            # ylim(0.5, nlevels(df[, X_i]) + 0.5) +
            xlab(prefix$static_data$choice_names %>% filter(names == X_i) %>%
                   pull(choice_name) %>% as.character()) +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.y = element_blank())
        }
        p + theme_bw()
      }) -> p_x
      
      ggplotly(subplot(p_x, titleX = TRUE,
                       nrows = nrow_react(),
                       margin = c(0.05, 0.05, 0.05, 0.05))) %>%
        highlight(on = "plotly_selected", off= "plotly_deselect",
                  color = "red")
    })
   
  })
}
    
## To be copied in the UI
# mod_decision_space_ui("decision_space_ui_1")
    
## To be copied in the server
# mod_decision_space_server("decision_space_ui_1")
