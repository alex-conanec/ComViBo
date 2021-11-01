#' beta_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_beta_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Modèles de prédiction", 
      closable = FALSE, 
      status = "danger", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      fluidRow(column(width = 6, uiOutput(ns("plot_R2"))),
               column(width = 6, plotOutput(ns("plot_beta"))))
    )
  )
}
    
#' beta_plot Server Functions
#'
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradient2 aes geom_bar ylim xlab ylab theme theme_bw element_text scale_y_continuous
#' @importFrom plotly renderPlotly
#' @importFrom latex2exp TeX
#' @importFrom xtable xtable
#' @noRd 
mod_beta_plot_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot_R2 = renderUI({


      #taleau modele
      r2 = R2(prefix$X, prefix$Y, B = prefix$B, seed = 123,
              allowed_dependence = prefix$allowed_dependence)

      p = NCOL(prefix$res$Y)
      globale_tau = isolate(prefix$r$objectif_form$globale_tau)
      quantile = isolate(prefix$r$objectif_form$quantile)
      
      globale_confiance = tau_j = rep("-", p)
      methode = rep("Moy", p)
      tau_j[quantile] = prefix$res$tau_j[quantile]
      globale_confiance[globale_tau] = prefix$res$globale_confiance[globale_tau]
      methode[quantile] = "VaR"

      df = data.frame(n = unlist(prefix$res$n_models[1,,TRUE]),
                      R2 = round(r2, 2), methode = methode,
                      tau_j, globale_confiance)
      rownames(df) = isolate(prefix$r$objectif_form$names)
      df = t(df)
      
      rownames(df) = c("n", "R^2", "Méthode", "\\tau_j", "\\Phi")
      colnames(df) = paste0("\\text{", colnames(df), "}")
      
      LaTeXtab <- print(xtable(df, align=rep("c", ncol(df)+1)), 
                        floating=FALSE, tabular.environment="array", comment=FALSE, 
                        print.results=FALSE, 
                        sanitize.rownames.function = function(x) x,
                        sanitize.colnames.function=function(x)gsub("\\."," ",x))
      
      tagList(
        withMathJax(),
        HTML(paste0("$$", LaTeXtab, "$$"))
      )
      
      
    })
    
    
    plot_beta = function(beta, limits=NULL){
      beta[beta==0] = NA
      
      df = beta %>% as.data.frame() %>% rownames_to_column("X") %>%
        gather(key = Y, value = beta, -X)
      df$X = factor(df$X, levels = rownames(beta))
      df$Y = factor(df$Y, levels = colnames(beta))
      
      pp = ggplot(df) +
        geom_tile(aes(x = Y, y = X, fill = beta)) +
        scale_fill_gradient2(low="red", mid = "white", high="blue",
                             midpoint = 0, na.value = 'black',
                             limits = limits) +
        xlab("") +
        ylab("") +
        theme_bw()
      if (NCOL(beta) > 5){
        pp = pp + theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                                   vjust = 0.5))
      }
      pp
    }

    output$plot_beta = renderPlot({

      X = prefix$X
      beta = prefix$res$beta[-1,]
      is_fac = !sapply(X, is.numeric)
      
      for (name in colnames(X)[!is_fac]){
        rownames(beta)[grep(rownames(beta), pattern = name)] = prefix$static_data$choice_names %>% 
          filter(names == name) %>% pull(choice_name) %>% as.character()
      }

      for (fac in colnames(X)[is_fac]){
        idx = grep(rownames(beta), pattern = fac)
        fac_name = prefix$static_data$choice_names %>% filter(names == fac) %>% 
          pull(choice_name) %>% as.character()
        if (length(idx) > 0){
          new_beta = rbind(-colSums(beta[idx,,drop=F]), beta[idx,,drop=F])
          rownames(new_beta) = paste0(fac_name, ": ", levels(X[,fac,T]))
          if (all(idx > 1)){
            beta_prev = beta[(1:min(idx)-1),]
          }else{
            beta_prev = NULL
          }
          if (all(idx < NROW(beta))){
            beta_next = beta[(max(idx)+1):NROW(beta),,drop=F]
          }else{
            beta_next = NULL
          }

          beta = rbind(beta_prev, new_beta, beta_next)
        }
      }

      #plafond..
      beta[beta>1] = 1
      beta[beta<(-1)] = -1

      colnames(beta) = colnames(prefix$Y)
      plot_beta(beta)
      
    })
    
  })
}
    
## To be copied in the UI
# mod_beta_plot_ui("beta_plot_ui_1")
    
## To be copied in the server
# mod_beta_plot_server("beta_plot_ui_1")
