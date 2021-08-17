#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @import optisure  
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  n_objectif_max = 5
  TT = 10
  data("dataset")
  data("unites")

  static_data = list(
    data = dataset,
    indicators_idx = 18:NCOL(dataset),
    var_decision_idx = c(2,#:3, #enleve breed pour l'instant car trop de modalites fait planter le model quantil
                         6:17),
    unites = unites
  )
  
  static_data$var_decision_quali_name =
    colnames(dataset[static_data$var_decision_idx])[
      !sapply(dataset[static_data$var_decision_idx], is.numeric)]
  static_data$var_decision_quanti_name = 
    colnames(dataset[static_data$var_decision_idx])[
      sapply(dataset[static_data$var_decision_idx], is.numeric)]
  
  r = reactiveValues(
    data = dataset,
    objectif_form = reactiveValues(
      closed = c(FALSE, FALSE),
      formule_ok = c(FALSE, FALSE),
      globale = NULL,
      tau = NULL,
      globale_tau = NULL,
      sens = NULL,
      Y_calc = NULL,
      quantile = NULL,
      new_tau = NULL,
      mask = NULL),
    constraint_form = reactiveValues(
      closed = NULL,
      var_deci_input = NULL,
      constraint_function = NULL,
      value = NULL,
      filled = NULL,
      mask = NULL)
    )


  r = mod_objectif_form_server("objectif_form_ui_1",
                               prefix = list(id = 1, r = r,
                                             static_data = static_data))
  r = mod_objectif_form_server("objectif_form_ui_2",
                               prefix = list(id = 2, r = r, 
                                             static_data = static_data))
  

  
  #add objectif
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
    r$objectif_form$closed[last_id+1] = FALSE
    r$objectif_form$formule_ok[last_id+1] = FALSE
    r = mod_objectif_form_server(paste0("objectif_form_ui_", last_id+1),
                                 prefix = list(id = last_id+1, r = r,
                                               static_data = static_data))
  })
  
  #add constraint
  observeEvent(input$add_constraint,{
    
    #get the last id of the objectif # un peu bourrin...
    previous_constraint = grep(names(input), pattern = "constraint_form_ui_", value = TRUE)
    if (length(previous_constraint) > 0){
      last_id = strsplit(previous_constraint, "-") %>% sapply(function(x){
          strsplit(x[1], "") %>%
            unlist() %>% tail(1) %>% as.numeric()
        }) %>% max()
    }else{
      last_id = 0
    }
    
    insertUI(
      selector = "#fluidRow_button",
      where = c("beforeBegin"),
      ui = mod_constraint_form_ui(paste0("constraint_form_ui_", last_id+1))
    )
    
    r$constraint_form$closed[last_id+1] = FALSE
    r$constraint_form$filled[last_id+1] = FALSE
    r = mod_constraint_form_server(paste0("constraint_form_ui_", last_id+1),
                                 prefix = list(id = last_id+1, r = r,
                                               static_data = static_data))
  })
  
  
  #disable/enable the add_objectif button
  observe({
    if (sum(!r$objectif_form$closed) >= n_objectif_max){
      shinyjs::disable("add_objectif")
    }else{
      shinyjs::enable("add_objectif")
    }
  }) 
  

  #disable/enable the run_simu button if objectif and constraint are filled correctly
  observe({
    if (sum(r$objectif_form$formule_ok[!r$objectif_form$closed]) <
        sum(!r$objectif_form$closed)){
      shinyjs::disable("run_simu")
    }else{
      if (is.null(r$constraint_form$closed)){
        shinyjs::enable("run_simu")
      }else{
        if (any(!r$constraint_form$filled[!r$constraint_form$closed])){
          shinyjs::disable("run_simu")
        }else{
          shinyjs::enable("run_simu")
        }
      }
    }
  })
  

  #mask
  observe({
    mask = as.data.frame(r$objectif_form$mask) %>% apply(1, all)
    if (length(mask) > 0){
      r$data = static_data$data[mask,]
    }
    print(NROW(r$data))
  })
  
  #if run_simu is pressed
  observeEvent(input$run_simu,{
    
    #filter the data with the constraint
    data = isolate(r$data)
    mask = as.data.frame(isolate(r$constraint_form$mask))
    if (NCOL(mask) > 0){
      mask_obj = as.data.frame(isolate(r$objectif_form$mask)) %>% apply(1, all)
      mask = mask %>% apply(1, all)
      data = data[mask[mask_obj],]
      mask_pr_Y = mask_obj & mask 
    }else{
      mask_pr_Y = as.data.frame(isolate(r$objectif_form$mask)) %>% apply(1, all) 
    }
    print(NROW(data))
    
    #set the progress bar 
    progress <- shiny::Progress$new()
    progress$set(message = "Simulation", value = 0)
    on.exit(progress$close())
    
    # TT = 10
    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1/(TT + 4), detail = detail)
    }
    
    #call the optimise function
    Y = as.data.frame(isolate(r$objectif_form$Y_calc))[mask_pr_Y,]
    colnames(Y) = paste0("objectif_", 1:NCOL(Y))
    
    if (NROW(Y) < 200){
      N = NULL
    }else{
      N = 100
    }
    
    res = optisure(X = data[,static_data$var_decision_idx],
                   Y = Y,
                   sens = isolate(r$objectif_form$sens),
                   quantile_utility_idx = isolate(r$objectif_form$quantile),
                   tau = isolate(r$objectif_form$tau),
                   globale_tau = isolate(r$objectif_form$globale_tau),
                   g = isolate(r$constraint_form$constraint_function),
                   X_space_csrt = TRUE,
                   alpha = 0.12,
                   TT = TT,
                   N = N,
                   updateProgress = updateProgress,
                   seed = 123)

    # saveRDS(res, "res2.RDS")
    # res = readRDS("res2.RDS")
    
    #render decision espace 
    output$decision_space_ui = renderUI({
      mod_decision_space_ui("decision_space_ui_1")
    })
    
    mod_decision_space_server("decision_space_ui_1",
                              prefix = list(r = r, static_data = static_data,
                                            data = data, res = res))
    
    #render plot res
    output$tradeoff_plot_ui = renderUI({
      mod_tradeoff_plot_ui("tradeoff_plot_ui_1")
    })
    mod_tradeoff_plot_server("tradeoff_plot_ui_1", 
                             prefix = list(res = res, static_data = static_data,
                                           p = sum(!isolate(r$objectif_form$closed))))
    
  })

}
