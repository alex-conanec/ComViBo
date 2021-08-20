#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @importFrom promises %...>% catch
#' @importFrom future plan multisession future
#' @import optisure  
#' @noRd
app_server <- function( input, output, session ) {
  
  n_objectif_max = 5
  TT = 2
  N=10
  data("data_quali")
  dataset = data_quali
  data("unites")
  
  running = reactiveVal()
  running(FALSE)
  first_time_running = reactiveVal(TRUE)
  id_notif = reactiveVal()
  path_tracking = "tracking.RDS"
  N_tracking = 1 + 1 + TT + 3 
  size_by_plot = 100

  
  static_data = list(
    data = dataset,
    indicators_idx = 18:NCOL(dataset),
    # var_decision_idx = c(2,#:3, #enleve breed pour l'instant car trop de modalites fait planter le model quantil
    #                      6:17),
    var_decision_idx = c(6:16), #avec data_quali
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
    n_x_plot = NULL,
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

  observe({ r$n_x_plot = input$windows_dim[1] %/% size_by_plot })

  observe({
    req(r$n_x_plot)
    print(r$n_x_plot)
  })
  
  r = mod_objectif_form_server("objectif_form_ui_1",
                               prefix = list(id = 1, r = r,
                                             static_data = static_data))
  r = mod_objectif_form_server("objectif_form_ui_2",
                               prefix = list(id = 2, r = r, 
                                             static_data = static_data))
  

  observe({
    if (!running()){
      show("add_objectif")
      show("add_constraint")
      show("run_simu")
      toggle("cancel")
    }else{
      toggle("add_objectif")
      toggle("add_constraint")
      toggle("run_simu")
      show("cancel")
    }
  })
  
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
  
  
  #to prevent time out
  autoInvalidate1 <- reactiveTimer(1000)
  observe({
    autoInvalidate1()
    cat("---\n")
  })
  
  

  res <- reactiveVal()
  
  #if run_simu is pressed
  observeEvent(input$run_simu,{
    
    id_notif(NULL)
    running(TRUE)
    
    if (first_time_running()){
      first_time_running(FALSE)
      plan(multisession)
    }
    

    tracking = list()
    tracking[[1]] = "begenning"
    saveRDS(tracking, path_tracking)
    
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
    
    Y = as.data.frame(isolate(r$objectif_form$Y_calc))[mask_pr_Y,]
    colnames(Y) = paste0("objectif_", 1:NCOL(Y))
    
    X = data[,static_data$var_decision_idx]
    is_fac = !sapply(X, is.numeric)
    for (i in which(is_fac)){
      X[,i] = droplevels(X[,i,T])
    }
    
    res(NULL)
    thread = future({
      optisure(X = X,
               Y = Y,
               sens = isolate(r$objectif_form$sens),
               quantile_utility_idx = isolate(r$objectif_form$quantile),
               tau = isolate(r$objectif_form$tau),
               globale_tau = isolate(r$objectif_form$globale_tau),
               g = isolate(r$constraint_form$constraint_function),
               X_space_csrt = TRUE,
               alpha = 0.50,
               TT = TT,
               N = N,
               path_tracking = path_tracking,
               seed = 123)
    }) %...>% res() 
  
    thread <- catch(thread,
                    function(e){
                      print(e$message)
                      running(FALSE)
                    })

    NULL
  })

  # notif running status
  notif_update <- reactiveTimer(1100)
  observe({
    if (running() & !is.null(id_notif())){
      tracking = readRDS(path_tracking)
      showNotification(id = id_notif(),
                       paste0("Running: ", tracking[[length(tracking)]],
                              " --> ", 
                              round(100 * length(tracking) / N_tracking),
                              "%"),
                       duration = 1,
                       closeButton = FALSE,
                       type = "message")
    }
  })
  
  observeEvent(notif_update(), {
    if (running()){
      old_id_notif = id_notif()
      if (!is.null(old_id_notif)){
        removeNotification(id = old_id_notif, session = session)
      }
      Sys.sleep(0.1)
      id_notif(paste0("notif_", runif(1)))
    }
  })
  
  observe({
    req(res())
    running(FALSE)
    # runjs(' #tentative d'accroche...
    #   document.getElementById("decision_space_ui").scrollIntoView();
    # ')
  })
  
  observeEvent(input$cancel, {
    tracking = readRDS(path_tracking)
    tracking[[length(tracking)+1]] = "stop"
    saveRDS(tracking, path_tracking)
  })
  
  
  #render decision espace 
  output$decision_space_ui = renderUI({
    req(res())
    mod_decision_space_ui("decision_space_ui_1")
  })

  observe({
    req(res())
    mod_decision_space_server("decision_space_ui_1",
                              prefix = list(r = r, static_data = static_data,
                                            data = data, res = res()))
    })
  



  #render plot res
  output$tradeoff_plot_ui = renderUI({
    req(res())
    mod_tradeoff_plot_ui("tradeoff_plot_ui_1")
  })
  
  observe({
    req(res())
    mod_tradeoff_plot_server("tradeoff_plot_ui_1",
                             prefix = list(res = res(), static_data = static_data,
                                           r = r,
                                           p = sum(!isolate(r$objectif_form$closed))))
  })
  
}
