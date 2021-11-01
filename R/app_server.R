#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>% 
#' @importFrom promises %...>% catch
#' @importFrom future plan multisession future
#' @import MOOVaR 
#' @importFrom shinyjs toggle show enable disable runjs
#' @noRd
app_server <- function( input, output, session ) {
  
  n_objectif_max = 5
  B =  10
  TT = 2
  N=10
  data("data_appli")
  dataset = data_appli
  data("choice_names")
  
  running = reactiveVal()
  running(FALSE)
  first_time_running = reactiveVal(TRUE)
  id_notif = reactiveVal()
  path_tracking = "tracking.RDS"
  N_tracking = 1 + 1 + TT + 3 
  size_by_plot = 230 #200

  
  static_data = list(
    data = dataset,
    indicators_idx = 20:48,
    x_cov_adjust = c(1:4, 12:13, 16), #enleve maturation et temperature
    var_decision_idx = c(2:6, 
                         7:12,14, #bouffe sans betterave
                         15:19), 
    choice_names = choice_names
  )
  
  #prevent dependance between indicators and variable
  allowed_dependence = matrix(TRUE, nrow = length(static_data$var_decision_idx), 
                              ncol = length(static_data$indicators_idx))
  
  rownames(allowed_dependence) = colnames(dataset)[static_data$var_decision_idx]
  colnames(allowed_dependence) = colnames(dataset)[static_data$indicators_idx]
  allowed_dependence["TEMPERATURE", c(1:21, 26:29)] = FALSE
  allowed_dependence["MATURATION", c(1:21, 27:29)] = FALSE
  
  static_data$allowed_dependence = allowed_dependence
  
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
      names = NULL,
      new_tau = NULL,
      mask = NULL,
      allowed_dependence = NULL),
    constraint_form = reactiveValues(
      closed = NULL,
      var_deci_input = NULL,
      constraint_function = list(),
      value = NULL,
      filled = NULL,
      mask = NULL),
    tradeoff_plot = reactiveValues(
      n_y = NULL,
      choice_plot_axes = reactiveValues(
        x = NULL, 
        y = NULL,
        changed = FALSE)
      )
    )

  observe({
    print(input$windows_dim[1])
    r$window_width = input$windows_dim[1]
    r$n_x_plot = input$windows_dim[1] %/% size_by_plot 
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
  
  
  #disable/enable the run_simu button if objectif is filled correctly
  observe({
    if (sum(r$objectif_form$formule_ok[!r$objectif_form$closed]) <
        sum(!r$objectif_form$closed)){
      disable("run_simu")
    }else{
      enable("run_simu")
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

    toggle("add_objectif")
    toggle("add_constraint")
    toggle("run_simu")
    show("cancel")

    if (first_time_running()){
      first_time_running(FALSE)
      plan(multisession)
    }

    tracking = list()
    tracking[[1]] = "begenning"
    saveRDS(tracking, path_tracking)

    # data = isolate(r$data)
    # mask = as.data.frame(isolate(r$constraint_form$mask))
    # if (NCOL(mask) > 0){
    #   mask_obj = as.data.frame(isolate(r$objectif_form$mask)) %>% apply(1, all)
    #   mask = mask %>% apply(1, all)
    #   data = data[mask[mask_obj],]
    #   mask_pr_Y = mask_obj & mask
    # }else{
    #   mask_pr_Y = as.data.frame(isolate(r$objectif_form$mask)) %>% apply(1, all)
    # }
    # print(NROW(data))
    # 
    # Y = as.data.frame(isolate(r$objectif_form$Y_calc))[mask_pr_Y,]
    # colnames(Y) = paste0("objectif_", 1:NCOL(Y))
    # 
    # X = data[,static_data$var_decision_idx]
    # is_fac = !sapply(X, is.numeric)
    # 
    # for (i in which(is_fac)){
    #   X[,i] = droplevels(X[,i,T])
    # }
    X = static_data$data[,c(1, static_data$var_decision_idx)]
    Y = as.data.frame(isolate(r$objectif_form$Y_calc))
    # colnames(Y) = isolate(r$objectif_form$names)
    
    list_dep = isolate(r$objectif_form$allowed_dependence)
    allowed_dependence = as.matrix(as.data.frame(list_dep))
    colnames(allowed_dependence) = colnames(Y)
    allowed_dependence = allowed_dependence[,order(as.numeric(names(list_dep)))]
    
    res(NULL)
    thread = future({
      MOOVaR(X = X,
             Y = Y,
             sens = isolate(r$objectif_form$sens),
             quantile_utility_idx = isolate(r$objectif_form$quantile),
             tau = isolate(r$objectif_form$tau),
             globale_tau = isolate(r$objectif_form$globale_tau),
             g = isolate(r$constraint_form$constraint_function),
             # X_space_csrt = TRUE,
             # alpha = 0.50,
             allowed_dependence = allowed_dependence,
             optim_method = "real_ind",
             # TT = TT,
             # N = N,
             path_tracking = path_tracking,
             seed = 123)
    }) %...>% res()

    thread <- catch(thread,
                    function(e){
                      print(e$message)
                      if (e$message == "missing value where TRUE/FALSE needed"){
                        showNotification(paste0("Sorry not enough data (n=", 
                                                NROW(Y),
                                                ") to estimate the models"),
                                         duration = 10,
                                         closeButton = TRUE,
                                         type = "error")
                      }else if (e$message == "subscript out of bounds"){
                        showNotification(paste0("Sorry not enough data (n=", 
                                                NROW(Y),
                                                ") to estimate the models"),
                                         duration = 10,
                                         closeButton = TRUE,
                                         type = "error")
                      }
                      running(FALSE)
                    })

    NULL
  })
  
  # notif running status
  notif_update <- reactiveTimer(1100)
  observe({
    if (running() & !is.null(id_notif())){
      tracking = readRDS(path_tracking)
      
      if (tracking[[length(tracking)]] == "stop"){
        showNotification(id = id_notif(),
                         paste0("Stoping..."),
                         duration = 1,
                         closeButton = FALSE,
                         type = "message")
      }else{
        showNotification(id = id_notif(),
                         # paste0("Running: ", tracking[[length(tracking)]],
                         #        " --> ",
                         #        round(100 * length(tracking) / N_tracking),
                         #        "%"),
                         paste0("Running"),
                         duration = 1,
                         closeButton = FALSE,
                         type = "message")
      }  
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
    disable("cancel")
  })


  #render decision espace
  output$decision_space_ui = renderUI({
    req(res())
    mod_decision_space_ui("decision_space_ui_1")
  })

  observe({
    req(res())
    # saveRDS(res(), "res_simu.RDS")
    # saveRDS(res(), "res_3p.RDS")
    res = readRDS("res_simu.RDS")
    mod_decision_space_server("decision_space_ui_1",
                              prefix = list(r = r, static_data = static_data,
                                            data = data, res = res()))
    })

  output$beta_plot_ui = renderUI({
    req(res())
    mod_beta_plot_ui("beta_plot_ui_1")
  })

  observe({
    req(res())
    X = static_data$data[,c(1, static_data$var_decision_idx)]
    Y = as.data.frame(isolate(r$objectif_form$Y_calc))
    colnames(Y) = isolate(r$objectif_form$names)
    mod_beta_plot_server("beta_plot_ui_1",
                         prefix = list(res = res(), r = r,
                                       X = X, Y = Y, B = B,
                                       allowed_dependence = allowed_dependence,
                                       static_data = static_data))
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

  #launch choice_plot_axes_ui modules
  observe({
    if (!is.null(r$tradeoff_plot$n_y)){
      for (i in 1:r$tradeoff_plot$n_y){
        r = mod_choice_plot_axes_server(paste0("choice_plot_axes_ui_", i),
                                        prefix = list(r = r, id = i,
                                                      p = sum(!isolate(r$objectif_form$closed)) ))
      }
    }
  })
  
}
