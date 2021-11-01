#' objectif_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets radioGroupButtons
mod_objectif_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      id = ns("mybox"),
      title = "Objectif N°",
      status = "warning",
      width = 12,
      closable = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 6, uiOutput(ns("indicators_ui")) ),
        column(width = 6,
               textInput(inputId = ns("formula"), label = "Formule", value = "")),
        column(width = 12,
               textInput(inputId = ns("name"), label = "Nom de l'objectif",
                         value = "")),
        column(width = 6, 
               radioGroupButtons(inputId = ns("sens"), label = "",
                                 choices = c("max", "min"), selected = "max",
                                 justified = TRUE)),
        column(width = 6,
               radioGroupButtons(inputId = ns("uncertainty_choice"), label = "",
                                 choiceNames = c("VaR", "Moyenne"),
                                 choiceValues = c("Quantile", "Espérance"),
                                 selected = "Quantile", justified = TRUE)),
        uiOutput(ns("quantile_choice")) 
      )
      
      
    )
  )
}
    
#' objectif_form Server Functions
#' 
#' @import shinydashboardPlus
#' @importFrom shinyjs runjs
#' @importFrom shinyWidgets radioGroupButtons pickerInput updateRadioGroupButtons
#' @noRd 
mod_objectif_form_server <- function(id, prefix = NULL){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #inform the reactiveValues when the box is closed
    observeEvent(input$mybox$visible, {
      
      if (!input$mybox$visible){
        prefix$r$objectif_form$closed[prefix$id] = TRUE
      }
      
      updateBox("mybox", action = "update",
                options = list(title = h2(paste0("Objectif N°", id_new())) ))
    })
    
    # closable become FALSE when the number of box is lower or equal than 2
    observe({
      if (sum(!prefix$r$objectif_form$closed) <= 2){
        updateBox("mybox", action = "update",
                  options = list(title = h2(paste0("Objectif N°", id_new())),
                                 closable = FALSE))
      }else{
        updateBox("mybox", action = "update",
                  options = list(title =  h2(paste0("Objectif N°", id_new())),
                                 closable = TRUE))
      }
    })
    
    #update number
    id_new = reactive({
      prefix$id - sum(prefix$r$objectif_form$closed[1:prefix$id])
    })
    
    #make the indicator picker with the given list of item
    output$indicators_ui = renderUI({
      pickerInput(inputId = ns("indicators"), label = "Indicateurs",
                  # choices = colnames(prefix$static_data$data)[prefix$static_data$indicators_idx],
                  choices = prefix$static_data$choice_names[prefix$static_data$indicators_idx, 1],
                  choicesOpt = list(
                    content = prefix$static_data$choice_names[prefix$static_data$indicators_idx, 2]
                  ),
                  multiple = TRUE)
    })
    
    #update text input with the choice of indicator
    observe({
      updateTextInput(session = session, inputId = "formula",
                      value = paste(paste0("`", input$indicators, "`"),
                                    collapse = " + "))
    })
    
    observe({
      indicators = prefix$static_data$choice_names %>%
        filter(names %in% input$indicators) %>%
        pull(choice_name)
      updateTextInput(session = session, inputId = "name",
                      value = paste(indicators, collapse = "_et_"))
    })
    
    #function checking the formula
    error_formula = reactive({
      variables = paste0("`", input$indicators, "`")
      eval(parse(text = paste(variables, "=1",  collapse = ";")))
      testit::has_error(eval(parse(text = input$formula)), silent = TRUE)
    })
    
    
    calc_objective = reactive({
      df = prefix$static_data$data
      colnames(df) = paste0("`", colnames(df), "`")
      for (ind in input$indicators){
        ind = paste0("`", ind, "`")
        eval(parse(
          text = paste0(ind, " = c(" , paste0(df[,ind, drop = TRUE],
                                              collapse = ", "), ")")
          ))
      }
      eval(parse(text = input$formula))
    })
    
    #check the formula
    observe({
      if (!is.null(input$indicators)){
        if (error_formula()){
          print("erreur dans la formule")
          prefix$r$objectif_form$formule_ok[prefix$id] = FALSE
          # color = "red"
          # runjs(paste0("document.getElementById('formula').style.border ='", color ,"'"))
          
        }else{
          # color = "green"
          # runjs(paste0("document.getElementById('formula').style.border ='", color ,"'"))
          # print("formule ok")
          prefix$r$objectif_form$formule_ok[prefix$id] = TRUE
        }
      }
    })
    
    #return the allowed dependance for this objective
    observe({
      prefix$r$objectif_form$allowed_dependence[[as.character(prefix$id)]] =
        as.data.frame(apply(prefix$static_data$allowed_dependence[,input$indicators,drop = FALSE], 1, all))
    })
    
    # render global or individual button if uncertainty_choice == "Quantile"
    output$quantile_choice = renderUI({

      if (input$uncertainty_choice == "Quantile"){
        tagList(
          column(width = 6,
                 radioGroupButtons(inputId = ns("global_quantile"), 
                                   label = "Confiance globale",
                                   choiceNames = c("Oui", "Non"),
                                   choiceValues = c("Globale", "Individuel"),
                                   selected = "Globale", justified = TRUE)),
          column(width = 6,
                 sliderInput(inputId = ns("tau"), label = "Risque", min = 0, max = 1,
                             value = 0.5, step = 0.01))
        )
      }
    })
    
    #copy input$tau in reactvalue
    observe({
      if (!is.null(input$tau)){
        prefix$r$objectif_form$tau[prefix$id] = input$tau
      }
    })
    
    observe({
      if (!is.null(input$global_quantile)){
        prefix$r$objectif_form$globale_tau[prefix$id] = input$global_quantile == "Globale"
      }
    })
    
    observe({
      if (!is.null(input$sens)){
        prefix$r$objectif_form$sens[prefix$id] = input$sens
      }
    })
    
    observe({
      # req(input$name)
      # prefix$r$objectif_form$names[prefix$id] = 
      #   prefix$static_data$choice_names %>% filter(names == input$name) %>%
      #   pull(choice_name)
      
      req(input$name)
      prefix$r$objectif_form$names[prefix$id] = input$name
    })
    
    observe({
      if (!is.null(input$indicators)){
        if (!error_formula()){
          prefix$r$objectif_form$Y_calc[[prefix$id]] = calc_objective()
        }
      }
    })
    
    observe({
      if (!is.null(input$uncertainty_choice)){
        prefix$r$objectif_form$quantile[prefix$id] = input$uncertainty_choice == "Quantile"
      }
    })
    
    #copy input$globale_quantile in reactvalue
    observe({
      if (!is.null(input$global_quantile)){
        prefix$r$objectif_form$globale[prefix$id] = input$global_quantile == "Globale"
      }
    })
   
    #link the globale tau together
    observeEvent(input$tau,{
      if (prefix$r$objectif_form$globale[prefix$id]){
        prefix$r$objectif_form$new_tau = input$tau
      }
    })
    
    #link the globale tau together
    observeEvent(prefix$r$objectif_form$new_tau,{
      if(!is.na(prefix$r$objectif_form$globale[prefix$id])){
        if (prefix$r$objectif_form$globale[prefix$id]){
          updateSliderInput(session = session, inputId = "tau",
                            value = prefix$r$objectif_form$new_tau)
        }
      }
    })
    
    #create a mask depending on na value on he indicators selected
    observe({
      if (!is.null(input$indicators)){
        prefix$r$objectif_form$mask[[as.character(prefix$id)]] =
          sapply(input$indicators, function(x){
            col_name = colnames(prefix$static_data$data) == x
            !is.na(prefix$static_data$data[,col_name])
          }) %>% apply(1, all)
      }else{
        prefix$r$objectif_form$mask[[as.character(prefix$id)]] = NULL
      }
    })

    prefix$r
  })
}
    
## To be copied in the UI
# mod_objectif_form_ui("objectif_form_ui_1")
    
## To be copied in the server
# mod_objectif_form_server("objectif_form_ui_1")
