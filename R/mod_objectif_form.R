#' objectif_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
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
      tagList(
        uiOutput(ns("indicators_ui")),
        textInput(inputId = ns("formula"), label = "Formule", value = ""),
        radioGroupButtons(inputId = ns("sens"), label = "",
          choices = c("Max", "Min"), selected = "Max", 
          justified = TRUE),
        radioGroupButtons(inputId = ns("uncertainty_choice"), label = "",
                          choices = c("Quantile", "Espérance"), 
                          selected = "Quantile", justified = TRUE),
        uiOutput(ns("quantile_choice"))
      )
      
      
    )
  )
}
    
#' objectif_form Server Functions
#' 
#' @import shinydashboardPlus
#' @import shinyjs
#' @noRd 
mod_objectif_form_server <- function(id, prefix = NULL){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$mybox$visible, {
      
      if (!input$mybox$visible){
        prefix$r$closed[prefix$id] = TRUE
      }
      
      updateBox("mybox", action = "update",
                options = list(title = h2(paste0("Objectif N°", id_new())) ))
    })
    
    # closable become FALSE when the number of box is lower or equal than 2
    observe({
      if (sum(!prefix$r$closed) <= 2){
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
      prefix$id - sum(prefix$r$closed[1:prefix$id])
    })
    
    #make the indicator picker with the given list of item
    output$indicators_ui = renderUI({
      pickerInput(inputId = ns("indicators"), label = "Indicateurs",
                  choices = LETTERS[1:4], multiple = TRUE)
    })
    
    #update text input with the choice of indicator
    observe({
      updateTextInput(session = session, inputId = "formula",
                      value = paste(input$indicators, collapse = " + "))
    })
    
    #function checking the formula
    error_formula = function(variables, formule){
      eval(parse(text = paste(variables, "=1",  collapse = ";")))
      testit::has_error(eval(parse(text = formule)), silent = TRUE)
    }
    
    #check the formula
    observe({
      if (!is.null(input$indicators)){
        if (error_formula(variables = input$indicators, formule = input$formula)){
          print("erreur dans la formule")
          prefix$r$formule_ok[prefix$id] = FALSE
          # color = "red"
          # runjs(paste0("document.getElementById('formula').style.border ='", color ,"'"))
          
        }else{
          # color = "green"
          # runjs(paste0("document.getElementById('formula').style.border ='", color ,"'"))
          # print("formule ok")
          prefix$r$formule_ok[prefix$id] = TRUE
        }
      }
    })
    
    # render global or individual button if uncertainty_choice == "Quantile"
    output$quantile_choice = renderUI({

      if (input$uncertainty_choice == "Quantile"){
        tagList(
          radioGroupButtons(inputId = ns("global_quantile"), label = "",
                            choices = c("Globale", "Individuel"),
                            selected = "Globale", justified = TRUE),
          sliderInput(inputId = ns("tau"), label = "Risque", min = 0, max = 1,
                      value = 0.5, step = 0.01)
        )
      }
    })
    
    observe({
      if (!is.null(input$tau)){
        prefix$r$tau[prefix$id] = input$tau
      }
    })
    
    observe({
      if (!is.null(input$global_quantile)){
        prefix$r$globale[prefix$id] = input$global_quantile == "Globale"
      }
    })
   
    observeEvent(input$tau,{
      if (prefix$r$globale[prefix$id]){
        prefix$r$new_tau = input$tau
      }
    })
    
    observeEvent(prefix$r$new_tau,{
      if(!is.na(prefix$r$globale[prefix$id])){
        if (prefix$r$globale[prefix$id]){
          updateSliderInput(session = session, inputId = "tau",
                            value = prefix$r$new_tau)
        }
      }
    })

    prefix$r
  })
}
    
## To be copied in the UI
# mod_objectif_form_ui("objectif_form_ui_1")
    
## To be copied in the server
# mod_objectif_form_server("objectif_form_ui_1")
