#' constraint_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_constraint_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      id = ns("mybox"),
      title = "Contrainte N°",
      status = "primary",
      width = 12,
      closable = TRUE,
      collapsible = TRUE,
      tagList( 
        fluidRow(
          column(width = 6, uiOutput(ns("var_deci_ui"))),
          column(width = 2, pickerInput(inputId = ns("operator"),
                                        choices = c(">", "<", "="),
                                        options = list(style = "btn-primary"))),
          uiOutput(ns("value_ui"))
        )
      )
    )
  )
}
    
#' constraint_form Server Functions
#'
#' @importFrom dplyr pull filter
#' @importFrom shinyWidgets updatePickerInput
#' @noRd 
mod_constraint_form_server <- function(id, prefix = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$mybox$visible, {
      
      if (!input$mybox$visible){
        prefix$r$constraint_form$closed[prefix$id] = TRUE
      }
      
      updateBox("mybox", action = "update",
                options = list(title = h2(paste0("Contrainte N°", id_new())) ))
    })
    
    observe({
      updateBox("mybox", action = "update",
                  options = list(title =  h2(paste0("Contrainte N°", id_new())) ))
    })

    #update number
    id_new = reactive({
      prefix$id - sum(prefix$r$constraint_form$closed[1:prefix$id])
    })
    
    
    #make the indicator picker with the given list of item
    output$var_deci_ui = renderUI({
      pickerInput(inputId = ns("var_deci"), 
                  choices = prefix$static_data$choice_names[prefix$static_data$var_decision_idx, 1],
                  choicesOpt = list(
                    content = prefix$static_data$choice_names[prefix$static_data$var_decision_idx, 2]
                  ),
                  multiple = FALSE) #can change multiple arg ??
    })
    

    observe({
      prefix$r$constraint_form$var_deci_input = input$var_deci
    })
    
    # #update text input with the choice of indicator
    # observe({
    #   updateTextInput(session = session, inputId = "formula",
    #                   value = paste(input$var_deci, collapse = " + "))
    # })
    
    #add the possibility to use "in" if there is only one categorical indicator in he formula 
    observe({
      req(input$var_deci)
      if (!is.null(input$var_deci)){
        if (input$var_deci %in% prefix$static_data$var_decision_quali_name){
          updatePickerInput(session = session, inputId = "operator", 
                            choices = c("%in%", "not %in%"),
                            choicesOpt = list( content = c("in", "not in") ))
        }else{
          updatePickerInput(session = session, inputId = "operator", 
                            choices = c(">", "<", "==", "!="),
                            choicesOpt = list( content = c(">", "<", "=", "!=") ))
        }
      }
    })
    
    #change le ui de value si c'est une quali
    output$value_ui = renderUI({
      req(input$var_deci)
      if (!is.null(input$var_deci)){
        if (!input$var_deci %in% prefix$static_data$var_decision_quali_name){
          column(width = 2, numericInput(inputId = ns("value"), label = "", 
                                         value = 0)) 
        }else{
          
          choices = as.character(unique(prefix$r$data[,input$var_deci, drop = TRUE]))
          column(width = 4, pickerInput(inputId = ns("value"), choices = choices,
                                        # selected = choices[1], 
                                        multiple = TRUE))
        }
      }
    })
    
    #tell if the constraint is dully filled
    # observe({
    #   if (!is.null(input$var_deci)){
    #     if (input$var_deci %in% prefix$static_data$var_decision_quali_name){
    #       if (!is.null(input$value)){
    #         if (length(input$value) > 1){
    #           prefix$r$constraint_form$filled[prefix$id] = TRUE
    #         }else{
    #           prefix$r$constraint_form$filled[prefix$id] = input$value != "0"
    #         }
    #       }else{
    #         prefix$r$constraint_form$filled[prefix$id] = FALSE
    #       }
    #     }else{
    #       if (!is.null(input$value)){
    #         prefix$r$constraint_form$filled[prefix$id] = TRUE
    #       }else{
    #         prefix$r$constraint_form$filled[prefix$id] = FALSE
    #       }
    #     }
    #   }else{
    #     prefix$r$constraint_form$filled[prefix$id] = FALSE
    #   }
    # })
    
    # #create a mask depending on the constraint registered
    # observe({
    #   if (prefix$r$constraint_form$filled[prefix$id]){
    # 
    #       operator = ifelse(input$operator %in% c("in", "not in"), "%in%", input$operator)
    # 
    #       mask = eval(parse(
    #         text = paste("prefix$static_data$data[,input$var_deci, drop = TRUE]",
    #                     operator, "input$value")
    #         ))
    # 
    # 
    #       if (input$operator == "not in"){
    #         mask = !mask
    #       }
    # 
    #       prefix$r$constraint_form$mask[[prefix$id]] = mask
    # 
    #   }else{
    #     prefix$r$constraint_form$mask[[prefix$id]] =
    #       rep(TRUE, NROW(prefix$static_data$data))
    #   }
    # })
    
    f = reactive({
      req(input$value)
      req(input$operator)
      req(input$var_deci)
      if (!is.null(input$value)){
        f = function(X, var, operator, value){
          X = as.data.frame(X)
          if (operator == "not %in%"){
            !X[,var,drop=TRUE] %in% value
          }else{
            eval(parse( text = paste("X[,var,drop=TRUE]", operator, "value") ))
          }
        }
        
        formals(f)$var = paste0(input$var_deci)
        formals(f)$operator = input$operator
        formals(f)$value = input$value
        f
      }else NULL
    })
    
    observe({ prefix$r$constraint_form$constraint_function[[prefix$id]] = f() })
    
    prefix$r
    
  })
}
    
## To be copied in the UI
# mod_constraint_form_ui("constraint_form_ui_1")
    
## To be copied in the server
# mod_constraint_form_server("constraint_form_ui_1")
