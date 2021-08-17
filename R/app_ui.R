#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(),
      sidebar = dashboardSidebar(minified = TRUE, collapsed = TRUE),
      body = dashboardBody(
        useShinyjs(),
        mod_objectif_form_ui("objectif_form_ui_1"),
        mod_objectif_form_ui("objectif_form_ui_2"),
        fluidRow(id = "fluidRow_button",
          column(width = 4, actionBttn(inputId = "add_objectif", 
                                       label = "Ajouter objectif",
                                       style = "gradient")),
          column(width = 4, actionBttn(inputId = "add_constraint",
                                       label = "Ajouter contrainte",
                                       style = "gradient")),
          column(width = 4, actionBttn(inputId = "run_simu", 
                                       label = "Lancer simulation",
                                       style = "gradient"))
        ),
        uiOutput("decision_space_ui"),
        uiOutput("tradeoff_plot_ui")
      ),
      controlbar = dashboardControlbar(),
      title = "DashboardPage"
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CDCapp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

