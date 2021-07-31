#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(),
      sidebar = dashboardSidebar(minified = TRUE, collapsed = TRUE),
      body = dashboardBody(
        mod_objectif_form_ui("objectif_form_ui_1"),
        mod_objectif_form_ui("objectif_form_ui_2"),
        actionBttn("add_objectif", "add")
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

