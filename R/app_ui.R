#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardBody
#' @import shinydashboardPlus
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyjs hidden useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(tags$script('
                                var windows_dim = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    windows_dim[0] = window.innerWidth;
                                    windows_dim[1] = window.innerHeight;
                                    Shiny.onInputChange("windows_dim", windows_dim);
                                });
                                $(window).resize(function(e) {
                                    windows_dim[0] = window.innerWidth;
                                    windows_dim[1] = window.innerHeight;
                                    Shiny.onInputChange("windows_dim", windows_dim);
                                });
                            ')),
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
                                              style = "gradient")),
                 # column(width = 4, hidden(actionBttn(inputId = "cancel",
                 #                              label = "Stop simulation",
                 #                              style = "gradient")))
                 column(width = 4, actionBttn(inputId = "cancel",
                                                     label = "Stop simulation",
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
      app_title = 'ComViBo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

