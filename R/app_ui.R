#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    
    
    ui <- shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        # title = "School reopening testing strategy"
        title = "COVID School Model"
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem("Parameters", tabName = "inputs"),
          shinydashboard::menuItem("Graphs", tabName = "graphs"),
          shinydashboard::menuItem("Documentation", tabName = "documentation")
        )
      ),
      shinydashboard::dashboardBody(
        shiny::tags$head(shiny::tags$style(shiny::HTML(".shiny-notification {position: fixed; top: 15% ;left: 1%;"))),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "inputs",
            shiny::h2("Parameters"),
            sidebar_column()),
          
          shinydashboard::tabItem(
            tabName = "graphs",
            shiny::h2("Graphs will appear shortly after simulation finishes running; if they appear distorted, resize the browser window."),
            shiny::fluidRow(
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "attendance and transmission-free rates")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "plot_student_cumul_inc_rates")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "plot_hh_adult_cumul_inc_rates"))),
            shiny::fluidRow(
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "School days missed and classes quarantined")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "Student quarantine rates")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "Rates of infected students in school"))),
            shiny::fluidRow(
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "Daily incidence rates students")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "Daily incidence rates household adults")),
              shinydashboard::box(width = 4, plotly::plotlyOutput(outputId = "Outreach after positive attestations")))
          ),
          
          shinydashboard::tabItem(
            tabName = "documentation",
            shiny::uiOutput("tab")
          )
        )
      ))
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
 
  shiny::tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'school.epi.abm'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

