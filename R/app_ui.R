#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
 tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    shinybusy::add_busy_spinner(color = "#bc3c4b", position = "bottom-right", timeout = 150),
    # List the first level UI elements here 
    
    bs4Dash::dashboardPage( dark = FALSE,
     header = bs4Dash::dashboardHeader(title = "Litigii Risc"),
     sidebar = bs4Dash::dashboardSidebar(mod_sidebar_ui("sidebar_ui_1"),
                                    skin = "light",width = "300px"),
    body = bs4Dash::dashboardBody(
          shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "litigii_current",mod_litigii_current_ui("litigii_current_ui_1")),
          shinydashboard::tabItem(tabName = "actualizare_sentinta",
                                  mod_actualizare_sentinte_ui("actualizare_sentinte_ui_1")),
          shinydashboard::tabItem(tabName = "litigiu_numar_nou",
                                  mod_litigii_numar_nou_ui("litigii_numar_nou_ui_1")),
          shinydashboard::tabItem(tabName = "litigiu_nou_automat",
                                  mod_litigii_noi_automate_ui("litigii_noi_automate_ui_1")),
          shinydashboard::tabItem(tabName = "litigii_noi",
                                  mod_litigii_noi_manuale_ui("litigii_noi_manuale_ui_1")),
          shinydashboard::tabItem(tabName = "plati_litigii",
                                  mod_litigii_platite_ui("litigii_platite_ui_1")),
          shinydashboard::tabItem(tabName = "regularizare_provizioane",
                                  mod_regularizare_provizioane_ui("regularizare_provizioane_ui_1")),
          shinydashboard::tabItem(tabName = "litigiu_individual",
                                  mod_litigiu_individual_ui("litigiu_individual_ui_1")),
          
          shinydashboard::tabItem(tabName = "admin",  mod_admin_ui("admin_ui_1"))
          )
    )
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
    #favicon(ico = "external-link-alt-solid",ext = ".svg"),
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'LitiigiiDatabase'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    #waiter::use_waiter(), 
    #waiter::waiter_show_on_load(html = waiter::spin_3circles(),color = "#bc3c4b"),
    #waiter::waiter_on_busy(html = waiter::spin_facebook(),color = "#3cbcad"),
    gfonts::use_font(css_path = "inst/app/www/montserrat_100.css",id = "montserrat")
  )
}

