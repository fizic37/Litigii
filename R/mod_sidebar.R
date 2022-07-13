#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  
  bs4Dash::sidebarMenuOutput(outputId = ns("sidebar"))
  
}
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(id, vals) {
  
  moduleServer(id, function(input, output, session) {
 
     risk_user_sidebar <- bs4Dash::sidebarMenu(
    id = session$ns("tabs"),
    bs4Dash::menuItem(
      tabName = "home",
      text = "Home",
      icon = icon("home"),
      selected = F),
    
    bs4Dash::menuItem(
      tabName = "litigii",
      text = "Litigii actualizate",
      icon = icon("balance-scale"),
      selected = FALSE,startExpanded = TRUE,
      
      bs4Dash::menuSubItem(
        text = "Litigii la zi ",
       tabName = "litigii_current",
       selected = FALSE,
        icon = icon("clock") ),
      
      bs4Dash::menuSubItem(
        text = "Regularizare provizioane",
        icon = icon("calculator"),
        tabName = "regularizare_provizioane",
        selected = TRUE  ) ),
    
    bs4Dash::menuItem(
      tabName = "actualizare_sentinta",
      icon = icon("external-link-alt"),selected = FALSE,
      text = "Solutii noi dosare existente" ),
    
    bs4Dash::menuItem(
      tabName = "litigii_noi",
      text = "Litigii noi",
      icon = icon("plus-square"),
      selected = FALSE,
      
      bs4Dash::menuSubItem(
        tabName = "litigiu_numar_nou",
        icon = icon("edit"), selected = FALSE,
        text = "Litigii existente cu numar nou"),
      
      bs4Dash::menuSubItem(
        tabName = "litigiu_nou_automat",
        icon = icon("laptop-code"), selected = FALSE,
        text = "Litigii noi portal just"),
      
      bs4Dash::menuSubItem(
        tabName = "litigii_noi",
        text = "Litigii noi manuale",
        icon = icon("keyboard"),
        selected = FALSE) ),
      
      bs4Dash::menuItem(
        tabName = "plati_litigii",
        icon = icon("euro-sign"),selected = FALSE,
        text = "Actualizare litigii platite"),
    
    bs4Dash::menuItem(
      tabName = "litigiu_individual",
      selected = FALSE,icon = icon("search"),
      text = "Check litigiu individual"),
    
    bs4Dash::menuItem(tabName = "admin",text = "Admin",icon = icon("tools"),selected = F)
    
  )
     
  
  output$sidebar <- bs4Dash::renderMenu(risk_user_sidebar)
  
  
  
  observeEvent(input$tabs,{ 
    # I use this in order to have a selection of all inputs in sidebar. This way, I don`t have to call modules
    # every time a sidebar is selected, I only call modules ones.`
    vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)})
  
  
} )
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 
