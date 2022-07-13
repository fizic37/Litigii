#' litigii_noi_manuale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_noi_manuale_ui <- function(id){
  ns <- NS(id)
  
    fluidRow(
      shinyjs::useShinyjs(),
      
  shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),
  
  
  actionButton(inputId = ns("new_litigiu"), label = "Litigiu nou manual", 
               style = "color: #3cbcad; border-color: #fff;", width = "20%", icon = icon('plus'))
  
  )
}
    
#' litigii_noi_manuale Server Function
#'
#' @noRd 
mod_litigii_noi_manuale_server <- function(input, output, session, vals){
  ns <- session$ns
  
  vals_litigii_noi_manuale <- reactiveValues()
  
  bi_contracte <- readRDS("R/reactivedata/bi_contracte.rds")
  
  
  observeEvent(input$new_litigiu,{
    
    showModal(modalDialog(title = "New Litigiu", size = "l", footer = list(modalButton("Cancel"),
            actionButton(session$ns("save_new_litigiu_manual"),label = "Save",icon = icon("save"))),
                          fluidRow(
                            #shinyjs::useShinyjs(),
                            column(width = 6,
                                   textInput(inputId = session$ns("numar_dosar_litigiu_nou_manual"),
                                             label = "Numarul dosarului:", value = ""),
                                   
                                  shinyWidgets::pickerInput(session$ns("banca_litigiu_nou_manual"),
                                            label = "Selecteaza banca:",
                                               choices = c("",unique(bi_contracte$Banca))),
                                   
                                   shinyWidgets::pickerInput(session$ns("beneficiar_litigiu_nou_manual"),
                                      options=list('live-search' = TRUE),"Selecteaza beneficiarul",choices = ""),
                                   
                                   shinyWidgets::pickerInput(session$ns("contract_litigiu_nou_manual"),
                                            "Selecteaza contractul de garantare",   choices = "",
                                      options = list('live-search' = TRUE)),
                                   
                                  uiOutput(session$ns("show_cui_cod_partener")),
                                  verbatimTextOutput(ns("diverse"))),
                                   
                             
                           column(width = 6, 
                                   shinyWidgets::autonumericInput(inputId = ns("valoare_litigiu_nou_manual"), 
                                        value = 0,align = "right",
                                      digitGroupSeparator = ",",decimalPlaces = 2, decimalCharacter = ".",
                                     label = "Introdu valoarea litigiului in lei", width = "400px"),
                                     
                                   
                                   shinyWidgets::pickerInput(session$ns("tip_litigiu_nou_manual"),
                                               label = "Selecteaza tipul litigiului",width = "370px",
                                      choices = unique(vals$litigii_curente$Tip_litigiu), selected = "somatie"),
                                   
                                   dateInput(inputId = session$ns("data_litigiu_nou_manual"),
                                             min = as.Date("2018-01-01"),format = "dd MM yyyy",language = "ro",
                                             label = "Data dosarului:",value = Sys.Date(), width = "370px")
                                   )
                          ) )  )
   
  })
  
  observeEvent(input$numar_dosar_litigiu_nou_manual,{
    
    shinyFeedback::feedbackDanger(inputId = "numar_dosar_litigiu_nou_manual",
                                  show = input$numar_dosar_litigiu_nou_manual == "",color = "red", session = session,
                                  text = "Completeaza numarul dosarului pentru a activa butonul save")
    
   #if(input$numar_dosar_litigiu_nou_manual == "") shinyjs::disable(id = 'save_new_litigiu_manual', asis = FALSE)
    
  })
  
  observeEvent(c(input$numar_dosar_litigiu_nou_manual,input$valoare_litigiu_nou_manual), {
    
    if (!is.null(input$valoare_litigiu_nou_manual) && input$valoare_litigiu_nou_manual > 1000 && 
        input$numar_dosar_litigiu_nou_manual != "") (shinyjs::enable(id = "save_new_litigiu_manual", asis = FALSE))
    
  })
  
  observeEvent(input$valoare_litigiu_nou_manual,{
    
    
    
   if( length(input$valoare_litigiu_nou_manual) == 0 ||  input$valoare_litigiu_nou_manual < 1000) shinyjs::disable(id = 'save_new_litigiu_manual', asis = FALSE)
    
  })
  
  
  
  observeEvent(input$banca_litigiu_nou_manual, {
    
    vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual <-   bi_contracte %>%
      dplyr::filter(Banca == input$banca_litigiu_nou_manual)
    
    shinyWidgets::updatePickerInput(session = session,
                                    inputId = "contract_litigiu_nou_manual", selected = character(0),
                                    choices = c(
                                      character(0),
                                      vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                                        dplyr::pull(NumarContract) %>% unique()   )    )
    
    shinyWidgets::updatePickerInput(session = session,
                                    inputId = "beneficiar_litigiu_nou_manual", selected = character(0),
                                    choices = c(character(0),
                                                vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                                                  dplyr::pull(NumeBeneficiar) %>% unique()   ) )    
    
  })
  
  observeEvent(input$beneficiar_litigiu_nou_manual,{ 
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "contract_litigiu_nou_manual",
      choices = c(vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                    dplyr::pull(NumarContract) %>% unique())    )
    
    output$show_cui_cod_partener   <- renderUI({ req(!is.null(input$beneficiar_litigiu_nou_manual))
      tagList(
        shinyWidgets::pickerInput(session$ns("cui_beneficiar_litigiu_nou_manual"),
                                  "CUI-ul beneficiarului:",   
                                  choices = vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                    dplyr::pull(CUI) %>% unique()  ) ,
        
        shinyWidgets::pickerInput(session$ns("cod_partener_beneficiar_litigiu_nou_manual"),
                                  "Cod partener:",   
                                  choices = vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
                                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou_manual) %>%
                                    dplyr::pull(CodPartener) %>% unique())
      )
    })
    
  })
  
  observeEvent(input$contract_litigiu_nou_manual, { 
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "beneficiar_litigiu_nou_manual",
      choices = c(
        vals_litigii_noi_manuale$bi_contracte_banca_selectata_litigiu_nou_manual %>%
          dplyr::filter(NumarContract == input$contract_litigiu_nou_manual) %>%
          dplyr::pull(NumeBeneficiar) %>% unique(), ""     )  )
  })
  
  # Observer for save litigiu nou manual
  observeEvent(input$save_new_litigiu_manual,{
    
    if (input$numar_dosar_litigiu_nou_manual == "") {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                   text = "Trebuie sa completetezi numarul dosarului",btn_colors = '#3cbcad')
    }
    
    else if (as.numeric(input$valoare_litigiu_nou_manual) < 2) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                   text = "Nu pot salva un litigiu nou fara valoare",btn_colors = '#3cbcad')
    }
    
    else {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_new_litigiu_manual"),
                                     btn_colors = c("#bc3c4b","#3cbcad"),type = "success",
                                     title = "CONFIRM",text = "Esti sigur ca vrei sa salvezi litigiul nou?")
    }
  })
  
  observeEvent(input$confirm_new_litigiu_manual,{
    
    removeModal(session = session)
    
    sql_insert_main <-
      "INSERT INTO main_litigii (data_initiala_dosar, Tip_litigiu, contract_garantare, Banca, IMM, Cod_Partener, CUI) VALUES (?id1,?id2,?id3,?id4,?id5,?id6,?id7);"
    sql_insert_valoare <-
      "INSERT INTO valoare_litigiu (id_litigiu,valoare_litigiu,data_modificare) VALUES (last_insert_id(),?id8,?id1);"
    sql_insert_stare <-
      "INSERT INTO stare_litigiu (id_litigiu, stare_litigiu, from_date, to_date) VALUES (last_insert_id(), 'activ',?id1,'9999-12-31');"
    sql_insert_actualizare_litigii <- 
      "INSERT INTO actualizare_litigiu (id_litigiu, numar_litigiu, data_modificare) VALUES (last_insert_id(),?id9,?id1);"
    
    tryCatch(      expr = {
          db <- config::get("database", file = "inst/golem-config.yml")
    
          my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, 
                                          dbname = db$dbname, host = db$host)
    
    vals_litigii_noi_manuale$sql_query_insert_main <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_main,
                            id1 = input$data_litigiu_nou_manual,
                            id2 = input$tip_litigiu_nou_manual,
                            id3 = ifelse(is.null(input$contract_litigiu_nou_manual),NA,input$contract_litigiu_nou_manual),
                            id4 = ifelse(is.null(input$banca_litigiu_nou_manual), NA, input$banca_litigiu_nou_manual),
                            id5 = ifelse(is.null(input$beneficiar_litigiu_nou_manual), NA, input$beneficiar_litigiu_nou_manual),
                            id6 = ifelse(is.null(input$cod_partener_beneficiar_litigiu_nou_manual), NA, input$cod_partener_beneficiar_litigiu_nou_manual),
                            id7 = ifelse(is.null(input$cui_beneficiar_litigiu_nou_manual), NA, input$cui_beneficiar_litigiu_nou_manual))
    
    
    vals_litigii_noi_manuale$sql_query_insert_valoare <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_valoare,
                       id8 = input$valoare_litigiu_nou_manual,     id1 = input$data_litigiu_nou_manual)
    
    vals_litigii_noi_manuale$sql_query_insert_stare <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_stare,
                                                                           id1 = input$data_litigiu_nou_manual)
    
    vals_litigii_noi_manuale$sql_query_insert_actualizare <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_actualizare_litigii,
                                          id9 = input$numar_dosar_litigiu_nou_manual,  id1 = input$data_litigiu_nou_manual)
    
    DBI::dbSendQuery(my_connection,
                     statement = isolate(vals_litigii_noi_manuale$sql_query_insert_main))
    
    DBI::dbSendQuery(my_connection,
                     statement = isolate(vals_litigii_noi_manuale$sql_query_insert_valoare))
    
    DBI::dbSendQuery(my_connection,
                     statement = isolate(vals_litigii_noi_manuale$sql_query_insert_stare))
    
    DBI::dbSendQuery(my_connection,
                     statement = isolate(vals_litigii_noi_manuale$sql_query_insert_actualizare))
    DBI::dbDisconnect(conn = my_connection)
    
    shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
      .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
        }, error = function(e) {
          shinyFeedback::showToast(message = "Database error",conditionMessage(e),type = "error",title = "ERROR",
                .options = list("timeOut"=0, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        } )
  })
}
    
## To be copied in the UI
# mod_litigii_noi_manuale_ui("litigii_noi_manuale_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_noi_manuale_server, "litigii_noi_manuale_ui_1")
 
