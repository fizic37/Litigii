#' individual_sentinta_noua UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_individual_sentinta_noua_ui <- function(id){
  ns <- NS(id)
  
  # This modules handles Adauga sentinta noua button from check litigiu individual sidebar menu - sentinte
  tagList(
  actionButton(inputId = ns("new_sentinta"), label = "Adauga o sentinta noua", 
               style = "color: #3cbcad; border-color: #fff;", width = "20%", icon = icon('plus')),
  DT::dataTableOutput(ns("sentinte"))
  )
}
    
#' individual_sentinta_noua Server Functions
#'
#' @noRd 
mod_individual_sentinta_noua_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_sentinte <- reactiveValues()
    
    db <- config::get("database", file = "inst/golem-config.yml")
    
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, 
                                    dbname = db$dbname, host = db$host)
    
    sentinte <- my_connection %>% dplyr::tbl("sentinte") %>% dplyr::left_join(y = my_connection %>%
                                dplyr::tbl("provizion"), by = "id_litigiu") %>% dplyr::collect()
    
    DBI::dbDisconnect(conn = my_connection)
   
    observeEvent( vals$numar_litigiu_individual, { req(!is.null(vals$numar_litigiu_individual),vals$id_litigiu_individual,sentinte)
      
      vals_sentinte$litigiu_selectat <- sentinte %>% dplyr::filter(id_litigiu == vals$id_litigiu_individual)  %>%
        dplyr::select(id_litigiu, data_sentinta) %>% unique() %>% dplyr::mutate(numar_litigiu = vals$numar_litigiu_individual )
      
      vals_sentinte$tabel_sentinte_litigiu_selectat <-  sentinte %>% dplyr::filter(id_litigiu == vals$id_litigiu_individual) %>% 
        dplyr::select(-provizion_contabil, -data_provizion_contabil, -solutie_cumulata) %>% unique()
      
      
      output$sentinte <- DT::renderDataTable( { req( vals_sentinte$tabel_sentinte_litigiu_selectat )
        DT::datatable(data = vals_sentinte$tabel_sentinte_litigiu_selectat %>% dplyr::select(-id_litigiu),
                      rownames = FALSE,options = list(dom="t"),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
        paste0("Sentinte primite pentru litigiul ", vals$numar_litigiu_individual, ". Click pe sentinta pentru a actualiza coeficientul de risc")),
                      selection = list(mode = "single",selected = NULL, target = "row") ) })
    })
    
    observeEvent(input$sentinte_rows_selected,{
      
      vals_sentinte$sentinta_selectata <- vals_sentinte$tabel_sentinte_litigiu_selectat  %>%  
        dplyr::slice(input$sentinte_rows_selected)
      
      showModal(session = session,  modalDialog( size = "l",
        title = paste("Sentinta din data de ", vals_sentinte$sentinta_selectata$data_sentinta, " pentru litigiul ",
                      vals$numar_litigiu_individual),
        footer = list( modalButton(label = "Close",icon = icon("times")),
          actionButton(inputId =  ns("save_new_coef_risc"),label = "Save", icon = icon("save")) ),
        
        fluidRow(column(width = 6, shinyWidgets::pickerInput( ns("coef_risc"),"Selecteaza noul coeficient de risc",
                         choices = c(0,0.25,0.65,1), selected = vals_sentinte$sentinta_selectata$coef_risc)),
                
                 column(width = 6, textAreaInput( ns("sentinta_selectata"),label = "Sentinta",
                       value =  vals_sentinte$sentinta_selectata$solutie,height = "100px",width = "600px"))
                                              )))
      shinyjs::disable("sentinta_selectata")
      shinyjs::disable(id = 'save_new_coef_risc')
    })
    
    # Observer to enable save button on coef risc updated and handle of ShinyFeedback near coef_risc selection
    observeEvent(input$coef_risc,{
      
      shinyFeedback::feedbackDanger(inputId = "coef_risc",
                                    show = input$coef_risc == vals_sentinte$sentinta_selectata$coef_risc,
                                    color = "red", session = session,
                                    text = "Selecteaza un alt coeficient de risc pentru a putea salva")
      
     if ( input$coef_risc != vals_sentinte$sentinta_selectata$coef_risc) { shinyjs::enable(id = 'save_new_coef_risc') }
      
    })
    
    observeEvent(input$save_new_coef_risc,{ 
      removeModal(session=session)
      
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_sentinta"),title = 'CONFIRM',
                                     btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                     text = "Esti sigur ca vrei sa salvezi noul coeficient de risc?")
      })
    
    observeEvent(input$confirm_save_sentinta,{ req(input$confirm_save_sentinta==TRUE)
      
      sql_update_sentinte <- "UPDATE sentinte SET coef_risc = ?id1 where id_litigiu=?id2 and data_sentinta=?id3"
      
      send_query(sql_update_sentinte,success_message= "Coeficientul de risc a fost updatat cu succes",
                 error_message = "Coeficientul de risc NU a fost actualizat",
                 id1 = as.numeric(input$coef_risc), id2 = vals_sentinte$sentinta_selectata$id_litigiu,
                 id3 = vals_sentinte$sentinta_selectata$data_sentinta)
      
    })
    
    # NEW SENTINTA
    
    observeEvent(input$new_sentinta, { req(  vals_sentinte$litigiu_selectat )
      
      vals_sentinte$new_litigiu_selectat <- vals_sentinte$litigiu_selectat %>% dplyr::slice(1) %>% dplyr::select(-data_sentinta)
      
      showModal(modalDialog(title = paste0("Sentinta noua pentru litigiul ",vals_sentinte$new_litigiu_selectat$numar_litigiu),
                      size = "l", footer = list(modalButton("Cancel"),
                      actionButton(session$ns("save_new_sentinta"),label = "Save",icon = icon("save"))),
                      
                      fluidRow(column(width = 6, shinyWidgets::airDatepickerInput(ns("data_sentinta_noua"),
                            disabledDates =vals_sentinte$litigiu_selectat$data_sentinta,label = "Data sentinta",
                            value = Sys.Date(),language = "ro",maxDate = Sys.Date(),autoClose = TRUE),
                            
                            textAreaInput(ns("solutie_sentinta_noua"),label = "Solutia noua", value = c(),
                                          placeholder = "Paste here new solutie")    ),
                            
                            column(width = 6, shinyWidgets::textInputIcon(inputId = ns("tip_solutie_sentinta_noua"),
                                              value = c(),label = "Tip solutie",placeholder = "paste here new tip solutie",
                                              icon = icon("gavel")),
                                   br(),
                                   shinyWidgets::pickerInput(ns("new_coef_risc"),label = "Coeficientul de risc",
                                            choices = c(0,1,0.65,0.25) ))
                                              
                      )
      )
      )
      
    })
    
    observeEvent(input$save_new_sentinta, {
      shinyWidgets::ask_confirmation(inputId = ns("confirm_save_new_sentinta"),title = 'CONFIRM',
                                     btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                     text = "Esti sigur ca vrei sa salvezinoua sentinta?")
    })
    
    observeEvent( input$confirm_save_new_sentinta, { req(input$confirm_save_new_sentinta == TRUE) 
      
      removeModal(session = session)
      
      query_insert_sentinte <- 'INSERT  INTO sentinte (id_litigiu,data_sentinta,tip_solutie,solutie,coef_risc) VALUES (?id1,?id2,?id3,?id4,?id5);'
      
      
      send_query(query_insert_sentinte, success_message = "Sentinta actualizata cu succes",
                 id1 = vals$id_litigiu_individual, id2 = input$data_sentinta_noua,
                 id3 = stringi::stri_trans_general(str = input$tip_solutie_sentinta_noua,id = "latin-ASCII"), 
                 id4 = stringi::stri_trans_general(str = input$solutie_sentinta_noua,id = "latin-ASCII"),
                 id5 = input$new_coef_risc)
     
       
      
       })
 
  })
}
    
## To be copied in the UI
# mod_individual_sentinta_noua_ui("individual_sentinta_noua_ui_1")
    
## To be copied in the server
# mod_individual_sentinta_noua_server("individual_sentinta_noua_ui_1")
