#' litigiu_individual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigiu_individual_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinybusy::add_busy_spinner(color = "#bc3c4b", position = "bottom-right", timeout = 200),
    
    shinyFeedback::useShinyFeedback(),
    
    shinyWidgets::pickerInput(inputId = ns("numar_litigiu"),
                  options = list(`live-search` = TRUE ),   label = "Numar litigiu",choices = c() ),
    
  bs4Dash::box(title = "Date principale despre litigiu",width = 12,status = "info",
               collapsible = T,collapsed = T,maximizable = T,
               
               DT::dataTableOutput(ns("main_litigii")),
              DT::dataTableOutput(ns("close_litigiu"))
  ),
               
  bs4Dash::box(title = "Sentinte si provizioane aferente",status = "info",id = ns("box_sentinte"),
               width = 12,collapsible = T,collapsed = T,   maximizable = T, 
               mod_individual_sentinta_noua_ui("individual_sentinta_noua_ui_1")
               ),
  bs4Dash::box(title = "Evolutie litigiu de la o instanta la alta",width = 12,status = "info",
               collapsible = T,collapsed = T,maximizable = T,  
               DT::dataTableOutput(ns("actualizare_litigiu")))
  )
}
    
#' litigiu_individual Server Functions
#'
#' @noRd 
mod_litigiu_individual_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_individual <- reactiveValues()
    
    bi_contracte <- readRDS("R/reactivedata/bi_contracte.rds")
    
    observeEvent(input$box_sentinte, {req(any(input$box_sentinte$collapsed==FALSE, input$box_sentinte$maximized==TRUE))
      vals$box_selected <- c(vals$box_selected, "box_sentinte")
    })
    # Below observer passes litigiu selectat to module individual_sentinta_noua
    
    observeEvent( input$numar_litigiu, { req(input$numar_litigiu != "nothing selected", vals$main_litigii)
      
      vals$numar_litigiu_individual <- input$numar_litigiu
      vals$id_litigiu_individual <- vals$main_litigii %>% 
        dplyr::filter(numar_litigiu==vals$numar_litigiu_individual) %>% dplyr::pull(id) %>% unique()
      
      sql_get_stare_litigiu <- "SELECT * from stare_litigiu where id_litigiu=?id1"
      
      vals_individual$stare_litigiu <- get_query(query = sql_get_stare_litigiu,id1=vals$id_litigiu_individual)
      
      
      
      },ignoreInit = TRUE)
    
    observeEvent(vals$main_litigii, {
      
      vals_individual$main_litigii_no_duplicates <- vals$main_litigii[!duplicated(vals$main_litigii[, c("id", "numar_litigiu")]),]
      
      shinyWidgets::updatePickerInput(  session = session,  inputId = "numar_litigiu",
        choices = c("nothing selected",vals_individual$main_litigii_no_duplicates$numar_litigiu),
        choicesOpt = list( subtext = paste(c("nothing selected",vals_individual$main_litigii_no_duplicates$IMM,
                                             sep = " " ) ) ) )
      
      output$main_litigii <-  DT::renderDataTable( DT::datatable(data = vals$main_litigii %>%
              dplyr::filter(numar_litigiu == input$numar_litigiu) %>% dplyr::select(9, 2:8, 18:19) %>% unique(),
            options = list(dom = "t"), rownames = F,
            colnames = c( "Numar", "Data dosar", "Tip litigiu",  "Contract de garantare",
              "Banca",  "IMM", "Cod partener", "CUI",  "Valoare litigiu", "Data actualizare valoare litigiu"),
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                              "Date principale litigiu. Click pe litigiu pentru vizualizare si actualizare"),
            selection = list(mode = "single",selected = NULL, target = "row")
          ) %>% DT::formatRound(columns = "valoare_litigiu",digits = 2)
        )
      
      output$close_litigiu <- DT::renderDataTable( { req( vals_individual$stare_litigiu )
        
        
        dt_generate_function(df = vals_individual$stare_litigiu %>% dplyr::select(-id_litigiu)
              , caption = "Click in tabelul de mai jos pentru a inchide litigiul:", editable=TRUE) })
      
      output$actualizare_litigiu <-  DT::renderDataTable( DT::datatable(data = vals$main_litigii %>%
              dplyr::filter(numar_litigiu == input$numar_litigiu) %>% dplyr::select(9:16) %>% dplyr::arrange(data_modificare),
            rownames = FALSE, options = list(dom = "t"),
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                              "Evolutie litigiu")
          )
        )
    })
    
    observeEvent(input$close_litigiu_rows_selected,{
      showModal(modalDialog(
        title = paste0("Inchide litigiul ", input$numar_litigiu),  size = "l",
        footer = list(
          modalButton('Cancel'),
          actionButton(session$ns('save_close_litigiu'),
                       'Salveaza', class = "btn btn-primary", icon = icon("save"))),
          fluidRow(column(width = 6, shinyWidgets::pickerInput(inputId = ns("tip_litigiu_close_litigiu"),
                          label = "Stare litigiu", choices = c("activ", "inchis", "platit"),
                          selected = "inchis" )),
                   column(width = 6, shinyWidgets::airDatepickerInput(inputId = ns("data_inchidere_litigiu"),
                                label = "Data inchiderii litigiului", value = Sys.Date(),language = "ro",autoClose = T)))
      ))
    })
    
    observeEvent(input$save_close_litigiu,{ 
      shinyWidgets::ask_confirmation(ns("confirm_save_close_litigiu"),
                title = "CONFIRM", btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                text = "Esti sigur ca vrei sa salvezi modificarile efectuate?") })
    
    observeEvent( input$confirm_save_close_litigiu,{ req(input$confirm_save_close_litigiu == TRUE ) 
    sql_update_stare_litigiu <- "UPDATE stare_litigiu SET to_date = ?id1 where id_litigiu=?id2;"
    sql_insert_stare_litigiu <- "INSERT INTO stare_litigiu (id_litigiu, stare_litigiu, from_date, to_date) VALUES (?id1,?id2,?id3,?id4);"
    send_query(sql_update_stare_litigiu, id1=input$data_inchidere_litigiu,id2=vals$id_litigiu_individual)
    send_query(sql_insert_stare_litigiu, id1=vals$id_litigiu_individual,id2=input$tip_litigiu_close_litigiu,
                 id3=input$data_inchidere_litigiu, id4=as.Date("9999-12-31"))
      
      removeModal(session = session)
      
     # shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Baza de date a fost actualizata cu succes",
     # .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE) )
      
      })
    
    # Editare litigiu
    observeEvent(input$main_litigii_rows_selected,{
      
      vals_individual$litigiu_selected <- vals$main_litigii %>% 
        dplyr::filter(numar_litigiu == input$numar_litigiu) %>% 
        dplyr::arrange(desc(data_actualizare_valoare_litigiu))
      
      showModal(modalDialog(
        title = paste0("Editeaza litigiul ",input$numar_litigiu),  size = "l",
        footer = list(
          modalButton('Cancel'),
          actionButton(session$ns('save_litigiu'),
                       'Salveaza', class = "btn btn-primary", icon = icon("save"))),
        
        fluidRow(column(width = 6, shinyWidgets::pickerInput(inputId = ns("tip_litigiu"),label = "Tip litigiu",
                              choices = unique(vals_individual$main_litigii_no_duplicates$Tip_litigiu),
                                 selected = vals_individual$litigiu_selected$Tip_litigiu),
                        
                        shinyWidgets::autonumericInput(inputId = session$ns("valoare_litigiu"),decimalPlaces = 2,
                           label = "Valoare litigiu, echiv lei",value =  vals_individual$litigiu_selected %>% 
                                  dplyr::slice(1) %>%  dplyr::pull(valoare_litigiu),align = "right"),
                        uiOutput(outputId = ns("show_date_update_suma_litigiu")) ),
                 column(width = 6,
                        shinyWidgets::pickerInput(session$ns("beneficiar"),label = "Beneficiar",
                                                  #choices = NULL, 
                        selected = ifelse(is.na(vals_individual$litigiu_selected$IMM[1]),"none",vals_individual$litigiu_selected$IMM[1]),
                        choices = c("none",bi_contracte %>% dplyr::pull(NumeBeneficiar) %>% unique() ),
                                                  options = list(`live-search` = TRUE)),
                        
                        shinyWidgets::pickerInput(session$ns("contract_garantare"),label = "Contract de garantare",
                                                  choices = c(vals_individual$litigiu_selected$contract_garantare[1],"none"),
                                                  selected = vals_individual$litigiu_selected$contract_garantare[1]),
                        
                        shinyWidgets::pickerInput(session$ns("banca"),label = 'Banca',choices =  vals_individual$litigiu_selected$Banca,
                                                  selected = vals_individual$litigiu_selected$Banca))
        )
      ))
    })
    
    #Below observer employs a trick: I update beneficiar selectInput only after modal is open.
    # This way, the user first sees the modal and then sees the app workink to update lista beneficiarilor
    #observeEvent(input$tip_litigiu,{ shinyWidgets::updatePickerInput(session = session,inputId = "beneficiar",
                                      #selected = vals_individual$litigiu_selected$IMM[1],
        #choices = bi_contracte %>% dplyr::pull(NumeBeneficiar) %>% unique())   },once = TRUE) 
    # I only show data modificare valoare litigiu if valoare litigiu se modifica
    output$show_date_update_suma_litigiu <- renderUI( {  req( input$valoare_litigiu != vals_individual$litigiu_selected %>% 
                                                                dplyr::slice(1) %>%  dplyr::pull(valoare_litigiu))
      dateInput(inputId = ns("data_valoare_litigiu"),label = "Data modificarii valorii litigiului",
                value = Sys.Date(),min = as.Date("2020-12-31"), max = Sys.Date() )
      
    })
    
    observeEvent(input$beneficiar,{ 
      
      if ( is.na(vals_individual$litigiu_selected$IMM[1]) || 
           input$beneficiar != vals_individual$litigiu_selected$IMM[1]) {
      
      shinyWidgets::updatePickerInput( session = session,inputId = "contract_garantare",
                choices = c("none",bi_contracte %>% dplyr::filter(NumeBeneficiar == input$beneficiar) %>% 
                                                    dplyr::pull(NumarContract) %>% unique() ) )
      
      shinyWidgets::updatePickerInput( session = session,inputId = "banca",
                choices = bi_contracte %>% dplyr::filter(NumeBeneficiar == input$beneficiar) %>% 
                                            dplyr::pull(Banca) %>% unique())
      }
      vals_individual$cod_beneficiar <- bi_contracte %>% dplyr::filter(NumeBeneficiar == input$beneficiar) %>% 
        dplyr::slice(1) %>% dplyr::pull(CodPartener)
      
      vals_individual$CUI <- bi_contracte %>% dplyr::filter(NumeBeneficiar == input$beneficiar) %>% 
        dplyr::slice(1) %>% dplyr::pull(CUI)
      
    })
    
    observeEvent(input$save_litigiu,{
      
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_litigiu"),title = 'CONFIRM',
                                     btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                     text = "Esti sigur ca vrei sa salvezi modificarile efectuate?")      })
    
    observeEvent(input$confirm_save_litigiu,{
      
      removeModal(session = session)
      
      sql_update_main_litigii <- "UPDATE main_litigii SET Tip_litigiu=?id1,contract_garantare=?id2,Banca=?id3, IMM=?id4, Cod_Partener=?id5, CUI=?id6 where id=?id7;"
      
      sql_update_valoare_litigiu <-  "UPDATE valoare_litigiu SET valoare_litigiu=?id1, data_modificare=?id2 where id_litigiu=?id3;"
      
      
      send_query( sql_update_main_litigii, success_message = "Litigiul a fost updatat cu succes",
                 error_message = "Litigiul NU a fost actualizat", id1 = input$tip_litigiu,
                 id2 = input$contract_garantare, id3 = input$banca, id4 = input$beneficiar, 
                 id5 = vals_individual$cod_beneficiar, id6 = vals_individual$CUI, 
                 id7 = vals_individual$litigiu_selected$id[1] )
      
      if ( input$valoare_litigiu != vals_individual$litigiu_selected %>% dplyr::slice(1) %>%  dplyr::pull(valoare_litigiu) ) {
        
        send_query( sql_update_valoare_litigiu, success_message = "Valoarea litigiului a fost updatata cu succes",
                    error_message = "Valoarea litigiului NU a fost actualizata",
                    id1 = input$valoare_litigiu, id2 = input$data_valoare_litigiu,
                    id3 = vals_individual$litigiu_selected$id[1] )
      }
      
      })
    
  })
}
    
## To be copied in the UI
# mod_litigiu_individual_ui("litigiu_individual_ui_1")
    
## To be copied in the server
# mod_litigiu_individual_server("litigiu_individual_ui_1")
