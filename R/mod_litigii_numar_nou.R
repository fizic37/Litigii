#' litigii_numar_nou UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_numar_nou_ui <- function(id){
  ns <- NS(id)
  fluidRow(
 DT::dataTableOutput(outputId = ns("litigii_numar_nou")),
 verbatimTextOutput(ns("diverse")) )
}
    
#' litigii_numar_nou Server Function
#'
#' @noRd 
mod_litigii_numar_nou_server <- function(input, output, session, vals){
  ns <- session$ns
  
  vals_litigiu_numar_nou <- reactiveValues()
  
  sql_query_actualizare_litigiu <- "SELECT * from actualizare_litigiu;"
  
  vals_litigiu_numar_nou$actualizare_litigii <- get_query(query = sql_query_actualizare_litigiu)
  
  dosare_numar_nou <- readRDS("R/reactivedata/dosare_noi/dosare_numar_nou.rds") %>% 
    dplyr::filter( !numar_litigiu %in% vals_litigiu_numar_nou$actualizare_litigii$numar_litigiu)
  
  
  vals_litigiu_numar_nou$litigii_noi_updated <- dosare_numar_nou %>%
      dplyr::slice(purrr::map(.x = dosare_numar_nou$numar_litigiu,
                            # I check to see if numar old dosar se gaseste inside numarul nou
                            ~stringr::str_which(string = .x,
                                                pattern =  vals_litigiu_numar_nou$actualizare_litigii$numar_litigiu %>%
                                                  #remove ** as it outputs an error in regex
                                                  stringr::str_remove_all(pattern = "\\*\\*"))) %>%
                   purrr::map_dbl(.x = .,~purrr::detect_index(.x,~length(.x) != 0)) %>% which(x = .>0)) 
  
  output$litigii_numar_nou   <- DT::renderDataTable({ req(vals_litigiu_numar_nou$litigii_noi_updated)
    DT::datatable(data = vals_litigiu_numar_nou$litigii_noi_updated,
                  rownames = FALSE, selection = list(mode = "single",selected = NULL, target = "row"),
                  class = "nowrap",caption = "Click pe litigiu pentru a vizualiza",
                  options = list(dom = "ftp", scrollX = TRUE,pageLength = 5))   })
  
  observeEvent(input$litigii_numar_nou_rows_selected,{
    
    vals_litigiu_numar_nou$litigiu_selectat <- vals_litigiu_numar_nou$litigii_noi_updated  %>%  
                dplyr::slice(input$litigii_numar_nou_rows_selected)
    
    vals_litigiu_numar_nou$ultima_modificare <- vals_litigiu_numar_nou$actualizare_litigii %>% 
      dplyr::slice(  stringr::str_which( string = vals_litigiu_numar_nou$litigiu_selectat$numar_litigiu,
          pattern = vals_litigiu_numar_nou$actualizare_litigii$numar_litigiu %>%
            # I have to remove ** in order for pattern to work
              stringr::str_remove_all(pattern = "\\*\\*") )  ) %>% 
                  dplyr::arrange(desc(data_modificare)) %>% dplyr::slice(1)
    
    
    showModal(modalDialog(title = paste("Actualizare litigiul ", vals_litigiu_numar_nou$ultima_modificare$numar_litigiu),
                                  size = "l",   footer = list(actionButton(inputId = session$ns("save_litigiu_numar_nou"),
                                 label = "Save",icon = icon("save")), modalButton(label = "Close")),
                          fluidRow(
                            column(width = 6,
                                   textInput(session$ns("numar_dosar_existent"), width = "400px",
                                             value = vals_litigiu_numar_nou$ultima_modificare$numar_litigiu,
                                             label = "Numar dosar existent"),
                                   dateInput(session$ns("data_doar_existent"),
                                             value = vals_litigiu_numar_nou$ultima_modificare$data_modificare,
                                             label = "Ultima data modificare dosar existent", width = "400px"),
                                   textInput(ns("instanta_existenta"),
                                             value = vals_litigiu_numar_nou$ultima_modificare$instanta,
                                             label = "Instanta de judecata existenta",width = "400px"),
                                   textInput(session$ns("categorie_caz_existent"),
                                             value = vals_litigiu_numar_nou$ultima_modificare$Stadiu_procesual,
                                             label = "Stadiu procesual existent",width = "400px"),
                                   textInput(ns("calitate_fngcimm_existent"),
                                             value = vals_litigiu_numar_nou$ultima_modificare$Calitate_FNG,
                                             label = "Calitate FNGCIMM dosar existent",width = "400px"),
                                   textInput(session$ns("categorie_caz_dosar_existent"), width = "400px",
                                             value = vals_litigiu_numar_nou$ultima_modificare$Categorie_caz,
                                             label = "Materie dosar existent"),
                                   textAreaInput(ns("parti_adverse_dosar_existent"), width = "400px",
                                                 value = vals_litigiu_numar_nou$ultima_modificare$Parti,
                                                 label = "Parti adverse dosar existent"),
                                   textInput(ns("obiect_dosar_existent"),
                                             value = vals_litigiu_numar_nou$ultima_modificare$Obiect,
                                             label = "Obiect dosar existent",width = "400px") 
                            ),
                            column(width = 6,
                                   textInput(session$ns("numar_dosar_nou"), width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$numar_litigiu,
                                             label = "Numar dosar nou"),
                                   dateInput(session$ns("data_dosar_nou"),width = "400px",label = "Data dosar nou",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$data_litigiu_nou),
                                   textInput(session$ns("instanta_dosar_nou"),width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$instante_dosar_nou,
                                             label = "Instanta dosar nou"),
                                   
                                   textInput(session$ns("stadiu_procesual_dosar_nou"),width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$stadiu_procesual_dosar_nou,
                                             label = "Stadiu procesual dosar nou"),
                                   textInput(session$ns("calitate_fngcimm_dosar_nou"),width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$calitate_fngcimm,
                                             label = "Calitate FNGCIMM dosar nou"),
                                   textInput(session$ns("categorie_caz_dosar_nou"), width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$categorie_caz_dosar_nou,
                                              label = "Materie dosar nou"),
                                   textAreaInput(session$ns("parti_adverse_dosar_nou"), width = "400px",
                                                 value = vals_litigiu_numar_nou$litigiu_selectat$parti_adverse,
                                                 label = "Parti adverse dosar nou"),
                                   textAreaInput(session$ns("obiect_dosar_nou"),width = "400px",
                                             value = vals_litigiu_numar_nou$litigiu_selectat$obiect_dosar_nou,
                                             label = "Obiect dosar nou")
                                   )
                          )
                                        
    ))
    
   })
  
  observeEvent(input$save_litigiu_numar_nou, {
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_litigiu"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                   text = "Esti sigur ca vrei sa updatezi litigiul?")
    })
  
  observeEvent(input$confirm_save_litigiu,{ req(input$confirm_save_litigiu == TRUE)
    
    sql_insert_actualizare_litigii <- 
      "INSERT INTO actualizare_litigiu (id_litigiu, numar_litigiu, data_modificare,instanta, Categorie_caz,Obiect,Stadiu_procesual,
    Calitate_FNG,Parti) VALUES (?id1,?id2,?id3, ?id4,?id5,?id6,?id7,?id8,?id9) ;"
    
    #sql_update_actualizare_litigii <- "UPDATE actualizare_litigiu SET numar_litigiu = ?id1 where id_litigiu=?id2 and 
    #data_modificare>=?id3 ; "
   
   send_query(sql_insert_actualizare_litigii, id1 = vals_litigiu_numar_nou$ultima_modificare$id_litigiu,
              id2 = input$numar_dosar_nou,     id3 = input$data_dosar_nou,
              id4 = input$instanta_dosar_nou,  id5 = input$categorie_caz_dosar_nou,
              id6 = input$obiect_dosar_nou,    id7 = input$stadiu_procesual_dosar_nou,
              id8 = input$calitate_fngcimm_dosar_nou,  id9 = input$parti_adverse_dosar_nou)
      
      # I remove litigiu updatat from the table shown to user
      vals_litigiu_numar_nou$litigii_noi_updated <- vals_litigiu_numar_nou$litigii_noi_updated %>% 
        dplyr::filter(numar_litigiu != vals_litigiu_numar_nou$litigiu_selectat$numar_litigiu)
      
      dosare_numar_nou <- dosare_numar_nou %>% dplyr::filter(numar_litigiu != input$numar_dosar_nou)
      
      saveRDS(object = dosare_numar_nou, file = "R/reactivedata/dosare_noi/dosare_numar_nou.rds") 
      
      removeModal(session)
    
  })
  
  
  
  
  
  
}
    
## To be copied in the UI
# mod_litigii_numar_nou_ui("litigii_numar_nou_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_numar_nou_server, "litigii_numar_nou_ui_1")
 
