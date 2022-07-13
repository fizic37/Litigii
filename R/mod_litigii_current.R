#' litigii_current UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_litigii_current_ui <- function(id){
  ns <- NS(id)
  fluidRow(   shinyjs::useShinyjs(),
               column(width = 4, div( textOutput(outputId = ns("titlu_litigii_current") ),
                                                class = "down-link-2-column" ) ),
               
               column(width = 4, dateInput(inputId = ns("data_litigii_current"), value = Sys.Date(), 
                          label = "", min = as.Date("2020-12-31"), language = "ro", format = 'dd MM yyyy', 
                          autoclose = TRUE, max = Sys.Date(), width = "205px" ) ),
              
               column(width = 4, downloadLink(outputId = ns("down_baza_filtrata"),
                                              label = "Click aici pentru a downloada litigiile filtrate",
                                              class = "down-link-2-column")) ,
             hr(),
             
             
             DT::dataTableOutput(ns("litigiiCurrent")),
            
             shinyWidgets::prettyToggle(inputId = ns("show_report"),status_on = "info",width = "250px",
                                        icon_on = icon("wye-slash"), icon_off = icon("table"),
                                        value = FALSE,status_off = "info",
                                        label_on = "Hide table below",label_off = "Click to show sintetic table"),
             
             DT::dataTableOutput(ns("sinteza_litigii_curente")))
           
                            
}
    
#' litigii_current Server Function
#'
#' @noRd 
mod_litigii_current_server <- function(input, output, session,vals){
  ns <- session$ns
  
  vals_current <- reactiveValues()
  
  output$titlu_litigii_current <- renderText( { paste0("Litigiile active la data de ", input$data_litigii_current) } )
  
  observeEvent(input$data_litigii_current,{
    
    if (input$data_litigii_current != Sys.Date()) {
    vals$report_date <- input$data_litigii_current }
  })
  
  output$litigiiCurrent <- DT::renderDataTable({req(vals$litigii_curente)
    DT::datatable(rownames = FALSE, filter = list(position = "top", clear = TRUE, plain = TRUE),
                  selection = list(mode = "single",selected = NULL, target = "row"),
                  class = "nowrap",caption = "Click pe litigiu pentru a vizualiza",
                  options = list(dom = "ftp", scrollX = TRUE,pageLength = 5), 
                  data = vals$litigii_curente %>% 
                    dplyr::select(numar_litigiu,Tip_litigiu,valoare_litigiu,Banca,IMM,contract_garantare,CUI, 
                                  ultima_data_modificare_litigiu,id_litigiu) %>%
                    # Below step adds numar_litigiu even if data modificare > data litigii curente
                    dplyr::mutate(numar_litigiu = ifelse(is.na(numar_litigiu), 
            vals$dosare_modificate$numar_litigiu[match(id_litigiu, vals$dosare_modificate$id_litigiu)],
            numar_litigiu)) %>%
                    dplyr::mutate(dplyr::across(.cols = c(Tip_litigiu,Banca),~as.factor(.x)))) %>%
      DT::formatRound(columns = "valoare_litigiu",digits = 0) })
  
  observeEvent(input$litigiiCurrent_rows_selected,{
  
    vals_current$litigiu_selectat <- vals$litigii_curente  %>%  
      dplyr::slice(input$litigiiCurrent_rows_selected)
    
    showModal(modalDialog(title = paste("View litigiul",vals_current$litigiu_selectat$numar_litigiu),
                     size = "l",   footer = modalButton(label = "Close"),
              fluidRow(column(width = 5,
                              textInput(inputId = session$ns("tip_litigiu"),label = "Tip litigiu",width = "400px",
                                        value = vals_current$litigiu_selectat$Tip_litigiu),
                              textInput(inputId = session$ns("suma_litigiu"),label = "Suma litigiu",width = "400px",
                                        value = vals_current$litigiu_selectat$valoare_litigiu %>%
                                          formatC(digits = 2,big.mark = ",",format = "f",mode = "double")),
                              textInput(inputId = session$ns("banca_litigiu"),label = "Banca",width = "400px",
                                        value = vals_current$litigiu_selectat$Banca),
                  textInput(inputId = session$ns("beneficiar_litigiu"),label = "Beneficiar litigiu",width = "400px",
                          value = vals_current$litigiu_selectat$IMM),
                  textInput(inputId = session$ns("numar_contract_litigiu"), label = "Numar contract litigiu",width = "400px",
                        value = vals_current$litigiu_selectat$contract_garantare),
                  numericInput(inputId = session$ns("coef_risc"),label = "Coeficient curent de provizionare",width = "400px",
                               value = vals_current$litigiu_selectat$coef_risc),
                  textAreaInput(session$ns("parti_dosar"),label = "Parti adverse",
                                height = "100px",width = "400px",
                                value = vals_current$litigiu_selectat$Parti)),
                     column(width = 6,
                            textInput(inputId = session$ns("obiect_litigiu"),label = "Obiect litigiu",width = "400px",
                                      value = vals_current$litigiu_selectat$Obiect),
                            textInput(inputId = session$ns("calitate_fngcimm"),label = "Calitate FNGCIMM", width="400px",
                                      value = vals_current$litigiu_selectat$Calitate_FNG),
                            textInput(inputId = session$ns("categorie_caz"),label = "Categorie caz", width="400px",
                                      value = vals_current$litigiu_selectat$Categorie_caz),
                            textInput(inputId = session$ns("stadiu_dosar"),label = "Stadiu dosar", width="400px",
                                      value = vals_current$litigiu_selectat$Stadiu_procesual),
                            textInput(inputId = session$ns("instanta"),label = "Instanta", width="400px",
                                      value = vals_current$litigiu_selectat$instanta),
                            dateInput(inputId = session$ns("ultima_data_actualizare"),label = "Ultima data a unei solutii",
                                      value = vals_current$litigiu_selectat$ultima_data_actualizare,width="400px"),
                            textAreaInput(session$ns("sentinta_curenta"),label = "Istoric solutii dosar",
                                          height = "200px",width = "400px",
                                          value = vals_current$litigiu_selectat$solutie_cumulata)
                             )
                            
                            
              )  ) )
                  
    
  })
  
  
  output$sinteza_litigii_curente <-  DT::renderDataTable({ req(input$show_report == TRUE, vals$litigii_curente)
      DT::datatable(rownames = FALSE,      options = list(dom = "t"),
        data = vals$litigii_curente %>% dplyr::group_by(Tip_litigiu) %>%
          dplyr::summarise(  Valoare_litigii = sum(valoare_litigiu),
                Numar_litigii = dplyr::n() ) %>% dplyr::arrange(desc(Valoare_litigii)) %>%
          janitor::adorn_totals(where = "row", fill = "Total")  ) %>% 
        DT::formatRound(columns = 2:3,  digits = 0,    mark = ",")
    })
  
  output$down_baza_filtrata <- downloadHandler(filename = function() {"litigii_curente.csv"},content = function(file) {
    readr::write_csv(x = vals$litigii_curente,  file = file)
  })
 
}
    
## To be copied in the UI
# mod_litigii_current_ui("litigii_current_ui_1")
    
## To be copied in the server
# callModule(mod_litigii_current_server, "litigii_current_ui_1")
 
