#' regularizare_provizioane UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_regularizare_provizioane_ui <- function(id){
  ns <- NS(id)

  fluidRow(
    shinybusy::add_busy_spinner(color = "#bc3c4b", position = "bottom-right", timeout = 200),
    column(width = 3,

      shinyWidgets::pickerInput(inputId = ns("from_date"),label = "From date",
                                  choices = character(0),width = "250px")),

    column(width = 3, dateInput(inputId = ns("to_date"),label = "To date",value = Sys.Date(),
            min = as.Date("2020-12-31"), language = "ro", format = 'dd MM yyyy', autoclose = TRUE, max = Sys.Date())),

    column(width = 6, downloadLink(outputId = ns("down_provizioane_curente"),
                    label = "Click to download provizioanele curente", class = "down-link-3-column_padding") ),

    column(width = 12,  DT::dataTableOutput(outputId = ns("provizioane_curente")),  br() ),

    hr(),

    tagList(    column(width = 4,
    shinyWidgets::prettyToggle(inputId = ns("show_report"),status_on = "info",width = "250px",
              icon_on = icon("wye-slash"), icon_off = icon("table"),
                       value = FALSE,status_off = "info",label_on = "Hide table below",
              label_off = "Click to show sintetic data of the above table ")),
    column(width = 4,
           uiOutput(ns("show_toggle_missing"))),
    column(width = 4, actionLink(inputId = ns("save_provizioane"),icon = icon("save"),
                        label = "Click here to save provizions", class = "down-link-3-column_no_padding"))),

    DT::dataTableOutput(ns("sinteza_provizioane_curente"))
  )

}

#' regularizare_provizioane Server Function
#'
#' @noRd
mod_regularizare_provizioane_server <- function(input, output, session, vals){
  ns <- session$ns

  vals_regularizare_provizioane <- reactiveValues()

  observeEvent(vals$provision_dates,{ req(vals$provision_dates)
  shinyWidgets::updatePickerInput(session = session, inputId = "from_date",choices = vals$provision_dates)
   updateDateInput(session = session, inputId = "to_date",min = input$from_date)
  })

  observeEvent(input$to_date,{
      vals$provision_date <- input$to_date
      updateActionLink(session = session,inputId = "save_provizioane",
                       label = paste0("Click here to save provisions at date ", input$to_date))
  })

  observeEvent(input$from_date,{
    vals$provision_from_date <- as.Date(input$from_date )
  })

  observeEvent(vals$provizioane_curente,{

    vals_regularizare_provizioane$provizioane_curente <- vals$provizioane_curente %>%
      # I give 0.65 to litigii with no sentinte (litigii noi)
      dplyr::mutate(coef_risc = ifelse(!id_litigiu %in% vals$sentinte$id_litigiu,0.65,coef_risc)) %>%

      # Below step adds numar_litigiu even if data modificare > data litigii curente
      dplyr::mutate(numar_litigiu = ifelse(is.na(numar_litigiu),
                                      vals$actualizare_litigiu$numar_litigiu[match(id_litigiu, vals$actualizare_litigiu$id_litigiu)],
                                           numar_litigiu)) %>%
      dplyr::mutate(Necesar_provizion =  ifelse(stare_litigiu == "platit" | id_litigiu %in% vals$provizioane_inchise$id_litigiu,
                                                0,coef_risc * valoare_litigiu)) %>%
      dplyr::mutate(Regularizare_provizion = Necesar_provizion - ifelse(is.na(provizion_contabil),0,provizion_contabil))

    output$provizioane_curente <- DT::renderDataTable({ req(vals$provizioane_curente)
      DT::datatable(data = vals_regularizare_provizioane$provizioane_curente %>% dplyr::select(-id_litigiu, -Cod_Partener,-solutie) %>%
                      dplyr::mutate(dplyr::across(.cols = where(is.character),.fns = ~as.factor(.x))) %>%
                      dplyr::select(contract_garantare, Banca,IMM,CUI, valoare_litigiu,coef_risc,Necesar_provizion,Regularizare_provizion,
                                    provizion_contabil, data_provizion_contabil,stare_litigiu,DataPlata, numar_litigiu),
                    rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                          paste0("Regularizare provizioane litigii la data de ", input$to_date,
                                              " comparativ cu data de ", input$from_date) ),
                    options = list(dom = "ftp", scrollX=TRUE,pageLength=5),
                    class = "nowrap",filter = list(position = "top", clear = TRUE, plain = TRUE),
                    selection = list(mode = "single",selected = NULL, target = "row")) %>%
        DT::formatRound(columns = c(5, 7:9),digits = 0) })



    output$down_provizioane_curente <- downloadHandler(filename = function() {paste0("provizioane_",input$to_date,".csv")},
                                                       content = function(file) {readr::write_csv(x = vals_regularizare_provizioane$provizioane_curente,file = file) })

    output$show_toggle_missing <- renderUI({ req( any( is.na(vals_regularizare_provizioane$provizioane_curente$Necesar_provizion) ) )

      shinyWidgets::prettyToggle(inputId = session$ns("show_missing"),status_on = "info",width = "250px",
                                 icon_on = icon("exclamation-triangle"), icon_off = icon("table"),
                                 value = FALSE,status_off = "info",label_on = "Hide table below",
                                 label_off = "ATENTIE, lipsesc coeficienti de risc. Click here")
    })

  })


    observeEvent(input$save_provizioane,{
   shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_provisions"),title = "CONFIRM?",type = "success",
            text = paste0("Esti sigur ca vrei sa salvezi provizioanele la data de ",input$to_date),
            btn_colors = c("#bc3c4b","#3cbcad") )

 })

    observeEvent(input$confirm_save_provisions, {
  req (input$confirm_save_provisions == TRUE, vals_regularizare_provizioane$provizioane_curente )
   # Below I check if input$to_date is an end of month date
   if (lubridate::ceiling_date(x = input$to_date,unit = "month")-1 != input$to_date) {
     shinyWidgets::sendSweetAlert(session = session, title = "SORRY",btn_colors = '#bc3c4b',
          text = "Nu sunt programat sa salvez date care nu sunt sfarsit de luna.
          Selecteaza o noua data din butonul de mai sus To date",type = "error")   }
   else if (input$to_date %in% vals$provision_dates) {
     shinyWidgets::ask_confirmation(inputId = session$ns("confirm_owerwrite"),title = "STOP",
          text = "Am deja in baza de date data selectata de tine.
          Daca vei continua, voi sterge observatiile existente din baza de date si voi adauga cele de mai sus.",
          btn_labels = c("Nu, opreste","Da, overwite"),btn_colors = c("#bc3c4b","#3cbcad") ) }

   else {

     sql_insert_provizion <- paste0("INSERT INTO provizion (id_litigiu,provizion_contabil,data_provizion_contabil) VALUES (",
                                    paste0(vals_regularizare_provizioane$provizioane_curente$id_litigiu,",",
                                    vals_regularizare_provizioane$provizioane_curente$Necesar_provizion,",","'",
                                    rep(input$to_date, nrow(vals_regularizare_provizioane$provizioane_curente)),"'",
                                    collapse = "),("), ")")

     db <- config::get("database", file = "R/credentials/db_credentials.yml")
     my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                     dbname = db$dbname, host = db$host)

     tryCatch( expr = {
       DBI::dbSendQuery(conn = my_connection,statement = sql_insert_provizion )

       vals$provision_dates <- my_connection %>% dplyr::tbl("provizion") %>% dplyr::arrange(desc(data_provizion_contabil)) %>%
         dplyr::pull(data_provizion_contabil) %>% unique()

       DBI::dbDisconnect(conn = my_connection)

       removeUI(selector = "#regularizare_provizioane_ui_1-save_provisions")

       shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                                .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        }, error = function(e){
               DBI::dbDisconnect(conn = my_connection)
               shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = "ERROR updating database",
                .options = list("timeOut"=0, 'positionClass'="toast-bottom-right"))

             } )

   }

 })



    observeEvent(input$show_report,{ req(vals$provizioane_curente)

      output$sinteza_provizioane_curente <-  DT::renderDataTable({ req(input$show_report == TRUE, vals$provizioane_curente)
      DT::datatable(rownames = FALSE,      options = list(dom = "Bt",buttons = c('copy', 'csv', 'excel')),
                    extensions = 'Buttons',
                    data = vals_regularizare_provizioane$provizioane_curente %>%
                      dplyr::slice(input$provizioane_curente_rows_all) %>%
                      dplyr::group_by(Tip_litigiu) %>%
                      dplyr::summarise(  Valoare_litigii = sum(valoare_litigiu),
                                         Numar_litigii = dplyr::n(),
                                         provizion_contabil = sum(provizion_contabil, na.rm = TRUE),
                                         Necesar_provizion_curent = sum(Necesar_provizion),
                                         Regularizare_provizion = sum(Regularizare_provizion) ) %>%
                      dplyr::rename_at(.vars = 4, ~paste0('Provizion_contabil_', input$from_date)) %>%
                        dplyr::arrange(desc(Valoare_litigii)) %>%
                      janitor::adorn_totals(where = "row", fill = "Total")  ) %>%
        DT::formatRound(columns = 2:6,  digits = 0,    mark = ",")
    })

      shinyWidgets::updatePrettyToggle(session = session,inputId = "show_missing", value = FALSE)
    })

    observeEvent(input$show_missing,{

      output$sinteza_provizioane_curente <-  DT::renderDataTable({ req(input$show_missing == TRUE, vals$provizioane_curente)
        DT::datatable(rownames = FALSE,      options = list(dom = "Ftp", scrollX=TRUE),class = "nowrap",
          data = vals_regularizare_provizioane$provizioane_curente %>% dplyr::filter(is.na(Necesar_provizion)))
      })

      shinyWidgets::updatePrettyToggle(session = session,inputId = "show_report",value = FALSE)
    })


}

## To be copied in the UI
# mod_regularizare_provizioane_ui("regularizare_provizioane_ui_1")

## To be copied in the server
# callModule(mod_regularizare_provizioane_server, "regularizare_provizioane_ui_1")

