#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_ui <- function(id){
  ns <- NS(id)

  logs <- readRDS("R/logs/logs.rds")

  bs4Dash::tabsetPanel(  id = ns("admin"),
    selected = T,  shinyjs::useShinyjs(),
    shiny::tabPanel( title = "Logs", value = ns("logs"), icon = icon("clipboard-list"), br(),
      fluidRow(
        column(
          width = 6,
          shinyWidgets::airDatepickerInput(
            ns("logs_date"),
            label = "Data logurilor",
            value = Sys.Date(),
            autoClose = T,
            language = "ro"
          )
        ),
        column(
          width = 6,
          shinyWidgets::pickerInput(
            ns("logs_category"),
            label = "Categoria logurilor",
            choices = unique(logs$Category)
          )
        ),
        column(width = 12, DT::dataTableOutput(ns("logs")))
      )
    ),
     shiny::tabPanel( title = "Test",
                     DT::dataTableOutput(ns("test")) ),

    shiny::tabPanel( title = "Contracte-Beneficiari",
      value = "contracte",    icon = icon("file-contract"),    br(),
      fluidRow(
        column(width = 12,
               DT::dataTableOutput(ns("baza_contracte")), br()),

        column(
          width = 6,
          fileInput(
            ns("upload_bi_contracte"),
            "Upload BI to update contracte beneficiari database"
          )
        ),
        column(
          width = 6,
          br(),
          downloadLink(
            outputId = ns("link_bi_contracte"),
            label = "Click aici pentru a downloada modelul de BI",
            class = "down-link-2-column"
          )
        )
      )
    )

  )
}

#' admin Server Functions
#'
#' @noRd
mod_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      bi_contracte <- readRDS("R/reactivedata/dosare_noi/bi_contracte.rds")

      bi_contracte_snpashot <- readxl::read_excel(path = "R/reactivedata/bi_contracte.xlsx",range = "A1:B2") %>%
      dplyr::pull(All) %>% as.Date.character(format = "%Y-%m-%d 00:00:00.000")

      logs <- readRDS("R/logs/logs.rds")

     output$logs <- DT::renderDataTable( dt_generate_function(
       logs %>% dplyr::filter(lubridate::date(timestamp)==
                        input$logs_date, Category == input$logs_category) %>% dplyr::select(-Category)))

      vals_admin <- reactiveValues(bi_contracte = bi_contracte,bi_contracte_snpashot = bi_contracte_snpashot )

      updateTabsetPanel(session = session, inputId = 'admin',selected = "logs")

      shinyWidgets::updateAirDateInput(session = session,inputId = "logs_date",value = max(logs$timestamp, na.rm = TRUE))

      output$test <- DT::renderDataTable(dt_generate_function(df = readr::read_csv("test.csv")))

      observeEvent(vals_admin$bi_contracte, { req(vals_admin$bi_contracte_snpashot)

        output$baza_contracte <- DT::renderDataTable( DT::datatable(data =
        vals_admin$bi_contracte %>%  dplyr::summarise(Nr_contracte=dplyr::n(), Nr_beneficiari = dplyr::n_distinct(CUI)) %>%
        dplyr::mutate(data_snapshot = vals_admin$bi_contracte_snpashot),
        options = list(dom="Bt"),rownames = F, extensions = "Buttons") %>% DT::formatRound(columns = 1:2,digits = 0))
        })

    output$link_bi_contracte <- downloadHandler(filename = function(){"BI_contracte.xlsx"},
                                                    content = function(file) {
      file.copy(from = "R/reactivedata/bi_contracte.xlsx", to = file) } )

    observeEvent(input$upload_bi_contracte,{

      vals_admin$bi_snapshot <- readxl::read_excel(path = input$upload_bi_contracte$datapath,range = "A1:B2") %>%
        dplyr::pull(All) %>% as.Date.character(format = "%Y-%m-%d 00:00:00.000")

      vals_admin$bi_contracte_snpashot <- readxl::read_excel(path = input$upload_bi_contracte$datapath,range = "A1:B2") %>%
        dplyr::pull(All) %>% as.Date.character(format = "%Y-%m-%d 00:00:00.000")

      if (vals_admin$bi_snapshot <= bi_contracte_snpashot) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "warning",text = "Nu pot salva un BI cu
                        data snapshot mai mic decat ce am storat")  }

        else {
          vals_admin$bi_contracte <- readxl::read_excel(path = input$upload_bi_contracte$datapath, skip = 5) %>%
          dplyr::filter(!is.na(`Numar contract`)) %>% setNames(nm = c("NumarContract","Banca",
                              "NumeBeneficiar","CUI","CodPartener"))


          if (janitor::compare_df_cols_same(vals_admin$bi_contracte,bi_contracte)) {

            saveRDS(object = vals_admin$bi_contracte,file = "R/reactivedata/dosare_noi/bi_contracte.rds")

            file.copy(from = input$upload_bi_contracte$datapath,to = "R/reactivedata/bi_contracte.xlsx",overwrite = T)

            shinyFeedback::showToast(type = "success",title = "SUCCES",message = "BI successfully updated",
                  .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) }

          else { shinyFeedback::showToast( message = "Problems updating BI",
          type = "error",  title = "ERROR",
          .options = list("timeOut" = 0, 'positionClass' = "toast-bottom-right", "progressBar" = TRUE )) }
          }


    })


  })
}

## To be copied in the UI
# mod_admin_ui("admin_ui_1")

## To be copied in the server
# mod_admin_server("admin_ui_1")
