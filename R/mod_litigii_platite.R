#' litigii_platite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_litigii_platite_ui <- function(id){
  ns <- NS(id)

  fluidPage(
  br(),
  fluidRow(
    column(width = 6,
           fileInput( inputId = ns("plati_input"),   label = "Upload BI",accept = c(".xlsx",".xls"),
                      buttonLabel = "Excel only",   width = "300px",  placeholder =  "no file uploaded" )   ),

    column(width = 6, downloadLink(outputId = ns("down_bi_plati"),
                              label = "Click aici pentru a downloada BI-ul de plati", class="down-link-2-column" )  ) ,

    column(width = 12,
    DT::dataTableOutput(ns("litigii_platite")), br(), uiOutput(ns("show_save_buttons")), br() ),

    column(width =12,  DT::dataTableOutput(ns("sinteza_upload")), br(),br() ),



    column(width = 4,
    shinyWidgets::prettyToggle(inputId = ns("show_sinteza"),status_on = "info",width = "250px",
                               icon_on = icon("wye-slash"), icon_off = icon("table"),
                               value = FALSE,status_off = "info",
                               label_on = "Hide sinteza BI",label_off = "Show contracte platite stocate"), br() ),
    column(width = 8,
    DT::dataTableOutput(ns("sinteza_bi_plati")) ),

    br(),br(),br(),

    column(width = 4,
    shinyWidgets::prettyToggle(inputId = ns("show_help"),status_on = "info",width = "250px",
                               icon_on = icon("wye-slash"), icon_off = icon("info"),
                               value = FALSE,status_off = "info",
                               label_on = "Hide help info",label_off = "Click for more info") ),
    column(width = 8, uiOutput(ns("help")) )
  )
  )


}

#' litigii_platite Server Function
#'
#' @noRd
mod_litigii_platite_server <- function(input, output, session, vals){

  ns <- session$ns

  sinteza_bi_litigii <- readRDS('R/reactivedata/sinteza_bi_litigii.rds')

  vals_litigii_platite <- reactiveValues(sinteza_bi_litigii = sinteza_bi_litigii)

  output$down_bi_plati <- downloadHandler(filename = {"BI_contracte_platite.xlsx"},
                    content = function(file) {file.copy(from = "R/reactivedata/bi_litigii.xlsx",to = file)} )

  output$sinteza_bi_plati <- DT::renderDataTable({  req(input$show_sinteza == TRUE)
    DT::datatable(rownames = FALSE, data = vals_litigii_platite$sinteza_bi_litigii,
                  #caption = "Sinteza date stocate BI contracte platite:",
                  options = list(dom = "t"))  })

  output$help <- renderUI({ req(input$show_help == TRUE)
    tags$div(
      tags$ul(
        tags$li("Se downloadeaza template-ul de BI folosind link-ul furnizat mai sus"),
        tags$li("Se actualizeaza snapshot-ul si se salveaza local"),
        tags$li("Se uploadeaza folosind butonul Upload BI de mai sus"),
        tags$li("Dupa ce se uploadeaza fisierul, utilizatorul va putea opta daca salveaza noile date uploadate"),
        tags$li("Pentru a vedea datele deja disponbile se poate face click pe butonul de mai sus")
        )
    )


  })

  observeEvent(input$plati_input,{


    vals_litigii_platite$bi_litigii_contracte_platite <- readxl::read_excel(input$plati_input$datapath,skip = 5) %>%
      dplyr::mutate(dplyr::across(.cols = Day,  ~ as.Date(.x, format = "%Y-%m-%d 00:00:00.000"))) %>%
        dplyr::select(`Numar contract`, DataPlata1 = Day,  ValoarePlata1 = Total)


    vals_litigii_platite$bi_litigii_contracte_platite_snapshot <- readxl::read_excel(input$plati_input$datapath,
                 n_max = 4) %>%  dplyr::slice(1) %>% dplyr::pull(2) %>% as.Date()


    vals_litigii_platite$new_sinteza_bi_litigii <- data.frame(
      Current_Snapshot = vals_litigii_platite$bi_litigii_contracte_platite_snapshot,
      Minimum_DataPlata1 = min(vals_litigii_platite$bi_litigii_contracte_platite$DataPlata1, na.rm = TRUE),
      Maximum_DataPlata1 = max(vals_litigii_platite$bi_litigii_contracte_platite$DataPlata1, na.rm = TRUE),
      Nr_observatii_bi_stocat = nrow(vals_litigii_platite$bi_litigii_contracte_platite),
      stringsAsFactors = FALSE)

    vals_litigii_platite$caption_new_sinteza <- ifelse(vals_litigii_platite$bi_litigii_contracte_platite_snapshot >
                      vals_litigii_platite$sinteza_bi_litigii$Current_Snapshot &
                        nrow(vals_litigii_platite$bi_litigii_contracte_platite) >
                        vals_litigii_platite$sinteza_bi_litigii$Nr_observatii_bi_stocat,
                      "Ai uploadat datele de mai jos. Sunt mai recente decat ce am stocat si le voi salva automat.",
                      ifelse(vals_litigii_platite$bi_litigii_contracte_platite_snapshot <
                               vals_litigii_platite$sinteza_bi_litigii$Current_Snapshot &
                               nrow(vals_litigii_platite$bi_litigii_contracte_platite) ==
                               vals_litigii_platite$sinteza_bi_litigii$Nr_observatii_bi_stocat,
                             "Ai uploadat datele de mai jos. Snapshot-ul uploadat este mai mic decat ce am stocat,
                             dar numarul de contracte platite este acelasi.",
                             "Ai uploadat datele de mai jos. Datele sunt mai vechi decat ce am stocat."))

    output$sinteza_upload <- DT::renderDataTable({
      dt_generate_function( caption = vals_litigii_platite$caption_new_sinteza,
          df = vals_litigii_platite$new_sinteza_bi_litigii ) })



    vals_litigii_platite$litigii_platite <- vals$litigii_curente %>% dplyr::filter(contract_garantare %in%
          vals_litigii_platite$bi_litigii_contracte_platite$`Numar contract`) %>%
      dplyr::select(1,3:7, 23:25) %>% dplyr::left_join(
        y = vals_litigii_platite$bi_litigii_contracte_platite %>%
          dplyr::select(`Numar contract`, DataPlata1),
        by = c(contract_garantare = "Numar contract"))


    output$litigii_platite <- DT::renderDataTable({ dt_generate_function(df = vals_litigii_platite$litigii_platite %>%
        dplyr::select(2:6,10),  pageLength = 5,
        caption = ifelse( nrow(vals_litigii_platite$litigii_platite) == 0,
                  "Nu ai uploadat contracte platite neactualizate in baza de date",
                  "Ai uploadat contracte de garantare neactualizate in baza de date. Click save below to update."))  })

    output$show_save_buttons <- renderUI({ req( nrow(vals_litigii_platite$litigii_platite) > 0 )
      shinyWidgets::actionBttn(ns("save"),"Salveaza litigiile platite de mai sus in baza de date", icon = icon("save"),
                 style = "stretch",color = "danger",size = "sm")      })

   })

  observeEvent(input$save,{
   update_sql <- paste0("insert into temp_stare_litigiu(id_litigiu, stare_litigiu, from_date, to_date) values(",
                         paste0(vals_litigii_platite$litigii_platite$id_litigiu,",", "'",
                                rep("platit",nrow(vals_litigii_platite$litigii_platite)),"'", ",", "'",
                                as.character(vals_litigii_platite$litigii_platite$DataPlata1), "'", ",", "'",
                         rep("9999-12-31",nrow(vals_litigii_platite$litigii_platite)),"'",
                         collapse    ="),("),")")

    send_query(query = update_sql, success_message = "Litigiile platite au fost salvate in baza de date",
               error_message = "Litigiile platite NU au fost salvate in baza de date")

    file.copy(from = input$plati_input$datapath,to = "R/reactivedata/bi_litigii.xlsx",overwrite = TRUE)

    saveRDS( vals_litigii_platite$new_sinteza_bi_litigii,'R/reactivedata/sinteza_bi_litigii.rds')

    shinyjs::disable("save")

  })

  # I save sinteza BI litigii platite
  observeEvent(vals_litigii_platite$caption_new_sinteza, { req (vals_litigii_platite$caption_new_sinteza ==
                "Ai uploadat datele de mai jos. Sunt mai recente decat ce am stocat si le voi salva automat.")
  saveRDS(object = vals_litigii_platite$new_sinteza_bi_litigii, file = 'R/reactivedata/sinteza_bi_litigii.rds')
  })




}

## To be copied in the UI
# mod_litigii_platite_ui("litigii_platite_ui_1")

## To be copied in the server
# callModule(mod_litigii_platite_server, "litigii_platite_ui_1")

