#' litigii_noi_automate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_litigii_noi_automate_ui <- function(id){
  ns <- NS(id)

   fluidRow(
  shinyjs::useShinyjs(),


  h4("Litigiile de mai jos sunt preluate automat de pe portal just"),


  DT::dataTableOutput(ns("litigii_noi")),

  br(),
  hr(),
  br(),

  h4("Litigiile de mai jos au fost excluse din litigiile noi ale Fondului ca
     nefiind de interes pentru constituirea de provizioane"),

  DT::dataTableOutput(ns("litigii_excluse"))
  )


}

#' litigii_noi_automate Server Function
#'
#' @noRd
mod_litigii_noi_automate_server <- function(input, output, session, vals){
  ns <- session$ns

  dosare_noi <- readRDS("R/reactivedata/dosare_noi/dosare_noi.rds")

  dosare_excluse <- readRDS("R/reactivedata/dosare_noi/dosare_excluse.rds")

  vals_litigii_noi <- reactiveValues(dosare_excluse = dosare_excluse)


  # Process dosare noi: eliminate dosare excluse, categorie caz Faliment sau litigii de munca,
  # calitate FNGCIMM=reclamant, dosarele care au parte FRC, dosarele care au obiect incuviintare exec silita,
  # dosarele care se afla deja in litigii curente precum si dosarele care se afla in lista dosarelor cu numar nou
  vals_litigii_noi$dosare_noi <- dosare_noi %>%
    # I eliminate dosare exlcuse
    dplyr::filter(!numar_litigiu %in% vals_litigii_noi$dosare_excluse$numar_litigiu) %>%
    # I eliminate Faliment
    dplyr::filter(!categorie_caz_dosar_nou %in%  c("Faliment", "Litigii de munca")) %>%
    # I need to process is.na(calitate_fngcimm) in order to filter out calitate_fngcimm!=Reclamant
    dplyr::mutate(calitate_fngcimm = ifelse(is.na(calitate_fngcimm),"",calitate_fngcimm)) %>%
    # I eliminate calitate FNGCIMM - Reclamant
    dplyr::filter( calitate_fngcimm != "Reclamant")  %>%
    # I remove FRC dosare
    dplyr::filter( is.na(parti_adverse) | stringr::str_detect(string = parti_adverse, pattern = "FONDUL ROM.N DE CONTRAG",
                                            negate = TRUE)) %>%
    # I remove dosare cu incuviintare silita
    dplyr::filter(obiect_dosar_nou != "incuviintare executare silita") %>%
    # I remove dosare care se afla deja in litigii curente
    dplyr::filter(!numar_litigiu %in% vals$litigii_curente$numar_litigiu) %>%
    # I remove dosare care se regasesc la dosare cu numar care se modifica

    dplyr::filter(!numar_litigiu %in% vals$litigii_curente$numar_litigiu) %>%
        dplyr::filter(stringr::str_detect(string = numar_litigiu,pattern = "\\*", negate = TRUE))
    #dplyr::slice(-(  purrr::map(.x = .$numar_litigiu,
        # I check to see if numar old dosar se gaseste inside numarul nou
    #    ~ stringr::str_which( string = .x,  pattern = vals$litigii_curente$numar_litigiu %>%  stringr::str_remove_all(pattern = "\\*\\*")) ) %>%
     #   purrr::map_dbl(.x = .,  ~ purrr::detect_index(.x,  ~ length(.x) != 0)) %>% which(x = . > 0) ))


  output$litigii_noi <- DT::renderDataTable({ req(vals_litigii_noi$dosare_noi)
    DT::datatable(rownames = FALSE, selection = list(mode = "single",selected = NULL, target = "row"),
    class = "nowrap",caption = "Click pe litigiu pentru a vizualiza",
    options = list(dom = "ftp", scrollX = TRUE,pageLength = 5),
    colnames = c("Numar","Data","Materie","Stadiu","Instanta","Calitate FNGCIMM","Obiect", "Parti adverse"),
    data = vals_litigii_noi$dosare_noi %>%
      dplyr::select(1:4,6,8,5,7) %>%
      dplyr::mutate(dplyr::across(.cols = c(categorie_caz_dosar_nou, stadiu_procesual_dosar_nou,obiect_dosar_nou,
              instante_dosar_nou,calitate_fngcimm), ~as.factor(.x))), # DONT DO IT, IT WILL AFFECT select rows in DT%>% dplyr::arrange(data_litigiu_nou),
    filter = list(position = "top", clear = TRUE, plain = TRUE))
    })

    output$litigii_excluse <- DT::renderDataTable({ req(vals_litigii_noi$dosare_excluse)
    DT::datatable(rownames = FALSE, selection = list(mode = "single",selected = NULL, target = "row"),
                  class = "nowrap",caption = "Litigii excluse",
                  options = list(dom = "ftp", scrollX = TRUE,pageLength = 5),
                  data = vals_litigii_noi$dosare_excluse,
                  filter = list(position = "top", clear = TRUE, plain = TRUE))
  })


  # Observer for click inside tabel litigii noi
  observeEvent(input$litigii_noi_rows_selected, {
    vals_litigii_noi$dosar_selectat <-  vals_litigii_noi$dosare_noi  %>%
      dplyr::slice(input$litigii_noi_rows_selected)


    showModal(modalDialog(
      title = "Editeaza litigiul",  size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(session$ns('save_litigiu'),
                     'Save litigiu nou', class = "btn btn-primary", icon = icon("save")),
        actionButton(session$ns('exclude_litigiu'), 'Exclude litigiul', class = "btn btn-primary", icon = icon("minus-circle"))),

      fluidRow(column(width=6,
                      textInput(session$ns("numar_dosar_litigiu_nou"),
                        label = "Numar dosar instanta",    width = "400px",
                        value = vals_litigii_noi$dosar_selectat$numar_litigiu),

                      shinyWidgets::airDatepickerInput(inputId = ns("data_litigiu_nou"),label = "Data dosarului",
                            width = "400px", value = vals_litigii_noi$dosar_selectat$data_litigiu_nou),

                      textInput(session$ns("instanta_dosar_nou"), width = "400px",
                                value = vals_litigii_noi$dosar_selectat$instante_dosar_nou,
                                label = "Instanta"),
                      textInput(session$ns("categorie_caz_dosar_nou"),width = "400px",
                                value = vals_litigii_noi$dosar_selectat$categorie_caz_dosar_nou,
                                label = "Categorie caz"),
                      textAreaInput(session$ns("parti_dosar"),label = "Parti dosar",width = "400px",
                                    value = vals_litigii_noi$dosar_selectat$parti_adverse),

                      actionButton(inputId = session$ns("show_more"),label = "Show more to save litigiu",
                                   icon = icon("plus"),width = "380px"),

                      uiOutput(session$ns("show_banca")),
                      uiOutput(session$ns("show_contract")),
                      uiOutput(session$ns("show_tip_litigiu"))
                      ),
               column(width = 6,
                      shinyWidgets::autonumericInput(inputId = ns("valoare_litigiu_nou"),
                        value = 0, width = "400px",align = "right",decimalCharacter = ".",
                        decimalPlaces = 2,
                        label = "Introdu valoarea litigiului in lei"),
                        #help_text = "Litigiul trebuie sa fie mai mare ca zero pentru a putea fi salvat"),

                      shinyWidgets::pickerInput(session$ns("coef_proviz_litigiu_nou"),
                        label = "Selecteaza coeficientul de provizionare",
                        width = "380px",  choices = c(0.65, 1, 0.25, 0)),
                      textInput(session$ns("calitate_fngcimm"), width = "400px",
                                value = vals_litigii_noi$dosar_selectat$calitate_fngcimm,
                                label = "Calitate FNGCIMM"),
                      textInput(session$ns("stadiu_procesual"), width = "400px",
                               value = vals_litigii_noi$dosar_selectat$stadiu_procesual_dosar_nou,
                               label = "Stadiu procesual"),
                      textAreaInput(session$ns("obiect_dosar_nou"), width = "400px",
                                value = vals_litigii_noi$dosar_selectat$obiect_dosar_nou,
                                label = "Obiect dosar"),


                      uiOutput(session$ns("show_beneficiar")),
                      uiOutput(session$ns("show_cui_cod_partener"))
                      #uiOutput(session$ns("show_cui_litigiu_nou")),
                      #uiOutput(session$ns("show_cod_partener_litigiu_nou"))
                      )       ) ) )
    shinyjs::disable("save_litigiu")
  })


  # Observer related to the above modal edit litigiu nou
  observeEvent(input$show_more,{

    bi_contracte <<- readRDS("R/reactivedata/dosare_noi/bi_contracte.rds")

    vals_show_more <- reactiveValues(bi_contracte=bi_contracte, banci=unique(bi_contracte$Banca))

    removeUI("#litigii_noi_automate_ui_1-show_more")

    updateActionButton(session = session,inputId = "show_more")


    output$show_banca <- renderUI({req(input$show_more == 1)

      shinyWidgets::pickerInput(inputId = session$ns("banca_litigiu_nou"),
                                label = "Select Bank:", width = "380px",
                  choices = vals_show_more$banci,
                  selected = ifelse(
                    stringr::str_detect(
                      string = vals_litigii_noi$dosar_selectat$parti_adverse,
                      pattern = vals_show_more$banci),
                    vals_show_more$banci[stringr::str_which(pattern = vals_show_more$banci,
                    string = vals_litigii_noi$dosar_selectat$parti_adverse)],
                    "none") )   })

    removeUI(selector = "#litigii_noi_ui_1-show_more")

    shinyjs::enable("save_litigiu")

    output$show_beneficiar <- renderUI({req(input$banca_litigiu_nou != "none", input$show_more==1)

      shinyWidgets::pickerInput(inputId = session$ns("beneficiar_litigiu_nou"),label = "Select beneficiar:",
                                width = "380px", choices =   c("none"),options = list( `live-search` = TRUE) )
     })

    output$show_tip_litigiu <- renderUI({req(input$show_more==1)

      shinyWidgets::pickerInput(session$ns("tip_litigiu_nou"),label = "Tipul litigiului",
                                width = "400px",choices = c("somatie","popriri","pretentii"))
    })

  })

  observeEvent(input$banca_litigiu_nou,{req(input$banca_litigiu_nou != "none")

    shinyWidgets::updatePickerInput(session = session,inputId = "beneficiar_litigiu_nou",
                                    choices = c("none",bi_contracte %>% dplyr::filter(Banca==input$banca_litigiu_nou) %>%
                                                  dplyr::pull(NumeBeneficiar) %>% unique()))

    vals_litigii_noi$bi_contracte_banca_selectata <- bi_contracte %>% dplyr::filter(Banca == input$banca_litigiu_nou)
  })


  observeEvent(input$beneficiar_litigiu_nou,{ req(input$beneficiar_litigiu_nou != "none")

    output$show_contract <- renderUI({ req(input$beneficiar_litigiu_nou != "none")
      shinyWidgets::pickerInput(inputId = session$ns("contract_litigiu_nou"),label = "Selecteaza contractul de garantare",
                                choices = c(vals_litigii_noi$bi_contracte_banca_selectata  %>%
                                              dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                                              dplyr::pull(NumarContract) %>% unique(), "none")    )
      })


    output$show_cui_cod_partener   <- renderUI({ req(input$beneficiar_litigiu_nou != "none")
      tagList(
        shinyWidgets::pickerInput(session$ns("cui_beneficiar_litigiu_nou"),
                                  "CUI-ul beneficiarului:",
                                  choices = vals_litigii_noi$bi_contracte_banca_selectata  %>%
                                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                                    dplyr::pull(CUI) %>% unique()  ) ,

        shinyWidgets::pickerInput(session$ns("cod_partener_beneficiar_litigiu_nou"),
                                  "Cod partener:",
                                  choices = vals_litigii_noi$bi_contracte_banca_selectata  %>%
                                    dplyr::filter(NumeBeneficiar == input$beneficiar_litigiu_nou) %>%
                                    dplyr::pull(CodPartener) %>% unique())
      )   })

   })


  observeEvent(input$exclude_litigiu,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_litigiu_exclus"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                   text = "Esti sigur ca vrei sa excluzi litigiul?")


  })

  observeEvent(input$confirm_litigiu_exclus,{
    vals_litigii_noi$dosare_excluse <- dplyr::bind_rows(vals_litigii_noi$dosar_selectat,
                                                        vals_litigii_noi$dosare_excluse)

    dosare_excluse <- isolate(vals_litigii_noi$dosare_excluse)

    saveRDS(object = dosare_excluse,file = "R/reactivedata/dosare_noi/dosare_excluse.rds")

    vals_litigii_noi$dosare_noi <- vals_litigii_noi$dosare_noi %>% dplyr::filter(numar_litigiu !=
                        vals_litigii_noi$dosar_selectat$numar_litigiu)

    removeModal(session = session)

  })

  observeEvent(input$save_litigiu,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_litigiu"),title = 'CONFIRM',
                                   btn_colors = c("#bc3c4b","#3cbcad"), type = "success",
                                   text = "Esti sigur ca vrei sa salvezi litigiul?")
  })

  observeEvent(input$confirm_save_litigiu,{

    removeModal(session = session)

    sql_insert_main <-
      "INSERT INTO main_litigii (data_initiala_dosar, Tip_litigiu, contract_garantare, Banca, IMM, Cod_Partener, CUI) VALUES (?id1,?id2,?id3,?id4,?id5,?id6,?id7);"
    sql_insert_valoare <-
      "INSERT INTO valoare_litigiu (id_litigiu,valoare_litigiu,data_modificare) VALUES (last_insert_id(),?id8,?id1);"
    sql_insert_stare <-
      "INSERT INTO stare_litigiu (id_litigiu, stare_litigiu, from_date, to_date) VALUES (last_insert_id(), 'activ',?id1,'9999-12-31');"
    sql_insert_actualizare_litigii <-
      "INSERT INTO actualizare_litigiu (id_litigiu, numar_litigiu, data_modificare,instanta, Categorie_caz,Obiect,Stadiu_procesual,
    Calitate_FNG,Parti) VALUES (last_insert_id(),?id9,?id1, ?id10,?id11,?id12,?id13,?id14,?id15) ;"

    db <- config::get("database", file = "R/credentials/db_credentials.yml")

    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                    dbname = db$dbname, host = db$host)

    tryCatch(expr = {
    vals_litigii_noi$sql_query_insert_main <- DBI::sqlInterpolate(conn = my_connection,
        sql = sql_insert_main,
        id1 = input$data_litigiu_nou,
        id2 = input$tip_litigiu_nou,
        id3 = ifelse( is.null(input$contract_litigiu_nou),NA,   input$contract_litigiu_nou ),
        id4 = ifelse( is.null(input$banca_litigiu_nou), NA,  input$banca_litigiu_nou),
        id5 = ifelse(is.null(input$beneficiar_litigiu_nou),    NA,  input$beneficiar_litigiu_nou  ),
        id6 = ifelse( is.null(input$cod_partener_beneficiar_litigiu_nou),  NA,  input$cod_partener_beneficiar_litigiu_nou  ),
        id7 = ifelse( is.null(input$cui_beneficiar_litigiu_nou),  NA,  input$cui_beneficiar_litigiu_nou ) )

    vals_litigii_noi$sql_query_insert_valoare <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_valoare,
                        id8 = input$valoare_litigiu_nou,     id1 = input$data_litigiu_nou)


    vals_litigii_noi$sql_query_insert_stare <- DBI::sqlInterpolate(conn = my_connection, sql = sql_insert_stare,
                                                                           id1 = input$data_litigiu_nou)


    vals_litigii_noi$sql_query_insert_actualizare <- DBI::sqlInterpolate(  conn = my_connection,
        sql = sql_insert_actualizare_litigii,
        id9 = input$numar_dosar_litigiu_nou,
        id1 = input$data_litigiu_nou,
        id10 = input$instanta_dosar_nou,
        id11 = input$categorie_caz_dosar_nou,
        id12 = input$obiect_dosar_nou,
        id13 = input$stadiu_procesual,
        id14 = input$calitate_fngcimm,
        id15 = input$parti_dosar)
    }, error = function(e){
      DBI::dbDisconnect(conn = my_connection)
      shinyFeedback::showToast(message = "Problems updating database",conditionMessage(e),type = "error",title = "ERROR",
                    .options = list("timeOut"=0, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))

    } )

    tryCatch(expr = {

      DBI::dbBegin(my_connection)

      DBI::dbExecute(conn = my_connection,statement = vals_litigii_noi$sql_query_insert_main)

      DBI::dbExecute(conn = my_connection,statement = vals_litigii_noi$sql_query_insert_valoare)

      DBI::dbExecute(conn = my_connection,statement =  vals_litigii_noi$sql_query_insert_stare)

      DBI::dbExecute(conn = my_connection,statement = vals_litigii_noi$sql_query_insert_actualizare)

      DBI::dbCommit(conn = my_connection)

      DBI::dbDisconnect(conn = my_connection)

      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                               .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))

      vals_litigii_noi$dosare_noi <- vals_litigii_noi$dosare_noi %>% dplyr::filter(numar_litigiu !=
                                        vals_litigii_noi$dosar_selectat$numar_litigiu)

    }, error = function(e) {

      DBI::dbRollback(conn = my_connection)

      DBI::dbDisconnect(conn = my_connection)

      shinyFeedback::showToast(message = "Database error",conditionMessage(e),type = "error",title = "ERROR",
                               .options = list("timeOut"=0, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))

      })


  })




  }

## To be copied in the UI
# mod_litigii_noi_automate_ui("litigii_noi_automate_ui_1")

## To be copied in the server
# callModule(mod_litigii_noi_automate_server, "litigii_noi_automate_ui_1")

