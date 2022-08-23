#' actualizare_sentinte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_actualizare_sentinte_ui <- function(id){
  ns <- NS(id)

  fluidRow(
     shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),

     shinyjs::useShinyjs(),

     bs4Dash::box(title = "Sentinte actualizate. Completeaza coeficientul de risc", status = "info",width = 12,
                  collapsible = T,collapsed = F,icon = icon("external-link-square-alt"), maximizable = T,

     #htmlOutput(ns("titlu_tabel")),

     DT::dataTableOutput(outputId = ns("actualizare_sentinte"))

     ),

     bs4Dash::box(title = "Sentinte duplicate. Nu se regasesc in lista de mai sus de sentinte actualizate",
                  status = "info",width = 12,
                  collapsible = T, collapsed = T,icon = icon("clone"), maximizable = T,

     DT::dataTableOutput(outputId = ns("sentinte_duplicate"))

     ),

     bs4Dash::box( status = "info",width = 12,
     title = "Litigii neactualizate in portal just.Nu indeplinesc criteriile pentru a fi considerate numar valabil al unui dosar",
     collapsible = T,collapsed = T,icon = icon("lock"), maximizable = T,

     DT::dataTableOutput(outputId = ns("litigii_neactualizate"))  )



 )

}

#' actualizare_sentinte Server Function
#'
#' @noRd
mod_actualizare_sentinte_server <- function(input, output, session, vals) {
  ns <- session$ns

  litigii_neactualizate <- readRDS("R/reactivedata/litigii_neactualizate.rds")

  sentinte_duplicate <- readRDS("R/reactivedata/sentinte_duplicate.rds")

  regex_dosar <- '(?:[1-9]|[1-9][0-9]|[1-9][0-9]{2}|[1-9][0-9]{3}|[1-9][0-9]{4}|[1-4][0-9]{5}|500000)/(?:[1-9]|[1-9][0-9]|[1-9][0-9]{2}|[1-9][0-9]{3}|10000)/(?:200[5-9]|20[1-2][0-9]|2030)[*]*'

  #number_range(lo = 1,hi = 500000,allow_leading_zeroes = F) %R% "/" %R%  number_range(lo = 1,hi = 10000,allow_leading_zeroes = F)  %R% "/" %R% number_range(lo = 2005, hi = 2030,allow_leading_zeroes = F) %R% zero_or_more("*")

  vals_sentinte <- reactiveValues(counter_litigii_sterse=0, sentinte_duplicate = sentinte_duplicate)

  output$sentinte_duplicate <- DT::renderDataTable( dt_generate_function(df = vals_sentinte$sentinte_duplicate %>%
                dplyr::select(-id_litigiu),    editable = TRUE,color = "#bc3c4b",
                  caption = "Action required. Sentinte dublate. Click pe sentinta.")    )

  observeEvent( input$sentinte_duplicate_rows_selected,{

    vals_sentinte$duplicate_selected <- vals_sentinte$sentinte_duplicate %>%
      dplyr::slice(input$sentinte_duplicate_rows_selected)

    showModal(modalDialog(
      title = paste0("Actions regarding sentinta din data de ",vals_sentinte$duplicate_selected$date_sedinte,
                     " pentru litigiul numarul ",vals_sentinte$duplicate_selected$numar_litigiu ),  size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton(ns('save_sentinta_duplicata'),
                     'Salveaza aceasta sentinta', class = "btn btn-primary", icon = icon("save")),

        actionButton(session$ns('exclude_sentinta_duplicata'), 'Elimina aceasta sentinta',
                     class = "btn btn-primary", icon = icon("minus-circle"))),

      fluidRow(column(width=6, textAreaInput(inputId = ns("tip_solutie_duplicata"),
                                           label = "Tip solutie", value = vals_sentinte$duplicate_selected$tip_solutie)),
               column(width = 6, textAreaInput(inputId = ns("solutie_duplicata"),label = "Solutie pe scurt",
                                              value = vals_sentinte$duplicate_selected$solutie))
      )
    )
    )
    shinyjs::disable(id = ns("save_sentinta_duplicata"), asis = F)

    } )


  observeEvent( input$exclude_sentinta_duplicata, {
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_exclude_sentinta"),
        btn_colors = c("#bc3c4b","#3cbcad"),type = "success",  title = "CONFIRM",
        text = "Esti sigur ca vrei sa excluzi sentinta selectata?")
  } )


  observeEvent(input$confirm_exclude_sentinta, { req(input$confirm_exclude_sentinta == TRUE)
    removeModal( session = session )

    sentinte_duplicate_excluse <- readRDS("R/reactivedata/sentinte_dublate_eliminate.rds")

    vals_sentinte$sentinta_dublata_eliminata <- vals_sentinte$duplicate_selected %>%
      dplyr::mutate(identifier = paste0(id_litigiu, date_sedinte, tip_solutie,solutie))


    if ( janitor::compare_df_cols_same(vals_sentinte$sentinta_dublata_eliminata, sentinte_duplicate_excluse) ) {

      saveRDS(object = dplyr::bind_rows(vals_sentinte$sentinta_dublata_eliminata,sentinte_duplicate_excluse),
              file = "R/reactivedata/sentinte_dublate_eliminate.rds")

      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
         .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))

      vals_sentinte$sentinte_duplicate  <- vals_sentinte$sentinte_duplicate %>%
        dplyr::mutate(identifier = paste0(id_litigiu, date_sedinte, tip_solutie,solutie)) %>%
        dplyr::filter(identifier !=   vals_sentinte$sentinta_dublata_eliminata$identifier) %>%
        dplyr::select(-identifier)

      saveRDS( object = vals_sentinte$sentinte_duplicate, file = "R/reactivedata/sentinte_duplicate.rds" )

    }

    else { shinyFeedback::showToast(type = "error",title = "ERROR",message = "Failed to save", keepVisible = F) }



  })

  # I did not code this till the end. Sentinta to be saved will be processed anyway within the daily job sync.
   observeEvent(input$save_sentinta_duplicata,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_sentinta_duplicata"),
                                   btn_colors = c("#bc3c4b","#3cbcad"),type = "success",  title = "CONFIRM",
                                   text = "Esti sigur ca vrei sa salvezi sentinta selectata?")
  })


output$litigii_neactualizate <- DT::renderDataTable( dt_generate_function(df = litigii_neactualizate %>%
            dplyr::select(-id_litigiu), editable = TRUE,
  caption = "Litigii neactualizate in portal just. Nu indeplinesc criteriile pentru a fi considerate numar valabil al unui dosar."
  ))


  observeEvent( input$litigii_neactualizate_rows_selected,{

    vals_sentinte$litigiu_neactualizat_selected <- litigii_neactualizate %>%
      dplyr::slice(input$litigii_neactualizate_rows_selected)

    query_main <- "SELECT * from main_litigii where id = ?id1"

    vals_sentinte$litigiu_neactualizat_interogat <- get_query(query = query_main,
             id1 = vals_sentinte$litigiu_neactualizat_selected$id_litigiu)


    showModal(modalDialog(
      title = paste0(" Updateaza litigiul ", vals_sentinte$litigiu_neactualizat_selected$numar_litigiu, ". Atentie, se va updata doar numarul litigiului"),
      size = "l",
      footer = list(
        modalButton('Cancel'),
        actionButton( inputId = ns('save_litigiu_neactualizat'),
                      label = 'Salveaza acest litigiu', class = "btn btn-primary", icon = icon("save"))),

      fluidRow(column( width=6, textInput(inputId = ns("numar_litigiu_neactualizat"),label = "Numar litigiu",
                                    value = vals_sentinte$litigiu_neactualizat_selected$numar_litigiu),
                       #dateInput(ns("data_litigiu_neactualizat"),label = "Data litigiu",
                               #  value = vals_sentinte$litigiu_neactualizat_interogat$data_initiala_dosar),
                       textInput(ns("contract_garantare_litigiu_neactualizat"), label = "Contract de garantare",
                                 value = vals_sentinte$litigiu_neactualizat_interogat$contract_garantare)),
               column(width = 6, textInput(ns("tip_litigiu_neactualizat"), "Tip litigiu",
                                           value = vals_sentinte$litigiu_neactualizat_interogat$Tip_litigiu),
                      textInput(ns("banca_litigiu_neactualizat"),label = "Banca",value = vals_sentinte$litigiu_neactualizat_interogat$Banca),
                      textInput(ns("imm_litigiu_neactualizat"),label = "IMM",value = vals_sentinte$litigiu_neactualizat_interogat$IMM))

               )

    )
    )

    })

  observeEvent( input$numar_litigiu_neactualizat,{
    shinyFeedback::feedbackDanger(inputId = "numar_litigiu_neactualizat",
                                  show = stringr::str_detect(input$numar_litigiu_neactualizat,regex_dosar, negate = TRUE),
                                  color = "red", session = session, text = "Completeaza un numar corespunzator al dosarului")

    if ( stringr::str_detect(input$numar_litigiu_neactualizat,regex_dosar, negate = TRUE) ) shinyjs::disable(id = 'save_litigiu_neactualizat')
    else if ( stringr::str_detect(input$numar_litigiu_neactualizat,regex_dosar, negate = FALSE ) ) shinyjs::enable('save_litigiu_neactualizat')

    shinyjs::disable('contract_garantare_litigiu_neactualizat')
    shinyjs::disable('tip_litigiu_neactualizat')
    shinyjs::disable('banca_litigiu_neactualizat')
    shinyjs::disable('imm_litigiu_neactualizat')

  })

  observeEvent(input$save_litigiu_neactualizat,{
    shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save_litigiu_neactualizat"),
                                   btn_colors = c("#bc3c4b","#3cbcad"),type = "success",  title = "CONFIRM",
                                   text = "Esti sigur ca vrei sa salvezi modificarile efectuate pe acest litigiu?")
  })

  observeEvent( input$confirm_save_litigiu_neactualizat, { req( input$confirm_save_litigiu_neactualizat == TRUE )

    removeModal(session = session)

    sql_update_actualizare_litigiu <- "UPDATE actualizare_litigiu SET numar_litigiu = ?id1 where id_litigiu = ?id2"

    send_query(sql_update_actualizare_litigiu, success_message = "Numar litigiului a fost actualizat cu succes",
               error_message = "Nu am actualizat numarul litigiului", id1 = input$numar_litigiu_neactualizat,
               id2 = vals_sentinte$litigiu_neactualizat_selected$id_litigiu)

    litigii_neactualizate <- litigii_neactualizate %>%
      dplyr::filter(numar_litigiu != vals_sentinte$litigiu_neactualizat_selected$numar_litigiu)

    saveRDS(object = litigii_neactualizate,file = "R/reactivedata/litigii_neactualizate.rds")

    browser()
    output$litigii_neactualizate <- DT::renderDataTable(dt_generate_function(df = litigii_neactualizate %>% dplyr::select(-id_litigiu), editable = TRUE,
      caption = "Litigii neactualizate in portal just. Nu indeplinesc criteriile pentru a fi considerate numar valabil al unui dosar.") )


    })

  observeEvent( vals$sentinte,{

    vals_sentinte$actualizare_sentinte <- vals$sentinte %>% dplyr::filter(is.na(coef_risc),data_sentinta < Sys.Date(),
                                    tip_solutie != "") %>%
   dplyr::left_join(by = "id_litigiu", y = vals$provizioane_curente %>%
                      dplyr::select(id_litigiu,Tip_litigiu,Calitate_FNG,numar_litigiu,instanta))

 vals_sentinte$beneficiari <- vals$sentinte %>% dplyr::group_by(id_litigiu) %>%  dplyr::arrange(desc(data_sentinta)) %>%
    dplyr::summarise(coef_risc=dplyr::first(coef_risc[!is.na(coef_risc)]))



 vals_sentinte$tabel_sentinte <- vals_sentinte$actualizare_sentinte %>% dplyr::select(9,2,8,3:4,10,7,6,1)



 output$actualizare_sentinte <- DT::renderDataTable({ req(vals_sentinte$tabel_sentinte)
   dt_generate_function(df = vals_sentinte$tabel_sentinte, color = "black",pageLength = 5, class = "nowrap",
      caption = "Sentinte preluate de pe portaljust. Click pentru a vizualiza si actualiza coeficientul de risc")   })
  })



   #output$titlu_tabel <- renderUI({req(vals_sentinte$tabel_sentinte) HTML(paste(h3("Sentinte care au nevoie de completarea coeficientului de risc aferent")))      })

   observeEvent(input$actualizare_sentinte_rows_selected,{


   vals_sentinte$litigiu_selectat <- vals_sentinte$actualizare_sentinte  %>%
     dplyr::slice((input$actualizare_sentinte_rows_selected + vals_sentinte$counter_litigii_sterse))

   vals_sentinte$vechiul_coef_risc <- vals_sentinte$beneficiari %>% dplyr::filter(!is.na(coef_risc)) %>%
      dplyr::filter(id_litigiu==vals_sentinte$litigiu_selectat$id_litigiu) %>%
         dplyr::slice(1) %>% dplyr::pull('coef_risc')

      #vals_sentinte$beneficiari$coef_risc[
      #match(x = vals_sentinte$litigiu_selectat$id_litigiu,  table = vals_sentinte$beneficiari$id_litigiu)]

   showModal(modalDialog(title = paste("Sentina noua din data de ",vals_sentinte$litigiu_selectat$data_sentinta,
   " pentru litigiu ",vals_sentinte$litigiu_selectat$numar_litigiu),
                         size = "l",   footer = list(actionButton(inputId = session$ns("save_sentinta"),
                                            label = "Save",icon = icon("save")),
                                                     modalButton(label = "Close")),
                         fluidRow(column(width = 6,
                                  textInput(inputId = session$ns("calitate_fngc"), width = "400px",
                                           label = "Calitate FNGCIMM",
                                           value = vals_sentinte$litigiu_selectat$Calitate_FNG),

                           textInput(inputId = session$ns("tip_solutie"),vals_sentinte$litigiu_selectat$tip_solutie,
                                     label = "Tip solutie",width = "400px"),
                           textAreaInput(inputId = session$ns("solutie_noua"),resize = "vertical",
                                         width = "400px",height = "150px",
                                         value = vals_sentinte$litigiu_selectat$solutie,
                                         label = "Solutie noua")),

                           column(width = 6,
                                  textInput(inputId = session$ns("vechiul_coef_risc"), width = "400px",
                                            label = "Actualul coeficient de risc",
                                            value = vals_sentinte$vechiul_coef_risc),

                                  shinyWidgets::pickerInput(inputId = session$ns("coef_risc"),width = "380px",
                                          label = "Selecteaza noul coeficient de risc",choices = c(1,0.25,0.65,0),
                                          selected = ifelse(is.na(vals_sentinte$vechiul_coef_risc),0.65,
                                                            ifelse(vals_sentinte$litigiu_selectat$tip_solutie %in% c("Amana pronuntarea","Amana cauza") |
                                                               vals_sentinte$litigiu_selectat$solutie == "",
                                                            vals_sentinte$vechiul_coef_risc, 1))),

                                  textAreaInput(inputId = session$ns("solutie_cumulata"),resize = "vertical",
                                                width = "400px",height = "150px",
                                                value = vals_sentinte$litigiu_selectat$solutie_cumulata,
                                                label = "Solutie cumulata")

                           ) )  ) )
})

   observeEvent(input$save_sentinta,{
      removeModal(session = session)

      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_sentinta"),
                                   btn_colors = c("#bc3c4b","#3cbcad"),type = "success",
                                   title = "CONFIRM",text = "Esti sigur ca vrei sa salvezi noul coeficient de risc?")
    })

   observeEvent( input$confirm_sentinta,{ req(input$confirm_sentinta == TRUE)

      sentinte_proxy <- DT::dataTableProxy(outputId = "actualizare_sentinte")

      removeModal(session = session)

      update_sql <- "UPDATE sentinte SET coef_risc = ?id1 WHERE id_litigiu = ?id2 AND data_sentinta = ?id3;"

      db <- config::get("database", file = "R/credentials/db_credentials.yml")

      my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username,
                                      dbname = db$dbname, host = db$host)


      vals_sentinte$update_query <- DBI::sqlInterpolate(conn = my_connection,sql = update_sql,
            id1 = as.numeric(input$coef_risc), id2 = vals_sentinte$litigiu_selectat$id_litigiu,
                                                        id3 = vals_sentinte$litigiu_selectat$data_sentinta)

      DBI::dbExecute(conn = my_connection,statement = vals_sentinte$update_query)


      vals$sentinte <-  my_connection %>% dplyr::tbl("sentinte") %>%
        dplyr::collect() %>% dplyr::filter(data_sentinta <= vals$provision_date) %>%
         dplyr::mutate(dplyr::across(data_sentinta,~as.Date(.x)))

      DBI::dbDisconnect(conn = my_connection)

      updateActionButton(session = session,inputId = "save_sentinte")


      DT::replaceData(proxy = sentinte_proxy,data = vals_sentinte$tabel_sentinte,
                      resetPaging = TRUE,clearSelection = FALSE)


      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                               .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))



   })




}

## To be copied in the UI
# mod_actualizare_sentinte_ui("actualizare_sentinte_ui_1")

## To be copied in the server
# callModule(mod_actualizare_sentinte_server, "actualizare_sentinte_ui_1")

