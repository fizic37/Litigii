#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(dbplyr) 

app_server <- function( input, output, session) {
  
  sidebar_selected <- c()
  
  box_selected <- c()
  
  vals <- reactiveValues(sidebar_selected = sidebar_selected, report_date = Sys.Date(), provision_date = Sys.Date(),
                         provision_from_date = as.Date("2020-12-31"),
                         max_provision_date=as.Date("2020-12-31"),
                         box_selected=box_selected)
  
  
  # Here I calculate datele la care am inghetat provizioanele contabile
  observeEvent(vals$max_provision_date,{
    
    sql_max_provision_date <- "SELECT max(data_provizion_contabil) from provizion;"
    vals$max_provision_date <- get_query( query =  sql_max_provision_date, 
                                      error_message = "app_server error: Could not get max_provision_date")[[1]]
      #my_connection %>% dplyr::tbl("provizion") %>% dplyr::pull(data_provizion_contabil) %>% max(na.rm = T)
    
    sql_provision_dates <- "SELECT distinct data_provizion_contabil from provizion order by data_provizion_contabil desc;"
    vals$provision_dates <- get_query( query =  sql_provision_dates, error_message = 
                        "app_server error: Could not get provision dates")[[1]]
      #my_connection %>% dplyr::tbl("provizion") %>% dplyr::arrange(desc(data_provizion_contabil)) %>%  dplyr::pull(data_provizion_contabil) %>% unique()
    
  })
  
  observeEvent(vals$report_date,{
    
    tryCatch(expr = {
    db <- config::get("database", file = "inst/golem-config.yml")
    
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, dbname = db$dbname, host = db$host)
    
    
    report_date <- isolate(vals$report_date)
    
    max_provision_date <- isolate(vals$max_provision_date)
    
    
    vals$litigii_curente <-    my_connection %>% dplyr::tbl("main_litigii") %>% 
      dplyr::filter(data_initiala_dosar <= report_date) %>%
        dplyr::left_join(by = c("id" = "id_litigiu"), my_connection %>%  dplyr::tbl("stare_litigiu")) %>% 
          dplyr::filter(to_date >= report_date, stare_litigiu == "activ") %>%  dplyr::select(-from_date,-to_date)  %>% 
      # I introduce rename because below code is older and uses id_litigiu
      dplyr::rename_at(.vars = 'id',~c("id_litigiu")) %>%
      
      dplyr::left_join(by = "id_litigiu", my_connection %>% dplyr::tbl("provizion") %>%
                         dplyr::filter(data_provizion_contabil == max_provision_date) ) %>%
      
      dplyr::left_join(by = "id_litigiu",
                       my_connection %>% dplyr::tbl("valoare_litigiu") %>% 
                         dplyr::filter(data_modificare <=  report_date) %>%
                         dplyr::group_by(id_litigiu) %>% dplyr::summarise(data_modificare = max(data_modificare)) %>% 
                         dplyr::left_join(y = my_connection %>% dplyr::tbl("valoare_litigiu"), 
                                          by = c("id_litigiu" = "id_litigiu", "data_modificare" = "data_modificare")) %>%
                         dplyr::select(-data_modificare)  ) %>% 
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("actualizare_litigiu") %>% dplyr::filter(data_modificare <= report_date) %>%
                          dplyr::group_by(id_litigiu) %>% 
                          dplyr::summarise(ultima_data_modificare_litigiu=max(data_modificare,na.rm = T))) %>%
      dplyr::left_join(y = my_connection %>% dplyr::tbl("actualizare_litigiu"), 
                       by = c('id_litigiu' ='id_litigiu','ultima_data_modificare_litigiu' = 'data_modificare')) %>%
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("sentinte") %>% dplyr::group_by(id_litigiu) %>%
                          dplyr::summarise(ultima_data_actualizare = max(data_sentinta, na.rm = TRUE))) %>% 
      dplyr::left_join( my_connection %>% dplyr::tbl("sentinte"),
                        by = c( "id_litigiu" = "id_litigiu", "ultima_data_actualizare" = "data_sentinta") ) %>% 
      dplyr::collect()
    
    
    vals$dosare_modificate <- my_connection %>% dplyr::tbl("actualizare_litigiu") %>% 
      dplyr::select(id_litigiu,numar_litigiu, data_modificare) %>% dplyr::collect()
    
    vals$main_litigii <- my_connection %>% dplyr::tbl("main_litigii") %>% dplyr::left_join(y=
            my_connection %>% dplyr::tbl("actualizare_litigiu"), by = c("id" = "id_litigiu") ) %>%
      dplyr::left_join(y = my_connection %>% dplyr::tbl('valoare_litigiu') %>%
                         dplyr::select(id_litigiu, valoare_litigiu, data_actualizare_valoare_litigiu=data_modificare),
                       by=c("id"="id_litigiu")) %>% dplyr::collect()
    
    DBI::dbDisconnect(conn = my_connection)
    }, error = function(e) {
      DBI::dbDisconnect(conn = my_connection)
      shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = "app_server: observer vals$report_date",
                               keepVisible = TRUE )
    } )
  })
  
  # I define to_listen() a multiple event for the observer to listen
  to_listen <- reactive({
    list(vals$provision_date , vals$provision_from_date)
  })
  
  # Here I calculate provizioane curente
  observeEvent( to_listen(),{
    tryCatch( expr = {
    
    provision_date <- isolate(vals$provision_date)
    
    provision_from_date <- isolate(vals$provision_from_date)
    
    db <- config::get("database", file = "inst/golem-config.yml")
    
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, 
                                    dbname = db$dbname, host = db$host)
    
    vals$provizioane_curente <-    my_connection %>% dplyr::tbl("stare_litigiu") %>% dplyr::filter(to_date >
           provision_date, from_date <=  provision_date) %>%  dplyr::select(-to_date) %>%
            dplyr::left_join(by = c("id_litigiu" = "id"),
                       my_connection %>% dplyr::tbl("main_litigii"))  %>% 
            dplyr::left_join(by = "id_litigiu", my_connection %>% dplyr::tbl("provizion") %>% 
                         dplyr::filter(data_provizion_contabil == provision_from_date) ) %>%
            dplyr::left_join(by = "id_litigiu",
                       my_connection %>% dplyr::tbl("valoare_litigiu") %>% 
                         dplyr::filter(data_modificare <=  provision_date) %>%
                         dplyr::group_by(id_litigiu) %>% dplyr::summarise(data_modificare = max(data_modificare)) %>% 
                         dplyr::left_join(y = my_connection %>% dplyr::tbl("valoare_litigiu"), 
                                          by = c("id_litigiu" = "id_litigiu", "data_modificare" = "data_modificare")) %>%
                         dplyr::select(-data_modificare)  ) %>% 
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("actualizare_litigiu") %>% dplyr::filter(data_modificare <= provision_date) %>%
                          dplyr::group_by(id_litigiu) %>% dplyr::summarise(ultima_data_modificare_litigiu=max(data_modificare,na.rm = T))) %>%
      
      dplyr::left_join(y = my_connection %>% dplyr::tbl("actualizare_litigiu"), 
                       by = c('id_litigiu' ='id_litigiu','ultima_data_modificare_litigiu' = 'data_modificare')) %>%
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("sentinte") %>% dplyr::filter(data_sentinta <= provision_date) %>%
                          dplyr::group_by(id_litigiu) %>%
                          dplyr::summarise(ultima_data_actualizare = max(data_sentinta, na.rm = TRUE))) %>% 
      dplyr::left_join( my_connection %>% dplyr::tbl("sentinte"),
                        by = c( "id_litigiu" = "id_litigiu", "ultima_data_actualizare" = "data_sentinta") ) %>% 
      dplyr::collect()  %>% dplyr::mutate(DataPlata=ifelse(stare_litigiu=="platit",from_date,NA_character_)) %>% 
      dplyr::mutate(DataPlata=as.Date(DataPlata)) %>% dplyr::select(-from_date)
    
    # I need vals$actualizare_litigiu in order to extract numar_litigiu for new litigii with data_modificare > provision_date
    vals$actualizare_litigiu <- my_connection %>% dplyr::tbl("actualizare_litigiu") %>% 
      dplyr::select(id_litigiu,numar_litigiu, data_modificare) %>% dplyr::collect()  
    
    
    vals$sentinte <-  my_connection %>% dplyr::tbl("sentinte") %>% dplyr::filter(data_sentinta <=provision_date) %>%
      dplyr::collect() %>% dplyr::mutate(dplyr::across(data_sentinta,~as.Date(.x))) 
    
    # I need below provizioane inchise in order to correctly calculate regularizare provizioane pentru
    # acele litigii inchise intre from date si provision_date
    
    
    vals$provizioane_inchise <-  my_connection %>% dplyr::tbl("provizion") %>% 
      dplyr::filter(data_provizion_contabil>=provision_from_date, provizion_contabil>0) %>% 
      dplyr::left_join(by = c("id_litigiu" = "id"),
                       my_connection %>% dplyr::tbl("main_litigii"))  %>% 
      
      dplyr::left_join(by = "id_litigiu",
                       my_connection %>% dplyr::tbl("valoare_litigiu") %>% 
                         dplyr::filter(data_modificare <=  provision_date) %>%
                         dplyr::group_by(id_litigiu) %>% dplyr::summarise(data_modificare = max(data_modificare)) %>% 
                         dplyr::left_join(y = my_connection %>% dplyr::tbl("valoare_litigiu"), 
                                          by = c("id_litigiu" = "id_litigiu", "data_modificare" = "data_modificare")) %>%
                         dplyr::select(-data_modificare)  ) %>% 
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("actualizare_litigiu") %>% dplyr::filter(data_modificare <= provision_date) %>%
                          dplyr::group_by(id_litigiu) %>% dplyr::summarise(ultima_data_modificare_litigiu=max(data_modificare,na.rm = T))) %>%
      dplyr::left_join(y = my_connection %>% dplyr::tbl("actualizare_litigiu"), 
                       by = c('id_litigiu' ='id_litigiu','ultima_data_modificare_litigiu' = 'data_modificare')) %>%
      
      dplyr::left_join( by = "id_litigiu",
                        my_connection %>% dplyr::tbl("sentinte") %>% dplyr::group_by(id_litigiu) %>%
                          dplyr::summarise(ultima_data_actualizare = max(data_sentinta, na.rm = TRUE))) %>% 
      dplyr::left_join( my_connection %>% dplyr::tbl("sentinte"),
                        by = c( "id_litigiu" = "id_litigiu", "ultima_data_actualizare" = "data_sentinta") ) %>% 
      dplyr::collect() %>% dplyr::filter(!id_litigiu %in% vals$provizioane_curente$id_litigiu)
    
    
    DBI::dbDisconnect(conn = my_connection)
    
    }, error = function(e) {
      shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = "app_server: observer to_listen()",
                               keepVisible = TRUE ) 
    } )
    
    vals$provizioane_curente <- dplyr::bind_rows(vals$provizioane_inchise, vals$provizioane_curente)
    
  })
  
  
  mod_sidebar_server(id = "sidebar_ui_1", vals = vals)
  
  observeEvent(vals$box_selected,{ 
    
    if ( sum("box_sentinte" == vals$box_selected)==1 ) { 
      
      mod_individual_sentinta_noua_server("individual_sentinta_noua_ui_1", vals)
      
      vals$box_selected <- c(vals$box_selected, "box_sentinte")
      }
  })
  
  observeEvent(vals$sidebar_selected,{
    
    if (sum("litigii_current" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_current_server, "litigii_current_ui_1",vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii_current")
    }
    
    if (sum("actualizare_sentinta" == vals$sidebar_selected)==1) {
      
      callModule(mod_actualizare_sentinte_server, "actualizare_sentinte_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"actualizare_sentinta")
    }
    
    
    if (sum("litigiu_numar_nou" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_numar_nou_server, "litigii_numar_nou_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigiu_numar_nou")
    }
    
    if (sum("litigiu_nou_automat" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_noi_automate_server, "litigii_noi_automate_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigiu_nou_automat")
    }
    
    if (sum("litigii_noi" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_noi_manuale_server, "litigii_noi_manuale_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigii_noi")
    }
    
    if (sum("plati_litigii" == vals$sidebar_selected)==1) {
      
      callModule(mod_litigii_platite_server, "litigii_platite_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"plati_litigii")
    }
    
    if (sum("regularizare_provizioane" == vals$sidebar_selected)==1) {
      
      callModule(mod_regularizare_provizioane_server, "regularizare_provizioane_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"regularizare_provizioane")
    }
    
    if (sum("litigiu_individual" == vals$sidebar_selected)==1) {
      
      mod_litigiu_individual_server("litigiu_individual_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"litigiu_individual")
    }
    
    if (sum("admin" == vals$sidebar_selected)==1) {
      
      mod_admin_server("admin_ui_1")
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"admin")
    }
    
    
    
    
  })
  
}
