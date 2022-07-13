#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(magrittr)
library(dbplyr)

dt_generate_function <- function(df, class="compact",round_col = NULL,perc_col = NULL,caption = "",
                                 editable=FALSE,escape=T, dom = "tp", color = "#767f89",
                                 digits=0,show_buttons=FALSE,digits_perc=1, pageLength = NULL) {
  result <- DT::datatable(data = df,rownames = FALSE, escape=escape, class = class, 
                          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                            caption),editable=editable,extensions = "Buttons",
                          selection = list(mode = "single",selected = NULL, target = "row"),
                          options = list(pageLength = pageLength,
                                         buttons=c("copy","excel"),scrollX=TRUE,dom = ifelse(show_buttons,'Bfrtip',dom),info=FALSE,
                                         paging=ifelse(is.null(pageLength),FALSE,TRUE),searching=FALSE, 
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    DT::formatStyle(color = color,columns = 1:ncol(df))
  
  
  if (!is.null(round_col) & is.null(perc_col)) {return(result %>% 
                                                         DT::formatRound(columns = round_col,digits = digits,dec.mark = ".") ) }
  
  else if (is.null(round_col) & is.null(perc_col)) { return(result)  }
  
  else if (!is.null(round_col) & !is.null(perc_col)) {return(result %>% 
                                                               DT::formatRound(columns = round_col,digits = digits) %>%
                                                               DT::formatPercentage(columns = perc_col,digits = digits_perc) ) }
  else {return( result %>% DT::formatPercentage(columns = perc_col,digits = digits_perc) )  }
  
}


get_query <- function(query,success_message = "",error_message = "",...) {
  tryCatch(expr = {
    
    db <- config::get("database", file = "inst/golem-config.yml")
    
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, 
                                    dbname = db$dbname, host = db$host)
    
    final_query <- DBI::sqlInterpolate(conn = my_connection, sql = query, ...)
    
    
    DBI::dbBegin(my_connection)
    
    result <- DBI::dbGetQuery(conn = my_connection,statement = final_query)
    
    DBI::dbCommit(conn = my_connection)
    
    DBI::dbDisconnect(conn = my_connection)
    
    if (success_message != "") { shinyFeedback::showToast(type = "success",title = "SUCCES",message = success_message,
                                                          .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) }
    
    result
    
  }, error = function(e) {
   
    
    if ( error_message == "" ) { shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = "Database error",
                                                          keepVisible = TRUE ) } 
    else { shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = error_message, keepVisible = TRUE ) }
    
    DBI::dbDisconnect(conn = my_connection)
  }
  )
}

send_query <- function(query,success_message="",error_message = "", ...) {
  tryCatch(expr = {
    
    db <- config::get("database", file = "inst/golem-config.yml")
    
    my_connection <- DBI::dbConnect(RMySQL::MySQL(), password = db$password, username = db$username, 
                                    dbname = db$dbname, host = db$host)
    
    final_query <- DBI::sqlInterpolate(conn = my_connection, sql = query, ...)
    
    DBI::dbBegin(my_connection)
    
    DBI::dbExecute(conn = my_connection,statement = final_query)
    
    DBI::dbCommit(conn = my_connection)
    
    DBI::dbDisconnect(conn = my_connection)
    
    if (success_message != "") { shinyFeedback::showToast(type = "success",title = "SUCCES",message = success_message,
        .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) }
    
    
  }, error = function(e) {
    DBI::dbRollback(conn = my_connection)
    
   
    
    if ( error_message == "" ) { shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = "Database error",
                                                    keepVisible = TRUE ) } 
    else { shinyFeedback::showToast(message = conditionMessage(e),type = "error",title = error_message, keepVisible = TRUE ) }
    
    DBI::dbDisconnect(conn = my_connection)    
    
  }
  )
  
}



