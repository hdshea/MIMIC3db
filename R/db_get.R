#' Basic SELECT statement wrapper returning results in a tibble
#'

db_select_data <- function(con, select_statement) {
    res <- dbSendQuery(con, select_statement)
    rval <- tibble::tibble(dbFetch(res))
    dbClearResult(res)
    rm(res)
    rval
}

#' Simple generic SELECT from base tables
#' - note that we do some internal processing to reflect general usage patterns
#'

db_get_from_table <- function(con, table, where = NULL) {
    db_select_data(con, str_c("SELECT * FROM", table, where, sep = " ")) %>%
        select(-ROW_ID)  %>%  # default to removing the ROW_ID internal primary key
        mutate(
            # all columns ending in DATE convert to R DateTime - adjust format first if needed
            across(ends_with("DATE"), function(x) { ifelse(str_length(x) == 10, str_c(x," 00:00:00"), x) }),
            across(ends_with("DATE"), ymd_hms),
            # all columns ending in TIME convert to R DateTime - adjust format first if needed
            across(ends_with("TIME"), function(x) { ifelse(str_length(x) == 10, str_c(x," 00:00:00"), x) }),
            across(ends_with("TIME"), ymd_hms)
        )
}

