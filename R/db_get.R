#' @importFrom magrittr %>%
NULL

# hack for handling peculiarity of using dplyr and unquoted variable names inside a package
ROW_ID <- NULL

#' Basic SELECT statement wrapper returning results in a tibble
#'
#' Wrapper around DBI and formatting functions to execute a \code{SELECT} statement
#' and return the results in a tibble.
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param select_statement a character string representing a SQL \code{SELECT} statement.
#'
#' @returns a tibble with the results.
#' @export
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' p <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' a <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "PATIENTS", p)
#' RSQLite::dbWriteTable(con, "ADMISSIONS", a)
#'
#' patients <- db_select_data(con, "SELECT * FROM PATIENTS")
#' admissions <- db_select_data(con, "SELECT * FROM ADMISSIONS WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_select_data <- function(con, select_statement) {
  res <- DBI::dbSendQuery(con, select_statement)
  rval <- tibble::tibble(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  rval
}

#' Simple generic SELECT * wrapper for extracting data from base tables
#'
#' Helper function to perform a basic \code{SELECT *} from the noted table projected down by the optional
#' where clause.  Note that some internal processing is performed to reflect general usage patterns
#' including removing the \code{ROW_ID} column and converting \code{DATE} and \code{TIME} fields to
#' POSIXct date-time format.
#'
#' @param con  A DBIConnection object, as returned by dbConnect().
#' @param table a character string representing a valit MIMIC-III table name.
#' @param where a character string representing a SQL \code{WHERE} clause.
#'
#' @returns a tibble with the results.
#' @export
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' p <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' a <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "PATIENTS", p)
#' RSQLite::dbWriteTable(con, "ADMISSIONS", a)
#'
#' patients <- db_get_from_table(con, "PATIENTS")
#' admissions <- db_get_from_table(con, "ADMISSIONS", where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_from_table <- function(con, table, where = NULL) {
  db_select_data(con, stringr::str_c("SELECT * FROM", table, where, sep = " ")) %>%
    dplyr::select(-ROW_ID) %>% # default to removing the ROW_ID internal primary key
    dplyr::mutate(
      # all columns ending in DATE convert to R DateTime - adjust format first if needed
      dplyr::across(
        tidyselect::ends_with("DATE"),
        function(x) {
          ifelse(stringr::str_length(x) == 10,
            stringr::str_c(x, " 00:00:00"),
            x
          )
        }
      ),
      dplyr::across(tidyselect::ends_with("DATE"), lubridate::ymd_hms),
      # all columns ending in TIME convert to R DateTime - adjust format first if needed
      dplyr::across(
        tidyselect::ends_with("TIME"),
        function(x) {
          ifelse(stringr::str_length(x) == 10,
            stringr::str_c(x, " 00:00:00"),
            x
          )
        }
      ),
      dplyr::across(tidyselect::ends_with("TIME"), lubridate::ymd_hms)
    )
}
