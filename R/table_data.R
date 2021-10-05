# hack for handling peculiarity of using dplyr and unquoted variable names inside a package
DOB <- DOD <- DOD_HOSP <- DOD_SSN <- VALUE <- NULL

#' Simple table specific reference function for the ADMISSIONS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the ADMISSIONS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/admissions/}{The admissions table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "ADMISSIONS", ex)
#'
#' td <- db_get_admissions(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_admissions <- function(con, ...) {
  db_get_from_table(con, "admissions", ...)
}

#' Simple table specific reference function for the CALLOUT table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the CALLOUT table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/callout/}{The callout table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "CALLOUT", ex)
#'
#' td <- db_get_callout(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_callout <- function(con, ...) {
  db_get_from_table(con, "callout", ...)
}

#' Simple table specific reference function for the CAREGIVERS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the CAREGIVERS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/caregivers/}{The caregivers table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "CAREGIVERS", ex)
#'
#' td <- db_get_caregivers(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_caregivers <- function(con, ...) {
  db_get_from_table(con, "caregivers", ...)
}

#' Simple table specific reference function for the CHARTEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the CHARTEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/chartevents/}{The chartevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "CHARTEVENTS", ex)
#'
#' td <- db_get_chartevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_chartevents <- function(con, ...) {
  db_get_from_table(con, "chartevents", ...)
}

#' Simple table specific reference function for the CPTEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the CPTEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/cptevents/}{The cptevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "CPTEVENTS", ex)
#'
#' td <- db_get_cptevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_cptevents <- function(con, ...) {
  db_get_from_table(con, "cptevents", ...)
}

#' Simple table specific reference function for the DATETIMEEVENTS table data
#'
#' Note that some internal processing is performed to reflect general usage patterns
#' for converting \code{DATE} and \code{TIME} fields to POSIXct date-time format.
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the DATETIMEEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/datetimeevents/}{The datetimeevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011), VALUE = c("2020-10-10", "2020-10-10"))
#' RSQLite::dbWriteTable(con, "DATETIMEEVENTS", ex)
#'
#' td <- db_get_datetimeevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_datetimeevents <- function(con, ...) {
  db_get_from_table(con, "datetimeevents", ...) %>%
    dplyr::mutate(
      VALUE = ifelse(stringr::str_length(VALUE) == 10, stringr::str_c(VALUE, " 00:00:00"), VALUE),
      VALUE = lubridate::ymd_hms(VALUE)
    ) # VALUE column in DATETIMEEVENTS is a DATE
}

#' Simple table specific reference function for the DIAGNOSES_ICD table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the DIAGNOSES_ICD table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/diagnoses_icd/}{The diagnoses_icd table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "DIAGNOSES_ICD", ex)
#'
#' td <- db_get_diagnoses_icd(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_diagnoses_icd <- function(con, ...) {
  db_get_from_table(con, "diagnoses_icd", ...)
}

#' Simple table specific reference function for the DRGCODES table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the DRGCODES table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/drgcodes/}{The drgcodes table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "DRGCODES", ex)
#'
#' td <- db_get_drgcodes(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_drgcodes <- function(con, ...) {
  db_get_from_table(con, "drgcodes", ...)
}

#' Simple table specific reference function for the D_CPT table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the D_CPT table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/d_cpt/}{The d_cpt table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "D_CPT", ex)
#'
#' td <- db_get_d_cpt(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_d_cpt <- function(con, ...) {
  db_get_from_table(con, "d_cpt", ...)
}

#' Simple table specific reference function for the D_ICD_DIAGNOSES table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the D_ICD_DIAGNOSES table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/d_icd_diagnoses/}{The d_icd_diagnoses table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "D_ICD_DIAGNOSES", ex)
#'
#' td <- db_get_d_icd_diagnoses(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_d_icd_diagnoses <- function(con, ...) {
  db_get_from_table(con, "d_icd_diagnoses", ...)
}

#' Simple table specific reference function for the D_ICD_PROCEDURES table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the D_ICD_PROCEDURES table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/d_icd_procedures/}{The d_icd_procedures table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "D_ICD_PROCEDURES", ex)
#'
#' td <- db_get_d_icd_procedures(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_d_icd_procedures <- function(con, ...) {
  db_get_from_table(con, "d_icd_procedures", ...)
}

#' Simple table specific reference function for the D_ITEMS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the D_ITEMS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/d_items/}{The d_items table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "D_ITEMS", ex)
#'
#' td <- db_get_d_items(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_d_items <- function(con, ...) {
  db_get_from_table(con, "d_items", ...)
}

#' Simple table specific reference function for the D_LABITEMS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the D_LABITEMS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/d_labitems/}{The d_labitems table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "D_LABITEMS", ex)
#'
#' td <- db_get_d_labitems(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_d_labitems <- function(con, ...) {
  db_get_from_table(con, "d_labitems", ...)
}

#' Simple table specific reference function for the ICUSTAYS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the ICUSTAYS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/icustays/}{The icustays table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "ICUSTAYS", ex)
#'
#' td <- db_get_icustays(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_icustays <- function(con, ...) {
  db_get_from_table(con, "icustays", ...)
}

#' Simple table specific reference function for the INPUTEVENTS_CV table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the INPUTEVENTS_CV table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/inputevents_cv/}{The inputevents_cv table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "INPUTEVENTS_CV", ex)
#'
#' td <- db_get_inputevents_cv(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_inputevents_cv <- function(con, ...) {
  db_get_from_table(con, "inputevents_cv", ...)
}

#' Simple table specific reference function for the INPUTEVENTS_MV table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the INPUTEVENTS_MV table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/inputevents_mv/}{The inputevents_mv table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "INPUTEVENTS_MV", ex)
#'
#' td <- db_get_inputevents_mv(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_inputevents_mv <- function(con, ...) {
  db_get_from_table(con, "inputevents_mv", ...)
}

#' Simple table specific reference function for the LABEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the LABEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/labevents/}{The labevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "LABEVENTS", ex)
#'
#' td <- db_get_labevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_labevents <- function(con, ...) {
  db_get_from_table(con, "labevents", ...)
}

#' Simple table specific reference function for the MICROBIOLOGYEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the MICROBIOLOGYEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/microbiologyevents/}{The microbiologyevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "MICROBIOLOGYEVENTS", ex)
#'
#' td <- db_get_microbiologyevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_microbiologyevents <- function(con, ...) {
  db_get_from_table(con, "microbiologyevents", ...)
}

#' Simple table specific reference function for the NOTEEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the NOTEEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/noteevents/}{The noteevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "NOTEEVENTS", ex)
#'
#' td <- db_get_noteevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_noteevents <- function(con, ...) {
  db_get_from_table(con, "noteevents", ...)
}

#' Simple table specific reference function for the OUTPUTEVENTS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the OUTPUTEVENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/outputevents/}{The outputevents table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "OUTPUTEVENTS", ex)
#'
#' td <- db_get_outputevents(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_outputevents <- function(con, ...) {
  db_get_from_table(con, "outputevents", ...)
}

#' Simple table specific reference function for the PATIENTS table data
#'
#' Note that some internal processing is performed to reflect general usage patterns
#' for converting \code{DATE} and \code{TIME} fields to POSIXct date-time format.
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the PATIENTS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/patients/}{The patients table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(
#'   SUBJECT_ID = c(10006, 10011),
#'   DOB = c("2020-10-10", "2020-10-10"),
#'   DOD = c("2020-10-10", "2020-10-10"),
#'   DOD_HOSP = c("2020-10-10", "2020-10-10"),
#'   DOD_SSN = c("2020-10-10", "2020-10-10")
#' )
#' RSQLite::dbWriteTable(con, "PATIENTS", ex)
#'
#' td <- db_get_patients(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_patients <- function(con, ...) {
  db_get_from_table(con, "patients", ...) %>%
    dplyr::mutate(
      DOB = ifelse(stringr::str_length(DOB) == 10, stringr::str_c(DOB, " 00:00:00"), DOB),
      DOB = lubridate::ymd_hms(DOB),
      DOD = ifelse(stringr::str_length(DOD) == 10, stringr::str_c(DOD, " 00:00:00"), DOD),
      DOD = lubridate::ymd_hms(DOD),
      DOD_HOSP = ifelse(stringr::str_length(DOD_HOSP) == 10, stringr::str_c(DOD_HOSP, " 00:00:00"), DOD_HOSP),
      DOD_HOSP = lubridate::ymd_hms(DOD_HOSP),
      DOD_SSN = ifelse(stringr::str_length(DOD_SSN) == 10, stringr::str_c(DOD_SSN, " 00:00:00"), DOD_SSN),
      DOD_SSN = lubridate::ymd_hms(DOD_SSN)
    ) # DOB and DOD... columns in PATIENTS are DATEs
}

#' Simple table specific reference function for the PRESCRIPTIONS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the PRESCRIPTIONS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/prescriptions/}{The prescriptions table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "PRESCRIPTIONS", ex)
#'
#' td <- db_get_prescriptions(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_prescriptions <- function(con, ...) {
  db_get_from_table(con, "prescriptions", ...)
}

#' Simple table specific reference function for the PROCEDUREEVENTS_MV table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the PROCEDUREEVENTS_MV table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/procedureevents_mv/}{The procedureevents_mv table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "PROCEDUREEVENTS_MV", ex)
#'
#' td <- db_get_procedureevents_mv(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_procedureevents_mv <- function(con, ...) {
  db_get_from_table(con, "procedureevents_mv", ...)
}

#' Simple table specific reference function for the PROCEDURES_ICD table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the PROCEDURES_ICD table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/procedures_icd/}{The procedures_icd table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "PROCEDURES_ICD", ex)
#'
#' td <- db_get_procedures_icd(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_procedures_icd <- function(con, ...) {
  db_get_from_table(con, "procedures_icd", ...)
}

#' Simple table specific reference function for the SERVICES table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the SERVICES table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/services/}{The services table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "SERVICES", ex)
#'
#' td <- db_get_services(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_services <- function(con, ...) {
  db_get_from_table(con, "services", ...)
}

#' Simple table specific reference function for the TRANSFERS table data
#'
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @param ... an optional `where` parameter with a character string representing a SQL \code{WHERE} clause.
#'
#' @return a tibble with the results.
#' @export
#'
#' @seealso For an overview of the MIMIC-III database, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/}{Overview of the MIMIC-III data}.
#' For details on the TRANSFERS table, see:
#' \href{https://mimic.mit.edu/docs/iii/tables/transfers/}{The transfer table}.
#'
#' @examples
#' con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#' ex <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
#' RSQLite::dbWriteTable(con, "TRANSFERS", ex)
#'
#' td <- db_get_transfers(con, where = "WHERE SUBJECT_ID = 10006")
#'
#' RSQLite::dbDisconnect(con)
db_get_transfers <- function(con, ...) {
  db_get_from_table(con, "transfers", ...)
}
