test_that("db_select_data works", {
  db_file <- system.file("extdata", "MIMIC-III.db", package = "MIMIC3db")
  if (RSQLite::dbCanConnect(RSQLite::SQLite(), db_file)) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
  } else {
    stop(stringr::str_c("Database file: ", db_file, " not found.", sep = ""))
  }

  expect_equal(
    t_patients_raw,
    db_select_data(con, "SELECT SUBJECT_ID, GENDER, DOB, DOD, DOD_HOSP, DOD_SSN, EXPIRE_FLAG FROM PATIENTS")
  )

  RSQLite::dbDisconnect(con)
})
