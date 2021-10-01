test_that("db_get_from_table works", {
  base_dir <- here::here("")
  cat(base_dir)
  cat("\n")
  cat(getwd())
  cat("\n")
  load(fs::path(base_dir, "tests/t_data.RData"))

  db_file <- fs::path(base_dir, "tests/MIMIC-III.db")
  if (RSQLite::dbCanConnect(RSQLite::SQLite(), db_file)) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
  } else {
    stop(stringr::str_c("Database file: ", db_file, " not found.", sep = ""))
  }

  patients <- db_get_from_table(con, "PATIENTS")

  RSQLite::dbDisconnect(con)

  expect_equal(t_patients_raw, patients)
})
