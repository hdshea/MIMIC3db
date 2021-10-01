test_that("db_select_data works", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  p <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
  RSQLite::dbWriteTable(con, "PATIENTS", p)

  patients <- db_get_from_table(con, "PATIENTS")

  RSQLite::dbDisconnect(con)

  expect_equal(p, patients)
})
