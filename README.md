
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MIMIC3db

<!-- badges: start -->
<!-- badges: end -->

MIMIC3db provides a tightly bound set of routines to build a SQLite
version of the MIMIC-III v1.4 database on a local network. The package
has routines that then assist the user with loading the MIMIC-III data
into the local database. The package also presents base routines for
accessing data from the database.

## Installation

You can install MIMIC3db with:

``` r
# install.packages("devtools")
devtools::install_github("hdshea/MIMIC3db")
```

## Example

This is a basic example showing the use of some base function with an in
memory example database:

``` r
# library(MIMIC3db)
con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
p <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
a <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
RSQLite::dbWriteTable(con, "PATIENTS", p)
RSQLite::dbWriteTable(con, "ADMISSIONS", a)

# patients <- db_get_from_table(con, "PATIENTS")
# patients
# 
# admissions <- db_get_from_table(con, "ADMISSIONS", where = "WHERE SUBJECT_ID = 10006")
# admissions

RSQLite::dbDisconnect(con)
```
