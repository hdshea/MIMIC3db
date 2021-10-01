
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MIMIC3db

<!-- badges: start -->
<!-- badges: end -->

This work references data on from the Medical Information Mart for
Intensive Care MIMIC-III database v1.4. MIMIC-III is a large,
freely-available database comprising de-identified health-related data
from patients who were admitted to the critical care units of the Beth
Israel Deaconess Medical Center from 2001-2019. Detailed information can
be obtained on the [MIMIC-III website](https://mimic.mit.edu/docs/iii/).

MIMIC3db provides a tightly bound set of routines to build a SQLite
version of the [MIMIC-III
v1.4](https://physionet.org/content/mimiciii/1.4/) database on a local
network. The package has routines that then assist the user with loading
the MIMIC-III data into the local database. The package also presents
base routines for accessing data from the database.

## Installation

You can install MIMIC3db from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("hdshea/MIMIC3db")
```

## Basic Usage

Assuming that you have loaded the appropriate version of the MIMIC-III
database into a database at `db/MIMIC-III.db`, then the following code
will set up a connection `con` to that database.

``` r
library(MIMIC3db)

base_dir <- here::here("")
db_file <- fs::path(base_dir, "db/MIMIC-III.db")
if(RSQLite::dbCanConnect(RSQLite::SQLite(), db_file)) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
} else {
    stop(stringr::str_c("Database file: ", db_file, " not found.", sep=""))
}
```

## Building the database

Assuming you are in the base directory of an RStudio project that has a
`db` directory defined in it and that you have loaded the SQL scripts
from this GitHub directory, the following code run in the terminal from
the base directory will create the base tables for the MIMIC-III v1.4
database and tables.

``` bash
cd db
sqlite3 MIMIC-III.db < mimic3_create_scipt.sql
cd ..
```

This is a basic example showing the use of some base function with an in
memory example database:

``` r
library(MIMIC3db)
con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
p <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
a <- tibble::tibble(SUBJECT_ID = c(10006, 10011))
RSQLite::dbWriteTable(con, "PATIENTS", p)
RSQLite::dbWriteTable(con, "ADMISSIONS", a)

patients <- db_get_from_table(con, "PATIENTS")
patients
#> # A tibble: 2 × 1
#>   SUBJECT_ID
#>        <dbl>
#> 1      10006
#> 2      10011

admissions <- db_get_from_table(con, "ADMISSIONS", where = "WHERE SUBJECT_ID = 10006")
admissions
#> # A tibble: 1 × 1
#>   SUBJECT_ID
#>        <dbl>
#> 1      10006

RSQLite::dbDisconnect(con)
```
