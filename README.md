
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

## Building the database

Assumptions:

1.  you are in the base directory of an RStudio project that has a
    `data` directory defined in it,
2.  you have loaded you have loaded the SQL scripts from [this GitHub
    directory](https://github.com/hdshea/MIMIC3db/tree/main/data) into
    the `data` directory, and
3.  you have loaded the MIMIC-III v1.4 data files into the `data`
    directory

These assumptions having been met, the following code run in the
terminal from the base directory will

1.  create the database and base tables for MIMIC-III v1.4
2.  load the data into the tables, and
3.  clean up some data columns and create the indexes for the tables.

NOTE: the zip file for the base data tables is 6.2 GB and the built
database on my MacBook Pro M1 local hard drive is 85.6 GB.

``` bash
cd data
sqlite3 MIMIC-III.db < mimic3_create_script.sql
sqlite3 MIMIC-III.db < mimic3_load_small_tables.sql
sqlite3 MIMIC-III.db < mimic3_load_CHARTEVENTS.sql
sqlite3 MIMIC-III.db < mimic3_load_DATETIMEEVENTS.sql
sqlite3 MIMIC-III.db < mimic3_load_INPUTEVENTS_CV.sql
sqlite3 MIMIC-III.db < mimic3_load_INPUTEVENTS_MV.sql
sqlite3 MIMIC-III.db < mimic3_load_LABEVENTS.sql
sqlite3 MIMIC-III.db < mimic3_load_NOTEEVENTS.sql
sqlite3 MIMIC-III.db < mimic3_load_OUTPUTEVENTS.sql
sqlite3 MIMIC-III.db < mimic3_load_PRESCRIPTIONS.sql
sqlite3 MIMIC-III.db < fix_data_fields.sql
sqlite3 MIMIC-III.db < mimic3_add_indexes.sql
cd ..
```

This code then shows the tables in the new database.

``` r
RSQLite::dbListTables(con)
#>  [1] "ADMISSIONS"         "CALLOUT"            "CAREGIVERS"        
#>  [4] "CHARTEVENTS"        "CPTEVENTS"          "DATETIMEEVENTS"    
#>  [7] "DIAGNOSES_ICD"      "DRGCODES"           "D_CPT"             
#> [10] "D_ICD_DIAGNOSES"    "D_ICD_PROCEDURES"   "D_ITEMS"           
#> [13] "D_LABITEMS"         "ICUSTAYS"           "INPUTEVENTS_CV"    
#> [16] "INPUTEVENTS_MV"     "LABEVENTS"          "MICROBIOLOGYEVENTS"
#> [19] "NOTEEVENTS"         "OUTPUTEVENTS"       "PATIENTS"          
#> [22] "PRESCRIPTIONS"      "PROCEDUREEVENTS_MV" "PROCEDURES_ICD"    
#> [25] "SERVICES"           "TRANSFERS"
```

## Basic Usage

Assuming that you have loaded the appropriate version of the MIMIC-III
database into a database at `sata/MIMIC-III.db`, then the following code
will set up a connection `con` to that database.

``` r
library(MIMIC3db)

base_dir <- here::here("")
db_file <- fs::path(base_dir, "data/MIMIC-III.db")
if(RSQLite::dbCanConnect(RSQLite::SQLite(), db_file)) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
} else {
    stop(stringr::str_c("Database file: ", db_file, " not found.", sep=""))
}
```

This is a basic example showing the use of the two base functions.

``` r
patients <- db_select_data(con, "SELECT * FROM PATIENTS WHERE SUBJECT_ID = 10006")
patients
#> # A tibble: 0 × 8
#> # … with 8 variables: ROW_ID <int>, SUBJECT_ID <int>, GENDER <chr>, DOB <dbl>,
#> #   DOD <dbl>, DOD_HOSP <dbl>, DOD_SSN <dbl>, EXPIRE_FLAG <int>

admissions <- db_get_from_table(con, "ADMISSIONS", where = "WHERE SUBJECT_ID = 10006")
admissions
#> # A tibble: 0 × 18
#> # … with 18 variables: SUBJECT_ID <int>, HADM_ID <int>, ADMITTIME <dttm>,
#> #   DISCHTIME <dttm>, DEATHTIME <dttm>, ADMISSION_TYPE <chr>,
#> #   ADMISSION_LOCATION <chr>, DISCHARGE_LOCATION <chr>, INSURANCE <chr>,
#> #   LANGUAGE <chr>, RELIGION <chr>, MARITAL_STATUS <chr>, ETHNICITY <chr>,
#> #   EDREGTIME <dttm>, EDOUTTIME <dttm>, DIAGNOSIS <chr>,
#> #   HOSPITAL_EXPIRE_FLAG <int>, HAS_CHARTEVENTS_DATA <int>
```

(NOTE: The example database I use for testing has only empty tables.
That is why there are no results returned in the examples calss above.)

This is how you disconnect from the database when finished.

``` r
RSQLite::dbDisconnect(con)
```
